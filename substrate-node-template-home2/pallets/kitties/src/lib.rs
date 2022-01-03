#![cfg_attr(not(feature = "std"), no_std)]

pub use pallet::*;

#[cfg(test)]
mod mock;

#[cfg(test)]
mod tests;

#[frame_support::pallet]
pub mod pallet {
	use codec::{Decode, Encode, EncodeLike};
	use frame_support::{
		dispatch::DispatchResult,
		pallet_prelude::*,
		traits::{Currency, ExistenceRequirement, Randomness, ReservableCurrency},
		transactional,
	};
	use frame_system::pallet_prelude::*;
	use num_traits::bounds::Bounded;
	use scale_info::TypeInfo;
	use sp_io::hashing::blake2_128;
	use sp_runtime::traits::{AtLeast32Bit, CheckedAdd, One};

	#[derive(Clone, Encode, Decode, PartialEq, RuntimeDebug, TypeInfo)]
	#[scale_info(skip_type_params(T))]
	pub struct Kitty<T: Config> {
		pub dna: [u8; 16],
		pub owner: AccountOf<T>,
	}

	type AccountOf<T> = <T as frame_system::Config>::AccountId;
	type BalanceOf<T> =
		<<T as Config>::Currency as Currency<<T as frame_system::Config>::AccountId>>::Balance;

	#[pallet::config]
	pub trait Config: frame_system::Config {
		type Event: From<Event<Self>> + IsType<<Self as frame_system::Config>::Event>;
		type Randomness: Randomness<Self::Hash, Self::BlockNumber>;
		type Currency: Currency<Self::AccountId> + ReservableCurrency<Self::AccountId>;
		type KittyIndex: Parameter + Default + AtLeast32Bit + Copy + Bounded + EncodeLike;

		#[pallet::constant]
		type StakeAmountForKitty: Get<BalanceOf<Self>>;
	}

	#[pallet::pallet]
	#[pallet::generate_store(pub(super) trait Store)]
	pub struct Pallet<T>(_);

	#[pallet::event]
	#[pallet::generate_deposit(pub(super) fn deposit_event)]
	pub enum Event<T: Config> {
		/// A new Kitty was sucessfully created.
		Created(T::AccountId, T::KittyIndex),
		/// Kitty price was sucessfully set and on sell.
		OnSell(T::AccountId, T::KittyIndex, Option<BalanceOf<T>>),
		/// A Kitty was sucessfully transferred.
		Transferred(T::AccountId, T::AccountId, T::KittyIndex),
		/// A Kitty was sucessfully bought.
		Bought(T::AccountId, T::AccountId, T::KittyIndex, Option<BalanceOf<T>>),
	}

	#[pallet::storage]
	#[pallet::getter(fn kitties_count)]
	pub type KittiesCount<T: Config> = StorageValue<_, T::KittyIndex>;

	#[pallet::storage]
	#[pallet::getter(fn owner)]
	pub type Owner<T: Config> =
		StorageMap<_, Blake2_128Concat, T::KittyIndex, Option<T::AccountId>, ValueQuery>;

	#[pallet::storage]
	#[pallet::getter(fn kitties)]
	pub type Kitties<T: Config> =
		StorageMap<_, Blake2_128Concat, T::KittyIndex, Option<Kitty<T>>, ValueQuery>;

	#[pallet::storage]
	#[pallet::getter(fn kitties_list_for_sale)]
	pub type ListForSale<T: Config> =
		StorageMap<_, Blake2_128Concat, T::KittyIndex, Option<BalanceOf<T>>, ValueQuery>;

	#[pallet::error]
	pub enum Error<T> {
		/// Handles arithemtic overflow when incrementing the Kitty counter.
		KittiesCountOverflow,
		/// Buyer cannot be the owner.
		BuyerIsKittyOwner,
		/// Handles checking whether the Kitty exists.
		KittyNotExist,
		/// Handles checking that the Kitty is owned by the account transferring, buying or setting a price for it.
		NotKittyOwner,
		/// Ensures the Kitty is on sale.
		KittyNotOnSale,
		/// Ensures that an account has enough funds to purchase a Kitty.
		NotEnoughBalance,
		/// Ensures has enough balance to stake.
		NotEnoughBalanceForStaking,
	}

	#[pallet::call]
	impl<T: Config> Pallet<T> {
		#[pallet::weight(0)]
		pub fn create(origin: OriginFor<T>) -> DispatchResult {
			let who = ensure_signed(origin)?;
			let kitty_id = Self::mint(&who, None)?;
			Self::deposit_event(Event::Created(who, kitty_id));

			Ok(())
		}

		#[pallet::weight(100)]
		pub fn transfer(
			origin: OriginFor<T>,
			to: T::AccountId,
			kitty_id: T::KittyIndex,
		) -> DispatchResult {
			let who = ensure_signed(origin)?;
			ensure!(Self::is_kitty_owner(&kitty_id, &who)?, <Error<T>>::NotKittyOwner);
			Self::transfer_kitty_to(&kitty_id, &to)?;
			Self::deposit_event(Event::Transferred(who, to, kitty_id));

			Ok(())
		}

		#[pallet::weight(100)]
		pub fn breed(
			origin: OriginFor<T>,
			kitty_id_1: T::KittyIndex,
			kitty_id_2: T::KittyIndex,
		) -> DispatchResult {
			let who = ensure_signed(origin)?;

			let new_dna = Self::breed_dna(&who, kitty_id_1, kitty_id_2)?;
			let kitty_id = Self::mint(&who, Some(new_dna))?;

			Self::deposit_event(Event::Created(who, kitty_id));

			Ok(())
		}

		#[pallet::weight(100)]
		pub fn sell(
			origin: OriginFor<T>,
			kitty_id: T::KittyIndex,
			price: Option<BalanceOf<T>>,
		) -> DispatchResult {
			let sender = ensure_signed(origin)?;
			if !<Kitties<T>>::contains_key(&kitty_id) {
				Err(<Error<T>>::KittyNotExist)?;
			}
			ensure!(Self::is_kitty_owner(&kitty_id, &sender)?, <Error<T>>::NotKittyOwner);

			// Set price before sell.
			ListForSale::<T>::try_mutate(kitty_id, |p| -> DispatchResult {
				*p = price;
				Ok(().into())
			})?;

			Self::deposit_event(Event::OnSell(sender, kitty_id, price));

			Ok(())
		}

		#[transactional]
		#[pallet::weight(100)]
		pub fn buy_kitty(
			origin: OriginFor<T>,
			kitty_id: T::KittyIndex,
			// bid_price: BalanceOf<T>,
		) -> DispatchResult {
			let buyer = ensure_signed(origin)?;

			// Check the kitty exists and buyer is not the current kitty owner.
			let kitty = Self::kitties(&kitty_id).ok_or(<Error<T>>::KittyNotExist)?;
			ensure!(kitty.owner != buyer, <Error<T>>::BuyerIsKittyOwner);

			// Check the buyer has enough free balance.
			let ask_price =
				Self::kitties_list_for_sale(&kitty_id).ok_or(<Error<T>>::KittyNotOnSale)?;
			let stake = T::StakeAmountForKitty::get();
			let free_balance = T::Currency::free_balance(&buyer);
			ensure!(free_balance > (ask_price + stake), <Error<T>>::NotEnoughBalance);

			// Transfer the amount from buyer to seller.
			let seller = kitty.owner.clone();
			T::Currency::transfer(&buyer, &seller, ask_price, ExistenceRequirement::KeepAlive)?;

			// Transfer the kitty from seller to buyer.
			Self::transfer_kitty_to(&kitty_id, &buyer)?;

			// Remove kitty from sale list.
			ListForSale::<T>::remove(kitty_id);

			Self::deposit_event(Event::Bought(buyer, seller, kitty_id, Some(ask_price)));

			Ok(())
		}
	}

	impl<T: Config> Pallet<T> {
		fn is_kitty_owner(kitty_id: &T::KittyIndex, acct: &T::AccountId) -> Result<bool, Error<T>> {
			match Self::kitties(kitty_id) {
				Some(kitty) => Ok(kitty.owner == *acct),
				None => Err(<Error<T>>::KittyNotExist),
			}
		}

		fn random_value(sender: &T::AccountId) -> [u8; 16] {
			let payload = (
				T::Randomness::random_seed(),
				&sender,
				<frame_system::Pallet<T>>::extrinsic_index(),
			);
			payload.using_encoded(blake2_128)
		}

		pub fn mint(
			owner: &T::AccountId,
			dna: Option<[u8; 16]>,
		) -> Result<T::KittyIndex, Error<T>> {
			let dna_inner: [u8; 16];
			if let Some(v) = dna {
				dna_inner = v;
			} else {
				dna_inner = Self::random_value(&owner);
			}
			let kitty = Kitty::<T> { dna: dna_inner, owner: owner.clone() };

			let kitty_id = match Self::kitties_count() {
				Some(id) => {
					ensure!(id != T::KittyIndex::max_value(), Error::<T>::KittiesCountOverflow);
					id
				}
				None => T::KittyIndex::min_value(),
			};

			let stake = T::StakeAmountForKitty::get();
			T::Currency::reserve(&owner, stake)
				.map_err(|_| Error::<T>::NotEnoughBalanceForStaking)?;

			Kitties::<T>::insert(kitty_id, Some(kitty));
			Owner::<T>::insert(kitty_id, Some(owner));

			let one = T::KittyIndex::one();
			let next_kitty_index = kitty_id
				.checked_add(&one)
				.ok_or(one)
				.map_err(|_| <Error<T>>::KittiesCountOverflow)?;
			KittiesCount::<T>::put(next_kitty_index);

			Ok(kitty_id)
		}

		#[transactional]
		pub fn transfer_kitty_to(
			kitty_id: &T::KittyIndex,
			to: &T::AccountId,
		) -> Result<(), Error<T>> {
			let mut kitty = Self::kitties(&kitty_id).ok_or(<Error<T>>::KittyNotExist)?;
			let pre_owner = kitty.owner;

			// Update the kitty owner.
			kitty.owner = to.clone();

			// Staking from new owner and unstaking from the pre ownder
			let stake = T::StakeAmountForKitty::get();
			T::Currency::reserve(&to, stake).map_err(|_| Error::<T>::NotEnoughBalanceForStaking)?;
			T::Currency::unreserve(&pre_owner, stake);

			// Inert kitty.
			Kitties::<T>::insert(kitty_id, Some(kitty));

			// Insert new owner.
			Owner::<T>::insert(kitty_id, Some(to.clone()));

			Ok(())
		}

		fn breed_dna(
			who: &T::AccountId,
			parent1: T::KittyIndex,
			parent2: T::KittyIndex,
		) -> Result<[u8; 16], Error<T>> {
			let dna1 = Self::kitties(parent1).ok_or(<Error<T>>::KittyNotExist)?.dna;
			let dna2 = Self::kitties(parent2).ok_or(<Error<T>>::KittyNotExist)?.dna;

			let selector = Self::random_value(&who);
			let mut new_dna = [0u8; 16];

			for i in 0..dna1.len() {
				new_dna[i] = (selector[i] & dna1[i]) | (!selector[i] & dna2[i]);
			}

			Ok(new_dna)
		}
	}
}
