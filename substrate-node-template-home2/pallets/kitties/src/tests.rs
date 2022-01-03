use super::*;
use crate::mock::{new_test_ext, Event as MockEvent, KittiesModule, Origin, System, Test};
use frame_support::{assert_noop, assert_ok};

#[test]
fn test_create() {
	new_test_ext().execute_with(|| {
		assert_ok!(KittiesModule::create(Origin::signed(1)));
		assert_eq!(KittiesCount::<Test>::get(), Some(1));
		assert_eq!(Owner::<Test>::get(0), Some(1));
		System::assert_has_event(MockEvent::KittiesModule(Event::Created(1, 0)));
	});
}

#[test]
fn test_transfer() {
	new_test_ext().execute_with(|| {
		assert_ok!(KittiesModule::create(Origin::signed(1)));
		assert_ok!(KittiesModule::transfer(Origin::signed(1), 2, 0));
		assert_eq!(Owner::<Test>::get(0), Some(2));
		System::assert_has_event(MockEvent::KittiesModule(Event::Transferred(1, 2, 0)));
	});
}

#[test]
fn test_breed() {
	new_test_ext().execute_with(|| {
		assert_ok!(KittiesModule::create(Origin::signed(1)));
		assert_ok!(KittiesModule::create(Origin::signed(2)));
		assert_ok!(KittiesModule::breed(Origin::signed(1), 0, 1));
		assert_eq!(KittiesCount::<Test>::get(), Some(3));
		System::assert_has_event(MockEvent::KittiesModule(Event::Created(1, 2)));
	});
}

#[test]
fn test_sell() {
	new_test_ext().execute_with(|| {
		assert_ok!(KittiesModule::create(Origin::signed(1)));
		let price: u128 = 2000;
		assert_ok!(KittiesModule::sell(Origin::signed(1), 0, Some(price)));
		assert_eq!(ListForSale::<Test>::get(0), Some(price));
		System::assert_has_event(MockEvent::KittiesModule(Event::OnSell(1, 0, Some(price))));
	});
}

#[test]
fn test_buy() {
	new_test_ext().execute_with(|| {
		assert_ok!(KittiesModule::create(Origin::signed(1)));
		let price: u128 = 1_500;
		assert_ok!(KittiesModule::sell(Origin::signed(1), 0, Some(price)));
		assert_ok!(KittiesModule::buy_kitty(Origin::signed(2), 0));
		assert_eq!(Owner::<Test>::get(0), Some(2));
		System::assert_has_event(MockEvent::KittiesModule(Event::Bought(2, 1, 0, Some(price))));
	});
}

#[test]
fn test_kitties_count_overflow() {
	new_test_ext().execute_with(|| {
		KittiesCount::<Test>::put(u32::max_value());
		let account_id: u64 = 1;
		assert_noop!(
			KittiesModule::create(Origin::signed(account_id)),
			Error::<Test>::KittiesCountOverflow
		);
	});
}

#[test]
fn test_buyer_should_not_owner() {
	new_test_ext().execute_with(|| {
		assert_ok!(KittiesModule::create(Origin::signed(1)));
		let price: u128 = 1_500;
		assert_ok!(KittiesModule::sell(Origin::signed(1), 0, Some(price)));
		assert_noop!(
			KittiesModule::buy_kitty(Origin::signed(1), 0),
			Error::<Test>::BuyerIsKittyOwner
		);
	});
}

#[test]
fn test_kitty_not_exist() {
	new_test_ext().execute_with(|| {
		let price: u128 = 1_500;
		assert_noop!(
			KittiesModule::sell(Origin::signed(1), 0, Some(price)),
			Error::<Test>::KittyNotExist
		);
	});
}

#[test]
fn test_not_kitty_owner() {
	new_test_ext().execute_with(|| {
		assert_ok!(KittiesModule::create(Origin::signed(1)));
		assert_noop!(
			KittiesModule::transfer(Origin::signed(2), 3, 0),
			Error::<Test>::NotKittyOwner
		);
	});
}

#[test]
fn test_kitty_not_on_sale() {
	new_test_ext().execute_with(|| {
		assert_ok!(KittiesModule::create(Origin::signed(1)));
		assert_noop!(KittiesModule::buy_kitty(Origin::signed(2), 0), Error::<Test>::KittyNotOnSale);
	});
}

#[test]
fn test_not_enough_balance() {
	new_test_ext().execute_with(|| {
		assert_ok!(KittiesModule::create(Origin::signed(1)));
		let price: u128 = 7_500;
		assert_ok!(KittiesModule::sell(Origin::signed(1), 0, Some(price)));
		assert_noop!(
			KittiesModule::buy_kitty(Origin::signed(3), 0),
			Error::<Test>::NotEnoughBalance
		);
	});
}

#[test]
fn test_not_enough_balance_for_staking() {
	new_test_ext().execute_with(|| {
		assert_noop!(
			KittiesModule::create(Origin::signed(4)),
			Error::<Test>::NotEnoughBalanceForStaking
		);
	});
}