use crate::actions::{
    Action, ActionTag, BuildMethod, CityDistrictTarget, MagicianAction, Resource, WizardMethod,
};
use crate::districts::DistrictName;
use crate::game::{ActionOutput, ActionResult, CityDistrict, Followup, Game, PlayerIndex, Result};
use crate::roles::{Rank, RoleName};
use crate::types::{CardSuit, Marker, PlayerId, PlayerName};

use macros::tag::Tag;
use rand::prelude::*;
use std::borrow::{Borrow, BorrowMut, Cow};
use std::collections::HashMap;
use std::mem;

pub fn perform_action(game: &mut Game, action: &Action) -> ActionResult {
    Ok(match action {
        Action::RevealWarrant => match game.followup {
            Some(Followup::Warrant {
                magistrate,
                gold,
                district,
                signed,
            }) => {
                if !signed {
                    return Err("Cannot reveal unsigned warrant".into());
                }
                if game.players[magistrate.0].city_has(district) {
                    return Err("Cannot confiscate a district you already have.".into());
                }
                let player = game.active_player_mut().unwrap();
                player.gold += gold;
                let magistrate = game.players[magistrate.0].borrow_mut();
                magistrate.city.push(CityDistrict::from(district));
                let name = magistrate.name.clone();

                // clear all remaining warrants
                for c in game.characters.0.iter_mut() {
                    if let Some((i, _)) = c.markers.iter().enumerate().find(|(_, m)| m.is_warrant())
                    {
                        c.markers.remove(i);
                    }
                }

                ActionOutput::new(format!(
                            "The Magistrate ({}) reveals a signed warrant and confiscates the {}; {} gold is refunded.",
                            name,
                            district.data().display_name, 
                            gold
                        )
                    )
            }
            _ => return Err("cannot reveal warrant".into()),
        },

        Action::PayBribe => {
            let blackmailer = game.characters.get(RoleName::Blackmailer).unwrap().player.unwrap();
            let player = game.active_player_mut().unwrap();
            let half = player.gold / 2;
            player.gold -= half;
            game.players[blackmailer.0].gold += half;

            ActionOutput::new(format!(
                "They bribed the Blackmailer ({}) with {} gold.",
                game.players[blackmailer.0].name, half
            ))
        }

        Action::IgnoreBlackmail => {
            //
            let blackmailer= game.characters.get(RoleName::Blackmailer).unwrap().player.unwrap();
            ActionOutput::new("They ignored the blackmail. Waiting on the Blackmailer's response.")
                .followup(Followup::Blackmail { blackmailer })
        }

        Action::RevealBlackmail => match game.followup {
            Some(Followup::Blackmail { blackmailer }) => {
                let is_flowered = game
                    .active_role()?
                    .markers
                    .iter()
                    .any(|marker| *marker == Marker::Blackmail { flowered: true });

                if is_flowered {
                    let target = game.active_player_mut().unwrap();
                    let gold = std::mem::replace(&mut target.gold, 0);
                    game.players[blackmailer.0].gold += gold;

                    // clear all remaining blackmail
                    for c in game.characters.0.iter_mut() {
                        if let Some((i, _)) =
                            c.markers.iter().enumerate().find(|(_, m)| m.is_blackmail())
                        {
                            c.markers.remove(i);
                        }
                    }

                    ActionOutput::new(format!(
                            "The Blackmailer ({}) reveals an active threat, and takes all {} of their gold.", 
                            game.players[blackmailer.0].name,
                            gold 
                        ))
                } else {
                    let name = game.active_player().unwrap().name.clone();
                    ActionOutput::new(format!(
                        "The Blackmailer ({}) reveals an empty threat. Nothing happens.",
                        name
                    ))
                }
            }
            _ => return Err("Cannot reveal blackmail".into()),
        },

        Action::Pass => {
            //
            match game.followup {
                Some(Followup::Warrant {
                    gold,
                    district,
                    magistrate,
                    ..
                }) => {
                    //
                    game.complete_build(game.active_player_index()?, gold, district);
                    ActionOutput::new(format!(
                        "The Magistrate ({}) did not reveal the warrant.",
                        game.players[magistrate.0].name
                    ))
                }
                Some(Followup::Blackmail { blackmailer }) => {
                    //
                    ActionOutput::new(format!(
                        "The Blackmailer ({}) did not reveal the blackmail.",
                        game.players[blackmailer.0].name,
                    ))
                }
                _ => return Err("impossible".into()),
            }
        }

        Action::DraftPick { role } => {
            let draft = game.active_turn.draft_mut()?;

            Game::remove_first(&mut draft.remaining, *role);
            let c = game.characters.get_mut(*role).unwrap();
            c.player = Some(draft.player);
            let player = game.players[draft.player.0].borrow_mut();
            player.roles.push(*role);
            player.roles.sort_by_key(|r| r.rank());

            let output = ActionOutput::new(format!("{} drafts a role.", player.name));

            // the first player to draft in the two player game does not discard.
            // the last pick is between two cards.
            // the one they don't pick is automatically discarded.
            // So really only the middle two draft turns should show the discard button
            if game.players.len() == 2 && (draft.remaining.len() == 5 || draft.remaining.len() == 3)
            {
                output
            } else {
                output.end_turn()
            }
        }

        Action::DraftDiscard { role } => {
            let draft = game.active_turn.draft_mut()?;
            let i = (0..draft.remaining.len())
                .find(|i| draft.remaining[*i] == *role)
                .ok_or("selected role is not available")?;

            draft.remaining.remove(i);
            ActionOutput::new(format!(
                "{} discards a role face down.",
                game.active_player()?.name
            ))
            .end_turn()
        }

        Action::EndTurn => {
            //
            ActionOutput::new(format!("{} ends their turn.", game.active_player()?.name)).end_turn()
        }

        Action::GatherResourceGold => {
            let active = game.active_player_index()?;
            let mut amount = 2;
            let log = if game.players[active.0].city_has(DistrictName::GoldMine) {
                amount += 1;
                format!(
                    "{} gathers 3 gold. (1 extra from their Gold Mine).",
                    game.players[active.0].name
                )
            } else {
                format!("{} gathers 2 gold.", game.players[active.0].name)
            };

            game.players[active.0].gold += amount;

            ActionOutput::new(log).maybe_followup(game.after_gather_resources())
        }

        Action::GatherResourceCards => {
            let mut draw_amount = 2;
            if game.active_player()?.city_has(DistrictName::Observatory) {
                draw_amount += 1;
            }

            let mut drawn = game.deck.draw_many(draw_amount).collect();

            if game.active_player()?.city_has(DistrictName::Library) {
                game.active_player_mut()?.hand.append(&mut drawn);

                ActionOutput::new(format!(
                    "{} gathers cards. With the aid of their library they keep all {} cards.",
                    game.active_player()?.name,
                    draw_amount
                ))
                .maybe_followup(game.after_gather_resources())
            } else {
                let followup = if drawn.len() > 0 {
                    Some(Followup::GatherCardsPick { revealed: drawn })
                } else {
                    game.after_gather_resources()
                };
                ActionOutput::new(format!(
                    "{} reveals {} cards from the top of the deck.",
                    game.active_player()?.name,
                    draw_amount
                ))
                .maybe_followup(followup)
            }
        }

        Action::GatherCardsPick { district } => {
            let mut revealed = if let Some(Followup::GatherCardsPick { revealed, .. }) =
                game.followup.borrow_mut()
            {
                revealed
            } else {
                return Err("action is not allowed".into());
            };

            Game::remove_first(&mut revealed, *district).ok_or("invalid choice")?;
            revealed.shuffle(&mut game.rng);

            for remaining in revealed {
                game.deck.discard_to_bottom(*remaining);
            }
            game.active_player_mut()?.hand.push(*district);
            ActionOutput::new("They pick a card.").maybe_followup(game.after_gather_resources())
        }
        Action::GoldFromNobility => game.gain_gold_for_suit(CardSuit::Noble)?,
        Action::GoldFromReligion => game.gain_gold_for_suit(CardSuit::Religious)?,
        Action::GoldFromTrade => game.gain_gold_for_suit(CardSuit::Trade)?,
        Action::GoldFromMilitary => game.gain_gold_for_suit(CardSuit::Military)?,

        Action::CardsFromNobility => game.gain_cards_for_suit(CardSuit::Noble)?,
        Action::CardsFromReligion => game.gain_cards_for_suit(CardSuit::Religious)?,

        Action::MerchantGainOneGold => {
            let player = game.active_player_mut()?;
            player.gold += 1;
            ActionOutput::new(format!(
                "The Merchant ({}) gains 1 extra gold.",
                player.name
            ))
        }
        Action::ArchitectGainCards => {
            game.gain_cards(2);
            let player = game.active_player()?;
            ActionOutput::new(format!(
                "The Architect ({}) gains 2 extra cards.",
                player.name
            ))
        }

        Action::Build(method) => {
            if game.active_role().unwrap().role == RoleName::Navigator {
                Err("The navigator is not allowed to build.")?;
            }
            let district = match method {
                BuildMethod::Regular { district } => *district,
                BuildMethod::Cardinal { district, .. } => *district,
                BuildMethod::Framework { district } => *district,
                BuildMethod::ThievesDen { .. } => DistrictName::ThievesDen,
                BuildMethod::Necropolis { .. } => DistrictName::Necropolis,
            };

            let active = game.active_player()?;
            if active.hand.iter().all(|d| *d != district) {
                Err("Card not in hand")?;
            }

            if game
                .turn_actions
                .iter()
                .all(|a| !a.tag().is_resource_gathering())
            {
                Err("Must gather resources before building")?;
            }

            let is_free_build = district == DistrictName::Stables
                || (district.data().suit == CardSuit::Trade
                    && game.active_role().unwrap().role == RoleName::Trader);

            if !is_free_build && game.remaining_builds == 0 {
                Err(format!(
                    "With your role, you cannot build more than {} time(s), this turn.",
                    game.active_role().unwrap().role.build_limit()
                ))?;
            }

            if !(active.city_has(DistrictName::Quarry)
                || game.active_role().unwrap().role == RoleName::Wizard)
                && active.city.iter().any(|d| d.name == district)
            {
                return Err("cannot build duplicate".into());
            }

            if district == DistrictName::Monument && active.city.len() >= 5 {
                return Err("You can only build the Monument, if you have less than 5 districts in your city".into());
            }

            let district = district.data();

            let mut cost = district.cost;
            if district.suit == CardSuit::Unique
                && active.city.iter().any(|d| d.name == DistrictName::Factory)
            {
                cost -= 1;
            }

            match method {
                BuildMethod::Regular { .. } => {
                    if cost > active.gold {
                        Err("Not enough gold")?;
                    }

                    let active = game.active_player_mut()?;
                    Game::remove_first(&mut active.hand, district.name)
                        .ok_or("card not in hand")?;
                    active.gold -= cost;
                }
                BuildMethod::Cardinal {
                    discard, player, ..
                } => {
                    if game.active_role()?.role != RoleName::Cardinal {
                        Err("You are not the cardinal")?;
                    }
                    if active.gold + discard.len() < cost {
                        Err("Not enough gold or discarded")?;
                    }

                    if active.gold + discard.len() > cost {
                        Err("Must spend own gold first, before taking from others")?;
                    }

                    let target = game
                        .players
                        .iter()
                        .find_map(|p| {
                            if p.name == *player {
                                Some(p.index)
                            } else {
                                None
                            }
                        })
                        .ok_or("Player does not exist")?;

                    if game.players[target.0].gold < discard.len() {
                        Err("Cannot give more cards than the target has gold")?;
                    }

                    let gold = discard.len();
                    cost -= gold;
                    let mut discard = discard.clone();
                    let mut copy = discard.clone();
                    let mut new_hand = Vec::with_capacity(active.hand.len());
                    for card in active.hand.iter() {
                        if let Some((i, _)) = discard.iter().enumerate().find(|(_, d)| *d == card) {
                            discard.swap_remove(i);
                        } else {
                            new_hand.push(*card);
                        }
                    }

                    if discard.len() > 0 {
                        Err("Can't discard cards not in your hand")?;
                    }

                    Game::remove_first(&mut new_hand, district.name).ok_or("card not in hand")?;

                    let active = game.active_player_mut().unwrap();
                    active.gold -= cost;
                    active.hand = new_hand;

                    let target = game.players[target.0].borrow_mut();
                    target.gold -= gold;
                    target.hand.append(&mut copy);
                }

                BuildMethod::ThievesDen { discard } => {
                    if district.name != DistrictName::ThievesDen {
                        Err("You are not building the ThievesDen!")?;
                    }
                    if discard.len() > cost {
                        Err("Cannot discard more cards than the cost")?;
                    }

                    if active.gold + discard.len() < cost {
                        Err("Not enough gold or cards discarded")?;
                    }

                    cost -= discard.len();
                    let mut discard_set = discard.clone();
                    let mut new_hand = Vec::with_capacity(active.hand.len());
                    for card in active.hand.iter() {
                        if let Some((i, _)) =
                            discard_set.iter().enumerate().find(|(_, d)| *d == card)
                        {
                            discard_set.swap_remove(i);
                        } else {
                            new_hand.push(*card);
                        }
                    }

                    if discard_set.len() > 0 {
                        Err("Can't discard cards not in your hand")?;
                    }
                    Game::remove_first(&mut new_hand, district.name).ok_or("card not in hand")?;

                    let active = game.active_player_mut().unwrap();
                    active.gold -= cost;
                    active.hand = new_hand;
                    for card in discard {
                        game.deck.discard_to_bottom(*card);
                    }
                }

                BuildMethod::Framework { .. } => {
                    let city_index = active
                        .city
                        .iter()
                        .enumerate()
                        .find_map(|(i, c)| {
                            if c.name == DistrictName::Framework {
                                Some(i)
                            } else {
                                None
                            }
                        })
                        .ok_or("You don't own a framework!")?;

                    let active = game.active_player_mut().unwrap();
                    Game::remove_first(&mut active.hand, district.name)
                        .ok_or("card not in hand")?;
                    active.city.swap_remove(city_index);
                }

                BuildMethod::Necropolis { sacrifice: target } => {
                    if district.name != DistrictName::Necropolis {
                        Err("You are not building the necropolis!")?;
                    }
                    let city_index = active
                        .city
                        .iter()
                        .enumerate()
                        .find_map(|(i, c)| {
                            if c.name == target.district && c.beautified == target.beautified {
                                Some(i)
                            } else {
                                None
                            }
                        })
                        .ok_or("Cannot sacrifice a district you don't own!")?;

                    let active = game.active_player_mut().unwrap();
                    Game::remove_first(&mut active.hand, district.name)
                        .ok_or("card not in hand")?;

                    let district = active.city.swap_remove(city_index);
                    game.discard_district(district.name);
                }
            }

            if !is_free_build {
                game.remaining_builds -= 1;
            }

            if game.active_role().unwrap().role != RoleName::TaxCollector
                && game.characters.has_tax_collector()
            {
                let player = game.active_player_mut()?;
                if player.gold > 0 {
                    player.gold -= 1;
                    game.tax_collector += 1;
                }
            }

            // the magistrate can only confiscate the first build of a turn
            if game.active_role().unwrap().has_warrant()
                && !game.turn_actions.iter().any(|act| act.is_build())
            {
                ActionOutput::new(format!(
                    "{} begins to build a {}; waiting on the Magistrate's response.",
                    game.active_player().unwrap().name,
                    district.display_name
                ))
                .followup(Followup::Warrant {
                    magistrate: game.characters.get(RoleName::Magistrate).unwrap().player.unwrap(),
                    gold: cost,
                    district: district.name,
                    signed: game
                        .active_role()
                        .unwrap()
                        .markers
                        .iter()
                        .any(|m| *m == Marker::Warrant { signed: true }),
                })
            } else {
                game.complete_build(game.active_player().unwrap().index, cost, district.name);
                ActionOutput::new(format!(
                    "{} build a {}.",
                    game.active_player().unwrap().name,
                    district.display_name
                ))
            }
        }

        Action::TakeCrown => {
            // King & patrician always get the crown, even when bewitched.
            game.crowned = game
                .characters
                .get(RoleName::King).or(game.characters.get(RoleName::Patrician))
                .and_then(|game_role| game_role.player)
                .ok_or("No Royalty to take crown!")?;

            ActionOutput::new(format!(
                "{} takes the crown.",
                game.players[game.crowned.0].name,
            ))
        }

        Action::Assassinate { role } => {
            if *role == RoleName::Assassin {
                return Err("Cannot kill self.".into());
            }
            let target = match game.characters.get_mut(*role) {
                Some(target) => target,
                None => return Err(format!("Role {} is not in this game",role.display_name()).into())
            };
            target.markers.push(Marker::Killed);

            ActionOutput::new(format!(
                "The Assassin ({}) kills the {}; Their turn will be skipped.",
                game.active_player()?.name,
                role.display_name(),
            ))
        }

        Action::Steal { role } => {
            if *role == RoleName::Thief {
                return Err("Cannot steal from self.".into());
            }

            let target = match game.characters.get_mut(*role) {
                Some(target) => target,
                None => return Err(format!("Role {} is not in this game",role.display_name()).into())
            };

            if target.revealed {
                return Err(format!("Cannot steal from {} who has already taken their turn.",role.display_name()).into())
            }

            if target
                .markers
                .iter()
                .any(|marker| *marker == Marker::Killed)
            {
                return Err("Cannot rob from the dead.".into());
            }

            if target
                .markers
                .iter()
                .any(|marker| *marker == Marker::Bewitched)
            {
                return Err("Cannot rob from the bewitched.".into());
            }

            target.markers.push(Marker::Robbed);

            ActionOutput::new(format!( 
                    "The Thief ({}) robs the {}; At the start of their turn, all their gold will be taken.",
                    game.active_player()?.name,
                    role.display_name(),
                ))
        }

        Action::Magic(MagicianAction::TargetPlayer { player }) => {

            let mut hand = std::mem::take(&mut game.active_player_mut()?.hand);
            let target =  match game.players.iter_mut().find(|p| p.name == *player) {
                Some(target) => target,
None => 
                return Err("Invalid target.".into())
            };
            let hand_count = hand.len();
            let target_count = target.hand.len();

            std::mem::swap(&mut hand, &mut target.hand);
            game.active_player_mut()?.hand = hand;

            ActionOutput::new(format!(
                "The Magician ({}) swaps their hand of {} cards with {}'s hand of {} cards.",
                game.active_player()?.name,
                hand_count,
                player,
                target_count,
            ))
        }

        Action::Magic(MagicianAction::TargetDeck { district }) => {
            let active = game.active_player_mut()?;
            let mut discard = district.clone();
            let mut new_hand = Vec::with_capacity(active.hand.len());

            for card in active.hand.iter() {
                if let Some((i, _)) = discard.iter().enumerate().find(|(_, d)| *d == card) {
                    discard.swap_remove(i);
                } else {
                    new_hand.push(*card);
                }
            }

            if discard.len() > 0 {
                Err("Can't discard cards not in your hand")?;
            }
            active.hand = new_hand;

            for card in district.iter() {
                game.deck.discard_to_bottom(*card);
            }

            let gained = game.gain_cards(district.len());

            ActionOutput::new(format!(
                "The Magician ({}) discarded {} cards and drew {} more.",
                game.active_player()?.name,
                district.len(),
                gained
            ))
        }

        Action::WarlordDestroy { district: target } => {
            if target.district == DistrictName::Keep {
                return Err("Cannot target the Keep".into());
            }

            let available_gold = game.active_player()?.gold;
            let complete_size = game.complete_city_size();
            let player = game
                .players
                .iter_mut()
                .find(|p| p.name == target.player)
                .ok_or("Player does not exist")?;

            if game.characters.has_bishop_protection(player.index) {
                Err("Cannot target the Bishop")?
            }
            if player.city_size() >= complete_size {
                Err("Cannot target a completed city")?
            }

            let city_index = player
                .city
                .iter()
                .enumerate()
                .find_map(|(i, c)| {
                    if c.name == target.district && c.beautified == target.beautified {
                        Some(i)
                    } else {
                        None
                    }
                })
                .ok_or("does not exist in the targeted player's city")?;

            let mut destroy_cost = target.effective_cost() - 1;
            if player.city_has(DistrictName::GreatWall) {
                destroy_cost += 1;
            }

            if available_gold < destroy_cost {
                return Err("not enough gold to destroy".into());
            }

            player.city.remove(city_index);
            game.active_player_mut()?.gold -= destroy_cost;
            game.discard_district(target.district);

            ActionOutput::new(format!(
                "The Warlord ({}) destroys {}'s {}.",
                game.active_player()?.name,
                target.player,
                target.district.data().display_name,
            ))
        }

        Action::Armory { district: target } => {
            if target.district == DistrictName::Keep {
                return Err("Cannot destroy the Keep".into());
            }

            if target.district == DistrictName::Armory {
                return Err("The armory cannot destroy itself".into());
            }

            let complete_size = game.complete_city_size();
            let targeted_player = game
                .players
                .iter_mut()
                .find(|p| p.name == target.player && p.city_size() < complete_size)
                .ok_or("player does not exist or cannot destroy from complete city")?;

            let city_index = targeted_player
                .city
                .iter()
                .enumerate()
                .find_map(|(i, c)| {
                    if c.name == target.district && c.beautified == target.beautified {
                        Some(i)
                    } else {
                        None
                    }
                })
                .ok_or("does not exist in the targeted player's city")?;

            targeted_player.city.remove(city_index);
            let active_player = game.active_player_mut()?;
            let (city_index, _) = active_player
                .city
                .iter()
                .enumerate()
                .find(|(_, d)| d.name == DistrictName::Armory)
                .ok_or("You do not have the armory")?;

            active_player.city.remove(city_index);
            game.discard_district(DistrictName::Armory);
            game.discard_district(target.district);

            ActionOutput::new(format!(
                "{} sacrifices their Armory to destroy {}'s {}.",
                game.active_player()?.name,
                target.player,
                target.district.data().display_name,
            ))
        }

        Action::Beautify {
            district: CityDistrictTarget { district, .. },
        } => {
            let player = game.active_player_mut()?;

            if player.gold < 1 {
                return Err("Not enough gold".into());
            }

            let city_district = player
                .city
                .iter_mut()
                .find(|d| !d.beautified && d.name == *district)
                .ok_or("Invalid target. Is it already beautified?")?;

            city_district.beautified = true;
            player.gold -= 1;

            ActionOutput::new(format!(
                "The Artist ({}) beautifies their {}.",
                game.active_player()?.name,
                district.data().display_name,
            ))
        }

        Action::NavigatorGain {
            resource: Resource::Cards,
        } => {
            game.gain_cards(4);
            ActionOutput::new(format!(
                "The Navigator ({}) gains 4 extra cards.",
                game.active_player()?.name
            ))
        }

        Action::NavigatorGain {
            resource: Resource::Gold,
        } => {
            let player = game.active_player_mut()?;
            player.gold += 4;

            ActionOutput::new(format!(
                "The Navigator ({}) gains 4 extra gold.",
                player.name
            ))
        }

        Action::SeerTake => {
            let my_index = game.active_player_index()?;
            let mut taken_from = Vec::with_capacity(game.players.len() - 1);
            let mut active_hand = mem::replace(&mut game.active_player_mut()?.hand, Vec::new());
            for player in game.players.iter_mut() {
                if player.index != my_index && player.hand.len() > 0 {
                    let i = game.rng.gen_range(0..player.hand.len());
                    let card = player.hand.remove(i);
                    taken_from.push(player.index);
                    active_hand.push(card);
                }
            }
            game.active_player_mut()?.hand = active_hand;
            ActionOutput::new(format!(
                "The Seer ({}) takes 1 card from everyone.",
                game.active_player()?.name
            ))
            .maybe_followup(if taken_from.is_empty() {
                None
            } else {
                Some(Followup::SeerDistribute {
                    players: taken_from,
                })
            })
        }

        Action::SeerDistribute { seer } => {
            let players: HashMap<_, _> = game
                .players
                .iter()
                .map(|p| (p.name.borrow(), p.index))
                .collect();
            let pairs: Vec<(PlayerIndex, DistrictName)> = seer
                .iter()
                .map(|(name, district)| {
                    Ok((
                        *players
                            .get(name)
                            .ok_or(format!("Cannot give {} a card.", name).to_owned())?,
                        *district,
                    ))
                })
                .collect::<Result<_>>()?;

            let mut removed = Vec::new();
            for (_, district) in pairs.iter() {
                let active_hand = &mut game.active_player_mut()?.hand;
                if let Some(district) = Game::remove_first(active_hand, *district) {
                    removed.push(district);
                } else {
                    return Err("cannot assign district not in hand!".into());
                }
            }
            for (index, district) in pairs {
                game.players[index.0].hand.push(district);
            }

            ActionOutput::new(format!("The Seer gives cards back."))
        }

        Action::ResourcesFromReligion { gold, cards, .. } => {
            let player = game.active_player()?;
            let count = player.count_suit_for_resource_gain(CardSuit::Religious);
            if gold + cards < count {
                return Err(format!("Too few resources, you should select {}", count).into());
            }

            if gold + cards > count {
                return Err(format!("Too many resources, you should select {}", count).into());
            }

            let _amount = game.gain_cards(count);
            let player = game.active_player()?;

            ActionOutput::new(format!(
                "The Abbot ({}) gained {} gold and {} cards from their Religious districts",
                player.name, gold, cards
            ))
        }
        Action::CollectTaxes => {
            let taxes = game.tax_collector;
            game.active_player_mut()?.gold += taxes;
            game.tax_collector = 0;
            ActionOutput::new(format!(
                "The Tax Collector collects {} gold in taxes.",
                taxes
            ))
        }
        Action::QueenGainGold => {
            let active = game.active_player_index()?;
            let left = PlayerIndex((active.0 + game.players.len() - 1) % game.players.len());
            let right = PlayerIndex((active.0 + 1) % game.players.len());
            let seated_next_to_royalty =
            game.characters.0.iter().find(|game_role| game_role.revealed && game_role.role.rank() == Rank::Four && game_role.player.is_some_and(|p| p == left || p == right));
            let log = if let Some(c) = seated_next_to_royalty {
                game.players[active.0].gold += 3;
                format!(
                    "The Queen is seated next to the {}, and gains 3 gold.",
                    c.role.display_name()
                )
            } else {
                format!(
                    "The Queen is not seated next to royalty.",
                )
            };
            ActionOutput::new(log)
        }

        Action::SpyAcknowledge => {
            //
            ActionOutput::new(format!("Spy is done peeking at the revealed hand"))
        }

        Action::Spy { player, suit } => {
            if player == game.active_player().unwrap().name {
                return Err("Cannot spy on game.".into());
            }
            let target = game
                .players
                .iter()
                .find_map(|p| {
                    if &p.name == player {
                        Some(p.index)
                    } else {
                        None
                    }
                })
                .ok_or("no player with that name")?;
            let matches = game.players[target.0]
                .hand
                .iter()
                .filter(|d| d.data().suit == *suit)
                .count();
            let gold_taken = std::cmp::min(game.players[target.0].gold, matches);
            game.players[target.0].gold -= gold_taken;
            game.active_player_mut().unwrap().gold += gold_taken;
            let cards_drawn = game.gain_cards(matches);
            ActionOutput::new(format!(
                    "The Spy ({}) is counting {} districts. They spy on {}, and find {} matches. They take {} gold, and draw {} cards.",
                    game.active_player()?.name,
                    suit,
                    game.players[target.0].name,
                    matches,
                    gold_taken,
                    cards_drawn
                )).followup(
                    Followup::SpyAcknowledge {
                        player: game.players[target.0].name.clone(),
                        revealed: game.players[target.0].hand.clone(),
                    },
                )
        }
        Action::TakeFromRich { player } => {
            if player == game.active_player().unwrap().name {
                return Err("Cannot take from yourgame".into());
            }

            let my_gold = game.active_player().unwrap().gold;

            let mut richest = Vec::with_capacity(game.players.len());
            for player in game.players.iter_mut().filter(|p| p.gold > my_gold) {
                if richest.len() == 0 {
                    richest.push(player);
                } else if player.gold == richest[0].gold {
                    richest.push(player);
                } else if player.gold > richest[0].gold {
                    richest.clear();
                    richest.push(player);
                }
            }

            let target = richest
                .iter_mut()
                .find(|p| p.name == *player)
                .ok_or("Not among the richest".to_owned())?;

            target.gold -= 1;
            let name = target.name.clone();
            game.active_player_mut().unwrap().gold += 1;
            ActionOutput::new(format!(
                "The Abbot ({}) takes 1 gold from the richest: {}",
                game.active_player()?.name,
                name
            ))
        }
        Action::SendWarrants { signed, unsigned } => {
            let mut roles = Vec::with_capacity(3);
            roles.push(signed);
            for role in unsigned {
                if roles.iter().any(|r| *r == role) {
                    return Err("Cannot assign more than 1 warrant to a role.".into());
                }
                roles.push(role);
            }
            if roles.iter().any(|r| **r == RoleName::Magistrate) {
                return Err("Cannot assign warrant to self.".into());
            }
            roles.sort_by_key(|r| r.rank());

            game.characters
                .get_mut(*signed)
                .unwrap()
                .markers
                .push(Marker::Warrant { signed: true });

            for role in unsigned {
                game.characters
                    .get_mut(*role)
                    .unwrap()
                    .markers
                    .push(Marker::Warrant { signed: false });
            }
            ActionOutput::new(format!(
                "The Magistrate ({}) sends warrants to the {}, the {}, and the {}.",
                game.active_player().unwrap().name,
                roles[0].display_name(),
                roles[1].display_name(),
                roles[2].display_name()
            ))
        }
        Action::Blackmail { flowered, unmarked } => {
            if flowered == unmarked {
                return Err("Cannot blackmail someone twice. ".into());
            }

            let flower_target = 
            match game .characters .get(*flowered) {
                Some(target) => target,
                None => return Err("Can not blackmail someone not in the game".into())
            };
            if !flower_target.revealed {

                return Err("Cannot blackmail anyone who hasn't gone yet".into());
            }

if flower_target
                .markers
                .iter()
                .any(|m| *m == Marker::Killed || *m == Marker::Bewitched)
            {
                return Err("Cannot blackmail the killed or bewitched".into());
            }

            let unmarked_target = 
            match game .characters .get(*unmarked) {
                Some(target) => target,
                None => return Err("Can not blackmail someone not in the game".into())
            };
                if unmarked_target
                .markers
                .iter()
                .any(|m| *m == Marker::Killed || *m == Marker::Bewitched)
            {
                return Err("Cannot blackmail the killed or bewitched".into());
            }

            game.characters
                .get_mut(*flowered)
                .unwrap()
                .markers
                .push(Marker::Blackmail { flowered: true });

            game.characters
                .get_mut(*unmarked)
                .unwrap()
                .markers
                .push(Marker::Blackmail { flowered: false });
            let mut roles = vec![flowered, unmarked];
            roles.sort_by_key(|r| r.rank());

            ActionOutput::new(format!(
                "The Blackmailer ({}) sends blackmail to the {} and the {}",
                game.active_player().unwrap().name,
                roles[0].display_name(),
                roles[1].display_name(),
            ))
        }

        Action::Smithy => {
            let active = game.active_player_mut()?;
            if active.gold < 2 {
                Err("Not enough gold")?;
            }
            active.gold -= 2;
            game.gain_cards(3);
            ActionOutput::new(format!(
                "At the Smithy, {} forges 2 gold into 3 cards.",
                game.active_player()?.name
            ))
        }

        Action::Laboratory { district } => {
            let active = game.active_player_mut()?;
            let (index, _) = active
                .hand
                .iter()
                .enumerate()
                .find(|(_, name)| *name == district)
                .ok_or("district not in hand")?;
            let card = active.hand.remove(index);
            active.gold += 2;
            game.deck.discard_to_bottom(card);

            ActionOutput::new(format!(
                "At the Laboratory, {} transmutes 1 card into 2 gold.",
                game.active_player()?.name
            ))
        }

        Action::Museum { district } => {
            let active = game.active_player_mut()?;
            let (index, _) = active
                .hand
                .iter()
                .enumerate()
                .find(|(_, name)| *name == district)
                .ok_or("district not in hand")?;
            let card = active.hand.remove(index);
            game.museum.tuck(card);

            ActionOutput::new(format!(
                "{} tucks a card face down under their Museum.",
                game.active_player()?.name
            ))
        }

        Action::ScholarReveal => {
            let drawn = game.deck.draw_many(7).collect::<Vec<_>>();

            ActionOutput::new(format!(
                "The Scholar ({}) is choosing from the top {} cards of the deck.",
                game.active_player()?.name,
                drawn.len(),
            ))
            .followup(Followup::ScholarPick { revealed: drawn })
        }

        Action::ScholarPick { district } => {
            let mut revealed =
                if let Some(Followup::ScholarPick { revealed, .. }) = game.followup.as_mut() {
                    revealed
                } else {
                    return Err("action is not allowed".into());
                };

            Game::remove_first(&mut revealed, *district).ok_or("invalid choice")?;
            for remaining in revealed {
                game.deck.discard_to_bottom(*remaining);
            }
            game.deck.shuffle(&mut game.rng);
            game.active_player_mut()?.hand.push(*district);

            ActionOutput::new(format!(
                "The Scholar ({}) picks a card, discarding the rest and shuffling the deck.",
                game.active_player()?.name,
            ))
        }

        Action::TheaterPass => {
            //
            ActionOutput::new(format!(
                "{} decided not to use the Theatre",
                game.active_player()?.name
            ))
            .end_turn()
        }
        Action::Theater { role, player } => {
            if game.active_player().unwrap().name == *player {
                return Err("Cannot swap with game".into());
            }

            Game::remove_first(&mut game.active_player_mut()?.roles, *role)
                .ok_or("You cannot give away a role you don't have")?;
            let target = game
                .players
                .iter_mut()
                .find(|p| p.name == *player)
                .ok_or("nonexistent player")?;

            let index = game.rng.gen_range(0..target.roles.len());
            let target_role = target.roles.swap_remove(index);
            target.roles.push(*role);
            target.roles.sort_by_key(|r| r.rank());
            for role in target.roles.iter() {
                game.characters.get_mut(*role).unwrap().player = Some(target.index);
            }

            let active = game.active_player_mut().unwrap();
            active.roles.push(target_role);
            active.roles.sort_by_key(|r| r.rank());
            let index = active.index;
            for role in active.roles.clone() {
                game.characters.get_mut(role).unwrap().player = Some(index);
            }

            ActionOutput::new(format!(
                "Theater: {} swaps roles with {}",
                game.active_player().unwrap().name,
                player
            ))
            .end_turn()
        }

        Action::MarshalSeize { district: target } => {
            if target.district == DistrictName::Keep {
                return Err("Cannot target the Keep".into());
            }
            if game.active_player().unwrap().city_has(target.district) {
                return Err("Cannot seize a copy of your own district".into());
            }
            let available_gold = game.active_player()?.gold;
            let complete_size = game.complete_city_size();
            let player = game
                .players
                .iter_mut()
                .find(|p| p.name == target.player)
                .ok_or("Player does not exist")?;

            if game.characters.has_bishop_protection(player.index) {
                Err("Cannot target the Bishop")?
            }
            if player.city_size() >= complete_size {
                Err("Cannot target a completed city")?
            }

            let city_index = player
                .city
                .iter()
                .enumerate()
                .find_map(|(i, c)| {
                    if c.name == target.district && c.beautified == target.beautified {
                        Some(i)
                    } else {
                        None
                    }
                })
                .ok_or("does not exist in the targeted player's city")?;
            let mut seize_cost = target.effective_cost();
            if seize_cost > 3 {
                return Err("Cannot seize district because it costs more than 3".into());
            }
            if player.city_has(DistrictName::GreatWall) {
                seize_cost += 1;
            }

            if available_gold < seize_cost {
                return Err("Not enough gold to seize".into());
            }

            let district = player.city.remove(city_index);
            player.gold += seize_cost;
            let active = game.active_player_mut().unwrap();
            active.gold -= seize_cost;
            active.city.push(district);

            ActionOutput::new(format!(
                "The Marshal ({}) seizes {}'s {}.",
                game.active_player()?.name,
                target.player,
                target.district.data().display_name,
            ))
        }

        Action::EmperorGiveCrown { player, resource } => {
            if game.active_player().unwrap().name == *player {
                Err("Cannot give the crown to yourself.")?;
            }

            let target = game
                .players
                .iter_mut()
                .find(|p| p.name == *player)
                .ok_or("Player does not exist.")?;

            if target.index == game.crowned {
                Err("Cannot give the crown to the already crowned player.")?;
            }

            game.crowned = target.index;

            match resource {
                Resource::Gold if target.gold > 0 => {
                    target.gold -= 1;
                    game.active_player_mut().unwrap().gold += 1;
                }
                Resource::Cards if target.hand.len() > 0 => {
                    let index = game.rng.gen_range(0..target.hand.len());
                    let card = target.hand.remove(index);
                    game.active_player_mut().unwrap().hand.push(card);
                }
                _ => {}
            }

            ActionOutput::new(format!(
                "The Emperor ({}) gives {} the crown and takes one of their {}.",
                game.active_player()?.name,
                player,
                match resource {
                    Resource::Gold => "gold",
                    Resource::Cards => "cards",
                }
            ))
        }

        Action::EmperorHeirGiveCrown { player } => {
            if game.active_player().unwrap().name == *player {
                Err("Cannot give the crown to yourself.")?;
            }

            let target = game
                .players
                .iter_mut()
                .find(|p| p.name == *player)
                .ok_or("Player does not exist")?;

            if target.index == game.crowned {
                Err("Cannot give the crown to the already crowned player.")?;
            }

            game.crowned = target.index;

            ActionOutput::new(format!(
                "The Emperor's advisor ({}) gives {} the crown.",
                game.active_player()?.name,
                player,
            ))
            .end_turn()
        }
        Action::DiplomatTrade {
            district: my_target,
            theirs: their_target,
        } => {
            if their_target.district == DistrictName::Keep {
                return Err("Cannot target the Keep".into());
            }

            let complete_city_size = game.complete_city_size();
            let player = game
                .players
                .iter()
                .find(|p| p.name == their_target.player)
                .ok_or("invalid player target")?;

            if game.characters.has_bishop_protection(player.index) {
                Err("Cannot target the Bishop")?
            }
            if player.city_size() >= complete_city_size {
                Err("Cannot target a completed city")?
            }

            let my_cost = my_target.effective_cost();
            let their_cost = their_target.effective_cost();
            let mut trade_cost = if my_cost < their_cost {
                their_cost - my_cost
            } else {
                0
            };

            if player.city_has(DistrictName::GreatWall) {
                trade_cost += 1;
            }
            if trade_cost > game.active_player().unwrap().gold {
                Err("Not enough gold")?
            }

            if player.city_has(my_target.district) {
                Err("The targeted player already has a copy of that district")?
            }

            if game
                .active_player()
                .unwrap()
                .city_has(their_target.district)
            {
                Err("You already have a copy of that district")?
            }

            let my_city_index = game
                .active_player()
                .unwrap()
                .city
                .iter()
                .enumerate()
                .find_map(|(i, c)| {
                    if c.name == my_target.district && c.beautified == my_target.beautified {
                        Some(i)
                    } else {
                        None
                    }
                })
                .ok_or("does not exist in the your city")?;

            let their_city_index = player
                .city
                .iter()
                .enumerate()
                .find_map(|(i, c)| {
                    if c.name == their_target.district && c.beautified == their_target.beautified {
                        Some(i)
                    } else {
                        None
                    }
                })
                .ok_or("does not exist in the targeted player's city")?;
            let index = player.index.0;
            let player = game.players[index].borrow_mut();
            player.gold += trade_cost;
            player.city[their_city_index] = CityDistrict {
                name: my_target.district,
                beautified: my_target.beautified,
            };

            let active = game.active_player_mut().unwrap();
            active.gold -= trade_cost;
            active.city[my_city_index] = CityDistrict {
                name: their_target.district,
                beautified: their_target.beautified,
            };

            ActionOutput::new(format!(
                "The Diplomat ({}) traded their {} for {}'s {}{}.",
                active.name,
                my_target.district.data().display_name,
                their_target.player,
                their_target.district.data().display_name,
                if trade_cost > 0 {
                    format!("; they paid {} gold for the difference", trade_cost)
                } else {
                    "".into()
                }
            ))
        }
        Action::WizardPeek { player } => {
            let target = game
                .players
                .iter()
                .find(|p| p.name == *player)
                .ok_or("invalid player target")?;

            ActionOutput::new(format!(
                "The Wizard ({}) peeks at {}'s hand.",
                game.active_player().unwrap().name,
                player,
            ))
            .followup(Followup::WizardPick {
                player: target.index,
            })
        }
        Action::WizardPick(WizardMethod::Pick { district }) => match game.followup {
            Some(Followup::WizardPick { player: target }) => {
                Game::remove_first(&mut game.players[target.0].hand, *district)
                    .ok_or("district not in target player's hand")?;
                game.active_player_mut().unwrap().hand.push(*district);
                ActionOutput::new(format!(
                    "The Wizard ({}) takes a card from {}'s hand.",
                    game.active_player().unwrap().name,
                    game.players[target.0].name,
                ))
            }
            _ => Err("impossible")?,
        },

        Action::WizardPick(method) => match game.followup {
            Some(Followup::WizardPick { player: target }) => {
                let district = match method {
                    WizardMethod::Pick { .. } => Err("Impossible!")?,
                    WizardMethod::Build { district } => *district,
                    WizardMethod::Framework { district } => *district,
                    WizardMethod::ThievesDen { .. } => DistrictName::ThievesDen,
                    WizardMethod::Necropolis { .. } => DistrictName::Necropolis,
                };

                if game.players[target.0].hand.iter().all(|d| *d != district) {
                    Err("Card not in hand")?;
                }

                let active = game.active_player().unwrap();
                if district == DistrictName::Monument && active.city.len() >= 5 {
                    return Err("You can only build the Monument, if you have less than 5 districts in your city".into());
                }

                let district = district.data();

                let mut cost = district.cost;
                if district.suit == CardSuit::Unique
                    && active.city.iter().any(|d| d.name == DistrictName::Factory)
                {
                    cost -= 1;
                }

                match method {
                    WizardMethod::Pick { .. } => Err("Impossible")?,
                    WizardMethod::Build { .. } => {
                        if cost > active.gold {
                            Err("Not enough gold")?;
                        }

                        let active = game.active_player_mut()?;
                        active.gold -= cost;
                    }
                    WizardMethod::ThievesDen { discard } => {
                        if district.name != DistrictName::ThievesDen {
                            Err("You are not building the ThievesDen!")?;
                        }
                        if discard.len() > cost {
                            Err("Cannot discard more cards than the cost")?;
                        }

                        if active.gold + discard.len() < cost {
                            Err("Not enough gold or cards discarded")?;
                        }

                        cost -= discard.len();
                        let mut discard_set = discard.clone();
                        let mut new_hand = Vec::with_capacity(active.hand.len());
                        for card in active.hand.iter() {
                            if let Some((i, _)) =
                                discard_set.iter().enumerate().find(|(_, d)| *d == card)
                            {
                                discard_set.swap_remove(i);
                            } else {
                                new_hand.push(*card);
                            }
                        }

                        if discard_set.len() > 0 {
                            Err("Can't discard cards not in your hand")?;
                        }

                        let active = game.active_player_mut().unwrap();
                        active.gold -= cost;
                        active.hand = new_hand;
                        for card in discard {
                            game.deck.discard_to_bottom(*card);
                        }
                    }

                    WizardMethod::Framework { .. } => {
                        let city_index = active
                            .city
                            .iter()
                            .enumerate()
                            .find_map(|(i, c)| {
                                if c.name == DistrictName::Framework {
                                    Some(i)
                                } else {
                                    None
                                }
                            })
                            .ok_or("Cannot sacrifice a district you don't own!")?;

                        let active = game.active_player_mut().unwrap();
                        active.city.swap_remove(city_index);
                    }

                    WizardMethod::Necropolis { sacrifice: target } => {
                        if district.name != DistrictName::Necropolis {
                            Err("You are not building the necropolis!")?;
                        }
                        let city_index = active
                            .city
                            .iter()
                            .enumerate()
                            .find_map(|(i, c)| {
                                if c.name == target.district && c.beautified == target.beautified {
                                    Some(i)
                                } else {
                                    None
                                }
                            })
                            .ok_or("Cannot sacrifice a district you don't own!")?;

                        let active = game.active_player_mut().unwrap();
                        let district = active.city.swap_remove(city_index);
                        game.discard_district(district.name);
                    }
                }
                Game::remove_first(&mut game.players[target.0].hand, district.name).unwrap();

                if game.characters.has_tax_collector() {
                    let player = game.active_player_mut()?;
                    if player.gold > 0 {
                        player.gold -= 1;
                        game.tax_collector += 1;
                    }
                }

                // the magistrate can only confiscate the first build of a turn
                if game.active_role().unwrap().has_warrant()
                    && !game.turn_actions.iter().any(|act| act.is_build())
                {
                    ActionOutput::new(format!(
                        "The Wizard begins to build a {}; waiting on the Magistrate's response.",
                        district.display_name
                    ))
                    .followup(Followup::Warrant {
                        magistrate: game.characters.get(RoleName::Magistrate).unwrap().player.unwrap(),
                        gold: cost,
                        district: district.name,
                        signed: game
                            .active_role()
                            .unwrap()
                            .markers
                            .iter()
                            .any(|m| *m == Marker::Warrant { signed: true }),
                    })
                } else {
                    game.complete_build(game.active_player().unwrap().index, cost, district.name);

                    ActionOutput::new(format!("The Wizard builds a {}.", district.display_name))
                }
            }

            _ => Err("impossible")?,
        },
        Action::Bewitch { role } => {
            if *role == RoleName::Witch{
                return Err("Cannot target self")?;
            }
            if let Some(c) = game.characters .get_mut(*role) {
                c.markers.push(Marker::Bewitched);
            } else {
                return Err(format!("Role {} not found", role.display_name()).into());
            }

            ActionOutput::new(format!("The Witch bewitches {}.", role.display_name())).end_turn()
        }
    })
}
