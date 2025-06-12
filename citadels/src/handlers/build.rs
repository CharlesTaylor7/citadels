use crate::districts::DistrictName;
use crate::game::{ActionOutput, Followup, Game};
use crate::roles::RoleName;
use crate::schema::{
    Action, ActionTrait, BuildAction, BuildMethod, CardinalBuildMethod, NecropolisBuildMethod,
    ThievesDenBuildMethod,
};
use crate::types::{CardSuit, Marker};
use color_eyre::eyre::{Result, anyhow, bail, ensure};
use macros::tag::Tag;

use std::borrow::BorrowMut;

impl Action {
    pub fn is_resource_gathering(&self) -> bool {
        match self {
            Action::GatherGold(_) => true,
            Action::GatherCards(_) => true,
            _ => false,
        }
    }
}

impl ActionTrait for BuildAction {
    fn act(&self, game: &mut Game) -> Result<ActionOutput> {
        if game.active_role().unwrap().role == RoleName::Navigator {
            bail!("The navigator is not allowed to build.")
        }
        match self.method {
            BuildMethod::ThievesDen(_) => ensure!(
                self.district == DistrictName::ThievesDen,
                "District must be thieves den"
            ),
            BuildMethod::Necropolis(_) => ensure!(
                self.district == DistrictName::Necropolis,
                "District must be necropolis"
            ),
            _ => (),
        };

        let district = self.district;

        let active = game.active_player()?;
        if active.hand.iter().all(|d| *d != district) {
            bail!("Card not in hand")
        }

        if game
            .turn_actions
            .iter()
            .all(|a| !a.tag().is_resource_gathering())
        {
            bail!("Must gather resources before building")
        }

        let is_free_build = district == DistrictName::Stables
            || (district.data().suit == CardSuit::Trade
                && game.active_role().unwrap().role == RoleName::Trader);

        if !is_free_build && game.remaining_builds == 0 {
            bail!(
                "With your role, you cannot build more than {} time(s), this turn.",
                game.active_role().unwrap().role.build_limit()
            )
        }

        if !(active.city_has(DistrictName::Quarry)
            || game.active_role().unwrap().role == RoleName::Wizard)
            && active.city.iter().any(|d| d.name == district)
        {
            bail!("cannot build duplicate")
        }

        if district == DistrictName::Monument && active.city.len() >= 5 {
            bail!("You can only build the Monument, if you have less than 5 districts in your city")
        }

        let district = district.data();

        let mut cost = district.cost;
        if district.suit == CardSuit::Unique
            && active.city.iter().any(|d| d.name == DistrictName::Factory)
        {
            cost -= 1;
        }

        match &self.method {
            BuildMethod::Build { .. } => {
                if cost > active.gold {
                    bail!("Not enough gold")
                }

                let active = game.active_player_mut()?;
                Game::remove_first(&mut active.hand, district.name)
                    .ok_or(anyhow!("card not in hand"))?;
                active.gold -= cost;
            }
            BuildMethod::Cardinal(CardinalBuildMethod { discard, player }) => {
                if game.active_role()?.role != RoleName::Cardinal {
                    bail!("You are not the cardinal")
                }
                if active.gold + discard.len() < cost {
                    bail!("Not enough gold or discarded")
                }

                if active.gold + discard.len() > cost {
                    bail!("Must spend own gold first, before taking from others")
                }

                let target = game
                    .players
                    .iter()
                    .find_map(|p| if p.id == *player { Some(p.index) } else { None })
                    .ok_or(anyhow!("Player does not exist"))?;

                if game.players[target.0].gold < discard.len() {
                    bail!("Cannot give more cards than the target has gold")
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
                    bail!("Can't discard cards not in your hand")
                }

                Game::remove_first(&mut new_hand, district.name)
                    .ok_or(anyhow!("card not in hand"))?;

                let active = game.active_player_mut().unwrap();
                active.gold -= cost;
                active.hand = new_hand;

                let target = game.players[target.0].borrow_mut();
                target.gold -= gold;
                target.hand.append(&mut copy);
            }

            BuildMethod::ThievesDen(ThievesDenBuildMethod { discard }) => {
                if discard.len() > cost {
                    bail!("Cannot discard more cards than the cost")
                }

                if active.gold + discard.len() < cost {
                    bail!("Not enough gold or cards discarded")
                }

                cost -= discard.len();
                let mut discard_set = discard.clone();
                let mut new_hand = Vec::with_capacity(active.hand.len());
                for card in active.hand.iter() {
                    if let Some((i, _)) = discard_set.iter().enumerate().find(|(_, d)| *d == card) {
                        discard_set.swap_remove(i);
                    } else {
                        new_hand.push(*card);
                    }
                }

                if discard_set.len() > 0 {
                    bail!("Can't discard cards not in your hand")
                }
                Game::remove_first(&mut new_hand, district.name)
                    .ok_or(anyhow!("card not in hand"))?;

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
                    .ok_or(anyhow!("You don't own a framework!"))?;

                let active = game.active_player_mut().unwrap();
                Game::remove_first(&mut active.hand, district.name)
                    .ok_or(anyhow!("card not in hand"))?;
                active.city.swap_remove(city_index);
            }

            BuildMethod::Necropolis(NecropolisBuildMethod { sacrifice }) => {
                let city_index = active
                    .city
                    .iter()
                    .enumerate()
                    .find_map(|(i, c)| {
                        if c.name == sacrifice.district && c.beautified == sacrifice.beautified {
                            Some(i)
                        } else {
                            None
                        }
                    })
                    .ok_or(anyhow!("Cannot sacrifice a district you don't own!"))?;

                let active = game.active_player_mut().unwrap();
                Game::remove_first(&mut active.hand, district.name)
                    .ok_or(anyhow!("card not in hand"))?;

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
        let output = if game.active_role().unwrap().has_warrant()
            && !game.turn_actions.iter().any(|act| act.is_build())
        {
            ActionOutput::new(format!(
                "{} begins to build a {}; waiting on the Magistrate's response.",
                game.active_player().unwrap().name,
                district.display_name
            ))
            .followup(Followup::Warrant {
                magistrate: game
                    .characters
                    .get(RoleName::Magistrate)
                    .unwrap()
                    .player
                    .unwrap(),
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
        };
        Ok(output)
    }
}
