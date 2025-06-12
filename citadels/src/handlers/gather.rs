use color_eyre::eyre::{Result, anyhow, bail};

use crate::districts::DistrictName;
use crate::game::{ActionOutput, Followup, Game};
use crate::schema::{ActionTrait, GatherCardsAction, GatherCardsPickAction, GatherGoldAction};
use std::borrow::BorrowMut;

use rand::prelude::*;

impl ActionTrait for GatherGoldAction {
    fn act(&self, game: &mut Game) -> Result<ActionOutput> {
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

        Ok(ActionOutput::new(log).maybe_followup(game.after_gather_resources()))
    }
}

impl ActionTrait for GatherCardsAction {
    fn act(&self, game: &mut Game) -> Result<ActionOutput> {
        let mut draw_amount = 2;
        if game.active_player()?.city_has(DistrictName::Observatory) {
            draw_amount += 1;
        }

        let mut drawn = game.deck.draw_many(draw_amount).collect();

        let output = if game.active_player()?.city_has(DistrictName::Library) {
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
        };
        Ok(output)
    }
}

impl ActionTrait for GatherCardsPickAction {
    fn act(&self, game: &mut Game) -> Result<ActionOutput> {
        let mut revealed =
            if let Some(Followup::GatherCardsPick { revealed, .. }) = game.followup.borrow_mut() {
                revealed
            } else {
                bail!("action is not allowed")
            };
        let district = self.district;

        Game::remove_first(&mut revealed, district).ok_or(anyhow!("invalid choice"))?;
        revealed.shuffle(&mut game.rng);

        for remaining in revealed {
            game.deck.discard_to_bottom(*remaining);
        }
        game.active_player_mut()?.hand.push(district);
        Ok(ActionOutput::new("They pick a card.").maybe_followup(game.after_gather_resources()))
    }
}
