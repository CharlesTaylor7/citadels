use crate::game::{ActionOutput, Game};
use crate::schema::{ActionTrait, DraftDiscardAction, DraftPickAction};
use anyhow::{Result, anyhow};

use std::borrow::BorrowMut;

impl ActionTrait for DraftPickAction {
    fn act(&self, game: &mut Game) -> Result<ActionOutput> {
        let role = self.role;

        let draft = game.active_turn.draft_mut()?;

        Game::remove_first(&mut draft.remaining, role);
        let c = game.characters.get_mut(role).unwrap();
        c.player = Some(draft.player);
        let player = game.players[draft.player.0].borrow_mut();
        player.roles.push(role);
        player.roles.sort_by_key(|r| r.rank());

        let output = ActionOutput::new(format!("{} drafts a role.", player.name));

        // the first player to draft in the two player game does not discard.
        // the last pick is between two cards.
        // the one they don't pick is automatically discarded.
        // So really only the middle two draft turns should show the discard button
        if game.players.len() == 2 && (draft.remaining.len() == 5 || draft.remaining.len() == 3) {
            Ok(output)
        } else {
            Ok(output.end_turn())
        }
    }
}
impl ActionTrait for DraftDiscardAction {
    fn act(&self, game: &mut Game) -> Result<ActionOutput> {
        let role = self.role;
        let draft = game.active_turn.draft_mut()?;
        let i = (0..draft.remaining.len())
            .find(|i| draft.remaining[*i] == role).ok_or(anyhow!(anyhow!("selected role is not available")))?;

        draft.remaining.remove(i);
        let output = ActionOutput::new(format!(
            "{} discards a role face down.",
            game.active_player()?.name
        ))
        .end_turn();
        Ok(output)
    }
}
