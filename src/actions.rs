use crate::game::Game;
use crate::roles::RoleName;
use macros::tag::Tag;
use serde::Deserialize;

#[derive(Deserialize, Tag, Debug)]
#[serde(tag = "action")]
pub enum Action {
    // Draft Phase
    DraftPick { role: RoleName },
    DraftDiscard { role: RoleName },
    // Role Call
    GainGold,
    GainCards,
    Build { district: String },
    //    character actions
    Assassinate,
    ThiefSteal,
    MagicianSwap(MagicianAction),
    KingGainGold,
    BishopGainGold,
    MerchantGainOne,
    MerchantGainGold,
    ArchitectGainCards,
    WarlordDestroy,
    WarlordGainGold,
    ArtistBeautify,
}

#[derive(Deserialize, Debug)]
#[serde(untagged)]
pub enum MagicianAction {
    TargetPlayer { player: String }, // name
    TargetDeck { discard: Vec<String> },
}

pub type Result<T> = std::result::Result<T, &'static str>;

impl Action {
    // TODO: convert to result
    pub fn perform(self, game: &mut Game) -> Result<()> {
        match self {
            Action::DraftPick { role } => {
                let player_id = game.active_turn.draft().ok_or("not the draft phase")?;
                let p = game
                    .players
                    .iter_mut()
                    .find(|p| p.id == player_id)
                    .ok_or("player does not exist")?;

                let i = (0..game.draft.remaining.len())
                    .find(|i| game.draft.remaining[*i].name == role)
                    .ok_or("selected role is not available")?;

                let role = game.draft.remaining.remove(i);
                p.roles.push(role);
                Ok(())
            }

            Action::DraftDiscard { role } => {
                let i = (0..game.draft.remaining.len())
                    .find(|i| game.draft.remaining[*i].name == role)
                    .ok_or("selected role is not available")?;

                game.draft.remaining.remove(i);
                Ok(())
            }

            _ => {
                todo!("action is not implemented");
            }
        }
    }
}
