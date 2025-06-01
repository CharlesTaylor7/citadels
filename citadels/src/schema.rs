use crate::actions::ActionTag;
use crate::game::{ActionOutput, Game};
use crate::types::{CardSuit, PlayerId, PlayerName};
use crate::{districts::DistrictName, roles::RoleName};
use poem_openapi::{Enum, NewType, Object, Union};
use serde::{Deserialize, Serialize};

pub trait ActionTrait {
    // const TAG: ActionTag;
    fn act(&self, game: &mut Game) -> anyhow::Result<ActionOutput>;
}

#[derive(Union)]
#[oai(discriminator_name = "tag")]
pub enum Action {
    DraftPick(DraftPickAction),
    DraftDiscard(DraftDiscardAction),
    GatherResource(GatherResourceAction),
    Build(BuildAction),
    Pass(PassAction),
    EndTurn(EndTurnAction),
}

#[derive(Object, Debug, Serialize, Deserialize)]
pub struct GatherResourceAction {
    resource: Resource,
}

#[derive(Object, Serialize, Deserialize, Debug, Clone)]
pub struct PassAction;

#[derive(Object, Serialize, Deserialize, Debug, Clone)]
pub struct EndTurnAction;

#[derive(Object)]
pub struct DraftPickAction {
    pub role: RoleName,
}

#[derive(Object)]
pub struct DraftDiscardAction {
    pub role: RoleName,
}

#[derive(Object, Serialize, Deserialize, Debug, Clone)]
pub struct BuildAction {
    pub district: DistrictName,
    pub method: BuildMethod,
}

#[derive(Union, Serialize, Deserialize, Debug, Clone)]
#[oai(discriminator_name = "tag")]
pub enum BuildMethod {
    Build(Empty),
    Framework(Empty),
    Necropolis(NecropolisBuildMethod),
    ThievesDen(ThievesDenBuildMethod),
    Cardinal(CardinalBuildMethod),
}

#[derive(Union, Serialize, Deserialize, Debug, Clone)]
#[oai(discriminator_name = "tag")]
pub enum WizardAction {
    Pick(WizardPickAction),
    Build(BuildAction),
}

#[derive(Object, Serialize, Deserialize, Debug, Clone)]
pub struct WizardPickAction {
    district: DistrictId,
}

#[derive(Object, Serialize, Deserialize, Debug, Clone)]
pub struct NecropolisBuildMethod {
    sacrifice: DistrictId,
}

#[derive(Object, Serialize, Deserialize, Debug, Clone)]
pub struct ThievesDenBuildMethod {
    discard: Vec<DistrictId>,
}

#[derive(Object, Serialize, Deserialize, Debug, Clone)]
pub struct CardinalBuildMethod {
    discard: Vec<DistrictId>,
    player: PlayerId,
}
#[derive(Object, Debug, Clone)]
pub struct CityDistrictTarget {
    pub player: PlayerId,
    pub district: DistrictId,
}

#[derive(Enum, Serialize, Deserialize, Debug, Clone)]
pub enum Resource {
    Gold,
    Cards,
}

/// id from 1 to 60ish.
/// assigned upon deck creation, consistent for whole game.
#[derive(NewType, Serialize, Deserialize, Debug, Clone)]
pub struct DistrictId(usize);

#[derive(Object, Serialize, Deserialize, Debug, Clone)]
pub struct Empty;
