use crate::game::{ActionOutput, Game};
use crate::types::PlayerId;
use crate::{districts::DistrictName, roles::RoleName};
use poem_openapi::{Enum, NewType, Object, Union};
use serde::{Deserialize, Serialize};

pub trait ActionTrait {
    // const TAG: ActionTag;
    fn act(&self, game: &mut Game) -> color_eyre::Result<ActionOutput>;
}

#[derive(Union)]
#[oai(discriminator_name = "tag")]
pub enum Action {
    DraftPick(DraftPickAction),
    DraftDiscard(DraftDiscardAction),
    GatherCards(GatherCardsAction),
    GatherGold(GatherGoldAction),
    Build(BuildAction),
    Pass(PassAction),
    EndTurn(EndTurnAction),
}

#[derive(Object, Debug, Serialize, Deserialize)]
pub struct GatherCardsAction;

#[derive(Object, Debug, Serialize, Deserialize)]
pub struct GatherGoldAction;

#[derive(Object, Debug, Serialize, Deserialize)]
pub struct GatherCardsPickAction {
    pub district: DistrictName,
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
    pub district: DistrictName,
}

#[derive(Object, Serialize, Deserialize, Debug, Clone)]
pub struct NecropolisBuildMethod {
    pub sacrifice: CityDistrictTarget,
}

#[derive(Object, Serialize, Deserialize, Debug, Clone)]
pub struct ThievesDenBuildMethod {
    pub discard: Vec<DistrictName>,
}

#[derive(Object, Serialize, Deserialize, Debug, Clone)]
pub struct CardinalBuildMethod {
    pub discard: Vec<DistrictName>,
    pub player: PlayerId,
}
#[derive(Object, Serialize, Deserialize, Debug, Clone)]
pub struct CityDistrictTarget {
    pub player: PlayerId,
    pub district: DistrictName,
    pub beautified: bool,
}

#[derive(Enum, Serialize, Deserialize, Debug, Clone)]
pub enum Resource {
    Gold,
    Cards,
}

#[derive(Object, Serialize, Deserialize, Debug, Clone)]
pub struct Empty;
