use crate::types::{CardSuit, PlayerId, PlayerName};
use crate::{districts::DistrictName, roles::RoleName};
use poem_openapi::{Enum, NewType, Object, Union};
use serde::{Deserialize, Serialize};

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
    role: RoleName,
}

#[derive(Object)]
pub struct DraftDiscardAction {
    role: RoleName,
}

#[derive(Object, Serialize, Deserialize, Debug, Clone)]
pub struct BuildAction {
    district: DistrictName,
    method: BuildMethod,
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
    discard: Vec<DistrictName>,
    player: PlayerId,
}

#[derive(Object, Serialize, Deserialize, Debug, Clone)]
pub struct ThievesDenBuildMethod {
    discard: Vec<DistrictName>,
}

#[derive(Object, Serialize, Deserialize, Debug, Clone)]
pub struct CardinalBuildMethod {
    district: DistrictName,
    discard: Vec<DistrictName>,
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
