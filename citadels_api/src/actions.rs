use poem_openapi::{Enum, Object, Union};

#[derive(Object, Debug, Clone)]
pub struct None;

#[derive(Union)]
#[oai(discriminator_name = "tag")]
pub enum Action {
    DraftPick(DraftPick),
    DraftDiscard(DraftDiscard),
    // BeginGather ( resource: Resource ),
    // Build { city: String },
    // EndTurn(None),
}

#[derive(Object)]
pub struct DraftPick {
    role: RoleName,
}

#[derive(Object)]
pub struct DraftDiscard {
    role: RoleName,
}

#[derive(Enum)]
pub enum Resource {
    Gold,
    Cards,
}

#[derive(Enum)]
pub enum RoleName {
    Assassin,
    Thief,
    Wizard,
    King,
    Bishop,
    Merchant,
    Architect,
    Warlord,
}
