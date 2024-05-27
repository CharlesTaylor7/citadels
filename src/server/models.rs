use crate::strings::UserId;
use serde::Deserialize;

#[derive(Deserialize, Debug)]
pub struct SupabaseUser {
    pub id: UserId,
    pub user_metadata: UserMetadata,
}

#[derive(Deserialize, Debug)]
pub struct UserMetadata {
    pub avatar_url: Option<String>,
    pub full_name: Option<String>,
    pub custom_claims: Option<CustomClaims>,
}

// TODO: custom deserializer if ever try to support more oauth providers.
// The `iss` field in the JWT can be used to distinguish.
#[derive(Deserialize, Debug)]
#[serde(untagged)]
pub enum CustomClaims {
    DiscordClaims { global_name: String },
}
