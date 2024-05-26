use crate::strings::{UserId, UserName};
use arcstr::ArcStr;
use serde::Deserialize;

// TODO: avatar url
// learn how to allow user uploads to supabase assets
// learn how to delete stale user uploads
#[derive(Deserialize, Debug)]
pub struct Profile {
    pub user_id: UserId,
    pub username: UserName,
}

#[derive(Deserialize, Debug)]
pub struct SupabaseUser {
    pub id: UserId,
    pub user_metadata: UserMetadata,
}

#[derive(Deserialize, Debug)]
pub struct UserMetadata {
    pub avatar_url: String,
    pub full_name: String,
    pub custom_claims: CustomClaims,
}

// TODO: custom deserializer if ever try to support more oauth providers.
// The `iss` field in the JWT can be used to distinguish.
#[derive(Deserialize, Debug)]
#[serde(untagged)]
pub enum CustomClaims {
    DiscordClaims { global_name: String },
}
