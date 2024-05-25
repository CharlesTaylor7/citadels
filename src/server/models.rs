use crate::strings::UserId;
use serde::Deserialize;

// TODO: avatar url
// learn how to allow user uploads to supabase assets
// learn how to delete stale user uploads
#[derive(Deserialize, Debug)]
pub struct Profile {
    username: String,
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

impl UserMetadata {
    pub fn default_profile(self) -> Profile {
        match self.custom_claims {
            CustomClaims::DiscordClaims { global_name } => Profile {
                username: global_name,
            },
        }
    }
}
