// New Maud based html markup belong under this module
// Older Askama templates are under /templates
pub mod base;
#[cfg(feature = "dev")]
pub mod dev;
pub mod game;
pub mod index;
pub mod lobby;
pub mod login;
pub mod profile;
