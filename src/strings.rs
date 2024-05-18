use arcstr::ArcStr;
use serde::{Deserialize, Serialize};
use std::fmt::{Debug, Display};
use std::marker::PhantomData;

pub type UserName = ImmutableString<tags::UserName>;
pub type UserId = ImmutableString<tags::UserId>;
pub type SessionId = ImmutableString<tags::SessionId>;
pub type AccessToken = ImmutableString<tags::AccessToken>;
pub type RefreshToken = ImmutableString<tags::RefreshToken>;
pub type OAuthCode = ImmutableString<tags::OAuthCode>;
pub type OAuthCodeVerifier = ImmutableString<tags::OAuthCodeVerifier>;

#[derive(PartialEq, Eq, Clone, Hash)]
pub struct ImmutableString<Tag> {
    str: ArcStr,
    _phantom: PhantomData<Tag>,
}
impl<Tag> ImmutableString<Tag> {
    pub fn new<T: Into<ArcStr>>(str: T) -> Self {
        Self {
            str: str.into(),
            _phantom: PhantomData,
        }
    }

    pub fn as_str(&self) -> &str {
        self.str.as_str()
    }
}

impl<Tag> Serialize for ImmutableString<Tag> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.str.serialize(serializer)
    }
}

impl<'de, Tag> Deserialize<'de> for ImmutableString<Tag> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(Self::new(ArcStr::deserialize(deserializer)?))
    }
}

impl<Tag> From<ImmutableString<Tag>> for ArcStr {
    fn from(value: ImmutableString<Tag>) -> Self {
        value.str
    }
}

impl<Tag: tags::Tag> Debug for ImmutableString<Tag> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if Tag::SECRET {
            write!(f, "SECRET:{}", Tag::NAME)
        } else {
            write!(f, "{}:{}", Tag::NAME, self.str)
        }
    }
}

impl<Tag: tags::Tag> Display for ImmutableString<Tag> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if Tag::SECRET {
            write!(f, "SECRET:{}", Tag::NAME)
        } else {
            write!(f, "{}", self.str)
        }
    }
}

impl<Tag> PartialEq<ImmutableString<Tag>> for &ImmutableString<Tag> {
    fn eq(&self, other: &ImmutableString<Tag>) -> bool {
        self.str.eq(&other.str)
    }
}

mod tags {
    pub trait Tag {
        const NAME: &'static str;
        const SECRET: bool;
    }
    impl Tag for UserName {
        const NAME: &'static str = "username";
        const SECRET: bool = false;
    }

    impl Tag for UserId {
        const NAME: &'static str = "user_id";
        const SECRET: bool = false;
    }

    impl Tag for SessionId {
        const NAME: &'static str = "session_id";
        const SECRET: bool = true;
    }

    impl Tag for AccessToken {
        const NAME: &'static str = "access_token";
        const SECRET: bool = true;
    }

    impl Tag for RefreshToken {
        const NAME: &'static str = "refresh_token";
        const SECRET: bool = true;
    }

    impl Tag for OAuthCode {
        const NAME: &'static str = "oauth_code";
        const SECRET: bool = true;
    }

    impl Tag for OAuthCodeVerifier {
        const NAME: &'static str = "oauth_code_verifier";
        const SECRET: bool = true;
    }

    #[derive(Debug, Eq, PartialEq, Clone, Hash)]
    pub enum UserName {}
    #[derive(Debug, Eq, PartialEq, Clone, Hash)]
    pub enum UserId {}
    #[derive(Debug, Eq, PartialEq, Clone, Hash)]
    pub enum SessionId {}
    #[derive(Debug, Eq, PartialEq, Clone, Hash)]
    pub enum AccessToken {}
    #[derive(Debug, Eq, PartialEq, Clone, Hash)]
    pub enum RefreshToken {}
    #[derive(Debug, Eq, PartialEq, Clone, Hash)]
    pub enum OAuthCode {}
    #[derive(Debug, Eq, PartialEq, Clone, Hash)]
    pub enum OAuthCodeVerifier {}
}
