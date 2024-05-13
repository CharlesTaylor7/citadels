use arcstr::ArcStr;
use macros::tag::Tag;
use serde::{Deserialize, Serialize};
use std::borrow::Borrow;
use std::fmt::{self, Debug, Display};
use std::marker::PhantomData;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)]
pub enum CardSuit {
    Trade,
    Religious,
    Military,
    Noble,
    Unique,
}

impl CardSuit {
    pub const ALL: [CardSuit; 5] = [
        Self::Trade,
        Self::Religious,
        Self::Military,
        Self::Noble,
        Self::Unique,
    ];
}

impl fmt::Display for CardSuit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CardSet {
    Base,
    DarkCity,
    Citadels2016,
    Custom,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Marker {
    Discarded,
    Killed,
    Bewitched,
    Robbed,
    Blackmail { flowered: bool },
    Warrant { signed: bool },
}

impl Marker {
    pub fn is_warrant(&self) -> bool {
        if let Marker::Warrant { .. } = self {
            true
        } else {
            false
        }
    }

    pub fn is_blackmail(&self) -> bool {
        if let Marker::Blackmail { .. } = self {
            true
        } else {
            false
        }
    }
}

pub type PlayerId = String;
pub type Result<T> = std::result::Result<T, &'static str>;

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize, Hash)]
pub struct ImmutableString<Tag> {
    str: ArcStr,
    _phantom: PhantomData<Tag>,
}

impl<Tag> Borrow<str> for ImmutableString<Tag> {
    fn borrow(&self) -> &str {
        self.str.borrow()
    }
}

impl<Tag, T: Into<ArcStr>> From<T> for ImmutableString<Tag> {
    fn from(str: T) -> Self {
        ImmutableString {
            str: str.into(),
            _phantom: PhantomData,
        }
    }
}

impl<Tag> Debug for ImmutableString<Tag> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.str)
    }
}

impl<Tag> Display for ImmutableString<Tag> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.str)
    }
}

impl<Tag> PartialEq<ImmutableString<Tag>> for &ImmutableString<Tag> {
    fn eq(&self, other: &ImmutableString<Tag>) -> bool {
        self.str.eq(&other.str)
    }
}

pub type PlayerName = ImmutableString<tags::UserName>;
pub type UserId = ImmutableString<tags::UserId>;
pub type SessionId = ImmutableString<tags::SessionId>;

mod tags {
    pub trait Tag {
        fn name() -> &'static str;
    }
    impl Tag for UserName {
        fn name() -> &'static str {
            "username"
        }
    }

    impl Tag for UserId {
        fn name() -> &'static str {
            "user_id"
        }
    }

    impl Tag for SessionId {
        fn name() -> &'static str {
            "session_id"
        }
    }
    #[derive(Debug, PartialEq, Clone, Hash)]
    pub enum UserName {}
    #[derive(Debug, PartialEq, Clone, Hash)]
    pub enum UserId {}
    #[derive(Debug, PartialEq, Clone, Hash)]
    pub enum SessionId {}
}
