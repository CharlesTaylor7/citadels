use arcstr::ArcStr;
use serde::{Deserialize, Serialize};
use std::borrow::Borrow;
use std::fmt::{Debug, Display};
use std::marker::PhantomData;

pub type UserName = ImmutableString<tags::UserName>;
pub type UserId = ImmutableString<tags::UserId>;
pub type SessionId = ImmutableString<tags::SessionId>;
pub type RefreshToken = ImmutableString<tags::RefreshToken>;
pub type AccessToken = ImmutableString<tags::AccessToken>;

#[derive(PartialEq, Eq, Clone, Serialize, Deserialize, Hash)]
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
}

impl<Tag> Borrow<str> for ImmutableString<Tag> {
    fn borrow(&self) -> &str {
        self.str.borrow()
    }
}

impl<Tag> From<ImmutableString<Tag>> for ArcStr {
    fn from(value: ImmutableString<Tag>) -> Self {
        value.str
    }
}
/*
impl<Tag, T: Into<ArcStr>> From<T> for ImmutableString<Tag> {
    fn from(str: T) -> Self {
        ImmutableString {
            str: str.into(),
            _phantom: PhantomData,
        }
    }
}
*/

impl<Tag: tags::Tag> Debug for ImmutableString<Tag> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", Tag::name(), self.str)
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
    #[derive(Debug, Eq, PartialEq, Clone, Hash)]
    pub enum UserName {}
    #[derive(Debug, Eq, PartialEq, Clone, Hash)]
    pub enum UserId {}
    #[derive(Debug, Eq, PartialEq, Clone, Hash)]
    pub enum SessionId {}

    #[derive(Debug, Eq, PartialEq, Clone, Hash)]
    pub enum RefreshToken {}

    #[derive(Debug, Eq, PartialEq, Clone, Hash)]
    pub enum AccessToken {}
}
