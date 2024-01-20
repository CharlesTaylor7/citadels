use super::{DistrictTemplate, RoleTemplate};
use crate::{districts::DistrictName, roles::RoleName, server::routes::CardName};

pub struct RoleFaqTemplate {
    pub name: RoleName,
    pub role: RoleTemplate,
}

pub struct DistrictFaqTemplate {
    pub name: DistrictName,
    pub district: DistrictTemplate<'static>,
}

impl From<DistrictName> for DistrictFaqTemplate {
    fn from(value: DistrictName) -> Self {
        Self {
            name: value,
            district: DistrictTemplate::from(value),
        }
    }
}

impl From<RoleName> for RoleFaqTemplate {
    fn from(value: RoleName) -> Self {
        Self {
            name: value,
            role: RoleTemplate::from(value, 200.0),
        }
    }
}

impl std::fmt::Display for CardName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::District(d) => write!(f, "{}", d.data().display_name),
            Self::Role(r) => write!(f, "{}", r.display_name()),
        }
    }
}
