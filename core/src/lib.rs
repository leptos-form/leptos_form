#[macro_use]
extern crate cfg_if;
#[macro_use]
extern crate paste;
#[macro_use]
extern crate thiserror;
#[macro_use]
extern crate typed_builder;

mod form_component;

pub use form_component::*;

use ::leptos::*;

pub trait Form: ::leptos::IntoView {}

#[derive(Clone, Debug, Error)]
pub enum FormError {
    #[error("{0}")]
    Parse(String),
}

impl FormError {
    pub fn parse(err: impl ::std::fmt::Display) -> Self {
        Self::Parse(format!("{err}"))
    }
}

pub fn format_form_id(id_prefix: Option<&Oco<'_, str>>, id: impl Into<Oco<'static, str>>) -> Oco<'static, str> {
    let id = id.into();
    match id_prefix {
        None => id,
        Some(prefix) if prefix == "" => id,
        Some(prefix) => Oco::Owned(format!("{prefix}-{id}")),
    }
}

pub fn format_form_name(
    name_prefix: Option<&Oco<'_, str>>,
    field_name: impl Into<Oco<'static, str>>,
) -> Oco<'static, str> {
    let field_name = field_name.into();
    match name_prefix {
        None => field_name,
        Some(prefix) if prefix == "" => field_name,
        Some(prefix) => Oco::Owned(format!("{prefix}[{field_name}]")),
    }
}
