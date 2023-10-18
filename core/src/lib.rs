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

pub fn format_form_id(id_prefix: Option<&Oco<'_, str>>, id: &'static str) -> Oco<'static, str> {
    match id_prefix {
        None => Oco::Borrowed(id),
        Some(prefix) if prefix == "" => Oco::Borrowed(id),
        Some(prefix) => Oco::Owned(format!("{prefix}-{id}")),
    }
}

pub fn format_form_name(name_prefix: Option<&Oco<'_, str>>, field_name: &'static str) -> Oco<'static, str> {
    match name_prefix {
        None => Oco::Borrowed(field_name),
        Some(prefix) if prefix == "" => Oco::Borrowed(field_name),
        Some(prefix) => Oco::Owned(format!("{prefix}[{field_name}]")),
    }
}
