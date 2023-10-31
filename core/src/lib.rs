#![allow(clippy::non_canonical_clone_impl)]

#[macro_use]
extern crate derivative;
#[macro_use]
extern crate derive_more;
#[macro_use]
extern crate paste;
#[macro_use]
extern crate thiserror;
#[macro_use]
extern crate typed_builder;

pub mod components;
mod form_component;

pub use form_component::*;

use ::leptos::*;

/// Error returned while rendering or parsing html form.
#[derive(Clone, Debug, Error)]
pub enum FormError {
    #[error("{0}")]
    Parse(String),
}

/// Wrapper type used for providing the initial and current value of the form's main type.
#[derive(Clone, Copy, Debug)]
pub struct FormDiff<T> {
    pub initial: T,
    pub current: T,
}

pub trait MapSubmit<T, U>: Fn(FormDiff<T>) -> U {}
impl<T, U, F> MapSubmit<T, U> for F where F: Fn(FormDiff<T>) -> U {}

impl FormError {
    pub fn parse(err: impl ::std::fmt::Display) -> Self {
        Self::Parse(format!("{err}"))
    }
}

#[doc(hidden)]
pub fn format_form_id(id_prefix: Option<&Oco<'_, str>>, id: impl Into<Oco<'static, str>>) -> Oco<'static, str> {
    let id = id.into();
    match id_prefix {
        None => id,
        Some(prefix) if prefix == "" => id,
        Some(prefix) => Oco::Owned(format!("{prefix}-{id}")),
    }
}

#[doc(hidden)]
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

#[derive(Clone, Copy, Debug)]
pub enum LabelCase {
    Camel,
    Kebab,
    Lower,
    Pascal,
    Snake,
    Title,
    Train,
    Upper,
    UpperKebab,
    UpperSnake,
}
