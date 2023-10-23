mod impls;

pub use impls::*;

use crate::*;
use ::leptos::ev::Event;
use ::leptos::*;
use ::web_sys::EventTarget;

/// Provides utilities for a data type's form field representation.
pub trait FormField<El>: Sized {
    /// A configuration type which is used for mapping between Self and the underlying value of Self::Signal.
    type Config: Clone + Default;
    /// A RwSignal or wrapper type containing RwSignals which contains the underlying form value.
    type Signal: 'static;

    fn default_signal() -> Self::Signal;
    fn is_default_value(signal: &Self::Signal) -> bool;
    fn into_signal(self, config: &Self::Config) -> Self::Signal;
    fn try_from_signal(signal: Self::Signal, config: &Self::Config) -> Result<Self, FormError>;
    fn validate(_: Self::Signal) -> Result<(), FormError> {
        Ok(())
    }
}

pub trait FormComponent<El>: FormField<El> {
    fn render(props: RenderProps<Self::Signal, Self::Config>) -> impl IntoView;
}

pub trait DefaultHtmlElement {
    type El;
}

#[derive(Clone, Debug, TypedBuilder)]
#[builder(field_defaults(setter(into)))]
pub struct RenderProps<T: 'static, Config = ()> {
    #[builder(default)]
    pub id: Option<Oco<'static, str>>,
    pub name: Oco<'static, str>,
    #[builder(default)]
    pub class: Option<Oco<'static, str>>,
    pub signal: T,
    pub config: Config,
}

#[doc(hidden)]
pub fn setter<T: 'static>(
    signal: RwSignal<T>,
    from_target: impl Copy + Fn(EventTarget) -> Option<T> + 'static,
) -> impl Fn(Event) {
    move |event| {
        if let Some(val) = event.target().and_then(from_target) {
            signal.update(|x| {
                *x = val;
            })
        }
    }
}

impl<T: DefaultHtmlElement> DefaultHtmlElement for Option<T> {
    type El = T::El;
}

impl<U, El> FormField<El> for Option<U>
where
    U: FormField<El>,
{
    type Config = U::Config;
    type Signal = U::Signal;

    fn default_signal() -> Self::Signal {
        U::default_signal()
    }
    fn is_default_value(signal: &Self::Signal) -> bool {
        U::is_default_value(signal)
    }
    fn into_signal(self, config: &Self::Config) -> Self::Signal {
        match self {
            Some(value) => U::into_signal(value, config),
            None => U::default_signal(),
        }
    }
    fn try_from_signal(signal: Self::Signal, config: &Self::Config) -> Result<Self, FormError> {
        match Self::is_default_value(&signal) {
            true => Ok(None),
            false => Ok(Some(U::try_from_signal(signal, config)?)),
        }
    }
}

impl<El, U> FormComponent<El> for Option<U>
where
    U: FormComponent<El>,
{
    fn render(props: RenderProps<Self::Signal, Self::Config>) -> impl IntoView {
        U::render(props)
    }
}
