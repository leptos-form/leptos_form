mod impls;

pub use impls::*;

use crate::*;
use ::leptos::*;
use ::wasm_bindgen::JsValue;

#[derive(AsRef, AsMut, Debug, Deref, DerefMut, Derivative, From, Into)]
#[derivative(Clone(bound = ""), Copy(bound = ""))]
pub struct FormFieldSignal<T: 'static>(pub RwSignal<FormFieldSignalValue<T>>);

#[derive(Clone, Debug, Default)]
pub struct FormFieldSignalValue<T> {
    pub value: T,
    pub error: Option<FormError>,
}

/// Provides utilities for a data type's form field representation.
pub trait FormField<El>: Sized {
    /// A configuration type which is used for mapping between Self and the underlying value of Self::Signal.
    type Config: Clone + Default + 'static;
    /// A RwSignal or wrapper type containing RwSignals which contains the underlying form value.
    type Signal: Clone + 'static;

    fn default_signal() -> Self::Signal;
    fn is_default_value(signal: &Self::Signal) -> bool;
    fn into_signal(self, config: &Self::Config) -> Self::Signal;
    fn try_from_signal(signal: Self::Signal, config: &Self::Config) -> Result<Self, FormError>;
    fn validate(_: Self::Signal) -> Result<(), FormError> {
        Ok(())
    }
    #[allow(unused_variables)]
    fn with_error<O>(signal: &Self::Signal, f: impl FnOnce(Option<&FormError>) -> O) -> O {
        f(None)
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
    fn with_error<O>(signal: &Self::Signal, f: impl FnOnce(Option<&FormError>) -> O) -> O {
        U::with_error(signal, f)
    }
    fn try_from_signal(signal: Self::Signal, config: &Self::Config) -> Result<Self, FormError> {
        match Self::is_default_value(&signal) {
            true => Ok(None),
            false => Ok(Some(U::try_from_signal(signal, config)?)),
        }
    }
    fn validate(signal: Self::Signal) -> Result<(), FormError> {
        U::validate(signal)
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

impl<T: Default + 'static> Default for FormFieldSignal<T> {
    fn default() -> Self {
        Self(create_rw_signal(Default::default()))
    }
}

impl<T: 'static> From<T> for FormFieldSignal<T> {
    fn from(value: T) -> Self {
        Self(create_rw_signal(FormFieldSignalValue { value, error: None }))
    }
}

impl<T> From<FormFieldSignalValue<T>> for JsValue
where
    JsValue: From<T>,
{
    fn from(value: FormFieldSignalValue<T>) -> JsValue {
        JsValue::from(value.value)
    }
}
