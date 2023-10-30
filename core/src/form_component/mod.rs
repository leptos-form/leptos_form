mod impls;

pub use impls::*;

use crate::*;
use ::leptos::*;
use ::std::marker::PhantomData;
use ::wasm_bindgen::JsValue;

// Note: the ty prop must be present because the props type produced for this component
// uses all the generics supplied on the item below, so without directly referencing the generics
// in some prop value, then an invocation of <FormField  props=.. /> would have no way of inferring the T and El types
#[allow(unused_variables)]
#[component]
pub fn FormField<T: FormComponent<El>, El>(
    props: RenderProps<T::Signal, T::Config>,
    #[prop(optional)] ty: PhantomData<(T, El)>,
) -> impl IntoView {
    T::render(props)
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
    fn reset_initial_value(signal: &Self::Signal);
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
    #[builder(default)]
    pub field_changed_class: Option<Oco<'static, str>>,
    pub signal: T,
    pub config: Config,
}

#[derive(AsRef, AsMut, Debug, Deref, DerefMut, Derivative, From, Into)]
#[derivative(Clone(bound = ""), Copy(bound = ""))]
pub struct FormFieldSignal<T: 'static>(pub RwSignal<FormFieldSignalValue<T>>);

#[derive(Clone, Debug, Default)]
pub struct FormFieldSignalValue<T> {
    pub value: T,
    pub initial: Option<T>,
    pub error: Option<FormError>,
}

impl<T: PartialEq + 'static, Config> RenderProps<FormFieldSignal<T>, Config> {
    pub fn class_signal(&self) -> RwSignal<Option<Oco<'static, str>>> {
        let signal = self.signal;
        let initial_has_changed = signal.has_changed();
        let class = self.class.clone();
        let field_changed_class = match (class.clone(), self.field_changed_class.clone()) {
            (Some(class), Some(field_changed_class)) => Some(Oco::Owned(format!("{class} {field_changed_class}"))),
            (None, Some(field_changed_class)) => Some(field_changed_class),
            (Some(class), None) => Some(class),
            (None, None) => None,
        };

        let compute_class = move |has_changed| match field_changed_class.clone() {
            Some(field_changed_class) => match has_changed {
                false => class.clone(),
                true => Some(field_changed_class),
            },
            None => class.clone(),
        };

        let class_signal = create_rw_signal(compute_class(initial_has_changed));

        create_effect(move |prev_has_changed| {
            let has_changed = signal.has_changed();
            if has_changed != prev_has_changed.unwrap_or(initial_has_changed) {
                class_signal.update(|x| *x = compute_class(has_changed));
            }
            has_changed
        });

        class_signal
    }
}

impl<T: PartialEq + 'static> FormFieldSignal<T> {
    pub fn has_changed(&self) -> bool {
        self.0.with(|sig| sig.has_changed())
    }
}

impl<T: PartialEq> FormFieldSignalValue<T> {
    pub fn has_changed(&self) -> bool {
        match self.initial.as_ref() {
            Some(initial) => *initial != self.value,
            None => true,
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
    fn reset_initial_value(signal: &Self::Signal) {
        U::reset_initial_value(signal);
    }
    fn validate(signal: Self::Signal) -> Result<(), FormError> {
        U::validate(signal)
    }
    fn with_error<O>(signal: &Self::Signal, f: impl FnOnce(Option<&FormError>) -> O) -> O {
        U::with_error(signal, f)
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

impl<T: Clone + 'static> From<T> for FormFieldSignal<T> {
    fn from(value: T) -> Self {
        Self(create_rw_signal(FormFieldSignalValue {
            initial: Some(value.clone()),
            value,
            error: None,
        }))
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
