mod impls;

pub use impls::*;

use crate::*;
use ::leptos::*;

pub use form_field_component::FormField;

#[doc(hidden)]
mod form_field_component {
    use super::{FormComponent, RenderProps};
    use ::leptos::*;
    use ::std::marker::PhantomData;

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
}

// docs are at workspace level
pub trait FormField<El>: Sized {
    /// A configuration type which is used for mapping between Self and the underlying value of Self::Signal.
    type Config: Clone + Default + 'static;
    /// A RwSignal or wrapper type containing RwSignals which contains the underlying form value.
    type Signal: Clone + 'static;

    fn default_signal(config: &Self::Config, initial: Option<Self>) -> Self::Signal;
    fn is_initial_value(signal: &Self::Signal) -> bool;
    fn into_signal(self, config: &Self::Config, initial: Option<Self>) -> Self::Signal;
    fn try_from_signal(signal: Self::Signal, config: &Self::Config) -> Result<Self, FormError>;
    fn recurse(signal: &Self::Signal);
    fn reset_initial_value(signal: &Self::Signal);
    fn validate(_: Self::Signal) -> Result<(), FormError> {
        Ok(())
    }
    #[allow(unused_variables)]
    fn with_error<O>(signal: &Self::Signal, f: impl FnOnce(Option<&FormError>) -> O) -> O {
        f(None)
    }
}

/// Rendering behavior for a particular data type given the html it is rendered in.
pub trait FormComponent<El>: FormField<El> {
    fn render(props: RenderProps<Self::Signal, Self::Config>) -> impl IntoView;
}

// docs are at workspace level
pub trait DefaultHtmlElement {
    /// The default html node this type will be rendered in.
    /// `Self` should impl `FormField<<Self as DefaultHtmlElement>::El>`
    type El;
}

/// Props provided during render to a type implementing [`trait@FormField`].
#[derive(Clone, Debug, TypedBuilder)]
#[builder(field_defaults(setter(into)))]
pub struct RenderProps<T: 'static, Config = ()> {
    #[builder(default)]
    pub id: Option<Oco<'static, str>>,
    #[builder(default)]
    pub name: Option<Oco<'static, str>>,
    #[builder(default)]
    pub class: Option<Oco<'static, str>>,
    #[builder(default)]
    pub style: Option<Oco<'static, str>>,
    #[builder(default)]
    pub field_changed_class: Option<Oco<'static, str>>,
    pub signal: T,
    pub config: Config,
}

/// A wrapper holding a signal for a current state, an initial state, and possibly an error.
#[derive(Debug, Deref, DerefMut, Derivative, TypedBuilder)]
#[derivative(Copy(bound = ""), Clone(bound = ""))]
pub struct FormFieldSignal<T: 'static> {
    #[deref]
    #[deref_mut]
    pub value: RwSignal<T>,
    pub initial: RwSignal<Option<T>>,
    pub error: RwSignal<Option<FormError>>,
}

impl<T: PartialEq + 'static + std::fmt::Debug, Config> RenderProps<FormFieldSignal<T>, Config> {
    pub fn class_signal(&self) -> RwSignal<Option<Oco<'static, str>>> {
        let signal = self.signal;
        let class = self.class.clone();
        let field_changed_class = match (class.clone(), self.field_changed_class.clone()) {
            (Some(class), Some(field_changed_class)) => Some(Oco::Owned(format!("{class} {field_changed_class}"))),
            (None, Some(field_changed_class)) => Some(field_changed_class),
            (Some(class), None) => Some(class),
            (None, None) => None,
        };

        let compute_class = std::rc::Rc::new(move |has_changed| match field_changed_class.clone() {
            Some(field_changed_class) => match has_changed {
                false => class.clone(),
                true => Some(field_changed_class),
            },
            None => class.clone(),
        });

        // intially render class assuming no changes have been made, otherwise
        // all fields will flash the field changed class on first render
        let class_signal = create_rw_signal(compute_class(false));

        create_render_effect({
            let compute_class = compute_class.clone();
            move |prev_has_changed| {
                let has_changed = signal.has_changed();
                if has_changed != prev_has_changed.unwrap_or(false) {
                    class_signal.update(|x| *x = compute_class(has_changed));
                }
                has_changed
            }
        });

        class_signal
    }
}

impl<T: PartialEq + 'static + std::fmt::Debug> FormFieldSignal<T> {
    pub fn has_changed(&self) -> bool {
        self.value.with(|value| {
            self.initial.with(|initial| match initial {
                Some(initial) => *initial != *value,
                None => true,
            })
        })
    }
}

impl<T: DefaultHtmlElement> DefaultHtmlElement for Option<T> {
    type El = T::El;
}

impl<T, El> FormField<El> for Option<T>
where
    T: FormField<El>,
{
    type Config = T::Config;
    type Signal = T::Signal;

    fn default_signal(config: &Self::Config, initial: Option<Self>) -> Self::Signal {
        T::default_signal(config, initial.flatten())
    }
    fn is_initial_value(signal: &Self::Signal) -> bool {
        T::is_initial_value(signal)
    }
    fn into_signal(self, config: &Self::Config, initial: Option<Self>) -> Self::Signal {
        match self {
            Some(value) => T::into_signal(value, config, initial.flatten()),
            None => T::default_signal(config, initial.flatten()),
        }
    }
    fn try_from_signal(signal: Self::Signal, config: &Self::Config) -> Result<Self, FormError> {
        match Self::is_initial_value(&signal) {
            true => Ok(None),
            false => Ok(Some(T::try_from_signal(signal, config)?)),
        }
    }
    fn recurse(signal: &Self::Signal) {
        T::recurse(signal)
    }
    fn reset_initial_value(signal: &Self::Signal) {
        T::reset_initial_value(signal);
    }
    fn validate(signal: Self::Signal) -> Result<(), FormError> {
        T::validate(signal)
    }
    fn with_error<O>(signal: &Self::Signal, f: impl FnOnce(Option<&FormError>) -> O) -> O {
        T::with_error(signal, f)
    }
}

impl<El, T> FormComponent<El> for Option<T>
where
    T: FormComponent<El>,
{
    fn render(props: RenderProps<Self::Signal, Self::Config>) -> impl IntoView {
        T::render(props)
    }
}

impl<T: Clone + Default + 'static> Default for FormFieldSignal<T> {
    fn default() -> Self {
        let default = T::default();
        Self {
            value: create_rw_signal(default.clone()),
            initial: create_rw_signal(Some(default)),
            error: create_rw_signal(None),
        }
    }
}

impl<T: std::fmt::Debug + 'static> FormFieldSignal<T> {
    fn new(value: T, initial: Option<T>) -> Self {
        Self {
            value: create_rw_signal(value),
            error: create_rw_signal(Default::default()),
            initial: create_rw_signal(initial),
        }
    }
}

impl<T: Default + 'static> FormFieldSignal<T> {
    fn new_with_default_value(initial: Option<T>) -> Self {
        Self {
            value: create_rw_signal(Default::default()),
            error: create_rw_signal(Default::default()),
            initial: create_rw_signal(initial),
        }
    }
}
