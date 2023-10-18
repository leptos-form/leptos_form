mod impls;

pub use impls::*;

use crate::*;
use ::leptos::*;
use ::leptos::ev::Event;
use ::leptos::html::*;
use ::web_sys::EventTarget;

pub trait FormSignalType<El>: Sized {
    type Config: Default;
    type SignalType;
    fn into_signal_type(self, config: Self::Config) -> Self::SignalType;
    fn try_from_signal_type(signal_type: Self::SignalType, config: Self::Config) -> Result<Self, FormError>;
}
pub trait FormComponent<T: 'static, El>: FormSignalType<El> {
    fn render(props: RenderProps<T, impl RefAccessor<T, Self::SignalType>, impl MutAccessor<T, Self::SignalType>, Self::Config>) -> impl IntoView;
}

pub trait DefaultHtmlElement {
    type El;
}

#[derive(Clone, Debug, TypedBuilder)]
#[builder(field_defaults(setter(into)))]
pub struct RenderProps<T: 'static, RefAx, MutAx, Config = ()> {
    #[builder(default)]
    pub id: Option<Oco<'static, str>>,
    pub name: Oco<'static, str>,
    /// the prefix used to create the `name` field
    pub id_prefix: Option<Oco<'static, str>>,
    /// the prefix used to create the `name` field
    pub name_prefix: Option<Oco<'static, str>>,
    #[builder(default)]
    pub class: Option<Oco<'static, str>>,
    pub signal: RwSignal<T>,
    pub config: Config,
    #[builder(setter(!into))]
    pub ref_ax: RefAx,
    #[builder(setter(!into))]
    pub mut_ax: MutAx,
}

pub trait RefAccessor<T, U>: Copy + Fn(&T) -> &U + 'static {}

pub trait MutAccessor<T, U>: Copy + Fn(&mut T) -> &mut U + 'static {}

#[doc(hidden)]
pub fn setter<T: 'static, U>(signal: RwSignal<T>, mut_ax: impl MutAccessor<T, U>, from_target: impl Copy + Fn(EventTarget) -> Option<U> + 'static) -> impl Fn(Event) {
    move |event| {
        event.target().and_then(from_target).map(|val| signal.update(|x| {
            *mut_ax(x) = val;
        }));
    }
}

#[doc(hidden)]
pub fn ref_ax_factory<T, U>(f: impl RefAccessor<T, U>) -> impl RefAccessor<T, U> { f }

#[doc(hidden)]
pub fn mut_ax_factory<T, U>(f: impl MutAccessor<T, U>) -> impl MutAccessor<T, U> { f }

impl<T, U, F: Copy + Fn(&T) -> &U + 'static> RefAccessor<T, U> for F {}

impl<T, U, F: Copy + Fn(&mut T) -> &mut U + 'static> MutAccessor<T, U> for F {}

impl<U> FormSignalType<Input> for Option<U>
where
    U: FormSignalType<Input>,
    U::SignalType: Default + Eq,
{
    type Config = U::Config;
    type SignalType = U::SignalType;
    fn into_signal_type(self, config: Self::Config) -> Self::SignalType {
        match self {
            Some(value) => U::into_signal_type(value, config),
            None => U::SignalType::default(),
        }
    }
    fn try_from_signal_type(signal_type: Self::SignalType, config: Self::Config) -> Result<Self, FormError> {
        match signal_type == U::SignalType::default() {
            true => Ok(None),
            false => Ok(Some(U::try_from_signal_type(signal_type, config)?))
        }
    }
}

impl<T: 'static, U> FormComponent<T, Input> for Option<U>
where
    U: FormComponent<T, Input>,
    U::SignalType: Default + Eq,
{
    fn render(props: RenderProps<T, impl RefAccessor<T, Self::SignalType>, impl MutAccessor<T, Self::SignalType>, Self::Config>) -> impl IntoView {
        U::render(props)
    }
}
