mod impls;

pub use impls::*;

use crate::*;
use ::leptos::ev::Event;
use ::leptos::*;
use ::web_sys::EventTarget;

pub trait FormSignalType<El>: Sized {
    type Config: Clone + Default;
    type SignalType: Default + 'static;
    fn into_signal_type(self, config: &Self::Config) -> Self::SignalType;
    fn try_from_signal_type(signal_type: Self::SignalType, config: &Self::Config) -> Result<Self, FormError>;
}
pub trait FormComponent<T: 'static, El>: FormSignalType<El> {
    fn render(
        props: RenderProps<
            T,
            impl RefAccessor<T, Self::SignalType>,
            impl MutAccessor<T, Self::SignalType>,
            Self::Config,
        >,
    ) -> impl IntoView;
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
pub fn setter<T: 'static, U>(
    signal: RwSignal<T>,
    mut_ax: impl MutAccessor<T, U>,
    from_target: impl Copy + Fn(EventTarget) -> Option<U> + 'static,
) -> impl Fn(Event) {
    move |event| {
        if let Some(val) = event.target().and_then(from_target) {
            signal.update(|x| {
                *mut_ax(x) = val;
            })
        }
    }
}

#[doc(hidden)]
pub fn ref_ax_factory<T, U>(ref_ax: impl RefAccessor<T, U>) -> impl RefAccessor<T, U> {
    ref_ax
}

#[doc(hidden)]
pub fn mut_ax_factory<T, U>(mut_ax: impl MutAccessor<T, U>) -> impl MutAccessor<T, U> {
    mut_ax
}

impl<T, U, F: Copy + Fn(&T) -> &U + 'static> RefAccessor<T, U> for F {}

impl<T, U, F: Copy + Fn(&mut T) -> &mut U + 'static> MutAccessor<T, U> for F {}

impl<T: DefaultHtmlElement> DefaultHtmlElement for Option<T> {
    type El = T::El;
}

impl<U, El> FormSignalType<El> for Option<U>
where
    U: FormSignalType<El>,
    U::SignalType: Default + Eq,
{
    type Config = U::Config;
    type SignalType = U::SignalType;
    fn into_signal_type(self, config: &Self::Config) -> Self::SignalType {
        match self {
            Some(value) => U::into_signal_type(value, config),
            None => U::SignalType::default(),
        }
    }
    fn try_from_signal_type(signal_type: Self::SignalType, config: &Self::Config) -> Result<Self, FormError> {
        match signal_type == U::SignalType::default() {
            true => Ok(None),
            false => Ok(Some(U::try_from_signal_type(signal_type, config)?)),
        }
    }
}

impl<T: 'static, El, U> FormComponent<T, El> for Option<U>
where
    U: FormComponent<T, El>,
    U::SignalType: Default + Eq,
{
    fn render(
        props: RenderProps<
            T,
            impl RefAccessor<T, Self::SignalType>,
            impl MutAccessor<T, Self::SignalType>,
            Self::Config,
        >,
    ) -> impl IntoView {
        U::render(props)
    }
}
