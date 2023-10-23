use crate::*;
use ::leptos::html::*;
use ::leptos::*;
use ::std::borrow::Cow;

macro_rules! str_impl {
    ($($ty:ty $({ $from_signal_type:expr })?),*$(,)?) => { $(
        str_impl! { @ $ty, Input $({ $from_signal_type })? }
        str_impl! { @ $ty, Textarea $({ $from_signal_type })? }

        impl DefaultHtmlElement for $ty {
            type El = HtmlElement<Input>;
        }
    )* };

    (@ $ty:ty, $el:ident $({ $from_signal_type:expr })?) => { paste! {
        impl FormSignalType<HtmlElement<$el>> for $ty {
            type Config = ();
            type SignalType = RwSignal<String>;

            fn default_signal() -> Self::SignalType {
                RwSignal::new(Default::default())
            }
            fn is_default_value(signal: &Self::SignalType) -> bool {
                signal.with(|value| value.is_empty())
            }
            fn into_signal_type(self, _: &Self::Config) -> Self::SignalType {
                RwSignal::new(self.into())
            }
            fn try_from_signal_type(signal_type: Self::SignalType, _: &Self::Config) -> Result<Self, FormError> {
                Ok(str_impl!(@from signal_type $($from_signal_type)?))
            }
        }

        impl FormComponent<HtmlElement<$el>> for $ty {
            fn render(props: RenderProps<Self::SignalType, Self::Config>) -> impl IntoView {
                props.signal.with(|value| {
                    view! {
                        <[<$el:lower>]
                            id={props.id.unwrap_or_else(|| props.name.clone())}
                            name={props.name}
                            class={props.class}
                            type="text"
                            value={value}
                            on:change=setter(props.signal, |x| x.as_string())
                        />
                    }
                })
            }
        }
    } };

    (@from $signal_type:ident) => { $signal_type.get() };
    (@from $signal_type:ident $from_signal_type:expr) => { $from_signal_type($signal_type.get()) };
}

str_impl!(
    String,
    Cow<'_, str> { Cow::Owned },
    Oco<'static, str> { Oco::Owned },
);
