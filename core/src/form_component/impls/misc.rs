#![allow(unused)]

use crate::*;
use ::leptos::html::*;
use ::leptos::*;

#[cfg(feature = "uuid")]
impl DefaultHtmlElement for ::uuid::Uuid {
    type El = HtmlElement<Input>;
}

#[cfg(feature = "uuid")]
impl FormSignalType<HtmlElement<Input>> for ::uuid::Uuid {
    type Config = ();
    type SignalType = RwSignal<String>;

    fn default_signal() -> Self::SignalType {
        RwSignal::new(Default::default())
    }
    fn is_default_value(signal: &Self::SignalType) -> bool {
        signal.with(|value| value.is_empty())
    }
    fn into_signal_type(self, _: &Self::Config) -> Self::SignalType {
        RwSignal::new(self.to_string())
    }
    fn try_from_signal_type(signal_type: Self::SignalType, _: &Self::Config) -> Result<Self, FormError> {
        use std::str::FromStr;
        signal_type.with(|value| ::uuid::Uuid::from_str(value).map_err(FormError::parse))
    }
}

#[cfg(feature = "uuid")]
impl FormComponent<HtmlElement<Input>> for ::uuid::Uuid {
    fn render(props: RenderProps<Self::SignalType, Self::Config>) -> impl IntoView {
        props.signal.with(|value| {
            view! {
                <input
                    id={props.id.unwrap_or_else(|| props.name.clone())}
                    name={props.name}
                    class={props.class}
                    type="text"
                    value={value}
                    on:change=setter(props.signal, |x| x.as_string())
                />
            }
        });
    }
}

cfg_if! { if #[cfg(feature = "chrono")] {
    use ::chrono::*;

    #[derive(Clone, Debug)]
    pub struct ChronoConfig {
        pub format: &'static str,
    }

    impl Default for ChronoConfig {
        fn default() -> Self {
            Self { format: "%c" }
        }
    }

    macro_rules! chrono_impl {
        ($($ty:ty { $($from:tt)* }),*$(,)?) => { $(
            chrono_impl! { @ $ty { $($from)* } }
        )* };

        (@ $ty:ty { $($from:tt)* }) => { paste! {
            impl DefaultHtmlElement for $ty {
                type El = HtmlElement<Input>;
            }

            impl FormSignalType<HtmlElement<Input>> for $ty {
                type Config = ChronoConfig;
                type SignalType = RwSignal<String>;

                fn default_signal() -> Self::SignalType {
                    RwSignal::new(Default::default())
                }
                fn is_default_value(signal: &Self::SignalType) -> bool {
                    signal.with(|value| value.is_empty())
                }
                fn into_signal_type(self, config: &Self::Config) -> Self::SignalType {
                    RwSignal::new(self.format(config.format).to_string())
                }

                $($from)*
            }

            impl FormComponent<HtmlElement<Input>> for $ty {
                fn render(props: RenderProps<Self::SignalType, Self::Config>) -> impl IntoView {
                    props.signal.with(|value| {
                        view! {
                            <input
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
    }

    chrono_impl!(
        NaiveDateTime {
            fn try_from_signal_type(signal: Self::SignalType, config: &Self::Config) -> Result<Self, FormError> {
                signal.with(|value| Self::parse_from_str(value, config.format).map_err(FormError::parse))
            }
        },
        DateTime<FixedOffset> {
            fn try_from_signal_type(signal: Self::SignalType, config: &Self::Config) -> Result<Self, FormError> {
                signal.with(|value| DateTime::parse_from_str(value, config.format).map_err(FormError::parse))
            }
        },
        DateTime<Utc> {
            fn try_from_signal_type(signal: Self::SignalType, config: &Self::Config) -> Result<Self, FormError> {
                signal.with(|value| DateTime::parse_from_str(value, config.format).map(|x| x.with_timezone(&Utc)).map_err(FormError::parse))
            }
        },
        DateTime<Local> {
            fn try_from_signal_type(signal: Self::SignalType, config: &Self::Config) -> Result<Self, FormError> {
                signal.with(|value| DateTime::parse_from_str(value, config.format).map(|x| x.with_timezone(&Local)).map_err(FormError::parse))
            }
        },
    );
} }
