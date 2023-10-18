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
    type SignalType = String;
    fn into_signal_type(self, _: Self::Config) -> Self::SignalType {
        self.to_string()
    }
    fn try_from_signal_type(signal_type: Self::SignalType, _: Self::Config) -> Result<Self, FormError> {
        use std::str::FromStr;
        ::uuid::Uuid::from_str(&signal_type).map_err(FormError::parse)
    }
}

#[cfg(feature = "uuid")]
impl<T: 'static> FormComponent<T, HtmlElement<Input>> for ::uuid::Uuid {
    fn render(
        props: RenderProps<
            T,
            impl RefAccessor<T, Self::SignalType>,
            impl MutAccessor<T, Self::SignalType>,
            Self::Config,
        >,
    ) -> impl IntoView {
        props.signal.with(|t| {
            let value = (props.ref_ax)(t);
            view! {
                <input
                    id={props.id.unwrap_or_else(|| props.name.clone())}
                    name={props.name}
                    class={props.class}
                    type="text"
                    value={value}
                    on:change=setter(props.signal, props.mut_ax, |x| x.as_string())
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
                type SignalType = String;
                fn into_signal_type(self, config: Self::Config) -> Self::SignalType {
                    self.format(config.format).to_string()
                }

                $($from)*
            }

            impl<T: 'static> FormComponent<T, HtmlElement<Input>> for $ty {
                fn render(props: RenderProps<T, impl RefAccessor<T, Self::SignalType>, impl MutAccessor<T, Self::SignalType>, Self::Config>) -> impl IntoView {
                    props.signal.with(|t| {
                        let value = (props.ref_ax)(t);
                        view! {
                            <input
                                id={props.id.unwrap_or_else(|| props.name.clone())}
                                name={props.name}
                                class={props.class}
                                type="text"
                                value={value}
                                on:change=setter(props.signal, props.mut_ax, |x| x.as_string())
                            />
                        }
                    })
                }
            }
        } };
    }

    chrono_impl!(
        NaiveDateTime {
            fn try_from_signal_type(signal_type: Self::SignalType, config: Self::Config) -> Result<Self, FormError> {
                Self::parse_from_str(&signal_type, config.format).map_err(FormError::parse)
            }
        },
        DateTime<FixedOffset> {
            fn try_from_signal_type(signal_type: Self::SignalType, config: Self::Config) -> Result<Self, FormError> {
                DateTime::parse_from_str(&signal_type, config.format).map_err(FormError::parse)
            }
        },
        DateTime<Utc> {
            fn try_from_signal_type(signal_type: Self::SignalType, config: Self::Config) -> Result<Self, FormError> {
                DateTime::parse_from_str(&signal_type, config.format).map(|x| x.with_timezone(&Utc)).map_err(FormError::parse)
            }
        },
        DateTime<Local> {
            fn try_from_signal_type(signal_type: Self::SignalType, config: Self::Config) -> Result<Self, FormError> {
                DateTime::parse_from_str(&signal_type, config.format).map(|x| x.with_timezone(&Local)).map_err(FormError::parse)
            }
        },
    );
} }
