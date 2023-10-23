use crate::components::*;
use crate::*;
use ::core::ops::*;
use ::leptos::*;
use ::std::rc::Rc;

#[derive(Clone, Default, Derivative, TypedBuilder)]
#[builder(field_defaults(default, setter(into)))]
#[derivative(Debug)]
pub struct VecConfig<Config: Default> {
    pub item: Config,
    #[builder(default)]
    pub item_class: Option<&'static str>,
    #[builder(setter(strip_option))]
    pub item_label: Option<ItemLabel>,
    pub items: VecItems,
    pub add: Adornment,
    pub remove: Adornment,
}

#[derive(Clone, Copy, Debug, Default, TypedBuilder)]
#[builder(field_defaults(default))]
pub struct ItemLabel {
    pub class: Option<&'static str>,
    pub style: ItemLabelStyle,
    pub punctuation: ItemLabelPunctuation,
}

#[derive(Clone, Copy, Debug, Default)]
pub enum ItemLabelStyle {
    CapitalLetter,
    Letter,
    #[default]
    Number,
}

#[derive(Clone, Copy, Debug, Default)]
pub enum ItemLabelPunctuation {
    #[default]
    None,
    Parenthesis,
    Period,
}

#[derive(Clone, Copy, Debug, Default)]
pub enum VecItems {
    Bounded {
        min: Option<usize>,
        max: Option<usize>,
    },
    Const(usize),
    #[default]
    Unbounded,
}

#[derive(Clone, Default, Derivative)]
#[derivative(Debug)]
pub enum Adornment {
    #[default]
    Default,
    Component(#[derivative(Debug = "ignore")] Rc<dyn Fn() -> View + 'static>),
    Spec(AdornmentSpec),
}

#[derive(Clone, Debug, TypedBuilder)]
#[builder(field_defaults(default, setter(into)))]
pub struct AdornmentSpec {
    #[builder(setter(strip_option))]
    pub class: Option<Oco<'static, str>>,
    #[builder(default = 24)]
    pub height: usize,
    #[builder(default = 24)]
    pub width: usize,
    pub text: Option<Oco<'static, str>>,
}

impl<U: DefaultHtmlElement> DefaultHtmlElement for Vec<U> {
    type El = Vec<U::El>;
}

impl<U, El> FormSignalType<Vec<El>> for Vec<U>
where
    U: FormSignalType<El>,
    <U as FormSignalType<El>>::SignalType: Clone,
{
    type Config = VecConfig<<U as FormSignalType<El>>::Config>;
    type SignalType = Vec<<U as FormSignalType<El>>::SignalType>;

    fn into_signal_type(self, config: &Self::Config) -> Self::SignalType {
        self.into_iter().map(|u| u.into_signal_type(&config.item)).collect()
    }
    fn try_from_signal_type(signal_type: Self::SignalType, config: &Self::Config) -> Result<Self, FormError> {
        signal_type
            .into_iter()
            .map(|inner_singal_type| U::try_from_signal_type(inner_singal_type, &config.item))
            .collect()
    }
}

impl<U, T: 'static, El> FormComponent<T, Vec<El>> for Vec<U>
where
    U: FormComponent<T, El>,
    <U as FormSignalType<El>>::SignalType: Clone,
{
    fn render(
        props: RenderProps<
            T,
            impl RefAccessor<T, Self::SignalType>,
            impl MutAccessor<T, Self::SignalType>,
            Self::Config,
        >,
    ) -> impl IntoView {
        let vec_config = &props.config;
        let (min_items, max_items) = vec_config.items.split();

        if min_items.is_some() || max_items.is_some() {
            props.signal.update(|t| {
                let value = (props.mut_ax)(t);
                if let Some(min_items) = min_items {
                    if value.len() < min_items {
                        value.resize(min_items, Default::default());
                    }
                }
                if let Some(max_items) = max_items {
                    if max_items < value.len() {
                        value.resize(max_items, Default::default());
                    }
                }
            });
        }

        props.signal.with(|t| {
            let value = (props.ref_ax)(t);
            let num_items = value.len();
            let mut nodes = value
                .iter()
                .enumerate()
                .map(|(i, _)| {
                    let id = crate::format_form_id(props.id.as_ref(), i.to_string());
                    let props = RenderProps::builder()
                        .id(id.clone())
                        .name(crate::format_form_name(Some(&props.name), i.to_string()))
                        .ref_ax(ref_ax_factory(move |t| &(props.ref_ax)(t)[i]))
                        .mut_ax(mut_ax_factory(move |t| &mut (props.mut_ax)(t)[i]))
                        .signal(props.signal)
                        .config(props.config.item.clone())
                        .build();
                    vec_config
                        .wrap(num_items, i, id, <U as FormComponent<T, El>>::render(props))
                        .into_view()
                })
                .collect::<Vec<_>>();

            if max_items.is_none() || num_items < max_items.unwrap() {
                nodes.push(match &vec_config.add {
                    Adornment::Component(component) => component(),
                    Adornment::Default => view! {
                        <input
                            type="button"
                            value="Add"
                            on:click={move |_| props.signal.update(|t| {
                                let value = (props.mut_ax)(t);
                                value.push(Default::default());
                            })}
                        />
                    }
                    .into_view(),
                    Adornment::Spec(adornment_spec) => view! {
                        <button
                            class={adornment_spec.class.clone()}
                            value={adornment_spec.text.clone().unwrap_or(Oco::Borrowed("Add"))}
                            on:click={move |_| props.signal.update(|t| {
                                let value = (props.mut_ax)(t);
                                value.push(Default::default());
                            })}
                        />
                    }
                    .into_view(),
                });
            }

            nodes
        })
    }
}

impl From<usize> for VecItems {
    fn from(value: usize) -> Self {
        Self::Const(value)
    }
}

impl From<(usize, usize)> for VecItems {
    fn from(value: (usize, usize)) -> Self {
        Self::Bounded {
            min: Some(value.0),
            max: Some(value.1),
        }
    }
}

static ASCII_LOWER: [char; 26] = [
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w',
    'x', 'y', 'z',
];

static ASCII_UPPER: [char; 26] = [
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W',
    'X', 'Y', 'Z',
];

impl<Config: Default> VecConfig<Config> {
    fn wrap(&self, num_items: usize, i: usize, id: Oco<'static, str>, item: impl IntoView) -> impl IntoView {
        static DEFAULT_ITEM_STYLE: &str = "display: flex; flex-direction: row;";

        let (min_items, _) = self.items.split();
        let label_wrapped = self.wrap_label(i, id, item);

        let more_items_than_min = min_items.unwrap_or_default() < num_items;

        let remove = if more_items_than_min || min_items.is_some() {
            let adornment_style = match (self.item_class.is_some(), more_items_than_min) {
                (false, true) => Some(Oco::Borrowed("margin-left: 0.5 rem;")),
                (false, false) => Some(Oco::Borrowed("margin-left: 0.5 rem; visibility: hidden")),
                _ => None,
            };

            match &self.remove {
                Adornment::Component(component) => component(),
                Adornment::Default => view! { <MaterialClose style={adornment_style} /> },
                Adornment::Spec(adornment_spec) => view! {
                    <MaterialClose
                        style={adornment_spec.class.is_none().then_some(adornment_style).flatten()}
                        class={adornment_spec.class.clone()}
                        width={adornment_spec.width}
                        height={adornment_spec.height}
                    />
                },
            }
        } else {
            View::default()
        };

        let style = self.item_class.is_none().then_some(DEFAULT_ITEM_STYLE);

        view! {
            <div class={self.item_class} style={style}>
                {label_wrapped}
                {remove}
            </div>
        }
    }

    fn wrap_label(&self, i: usize, id: Oco<'static, str>, item: impl IntoView) -> View {
        let item_label = match self.item_label {
            Some(item_label) => item_label,
            None => return item.into_view(),
        };
        view! {
            <label for={id} class={item_label.class}>
                {item_label.style.render(i)}{item_label.punctuation.render()}
                {item}
            </label>
        }
        .into_view()
    }
}

impl VecItems {
    fn split(&self) -> (Option<usize>, Option<usize>) {
        match *self {
            VecItems::Bounded { min, max } => (min, max),
            VecItems::Const(num_items) => (Some(num_items), Some(num_items)),
            VecItems::Unbounded => (None, None),
        }
    }
}

impl ItemLabelStyle {
    fn render(&self, i: usize) -> String {
        let ascii_set = match self {
            Self::CapitalLetter => &ASCII_UPPER,
            Self::Letter => &ASCII_LOWER,
            Self::Number => return (i + 1).to_string(),
        };
        let n = (i.ilog(ascii_set.len()) + 1) as usize;
        let mut chars = Vec::with_capacity(n);
        let mut num = i;
        for j in 0..n {
            chars[n - 1 - j] = ascii_set[num % ascii_set.len()];
            num /= ascii_set.len();
        }
        chars.into_iter().collect()
    }
}

impl ItemLabelPunctuation {
    fn render(&self) -> &'static str {
        match self {
            Self::None => "",
            Self::Parenthesis => ")",
            Self::Period => ".",
        }
    }
}
