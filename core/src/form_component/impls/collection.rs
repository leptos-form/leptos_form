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
    Component(#[derivative(Debug = "ignore")] Rc<dyn Fn(Rc<dyn Fn(web_sys::MouseEvent)>) -> View + 'static>),
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

impl Default for AdornmentSpec {
    fn default() -> Self {
        Self {
            class: None,
            height: 24,
            width: 24,
            text: None,
        }
    }
}

impl<U: DefaultHtmlElement> DefaultHtmlElement for Vec<U> {
    type El = Vec<U::El>;
}

#[derive(Clone, Copy, Debug)]
pub struct VecSignalItem<Signal> {
    id: usize,
    index: usize,
    signal: Signal,
}

impl<U, El> FormField<Vec<El>> for Vec<U>
where
    U: FormField<El>,
    <U as FormField<El>>::Signal: Clone,
{
    type Config = VecConfig<<U as FormField<El>>::Config>;
    type Signal = RwSignal<Vec<VecSignalItem<<U as FormField<El>>::Signal>>>;

    fn default_signal() -> Self::Signal {
        create_rw_signal(vec![])
    }
    fn is_default_value(signal: &Self::Signal) -> bool {
        signal.with(|items| items.is_empty())
    }
    fn into_signal(self, config: &Self::Config) -> Self::Signal {
        create_rw_signal(
            self.into_iter()
                .enumerate()
                .map(|(i, item)| VecSignalItem {
                    id: i,
                    index: i,
                    signal: item.into_signal(&config.item),
                })
                .collect(),
        )
    }
    fn try_from_signal(signal: Self::Signal, config: &Self::Config) -> Result<Self, FormError> {
        signal.with(|value| {
            value
                .iter()
                .map(|item| U::try_from_signal(item.signal.clone(), &config.item))
                .collect()
        })
    }
    fn with_error<O>(_: &Self::Signal, f: impl FnOnce(Option<&FormError>) -> O) -> O {
        f(None)
    }
}

impl<U, El> FormComponent<Vec<El>> for Vec<U>
where
    U: FormComponent<El>,
    <U as FormField<El>>::Signal: Clone + std::fmt::Debug,
{
    fn render(props: RenderProps<Self::Signal, Self::Config>) -> impl IntoView {
        let (min_items, max_items) = props.config.items.split();

        let next_id =
            create_rw_signal(
                props
                    .signal
                    .with(|items| if items.is_empty() { 1 } else { items[items.len() - 1].id + 1 }),
            );

        if min_items.is_some() || max_items.is_some() {
            props.signal.update(|items| {
                if let Some(min_items) = min_items {
                    if items.len() < min_items {
                        items.reserve(min_items - items.len());
                        while items.len() < min_items {
                            let id = next_id.get_untracked();
                            items.push(VecSignalItem {
                                id,
                                index: items.len(),
                                signal: U::default_signal(),
                            });
                            next_id.update(|x| *x += 1);
                        }
                    }
                }
                if let Some(max_items) = max_items {
                    while max_items < items.len() {
                        items.pop();
                    }
                }
            });
        }

        let key = move |item: &VecSignalItem<<U as FormField<El>>::Signal>| item.id;
        let VecConfig {
            item: item_config,
            item_class,
            item_label,
            items,
            add,
            remove,
        } = props.config;

        view! {
            <For
                key=key
                each=move || props.signal.get()
                children=move |item| {
                    let id = crate::format_form_id(props.id.as_ref(), item.index.to_string());
                    let item_props = RenderProps::builder()
                        .id(id.clone())
                        .name(crate::format_form_name(Some(&props.name), item.index.to_string()))
                        .class(item_class.map(Oco::Borrowed).or_else(|| props.class.clone()))
                        .signal(item.signal)
                        .config(item_config.clone())
                        .build();

                        VecConfig::<<U as FormField<El>>::Config>::wrap(
                            &items,
                            item_class,
                            item_label.as_ref(),
                            &remove,
                            props.signal,
                            item.index,
                            id,
                            <U as FormComponent<El>>::render(item_props),
                        ).into_view()
                }
            />
            {move || {
                let num_items = props.signal.with(|items| items.len());
                if num_items < max_items.unwrap_or(usize::MAX) {
                    let on_add = move |_| props.signal.update(|items| {
                            let id = next_id.get_untracked();
                            items.push(VecSignalItem { id, index: items.len(), signal: U::default_signal() });
                            next_id.update(|x| *x = id + 1);
                    });

                    match &add {
                        Adornment::Component(component) => component(Rc::new(on_add)),
                        Adornment::Default => view! {
                            <input
                                style="cursor: pointer;"
                                type="button"
                                value="Add"
                                on:click=on_add
                            />
                        }
                        .into_view(),
                        Adornment::Spec(adornment_spec) => view! {
                            <input
                                style="cursor: pointer;"
                                type="button"
                                class={adornment_spec.class.clone()}
                                value={adornment_spec.text.clone().unwrap_or(Oco::Borrowed("Add"))}
                                on:click=on_add
                            />
                        }
                        .into_view(),
                    }
                } else {
                    Default::default()
                }
            }}
        }
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
    fn wrap<Signal>(
        items: &VecItems,
        item_class: Option<&'static str>,
        item_label: Option<&ItemLabel>,
        remove: &Adornment,
        signal: RwSignal<Vec<VecSignalItem<Signal>>>,
        i: usize,
        id: Oco<'static, str>,
        item: impl IntoView,
    ) -> impl IntoView {
        static DEFAULT_ITEM_STYLE: &str =
            "display: flex; flex-direction: row; align-items: center; margin-bottom: 0.5rem";

        let num_items = signal.with(|items| items.len());
        let (min_items, _) = items.split();
        let label_wrapped = match item_label {
            Some(item_label) => item_label.wrap_label(i, id, item),
            None => item.into_view(),
        };

        let more_items_than_min = min_items.unwrap_or_default() < num_items;

        let remove = if more_items_than_min || min_items.is_some() {
            let adornment_style = match (item_class.is_some(), more_items_than_min) {
                (false, true) => Some(Oco::Borrowed("cursor: pointer; margin-left: 0.5rem;")),
                (false, false) => Some(Oco::Borrowed(
                    "cursor: pointer; margin-left: 0.5rem; visibility: hidden",
                )),
                _ => None,
            };

            let on_remove = move |_| {
                signal.update(|items| {
                    items.remove(i);
                    for j in i..items.len() {
                        items[j].index = j;
                    }
                });
            };

            match remove {
                Adornment::Component(component) => component(Rc::new(on_remove)),
                Adornment::Default => view! {
                    <MaterialClose
                        style={adornment_style}
                        on:click=on_remove
                    />
                },
                Adornment::Spec(adornment_spec) => view! {
                    <MaterialClose
                        style={adornment_spec.class.is_none().then_some(adornment_style).flatten()}
                        class={adornment_spec.class.clone()}
                        width={adornment_spec.width}
                        height={adornment_spec.height}
                        on:click=on_remove
                    />
                },
            }
        } else {
            View::default()
        };

        let style = item_class.is_none().then_some(DEFAULT_ITEM_STYLE);

        view! {
            <div class={item_class} style={style}>
                {label_wrapped}
                {remove}
            </div>
        }
    }
}

impl ItemLabel {
    fn wrap_label(&self, i: usize, id: Oco<'static, str>, item: impl IntoView) -> View {
        view! {
            <label for={id} class={self.class}>
                {self.style.render(i)}{self.punctuation.render()}
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
