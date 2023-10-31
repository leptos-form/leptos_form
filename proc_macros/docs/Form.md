Derives forms from structs.

# Example
```rust
#[derive(leptos_form::Form)]
#[form(component)]
pub struct MyForm {
    pub id: String,
}

#[leptos::component]
pub fn AnotherComponent() -> impl leptos::IntoView {
    let initial = MyForm { id: "1".to_string() };
    view! {
        <MyForm initial={initial} />
    }
}
```

See additional [examples](https://github.com/tlowerison/leptos_form/tree/main/examples).

# Struct attributes

| Attribute   | description                                                                                                                                      | Type                                       | Optional |
|-------------|--------------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------|----------|
| class       | `class` property set on the wrapping \<Form\> element                                                                                            | string                                     | Y        |
| component   | Derive a component for this type using [`leptos::component`]                                                                                     | [component](#component-attributes)         | Y        |
| error       | Specify error rendering behavior which will be used as a default for all fields; defaults to `default`                                           | [error handler](#error-handler-attributes) | Y        |
| field_class | Class property set on the wrapping element for each field by default                                                                             | string                                     | Y        |
| groups      | A list of all groups within the form                                                                                                             | list\<[container](#container-attributes)\> | Y        |
| id          | `id` property set on the wrapping \<Form\> element. Note that this id will prefixed by other ids if this type is used as a field in another form | string                                     | Y        |
| label       | Default label configuration used for all fields                                                                                                  | [label](#label-attributes)                 | Y        |

# Field attributes

Any type which implements [`trait@FormField`] can be used as a field in a struct which derives Form.

| Attribute | description                                                                                                                                      | Type                                       | Optional |
|-----------|--------------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------|----------|
| class     | `class` property set on this field's wrapping element                                                                                            | string                                     | Y        |
| config    | A Rust expression whose type is the [`FormField::Config`] type of this field's type                                                              | expr                                       | Y        |
| el        | The Rust type representing the html tag used to encode this field (note that the field type must implement `FormField<$el>`)                     | type                                       | Y        |
| error     | Specify error rendering behavior for this field, falling back on the container default where needed; defaults to `default`                       | [error handler](#error-handler-attributes) | Y        |
| group     | Group number if this field should be included in a group (0-indexed)                                                                             | usize                                      | Y        |
| id        | `id` property set on the wrapping \<Form\> element. Note that this id will prefixed by other ids if this type is used as a field in another form | string                                     | Y        |
| label     | Label configuration used for this field, falling back on the container default where needed                                                      | [label](#label-attributes)                 | Y        |

## Component attributes
If specified, a leptos component can be produced for this type which will render a form derived from this type's fields.

| Attribute           | description                                                                                                                                             | Type                                 | Optional |
|---------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------|----------|
| action              | Leptos action configuration for this form                                                                                                               | [action](#action-attribute)          | Y        |
| field_changed_class | An additional class to be appended to the containing element of any field whose value has changed                                                       | string                               | Y        |
| map_submit          | Maps this type given its initial and current values into another type which will then be passed to the provided action                                  | [`MapSubmit`]                        | Y        |
| name                | The name of the component function produced; if this type is a tuple struct, name cannot be the type name or the type name prepended with an underscore | ident                                | Y        |
| on_error            | A callback which is called after a form submission error; called with the action's error and the action signal                                          | [`OnError`](components::OnError)     | Y        |
| on_success          | A callback which is called after a successful form submission; called with the successful action outcome and the action signal                          | [`OnSuccess`](components::OnSuccess) | Y        |
| reset_on_success    | Configures whether the form's fields should be reset to the form's initial values upon successful submission; defaults to false                         | bool                                 | Y        |
| submit              | Submit element, typically should be a button                                                                                                            | `impl IntoView`                      | Y        |

## Action attribute
If specified, an action will be attached to the rendered [`Form`](leptos_router::Form) component.

An action can be specified one of two ways:
- a string literal, representing the url the form should be submitted to; in this case, quality of life attributes like `component.map_submit`, `component.on_error`, `component.on_success`, and `component.reset_on_success` cannot be used because the page will immediately reload upon form submission
- a path to a server function specified in a particular way:
    ```rust
    #[derive(Form)]
    #[form(component(action = my_server_fn(my_data)))]
    struct MyForm { .. }

    #[server]
    async fn my_server_fn(my_data: MyData) -> Result<.., ServerFnError> { .. }
    ```

  leptos_form *does not* use [`ActionForm`](leptos_router::ActionForm), and therefore must produce the props to the server function itself which requires knowing the argument names of the server function.
  Thus the peculiar way of specifying server functions.

  ActionForm is avoided because it does not provide hooks for parsing the form prior to form submission.
  We parse the form first into the deriving struct (i.e. the type [`macro@Form`] is derived on) and then either submit it to the server function or pass it to `map_submit` if specified.
  This allows us to keeps the internals of state managament in the form completely separate from representation of the form's data.

## Container attributes
Specifies a containing element to contain wrap children.

| Attribute | description                                       | Optional |
| ----------|---------------------------------------------------|----------|
| tag       | The html tag to use for the wrapping html element | N        |
| id        | `id` property set on the wrapping html element    | Y        |
| class     | `class` property set on the wrapping html element | Y        |

## Error handler attributes
When focus is removed from a field's input element, the input's value is parsed into this deriving struct's field's type.
If there is an error while parsing the input, it can be displayed around the input.
This attribute configures how that error is displayed.
Only one of the below attributes can be used at a time.

| Attribute | description                                                                     | Type                               |
|-----------|---------------------------------------------------------------------------------|------------------------------------|
| component | A leptos component which takes a single prop `error` with type [`FormError`]    | ident                              |
| container | A containing element which will render the stringified error as a child element | [container](#container-attributes) |
| default   | Renders the stringified error in a `<span style="color: red;">` element         | none                               |
| none      | No errors are ever rendered                                                     | none                               |
| raw       | The stringified error is rendered without any containing element                | none                               |

## Label attributes
Configuration for how to render a field's label.
Only one of the below attributes can be used at a time.

| Attribute | description                                                                                                                | Type                                         |
|-----------|----------------------------------------------------------------------------------------------------------------------------|----------------------------------------------|
| adjacent  | Renders the field label adjacent to the field input within a containing element (adjacent in the sense of the DOM tree)    | [adjacent label](#adjacent-label-attributes) |
| default   | Renders the field label adjacent to the field input without any containing element (adjacent in the sense of the DOM tree) | none                                         |
| none      | No label is rendered                                                                                                       | none                                         |
| wrap      | Wraps the field label string and input in a \<label\> element                                                              | [wrapped label](#wrapped-label-attributes)   |

## Adjacent label attributes
Configuration for an adjacent label.
Rendered adjacent labels have the following html structure:
```html
<attr.container.tag>
    <label for="my-field" id={attr.id} class={attr.class}>
        {attr.value}
    </label>
    <input id="my-field" type="text" value="foo" />
</attr.container.tag>
```

| Attribute  | description                                        | Type                                           | Optional                            |
| -----------|----------------------------------------------------|------------------------------------------------|-------------------------------------|
| container  | A containing element which will wrap the \<label\> | [container](#container-attributes)             | N if struct-level, Y if field-level |
| id         | `id` property set on the wrapping html element     | string                                         | Y                                   |
| class      | `class` property set on the wrapping html element  | string                                         | Y                                   |
| rename_all | Rename all labels to a particular case             | [`LabelCase`] (omit the `LabelCase::` portion) | Y (only allowed at struct-level)    |
| value      | A literal string to override the label value       | string                                         | Y (only allowed at field-level)     |

## Wrapped label attributes
Configuration for a wrapped label.
Rendered wrapped labels have the following html structure:
```html
<label for="my-field" id={attr.id} class={attr.class}>
    <div>{attr.value}</div>
    <input id="my-field" type="text" value="foo" />
</label>
```

| Attribute  | description                                        | Type                                           | Optional                            |
| -----------|----------------------------------------------------|------------------------------------------------|-------------------------------------|
| id         | `id` property set on the wrapping html element     | string                                         | Y                                   |
| class      | `class` property set on the wrapping html element  | string                                         | Y                                   |
| rename_all | Rename all labels to a particular case             | [`LabelCase`] (omit the `LabelCase::` portion) | Y (only allowed at struct-level)    |
| value      | A literal string to override the label value       | string                                         | Y (only allowed at field-level)     |
