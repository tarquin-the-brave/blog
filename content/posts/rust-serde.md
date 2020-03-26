+++
title = "Rust - Converting between file formats - JSON, YAML, & TOML"
date = 2020-03-26T21:00:00Z
images = []
tags = []
categories = []
draft = false
+++

Rust's [`serde` library][serde] is a generic serialize-deserialize framework
that has been implemented for [many file formats][ff].  It's

an incredibly powerful framework and well worth giving [the
documentation][docs] a read.

[serde]: https://crates.io/crates/serde
[ff]: https://docs.serde.rs/serde/#data-formats
[docs]: https://docs.serde.rs/serde/

It can deserialize a file format into a strongly typed rust data structure, so
that the data in code has no affiliation to the data format it was read from,
then can be serialized into another file format.

```rust
#![allow(unused_variables)]
use serde_derive::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
struct MyData {
    field_one: usize,
    field_two: String,
    field_three: bool,
    some_data: std::collections::HashMap<String, usize>,
}

fn main() -> anyhow::Result<()> {
    let my_data_yaml = r#"
        fieldOne: 7
        fieldTwo: "lorem"
        fieldThree: true
        someData:
            x: 1
            y: 2
            z: 3
        "#;

    let my_data_toml = r#"
        fieldOne = 7
        fieldTwo = "lorem"
        fieldThree = true

        [someData]
        x = 1
        y = 2
        z = 3
        "#;

    let my_data_json = r#"
        {
          "fieldOne": 7,
          "fieldTwo": "lorem",
          "fieldThree": true,
          "someData": {
            "x": 1,
            "y": 2,
            "z": 3
          }
        }
        "#;

    let deserialized_yaml = serde_yaml::from_str::<MyData>(my_data_yaml);
    let deserialized_toml = toml::from_str::<MyData>(my_data_toml);
    let deserialized_json = serde_json::from_str::<MyData>(my_data_json);

    assert!(deserialized_yaml.is_ok());
    assert!(deserialized_toml.is_ok());
    assert!(deserialized_json.is_ok());

    let deserialized_toml_copy = deserialized_toml.clone();

    assert_eq!(deserialized_yaml?, deserialized_toml?);
    assert_eq!(deserialized_toml_copy?, deserialized_json?);

    let my_data_yaml_missing_field = r#"
        fieldOne: 7
        fieldTwo: "lorem"
        someData:
            x: 1
            y: 2
            z: 3
        "#;

    let my_data_yaml_extra_field = r#"
        fieldOne: 7
        fieldTwo: "lorem"
        fieldThree: true
        someData:
            x: 1
            y: 2
            z: 3
        out_of_schema_data: 42
        "#;

    let data_missing_field = serde_yaml::from_str::<MyData>(my_data_yaml_missing_field);
    let data_extra_field = serde_yaml::from_str::<MyData>(my_data_yaml_extra_field);

    assert!(data_missing_field.is_err());
    // Because MyData is decorated with `deny_unknown_fields`, adding extra fields
    // will cause parsing to fail.
    assert!(data_extra_field.is_err());

    Ok(())
}
```

You can also read data into a type that can represent any data in a particular
format, if you don't want to or can't strongly define the contents:

```rust
let yaml_data = serde_yaml::from_str::<serde_yaml::Value>(my_data_yaml)?;
let toml_data = toml::from_str::<toml::Value>(my_data_toml)?;
let json_data = serde_json::from_str::<serde_json::Value>(my_data_json)?;
```

Recently I've found that it's even able to deserialize a type that's supposed
to represent one format straight into another, and conversely can serialize a
type for a certain format into another.  For JSON, YAML, and TOML formats there
are the types [`serde_json::Value`][jv], [`serde_yaml::Value`][yv], and
[`toml::Value`][tv] which represent any data in their respective formats and
can used to deserialize data when we can't or don't want to define the precise
structure of the data.  It turns out you can read a file format straight into
one of these other types.

[jv]: https://docs.serde.rs/serde_json/enum.Value.html
[yv]: https://docs.rs/serde_yaml/0.8.11/serde_yaml/enum.Value.html
[tv]: https://docs.rs/toml/0.5.6/toml/value/enum.Value.html

```rust
let toml_from_yaml = serde_yaml::from_str::<toml::Value>(my_data_yaml)?;
```

It might be that this conversion fails and the data is not readable into the
other format's `Value`, and you'll get a `Err`. E.g:
[`serde_yaml::Mapping`][ym] allows keys of any variant of [`Value`][yv],
however [`serde_json::map::Map`][sm] is only implemented with `String` as a key
type:

```rust
let some_yaml = r#"
    [5,6]: true
    "#;

let try_yaml = serde_yaml::from_str::<serde_yaml::Value>(some_yaml);
let try_json = serde_yaml::from_str::<serde_json::Value>(some_yaml);

assert!(try_yaml.is_ok());
assert!(try_json.is_err());
```

[ym]: https://docs.rs/serde_yaml/0.8.11/src/serde_yaml/mapping.rs.html#10-12
[sm]: https://docs.serde.rs/serde_json/map/struct.Map.html

This actually has real practical implication.  JSON, for example, is far less
human friendly than either YAML or TOML.  There are libraries, packed with
useful functionality that intend to receive data in a certain format.  Serde's
design decouples this.  [JSON Schema][js] is an example.  It's a richly
featured schema implementation.  But usually you have to write your schemas in
JSON. [JSON Schema Tool][jst] is an online tool that will generate a inferred
schema from some example JSON.  Using the example given when you visit the
page: this JSON:

[js]: https://json-schema.org/
[jst]: https://jsonschema.net/home

```json
{
    "checked": false,
    "dimensions": {
        "width": 5,
        "height": 10
    },
    "id": 1,
    "name": "A green door",
    "price": 12.5,
    "tags": [
        "home",
        "green"
    ]
}
```

, generates this schema:

```json
{
    "$schema": "http://json-schema.org/draft-07/schema",
    "$id": "http://example.com/root.json",
    "type": "object",
    "title": "The Root Schema",
    "description": "The root schema is the schema that comprises the entire JSON document.",
    "default": {},
    "required": [
        "checked",
        "dimensions",
        "id",
        "name",
        "price",
        "tags"
    ],
    "properties": {
        "checked": {
            "$id": "#/properties/checked",
            "type": "boolean",
            "title": "The Checked Schema",
            "description": "An explanation about the purpose of this instance.",
            "default": false,
            "examples": [
                false
            ]
        },
        "dimensions": {
            "$id": "#/properties/dimensions",
            "type": "object",
            "title": "The Dimensions Schema",
            "description": "An explanation about the purpose of this instance.",
            "default": {},
            "examples": [
                {
                    "height": 10.0,
                    "width": 5.0
                }
            ],
            "required": [
                "width",
                "height"
            ],
            "properties": {
                "width": {
                    "$id": "#/properties/dimensions/properties/width",
                    "type": "integer",
                    "title": "The Width Schema",
                    "description": "An explanation about the purpose of this instance.",
                    "default": 0,
                    "examples": [
                        5
                    ]
                },
                "height": {
                    "$id": "#/properties/dimensions/properties/height",
                    "type": "integer",
                    "title": "The Height Schema",
                    "description": "An explanation about the purpose of this instance.",
                    "default": 0,
                    "examples": [
                        10
                    ]
                }
            }
        },
        "id": {
            "$id": "#/properties/id",
            "type": "integer",
            "title": "The Id Schema",
            "description": "An explanation about the purpose of this instance.",
            "default": 0,
            "examples": [
                1
            ]
        },
        "name": {
            "$id": "#/properties/name",
            "type": "string",
            "title": "The Name Schema",
            "description": "An explanation about the purpose of this instance.",
            "default": "",
            "examples": [
                "A green door"
            ]
        },
        "price": {
            "$id": "#/properties/price",
            "type": "number",
            "title": "The Price Schema",
            "description": "An explanation about the purpose of this instance.",
            "default": 0,
            "examples": [
                12.5
            ]
        },
        "tags": {
            "$id": "#/properties/tags",
            "type": "array",
            "title": "The Tags Schema",
            "description": "An explanation about the purpose of this instance.",
            "default": [],
            "examples": [
                [
                    "home",
                    "green"
                ]
            ],
            "items": {
                "$id": "#/properties/tags/items",
                "type": "string",
                "title": "The Items Schema",
                "description": "An explanation about the purpose of this instance.",
                "default": "",
                "examples": [
                    "home",
                    "green"
                ]
            }
        }
    }
}
```

You could imagine that you might want to expand that schema even further with
defauts, examples and other rules constraining the values of some fields.

Rust has a library [`schemars`][sr] that implements JSON Schema. While the
documentation goes through examples of [defining schemas in code][sic], and
serializing with `serde_json`, you can read a [`SchemaObject`][so] straight
from YAML, or TOML.  So your application can leverage the functionality of the
`SchemaObject` and the `schemars` library while getting users write schemas in
a more human readable form.

[sr]: https://docs.rs/schemars/0.7.0/schemars/
[sic]: https://docs.rs/schemars/0.7.0/schemars/#serde-compatibility
[so]: https://docs.rs/schemars/0.7.0/schemars/schema/struct.SchemaObject.html

```rust
let json_schema = serde_json::from_str::<schemars::schema::RootSchema>(
    &std::fs::read_to_string("example.schema.json")?,
)?;

let json_schema_from_yaml = serde_yaml::from_str::<schemars::schema::RootSchema>(
    &std::fs::read_to_string("example.schema.yaml")?,
)?;
```

The same JSON Schema from above is much more readable when written in YAML:

```yaml
$schema: http://json-schema.org/draft-07/schema
$id: http://example.com/root.json
type: object
title: The Root Schema
description: The root schema is the schema that comprises the entire JSON document.
default: {}
required:
  - checked
  - dimensions
  - id
  - name
  - price
  - tags
properties:
  checked:
    $id: '#/properties/checked'
    type: boolean
    title: The Checked Schema
    description: An explanation about the purpose of this instance.
    default: false
    examples:
      - false
  dimensions:
    $id: '#/properties/dimensions'
    type: object
    title: The Dimensions Schema
    description: An explanation about the purpose of this instance.
    default: {}
    examples:
      - height: 10
        width: 5
    required:
      - width
      - height
    properties:
      width:
        $id: '#/properties/dimensions/properties/width'
        type: integer
        title: The Width Schema
        description: An explanation about the purpose of this instance.
        default: 0
        examples:
          - 5
      height:
        $id: '#/properties/dimensions/properties/height'
        type: integer
        title: The Height Schema
        description: An explanation about the purpose of this instance.
        default: 0
        examples:
          - 10
  id:
    $id: '#/properties/id'
    type: integer
    title: The Id Schema
    description: An explanation about the purpose of this instance.
    default: 0
    examples:
      - 1
  name:
    $id: '#/properties/name'
    type: string
    title: The Name Schema
    description: An explanation about the purpose of this instance.
    default: ''
    examples:
      - A green door
  price:
    $id: '#/properties/price'
    type: number
    title: The Price Schema
    description: An explanation about the purpose of this instance.
    default: 0
    examples:
      - 12.5
  tags:
    $id: '#/properties/tags'
    type: array
    title: The Tags Schema
    description: An explanation about the purpose of this instance.
    default: []
    examples:
      - - home
        - green
    items:
      $id: '#/properties/tags/items'
      type: string
      title: The Items Schema
      description: An explanation about the purpose of this instance.
      default: ''
      examples:
        - home
        - green
```

I learned that [serde][serde] could do this cross-format parsing by reading the
source code of the [refmt][refmt] project.

[refmt]: https://github.com/yoshihitoh/refmt

Examples from this post are [mastered in github][examples].

[examples]: https://github.com/tarquin-the-brave/serde-cross-format

