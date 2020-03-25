+++
title = "Rust - Converting between file formats - JSON, YAML, & TOML"
date = 2020-03-25T21:33:00Z
images = []
tags = []
categories = []
draft = true
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
```

It might be that this conversion fails and the data is not readable into the
other format's `Value`, and you'll get a `Result`. E.g:
[`serde_yaml::Mapping`][ym] allows keys of any variant of [`Value`][yv],
however [`serde_json::map::Map`][sm] is only implemented with `String` as a key
type. TODO: test this e.g - maybe strings can be read as keys, try YAML list.


[ym]: https://docs.rs/serde_yaml/0.8.11/src/serde_yaml/mapping.rs.html#10-12
[sm]: https://docs.serde.rs/serde_json/map/struct.Map.html

This actually has real practical implication.  JSON, for example, is far less
human friendly than either YAML or TOML.  There are libraries, packed with
useful functionality that intend to receive data in a certain format.  Serde's
design decouples this.  [JSON Schema][js] is an example.  It's a richly
featured schema implementation.  But usually you have to write your schemas in
JSON:

[js]: https://json-schema.org/

```json
```

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
```

The same JSON Schema from above is much more readable when written in YAML:

```yaml
```

Examples from this post are [mastered in github][examples].

[examples]:

