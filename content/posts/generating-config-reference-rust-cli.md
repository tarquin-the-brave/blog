+++
title = "Generating a Config File Reference for a CLI Tool in Rust"
date = 2020-11-17T21:00:00+00:00
images = []
tags = []
categories = []
draft = false
+++

There's something missing from the documentation of CLI tools.

They often have home pages that do a great job of explaining the core concepts,
giving a "Quick Start" guide, and demonstrating some use cases.

Where I find tools' documentation is missing something, is when you're already
well acquainted with the tool, and you want to know some specific detail about
a single field like what possible values it can have or exactly where it sits
in the structure of the config. I find myself really in need of a "config file
reference" akin to the ones you see for APIs which get generated from API Specs
or code.

Having worked with the Kubernetes API a fair bit over the last few years I find
myself going straight to [the API reference][k8sapi] whenever I need to look
something up, as I'm normally looking up the specifics of where a field lives
or what it does. Having documentation that allows you to follow links to
navigate through the structure of the API objects is really helpful. Kubernetes
also has a load of concept guides and walkthroughs in [its
documentation][k8sdocs]. It's something I'd hold up as a generally well
documented thing.

[k8sapi]: https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.19/
[k8sdocs]: https://kubernetes.io/docs/home/

It's often hard to find out the specifics of a config field if it doesn't
appear in examples or some explanation. Projects that make an attempt to
provide a reference for config seem to follow the same pattern.  For relatively
simple config files they provide an example with all the fields, optional and
not, provided, and then comment the example to say what's optional and what the
possible values are.

![](/images/rust-docs-config-ref/helm-chart-file.png)

This works OK while the config is small and simple.  But as config grows in
size and becomes more complex in terms of what fields are needed, where
sections of config could have alternatives, or be required only if some other
config is set, it starts to get out of hand. That example above is the
documentation for [Helm charts' `Chart.yaml`
file](https://helm.sh/docs/topics/charts/#the-chartyaml-file).

It seems beyond a certain point projects that do provide a config reference
resort to a more protracted form of a web page that lists each field and
provides the information on it.  This produces something that is complete, but
isn't always that easy to navigate, scrolling is often your only resort, and I
find I lose track of where a field is nested within the structure.

In both approaches mentioned there, there is the problem that the config file
reference is maintained separately from the code that defines it.  For
maintainability and correctness' sake, we really want the code that defines
config, and the documentation that attempts to provide a complete and correct
reference for it, to be together.

I've written a few CLI tool in Rust.  I'm not sure I fully understand why, but
Rust is a excellent language for writing CLI tools in.  I don't think there's
much about the core tenets of the language that necessarily make that so.  The
representation of errors in sum types, `Result<T, E>`, does strongly encourage
the errors to be handled right back to `main` and the user, but the `?`
operator makes it all to easy to throw out a cryptic error message from a
library you've called with no context relevant to your application. I think
really it's just that there's some really well designed libraries to make CLI
tools with: [`clap`][clap], [`structopt`][structopt], and [`serde`][serde] (for
parsing config files).

[clap]: https://docs.rs/clap/2.33.3/clap/
[structopt]: https://docs.rs/structopt/0.3.20
[serde]: https://serde.rs/

So what I'm looking for is config file documentation that:

- Reflects the structure of the config,
- Is navigable by following links into and back out of config substructure, and
- Is defined in code.

:mag:

# Rust Docs

I've noticed Rust docs going largely unused in binary crates.

Perhaps they could fulfil what I'm looking for.  Rust docs reflect the
structure of structures. Rust docs are navigable by following links.  The
source for them is defined in code. It's sounding like a strong candidate, on
paper at least...

I've made a dummy CLI tool to see how this looks.

Defining a reasonably simplistic config structure:

```rust
#[derive(serde::Deserialize)]
pub struct Config {
    name: String,
    version: semver::Version,
    description: String,
    source: Source,
    target: std::path::PathBuf,
}

#[derive(serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Source {
    File(std::path::PathBuf),
    Url(String),
}
```

The Rust docs looks like:

![](/images/rust-docs-config-ref/config1.png)

We can see the fields and their types. Not all the types are types in the
format language of the config file, but with a comment we can clarify that.
Where the type doesn't map to a type of the config file format people can
follow the link to see what it is.

This is in keeping with API documentations that may show a fields type to be
another object.  Take the documentation for a [Kubernetes
Pod](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.19/#pod-v1-core)
for example:

![](/images/rust-docs-config-ref/k8s1.png)

`ObjectMeta` & `PodSpec` aren't JSON types, but you follow the link to see
what JSON they're made of.

We can follow the link to `Source` to see what that is:

![](/images/rust-docs-config-ref/source1.png)

Granted, there's some obvious problems emerging already.

If a reader follows a link to a foreign type there's no guarantee that that
type's documentation will helpfully portray how that type is serialized in our
chosen config file format.

For the enum above, the representation in the Rust docs is quite diverged from
how it would appear in the config file.  And perhaps some Rust literacy is
going to be needed to know that an enum is something that could be one variant,
or another.

Any difference between the Rust structure and config file representation that's
given by attributes, e.g. the:

```rust
#[serde(rename_all = "snake_case")]
```

isn't accounted for.

But maybe these are things we can fix up with comments.  Let's assume for now
the config file format is YAML.

![](/images/rust-docs-config-ref/config2.png)

Following the link to `Source` again:

![](/images/rust-docs-config-ref/source2.png)

This doesn't look so bad now.  There's even a nice link to go back up to top
level config, which when you follow it, takes you to the field this data is
under in the parent.

![](/images/rust-docs-config-ref/config2source.png)

You can also expand and collapse the descriptions of each field as you browse
around.

I admit, I am massively skimming over the face that in both cases of `Source`
and `Config` what I'm showing above [isn't all that appears on the
page][config2]. There's methods and trait implementations below and a sidebar
that links you to them with a big Rust symbol in it.  This is definitely a
problem, but one that I'm going to come back to. :see_no_evil: :hear_no_evil:

[config2]: https://tarquin-the-brave.github.io/a-cli-tool/rust-docs/a_cli_tool/config2/struct.Config.html

Let's make this config more complex and see what happens! :smirk_cat:

```rust
#[derive(serde::Deserialize)]
pub struct Config {
    name: String,
    version: semver::Version,
    description: Option<String>,
    #[serde(flatten)]
    data: AppData,
}

#[derive(serde::Deserialize)]
pub struct AppData {
    source: Source,
    target: std::path::PathBuf,
    actions: Vec<Actions>,
}

#[derive(serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Source {
    File(std::path::PathBuf),
    Url(String),
}

#[derive(serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Actions {
    Foo,
    Bar,
    Baz,
    FooBar,
    BarBaz,
    #[serde(rename = "fbb")]
    FooBarBaz,
}
```

So now our top level `Config` object defines some metadata fields: `name`,
`version`, and a now optional `description`, and flattens in the configuration
data for the application.  This is going to take a bit more explaining in the
comments... :cold_sweat:

![](/images/rust-docs-config-ref/config4.png)

The `Option` in the `description` field looks OK.  With the accompanying
"_Optional_:" added to the field description it gets across the field is
optional without too much confusion.

The flattened fields under `data` are less ideal.  An explanation and an
example can "set the record straight" but we're starting to have the docs
deviate from our goal of representing the structure of the config data.

The docs that come from the `AppData` have come out OK as it's a structure that
we're not changing field names of structure of with the serde representation.

![](/images/rust-docs-config-ref/appdata4.png)

We're even able to link to the "possible operations" from the `actions` field.

![](/images/rust-docs-config-ref/actions4.png)

As with the `Sources` enum and the flattening of the `AppData` into the top
level config we're having the problem of:

> Every time the Rust code doesn't match the representation in a config file
> we need to compensate with a comment that explains things.

This is going to happen every time we do anything like:
- renaming fields,
- flattening structures,
- making enum variants be represented by lowercase versions of themselves,
- ["un-tag" an enum](https://serde.rs/container-attrs.html#untagged), or
- any other [alternative way of representing
  enums](https://serde.rs/enum-representations.html).

There's other things we might want to do to the Rust code that would cause
further explaining and back tracking in the comments.  We might find the top
level encapsulating structure of the config is something we want to reuse
throughout the codebase and make it generic over the data the user provides and
what we transform that data into:

```rust
#[derive(serde::Deserialize)]
pub struct Config<T> {
    name: String,
    version: semver::Version,
    description: Option<String>,
    #[serde(flatten)]
    data: T,
}
```

Parsing `Config<AppData>` when we read the config file, but later transform the
data under `data` into another type.

Finally the problem I've been ignoring so far:

> These are Rust docs and have a load of other stuff in them that aren't relevant
> to someone writing the config file.

![](/images/rust-docs-config-ref/config4wide.png)

This could cause some confusion for the reader. :dizzy_face:

So can you use Rust docs to generate a config file reference to supplement the
main docs for a tool? Probably not for a tool with a public user base. But if
you're developing a tool internally in an organisation that are predominately
Rust literate, the downsides may not be too bad for you, and you could benefit
from the completeness and navigability.

Obviously it's not what Rust docs were made to do, but it's been interesting to
see how far I can get.

My example project has [published these docs][config5] so you can take a look
for yourself.

[config5]: https://tarquin-the-brave.github.io/a-cli-tool/rust-docs/a_cli_tool/config5/struct.Config.html

# Via A Generated Schema

For this blog post I was looking around at "what APIs do" to provide this
navigable reference.  It seems a lot of them don't, providing only an OpenAPI
specification instead.  There seems to be a few tools out there that turn
OpenAPI specs into an HTML page. I wonder if something similar can be done for
config files.

"Something similar" in this case being:

> Have a description of the config file in a well known schema language and
  find a tool to turn that into HTML.

My initial thought about this was that I didn't really fancy mastering the
description of config in a schema language rather than in in code.

But take [JSON Schema][jsonsch] as an example, the [`schemars`][schemars] crate
lets you generate schemas from structures. So I stuck
`#[derive(schemars::JsonSchema)]` to all my config structures and got the tool
to output a JSON Schema:

[jsonsch]: https://json-schema.org/
[schemars]: https://docs.rs/schemars/0.8.0/schemars/

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "Config Reference",
  "description": "Config file reference for `a_cli_tool`.\n\nBy default `a_cli_tool` looks for configuration in `./config.yaml`, unless another path is specified with the `-c/--config` parameter.\n\n`Config` details the structure of the configuration.",
  "type": "object",
  "required": [
    "actions",
    "name",
    "source",
    "target",
    "version"
  ],
  "properties": {
    "name": {
      "description": "The name of the thing this CLI tool is building for you.",
      "type": "string"
    },
    "version": {
      "description": "The version of the thing this CLI tool will build for you.\n\nThis is a [SemVer][semver] version, e.g:\n\n```yaml version: 1.2.3 ```\n\n[semver]: https://semver.org/",
      "type": "string"
    },
    "description": {
      "description": "_Optional:_ A description of the thing this CLI tool is building for you.",
      "type": [
        "string",
        "null"
      ]
    },
    "source": {
      "description": "The configuration for the source of data for this tool.",
      "allOf": [
        {
          "$ref": "#/definitions/Source"
        }
      ]
    },
    "target": {
      "description": "A path to write the created thing to.",
      "type": "string"
    },
    "actions": {
      "description": "The operations to perform on the data this tool manipulates.\n\nThis array of operations will be performed in order and an operation may appear more than once.\n\nE.g:\n\n```yaml actions: [ foo, bar, baz, bar ] ```",
      "type": "array",
      "items": {
        "$ref": "#/definitions/Actions"
      }
    }
  },
  "definitions": {
    "Source": {
      "description": "The configuration for the source of data for this tool.\n\nThis can either be set to a local file:\n\n```yaml source: file: path/to/file.yaml ```\n\nOr a URL:\n\n```yaml source: url: https://urlofsource.com/sourcedata/ ```\n\n---\n\nBack to:\n\n- [App Configuration](./struct.AppData.html#structfield.source) - [Configuration Reference](./struct.Config.html#structfield.data)",
      "anyOf": [
        {
          "type": "object",
          "required": [
            "file"
          ],
          "properties": {
            "file": {
              "type": "string"
            }
          }
        },
        {
          "type": "object",
          "required": [
            "url"
          ],
          "properties": {
            "url": {
              "type": "string"
            }
          }
        }
      ]
    },
    "Actions": {
      "description": "The possible operations to perform on the data this tool manipulates.\n\nSee each option below for what it does and how it's referenced in config.",
      "type": "string",
      "enum": [
        "foo",
        "bar",
        "baz",
        "foo_bar",
        "bar_baz",
        "fbb"
      ]
    }
  }
}
```

There was one change I had to make. Where previously I had the `version` field
typed as:

```rust
  version: sever::Version,
```

This gave me error:

```
error[E0277]: the trait bound `semver::Version: schemars::JsonSchema` is not satisfied
  --> src/config5.rs:29:14
   |
29 |     version: semver::Version,
   |              ^^^^^^ the trait `schemars::JsonSchema` is not implemented for `semver::Version`
   |
   = note: required by `schemars::JsonSchema::add_schema_as_property`
```

I could have tried to implement the trait for a newtype wrapper around
`semver::Version` but for now I just changed that to be a `String`.  If using
this approach on a real tool, where there may be a few foreign types that don't
implement `JsonSchema` included in the tool's config, it might end up more
practical to parse the config first in terms of types that `JsonSchema` _is_
implemented for then perform a 2nd parsing stage.

In this example we could do:

```rust
#[derive(serde::Deserialize, schemars::JsonSchema)]
pub struct Config<V = String> {
    name: String,
    version: V,
    description: Option<String>,
    #[serde(flatten)]
    data: AppData,
}

impl Config<String> {
    pub fn parse(self) -> anyhow::Result<Config<semver::Version>> {
        Ok(Config {
            name: self.name,
            description: self.description,
            data: self.data,

            version: semver::Version::parse(&self.version)?,
        })
    }
}
```

and generate the JSON Schema for `Config<String>`.

Although as the number of foreign types, or types not implementing
`JsonSchema`, grows this might produce an unwieldy number of type parameters.

So after a quick look on the internet I found [a tool to turn a JSON Schema
into an HTML page](https://coveooss.github.io/json-schema-for-humans/).  The
result [looks rather smart](https://tarquin-the-brave.github.io/a-cli-tool/schema-ref/config/schema_doc.html).

Recall the config structures (with comments and derive attributes removed) are:

```rust
pub struct Config<V = String> {
    name: String,
    version: V,
    description: Option<String>,
    #[serde(flatten)]
    data: AppData,
}

pub struct AppData {
    source: Source,
    target: std::path::PathBuf,
    actions: Vec<Actions>,
}

pub enum Source {
    File(std::path::PathBuf),
    Url(String),
}

#[serde(rename_all = "snake_case")]
pub enum Actions {
    Foo,
    Bar,
    Baz,
    FooBar,
    BarBaz,
    #[serde(rename = "fbb")]
    FooBarBaz,
}
```

The HTML document looks like:

![](/images/rust-docs-config-ref/config6.png)

This time I'm not cropping anything out!  This is all that appears.  The fields
from `AppData` have been nicely flattened into the top level.

We can click on the fields to expand them:

![](/images/rust-docs-config-ref/config6meta.png)

It's even tried to render the markdown in the fields' doc comments.
Unfortunately it hasn't rendered the syntax highlighting hint properly.  But
this was the first tool I found from searching on the internet so I'll not let
small formatting details put a downer on things for now. :sun_with_face:

What it's done for our enums is nice:

![](/images/rust-docs-config-ref/config6source1.png)

Clicking on `Option 2` we see:

![](/images/rust-docs-config-ref/config6source2.png)

Here I put the doc comments on the enum rather than individual variants.  Had I
done that instead there'd be a description of `url` and `file` available.

I really like how the `actions` field has rendered:

![](/images/rust-docs-config-ref/config6actions.png)

This is all looking quite good. As config grows in complexity it looks like
this format will naturally extend and be navigable and readable.

The `schemars::Schema` trait seems to generate a good JSON Schema, and once you
have your JSON Schema you can make your choice of JSON Schema -> HTML renderer.

What could be a problem with this approach is generating that Schema. In this
example I went for a workaround to `JsonSchema` not being implemented for a
foreign type by parsing config in two steps, but looking at [some of the
implementations](https://docs.rs/schemars/0.8.0/src/schemars/json_schema_impls/serdejson.rs.html#7-17)
of the trait, it's not too hard to implement it for a newtype wrapper around a
foreign type.

```rust
#[derive(serde::Deserialize)]
pub struct Version(semver::Version);

impl schemars::JsonSchema for Version {
    fn is_referenceable() -> bool {
        false
    }

    fn schema_name() -> String {
        "Version".to_string()
    }

    fn json_schema(_: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        schemars::schema::SchemaObject {
            instance_type: Some(schemars::schema::InstanceType::String.into()),
            ..Default::default()
        }
        .into()
    }
}
```

So maybe there's a bit of work to get the JSON Schema for your config, but once
you've generated it, you can choose what you use to render it into HTML.

It might be a good thing on its own, for a tool to be able to produce a JSON
Schema for its config file.

# What About CLIs?

CLIs have a structure.  If you use [`structopt`][structopt] to define your CLI
you define a Rust structure with annotations on the fields.

The CLI help text
you get from [`clap`][clap] (the crate `structopt` calls) is pretty good, but
if we're creating a navigable HTML reference for our config, why not for our
CLI too?

I gave this a quick go, trying both of the approaches above.  Defining a dummy
CLI with:

```rust
#[derive(structopt::StructOpt, schemars::JsonSchema)]
pub struct Cli {
    #[structopt(short, long)]
    config: std::path::PathBuf,

    #[structopt(long)]
    dry_run: bool,

    #[structopt(subcommand)]
    subcommand: Subcommands,
}

#[derive(structopt::StructOpt, schemars::JsonSchema)]
pub enum Subcommands {
    Foo,
    Bar,
    Baz,
}
```

Without adding any comments: the Rust docs look like:

![](/images/rust-docs-config-ref/cli1.png)

![](/images/rust-docs-config-ref/subcommands1.png)

And rendering a generated JSON Schema looks like:

![](/images/rust-docs-config-ref/cli1html.png)

When generating a reference for the config file, the "rendered JSON Schema"
approach had an advantage over the "Rust docs" approach  as the `serde`
annotations that mapped the Rust representation to the representation the user
sees were taken account of. In this case, neither approach gets the `structopt`
annotations taken account of and both would require commenting to explain which
fields are arguments, parameters, subcommands and how they're represented on
the CLI.

An alternative approach here could be to generate man pages, then use something
like [Pandoc][pandoc] to convert them to HTML.

[pandoc]: https://pandoc.org/

I wanted to give this a go, but [the approach the "Rust CLI" book][genman]
suggests for generating man pages tells you to use the
`clap_generate::gen_manuals` function, which I can't find in the
[`clap_generate` documentation][clapgen]... That might require waiting for
`clap` V3.

[clapgen]: https://docs.rs/clap_generate/3.0.0-beta.2/clap_generate/
[genman]: https://rust-cli.github.io/book/in-depth/docs.html

# These Examples

I've published the examples from this blog on Github Pages. Take a look around.

- Rust Docs
  + [Config File Reference](https://tarquin-the-brave.github.io/a-cli-tool/rust-docs/a_cli_tool/config7/struct.Config.html)
  + [CLI Reference (uncommented)](https://tarquin-the-brave.github.io/a-cli-tool/rust-docs/a_cli_tool/cli/struct.Cli.html)
  + [All Rust Docs](https://tarquin-the-brave.github.io/a-cli-tool/rust-docs/a_cli_tool/)
- Generated from JSON Schema
  + [Config File Reference](https://tarquin-the-brave.github.io/a-cli-tool/schema-ref/config/schema_doc.html)
  + [CLI Reference (uncommented)](https://tarquin-the-brave.github.io/a-cli-tool/schema-ref/cli/schema_doc.html)

# Potential Project?

I wonder, and please do comment, if there's anything I've missed that can do a
better job of generating a reference to supplement user docs.

If I was making a new CLI tool today that took a config file, I think I'd take
[the JSON Schema approach](#via-a-generated-schema), and shop around a bit for
the "JSON Schema -> HTML" renderer.

But that approach still requires going to the effort of producing the JSON
Schema. I'm also not sure that every config file format, that have crates that
implement deserializers for `serde`, can have what you'd reasonably express in
it described by JSON Schema.  I say "reasonably", because I know of examples
like in YAML where you can have non-strings as keys to objects, but when you're
defining structured config, I don't know that you would expect to do that.

Wouldn't it be nice if there was a `cargo` subcommand that could generate these
navigable, complete, reference docs for both config files and CLIs?

Perhaps you could give it a structure, tell it whether it it's config or a CLI,
and get some HTML generated.

```bash
cargo user-docs config config::MyAppConfig
```

```bash
cargo user-docs cli cli::MyAppCli
```

My guess on how to start looking into how to do this would be to read though
the `cargo-doc` source code and see what it does, and whether it's possible to
use some of it.

At some point it'll have to spot the existence or expansion of the annotations
given that describe how the Rust structures are represented in config or as CLI
commands.  This might be problematic as it would probably have to have
knowledge of or assume particular crates have been used for each purpose.

Please do comment if you have any thoughts: knowing of a project like this,
advise on how one might go about doing this, or experience from other
languages.
