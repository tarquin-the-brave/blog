+++
title = "A Thought on CLI Design"
date = 2021-03-27T09:00:00Z
images = []
tags = []
categories = []
draft = false
+++

Recently I was using a wrapper around a CLI tool.  It does what you might
expect a CLI wrapper to do: set up some peripheral things, environment
variables and such like, then runs the underlying CLI tool with some CLI
parameters set and passes argument given to the wrapper down to provide extra
parameters.  This pattern can be pretty useful to make a particular use case of
a CLI tool easier to perform. E.g:

```bash
#!/usr/bin/env sh
underlying-cli-tool --some-param foo --other-param bar $@
```

But there was a problem.  One of the parameters I passed to the wrapper was
already being set by the wrapper.  The underlying CLI tool then errored with
something to the effect of:

> "You've set the same parameter twice! :scream:"

This was easy enough to work around.  I had ownership of the wrapper and could
edit the source code to _not_ set that parameter explicitly, rebuild it and run
it as I'd initially intended.  But I did think:

> "That was a bit annoying"

and had I not had ownership of the wrapper it could have been more annoying to
fork it, edit the source, then potentially deal with rebuilding it when I
haven't got the build environment for it at my fingertips.

---

This got me thinking:

> "How should have things worked here?"

I can see where the designers of the underlying CLI tool were coming from.

> "If a parameter is provided twice, when it makes no sense to provide it twice,
  obviously it's a user error and we should error on this."

But this does mean that any wrappers written around the CLI tool have to deal
with the complexity of:

- Look at the extra parameters
- For any of them that conflict with ones already set:
  + Does the underlying CLI tool allow it to be set twice?
  + If not: take that parameter out of the ones being set by default to allow
    the user's value to be set instead.

And maybe it isn't a user error.  Imagine you've just run a big long command
then want to change the one of the earlier parameters you gave and run it again.
It would be more convenient to arrow up then put the parameter again at the
end to override it than to go back into the command line and edit the parameter.

Wouldn't this all be easier if the underlying CLI tool accepted a repeated
parameter, taking the latter as overriding the former?

Ultimately there's a trade off between a potential bit of robustness against an
error by a direct user of the CLI tool and the ability to write convenient
wrappers around it.  I suspect in many cases the latter isn't considered.

---

In reality if you're writing a CLI tool, you're likely using a library to
do the heavy lifting, rather than hand rolling it yourself.

Let's have a look at a couple (using the basic mainline usage).

Rust's [`structopt`][structopt] ([`clap`][clap] under the covers):

```rust
#[derive(Debug, structopt::StructOpt)]
pub struct Cli {
    #[structopt(long)]
    foo: String,

    #[structopt(long)]
    bar: String,
}

fn main() {
    use structopt::StructOpt as _;
    let cli = Cli::from_args();

    println!("Got: {:?}", cli);
}
```

```
$ cargo run -q -- --foo foo --bar bar
Got: Cli { foo: "foo", bar: "bar" }
$ cargo run -q -- --foo foo --bar bar --foo foo2
error: The argument '--foo <foo>' was provided more than once, but cannot be used multiple times

USAGE:
    a-cli-tool --bar <bar> --foo <foo>

For more information try --help
$
```

Python's [`click`][click]:

```python
import click

@click.command()
@click.option('--foo', required=True)
@click.option('--bar', required=True)
def hello(foo, bar):
    print(f"Got: foo={foo}, bar={bar}")

if __name__ == '__main__':
    hello()
```

```
$ python3 some-tool.py --foo foo --bar bar
Got: foo=foo, bar=bar
$ python3 some-tool.py --foo foo --bar bar --foo foo2
Got: foo=foo2, bar=bar
$
```

Reading the docs for [`clap`][clap] I can get it to allow this overriding
behaviour:

```rust
#[derive(Debug, structopt::StructOpt)]
pub struct Cli {
    #[structopt(long, multiple = true, required = true)]
    foo: Vec<String>,

    #[structopt(long)]
    bar: String,
}

fn main() {
    use structopt::StructOpt as _;
    let mut cli = Cli::from_args();

    let foo = cli.foo.pop().unwrap();
    let bar = cli.bar.clone();

    println!("Got: foo={}, bar={}", foo, bar);
}
```

```
$ cargo run -q -- --foo foo --bar bar --foo foo2
Got: foo=foo2, bar=bar
```

But handling that `Vec` of values for the parameter is a pain when we want the
last to override what's come before.  Really we want to discard earlier values
of the parameter before parsing.

[`structopt`][structopt] is a library that lets you define your CLI as
structured data, adding annotations to specify behaviours of the
arguments/parameters.  It in turn calls down to the [`clap`][clap] library to
parse the CLI into structured data.  Using [`clap`][clap] directly we can
achieve the override behaviour I'm looking for:

```rust
fn main() {
    let matches = clap::App::new("Some CLI Tool")
        .global_setting(clap::AppSettings::AllArgsOverrideSelf)
        .arg(
            clap::Arg::with_name("foo")
                .takes_value(true)
                .required(true)
                .long("foo"),
        )
        .arg(
            clap::Arg::with_name("bar")
                .takes_value(true)
                .required(true)
                .long("bar"),
        )
        .get_matches();

    let foo = matches.value_of("foo").unwrap();
    let bar = matches.value_of("bar").unwrap();
    println!("Got: foo={}, bar={}", foo, bar);
}
```

```
$ cargo run -q -- --foo foo --bar bar
Got: foo=foo, bar=bar
$ cargo run -q -- --foo foo --bar bar --foo foo2
Got: foo=foo2, bar=bar
```

There might be a way to set the `AppSettings::AllArgsOverrideSelf` via
[`structopt`][structopt], but from a quick skim of the docs and a little
playing around I didn't spot it.

The fact you _can_ get this overriding behaviour from
[`clap`][clap]/[`structopt`][structopt] is somewhat beside the point, it's not
the default, and writing the CLI tool you'd explicitly have to go to the effort
of enabling it.

Without exhaustively going through every CLI building library in every
language, I can't say too much about whether there's a trend either way.  It's
probably safe to assume that some libraries allow you to repeat and override
parameters by default, some don't.  And it's also probably a safe assumption
that a lot of CLI tools will have adopted the default from the library their
using.

[structopt]: https://docs.rs/structopt/0.3.21/structopt/
[clap]: https://docs.rs/clap/2.33.3/clap/index.html
[click]: https://click.palletsprojects.com/en/7.x/

---

So what does some unix tooling have to say about all this?

Take [`tcpdump`][tcpdump].  When we give it two
`-w` parameters like so:

```
tcpdump port 8000 -w foo.pcap -w -
```

it writes the capture to the second `-w` parameter (`-w -` means "write to
stdout").  Sending some traffic into port 8000 I saw the capture appear in
stdout but no `foo.pcap` was created when I cancelled the command.  Swapping
the parameters around, and the opposite was true.

Running [`useradd`][useradd], specifying the home directory to create twice:

```
useradd foo --home-dir /home/foo/  --home-dir /home/foo2/
```

User `foo` is created with home directory `/home/foo2/`.

[tcpdump]: https://linux.die.net/man/8/tcpdump
[useradd]: https://linux.die.net/man/8/useradd

One could imagine having wrapper scripts for these that set a value then
include extra parameters to allow them to be overridden.

---

With the very small sample set of the tools/frameworks I've looked at above it
wouldn't be right to say there's definitely a trend here, but if we assume
there is, have CLI tools potentially lost something?  In attempting to catch a
possible error by a direct user have they lost something in terms of how easy
it is to write wrappers around them?

Maybe.

Next time I'm making a CLI tool, I'll consider this.
