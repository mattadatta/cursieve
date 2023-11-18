# Cursieve

[![Crates.io](https://img.shields.io/crates/v/sieve.svg)](https://crates.io/crates/sieve)
[![Documentation](https://docs.rs/sieve/badge.svg)](https://docs.rs/sieve)
[![License](https://img.shields.io/crates/l/sieve.svg)](https://github.com/mattadatta/sieve/blob/main/LICENSE)
<!-- [![Build Status](https://github.com/mattadatta/sieve/workflows/Rust/badge.svg)](https://github.com/mattadatta/sieve/actions) -->

Cursieve is a Rust library that makes it easy to deserialize byte arrays into annotated Rust structures.

Curseive generates the appropriate read and write methods to pack and unpack your custom Rust types using `std::io::Cursor`. This library is most useful when you have existing byte data you want to extract non-contiguous information out of and into Rust structures you can freely and easily modify, which can later be reserialized, leaving other bytes untouched.

## Features

Generates `sift` and `disperse` functions that allow you to generate a structure from existing byte data, or deserialize an existing structure across a given byte array.

- Implemented using `std::io::Cursor` and [byteorder](https://crates.io/crates/byteorder), with support for endianness
- Serialize primitives and other Cursieve-derived types easily; opt-in other fields with `try_from` support
- Annotate only where needed; Cursieve automatically computes the size of your structure and offsets required to serialize; for non-contiguous reading, simply annotate the start of each contiguous region

## Usage

Add this to your `Cargo.toml`:

```toml
[dependencies]
cursieve = "0.1.0"
```

Then, in your crate:

```rust
use cursieve::Sieve;

#[derive(Sieve)]
struct MyStruct {
    #[sieve(offset(0x8A))]
    my_var: i32,
    // ...
}
```

## Example

Here's a simple example of how to use `Cursieve`:

```rust
#[derive(Debug, Sieve)]
pub struct MyStruct {
    #[sieve(offset(0x12), count(7))]
    name: Vec<u8>,
    #[sieve(offset(0x80), try_from(u16))]
    ty: TypeSupportingTryFromPrimitive,
    #[sieve(try_from)]
    garden: Garden,
    happiness: i16,
    init: u8,
    #[sieve(offset(0x8A))]
    lifespan_1: i16,
    lifespan_2: i16,
    #[sieve(order(byteorder::BigEndian))]
    cool_beans: u16,
}


```

## Documentation

Full documentation to come later.
<!-- Please see the [API documentation](https://docs.rs/sieve) for more detailed information about this crate. -->

## Installation

You can include this library as a dependency in your Rust project by adding the following to your `Cargo.toml`:

```toml
[dependencies]
cursieve = "0.1.0"
```

## Contributing

Contributions would be neat once I get this a bit more organized.
<!-- Contributions are always welcome! Please read the [CONTRIBUTING.md](CONTRIBUTING.md) file for guidelines. -->

## License

This project is licensed under the [MIT license](LICENSE).

## Acknowledgements

Dogs!
