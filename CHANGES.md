# capnp-ocaml 3.2.1

## Other changes

* Update for API changes in Base and Core\_kernel (#52).

# capnp-ocaml 3.2.0

## Backwards-incompatible changes

* The `capnp` opam package no longer depends on the C++ compiler (#47).
  If your project compiles schema files, you should add
  `"conf-capnproto" {build}` to your opam dependencies.

## Other changes

* Add support for OCaml 4.06 (#45).

* Update for API changes in Base and Core\_kernel (#44).

# capnp-ocaml 3.1.0

## Backwards-incompatible changes

* The Unix-specific `IO` module has been moved from `Capnp.IO` to `Capnp_unix.IO`,
  and is now part of the new `capnp.unix` ocamlfind library.
  Both `capnp` and `capnp.unix` ocamlfind libraries are provided by the `capnp`
  opam package.

## New features

* The core `capnp` library no longer depends on `Core_kernel` or `Unix`.
  This makes binaries using the library several MB smaller and allows
  capnp to be used in [Mirage unikernels](https://mirage.io/).

# capnp-ocaml 3.0.0

## Backwards-incompatible changes

* The `Builder.X.reader_t` and `Reader.X.builder_t` types have gone
  (to avoid confusion with the generic `reader_t` and `builder_t` types).
  Use `X.struct_t reader_t` and `X.struct_t builder_t` instead.
  `Builder.X.t` is still defined (as an alias for `t builder_t`), and similarly
  for `Reader.X.t`.

* `StructStorage.t` now takes an extra type parameter. This is a phantom type,
  used to indicate what kind of struct it represents. This extra parameter
  has also been added to `Object.t`.

* In the generated files, the unique types names (e.g.
  `reader_t_Foo_14133145859926553711`) have gone.
  If you need to refer to these directly for some reason, use the replacement
  polymorphic type instead (e.g. `Foo_c42305476bb4746f reader_t`).

## New features

* RPC support (#35 and #36).
  The compiler now generates readers and builders for parameters and results,
  interface and method IDs, and a dispatch method for services.
  There is a new `MakeRPC` functor in the generated files that takes an
  extended version of `MessageSig`. The original `Make` is still provided - it
  is now a convenience wrapper around `MakeRPC` that passes some dummy functions.
  An RPC provider is available at <https://github.com/mirage/capnp-rpc>.

* Capability references can now be attached to messages.
  The getters and setters that previously took `Uint32.t` indexes can now take
  capabilities, as defined by an RPC provider library.
  The default `RPC.None` provider exposes the raw descriptor index, as before.

* Better AnyPointer support (#11). The new `of_pointer` and `init_pointer` functions
  can be used to read and add structs of any type to AnyPointer fields.
  The new `pointer_set_reader` can be used to set a pointer by copying from a reader.

* Replace camlp4 includes with flambda inlining (#23).
  The `-inc.ml` files that were previously copied into each generated file are
  now regular modules in the library, making them available to other code too.
  This makes the generated files much smaller. There is no performance penalty
  (when using flambda).

* Port to jbuilder (#24).
  Also, the required schema files are now included in the repository rather than being
  downloaded by the build scripts, which was fragile.

* Add support for cycles (#29).
  Before, the compiler would abort if a schema file contained cycles, but this
  is very common (especially with interfaces). It now generates forward
  references where needed.

* Add parameterised `reader_t` and `builder_t` types (#32 and #33).
  Instead of providing `Reader.Foo.t` and `Builder.Foo.t` as abstract types,
  provide a single `Foo.struct_t` and use `struct_t reader_t` and
  `struct_t builder_t` for its reader and builder.
  This makes it possible to define generic functions that work on all readers
  or builders, without having to generate a separate function for each one.
  The types are shared between files, so you can write generic code to work
  with schema files you haven't seen (needed for the RPC system).

* Use polymorphic variants for phantom types (#34).
  Instead of declaring a load of unique type names at the start of the file,
  use polymorphic variants. These don't need to be declared before use.
  The node ID is now formatted as hex, to make it shorter and to match the
  format used in the schema files.
  There are new generic `reader_of_builder` and `message_of_builder` functions
  in `StructStorage`.

## Other changes

* All uses of `Obj.magic` have been removed from the generated code.

* Update to latest core\_kernel (#26).
  Avoids conflict with newer sexplib.

* Fix some compiler warnings (#28). Jbuilder turns on more warnings by default.

* Add missing test dependency on ounit (#30).

* Fix code generation bug in 2.1.1 (#10).
  A missing space prevented the generated files from compiling.
  Added Travis CI tests to detect this kind of thing automatically.

# capnp-ocaml 2.1.1

* Fix `Not_found` exception when a capnp import is not referenced within the schema file.

* Omit references to capnp imports which are not used within the generated code.  In
  particular, importing `"/capnp/c++.capnp"` no longer generates a reference to an OCaml
  module with the confusing name of `C2b2b`, and there should no longer be a need to
  separately compile that imported schema file.

# capnp-ocaml 2.1.0

* Depend on Core\_kernel rather than Core. Thanks to Matthew Maurer for
  driving this change.

* Fix compatibility issue when building against Core 112.35.00.

# capnp-ocaml 2.0.1

* Invoke OCaml scripts using `-I $OCAML_TOPLEVEL_PATH`, to avoid missing
  `topfind`.

# capnp-ocaml 2.0.0

## Backwards-incompatible changes

* Module `Codecs`: change API to accept the more natural `BytesMessage.Message.t`
  instead of `Bytes.t list`.

* Module `Codecs`: change API to accept a `compression` specifier, instead of using
  separate functions and separate types for compressed and uncompressed streams.

* Module `Message`: `to_storage` now returns a list of (storage, length) pairs.

## Other changes

* Module `Codecs`: fix incorrect encoding of framing headers (for example,
  as generated by `serialize`).

* Module `Codecs`: fix infinite loop in `PackedStream` decoding.

* Module `Codecs`: reduce serialized message sizes, by omitting unused allocation
  regions from the serialized messages.

* Instantiate `BytesMessage = Message.Make(BytesStorage)`, so the user doesn't always
  need to. (Implementation is furthermore defunctorized for improved performance.)

* New module `IO`: functions for moving message data through various types of I/O
  channels.

* Compiler now says something about files it created.

* Corrected compilation errors when using 4.02 `-safe-string`.

* Significant performance improvements across the board.

# capnp-ocaml 1.0.1

* Avoid use of GNU `install` features in `omake install`.  This corrects installation
  problems on OS X and (most likely) other BSDs.

# capnp-ocaml 1.0.0

* Initial release.
