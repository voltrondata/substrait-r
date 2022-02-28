
<!-- README.md is generated from README.Rmd. Please edit that file -->

# substrait

<!-- badges: start -->

[![R-CMD-check](https://github.com/voltrondata/substrait-r/workflows/R-CMD-check/badge.svg)](https://github.com/voltrondata/substrait-r/actions)
[![Codecov test
coverage](https://codecov.io/gh/voltrondata/substrait-r/branch/master/graph/badge.svg)](https://app.codecov.io/gh/voltrondata/substrait-r?branch=master)
<!-- badges: end -->

The goal of substrait is to provide an R interface to the
[Substrait](https://substrait.io) cross-language serialization for
relational algebra. This is an experimental package that is under heavy
development!

## Installation

You can install the development version of substrait from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("voltrondata/substrait-r")
```

## Usage

You can create Substrait objects using the `substrait` base object or
using `substrait_create()`:

``` r
library(substrait)

substrait$Type$Boolean$create()
#> message of type 'substrait.Type.Boolean' with 0 fields set
substrait_create("substrait.Type.Boolean")
#> message of type 'substrait.Type.Boolean' with 0 fields set
```

You can convert an R object *to* a Substrait object using
`as_substrait(object, type)`:

``` r
(msg <- as_substrait(list(i8 = list()), "substrait.Type"))
#> message of type 'substrait.Type' with 1 field set
#> i8 {
#> }
```

The `type` can be either a string of the qualified name or an object
(which is needed to communicate certain types like
`"substrait.Expression.Literal.Decimal"` which has a `precision` and
`scale` in addition to the `value`).

Restore an R object *from* a Substrait object using
`from_substrait(message, prototype)`:

``` r
from_substrait(msg, list())
#> $i8
#> message of type 'substrait.Type.I8' with 0 fields set
```

Substrait objects are list-like (i.e., methods defined for `[[` and
`$`), so you can get or set fields. Note that unset is different than
`NULL` (just like an R list).

``` r
msg$i8$nullability <- substrait$Type$Nullability$NULLABILITY_NULLABLE
msg
#> message of type 'substrait.Type' with 1 field set
#> i8 {
#>   nullability: NULLABILITY_NULLABLE
#> }
```

The constructors are currently implemented using about 1000 lines of
auto-generated code made by inspecting the nanopb-compiled .proto files
and RProtoBuf. This is probably not the best final approach but allows
us to get started writing good `as_substrait()` and `from_substrait()`
methods for various types of R objects.

## Under the hood

Substrait objects are represented as a classed version of the `raw()`
vector containing serialized data. This makes the representation
independent from the protocol buffer library use to encode/deocde the
objects and is particularly useful with `expect_identical()`.

``` r
unclass(msg)
#> [1] 12 02 10 01
```

Currently, the [RProtoBuf](https://cran.r-project.org/package=RProtoBuf)
package is used to serialize and deserialize objects. The RProtoBuf
package provides excellent coverage of the protocol buffer API and is
well-suited to general use; however, the Substrait package only uses a
small portion of these features. The package registers the .proto files
from Substrait (of which this package has an internal copy) so that you
can use RProtobuf to read, modify, and write messages independent of the
constructors in the package.

``` r
(msg_rpb <- RProtoBuf::P("substrait.Type")$read(unclass(msg)))
#> message of type 'substrait.Type' with 1 field set
as_substrait(msg_rpb)
#> message of type 'substrait.Type' with 1 field set
#> i8 {
#>   nullability: NULLABILITY_NULLABLE
#> }
```

An approach more suited to the Substrait use-case is
[nanopb](https://github.com/nanopb/nanopb), which is vendorable (i.e.,
no linking to system libraries) and supports all the protocol buffer
features used by Substrait. The compiled .proto encoders/decoders and
vendored library is currently included in the `src/` directory but only
one encoder is implemented:

``` r
bytes <- substrait:::r_encode_substrait_Type_Boolean(
  type_variation_reference = 233,
  nullablity = substrait$Type$Nullability$NULLABILITY_REQUIRED
)

cat(RProtoBuf::P("substrait.Type.Boolean")$read(bytes)$toString())
#> type_variation_reference: 233
#> nullability: NULLABILITY_REQUIRED
```

A note that I did try to generate bindings using the official Google C++
code generator (i.e., what you get when you `brew install protobuf` and
run `protoc`), but this requires linking to the exact version of
protobuf that generated the C++ readers. This is pretty much impossible
to make work because everybody ships a slightly different version of
libprotobuf. The approach taken by RProtoBuf is to link to the system
protobuf *compiler* library and compile the .proto files at runtime.
That could work here, too, but would also require linking to a system
library (the same ones that RProtoBuf does).

There are probably other approaches, but most of them will require some
type of auto-generated wrapper code and the portability and compile
speed of nanopb is hard to beat.
