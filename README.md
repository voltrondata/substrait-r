
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
# install.packages("remotes")
remotes::install_github("voltrondata/substrait-r")
```

## Example

Basic `dplyr::select()`!

``` r
library(substrait)
#> 
#> Attaching package: 'substrait'
#> The following object is masked from 'package:stats':
#> 
#>     filter
library(dplyr)

compiler <- substrait_compiler()

query <- data.frame(letter = letters, number = 1:26) %>% 
  substrait_dplyr_query() %>% 
  filter(number < 5) %>% 
  select(letter)

substrait:::build_projections(
  query, 
  attr(query, "selected_columns")
)
#> [[1]]
#> message of type 'substrait.Expression' with 1 field set
#> selection {
#>   direct_reference {
#>     struct_field {
#>       child {
#>         struct_field {
#>         }
#>       }
#>     }
#>   }
#> }

substrait:::build_filters(
  query, 
  attr(query, "filtered_rows"), 
  compiler = compiler
)
#> [[1]]
#> message of type 'substrait.Expression' with 1 field set
#> scalar_function {
#>   function_reference: 1
#>   args {
#>     selection {
#>       direct_reference {
#>         struct_field {
#>           field: 1
#>           child {
#>             struct_field {
#>             }
#>           }
#>         }
#>       }
#>     }
#>   }
#>   args {
#>     literal {
#>       fp64: 5
#>     }
#>   }
#>   output_type {
#>   }
#> }

compiler$function_extensions_key[["1"]]
#> $name
#> [1] "<"
#> 
#> $arg_types
#> $arg_types[[1]]
#> message of type 'substrait.Type' with 1 field set
#> i32 {
#> }
#> 
#> $arg_types[[2]]
#> message of type 'substrait.Type' with 1 field set
#> fp64 {
#> }
```

## Create ‘Substrait’ proto objects

You can create Substrait proto objects using the `substrait` base object
or using `substrait_create()`:

``` r
substrait$Type$Boolean$create()
#> message of type 'substrait.Type.Boolean' with 0 fields set
substrait_create("substrait.Type.Boolean")
#> message of type 'substrait.Type.Boolean' with 0 fields set
```

You can convert an R object *to* a Substrait object using
`as_substrait(object, type)`:

``` r
(msg <- as_substrait(4L, "substrait.Expression"))
#> message of type 'substrait.Expression' with 1 field set
#> literal {
#>   i32: 4
#> }
```

The `type` can be either a string of the qualified name or an object
(which is needed to communicate certain types like
`"substrait.Expression.Literal.Decimal"` which has a `precision` and
`scale` in addition to the `value`).

Restore an R object *from* a Substrait object using
`from_substrait(message, prototype)`:

``` r
from_substrait(msg, integer())
#> [1] 4
```

Substrait objects are list-like (i.e., methods defined for `[[` and
`$`), so you can get or set fields. Note that unset is different than
`NULL` (just like an R list).

``` r
msg$literal <- substrait$Expression$Literal$create(i32 = 5L)
msg
#> message of type 'substrait.Expression' with 1 field set
#> literal {
#>   i32: 5
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
#> [1] 0a 02 28 05
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
#> bool_ {
#>   5: 5
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
