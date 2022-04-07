
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

Basic `dplyr::select()` using the Arrow Substrait consumer:

``` r
library(substrait)
#> 
#> Attaching package: 'substrait'
#> The following object is masked from 'package:stats':
#> 
#>     filter
library(dplyr)

mtcars %>% 
  arrow_substrait_compiler() %>%
  mutate(mpg_plus_one = mpg + 1) %>% 
  select(mpg, wt, mpg_plus_one) %>% 
  collect()
#> # A tibble: 32 × 3
#>      mpg    wt mpg_plus_one
#>    <dbl> <dbl>        <dbl>
#>  1  21    2.62         22  
#>  2  21    2.88         22  
#>  3  22.8  2.32         23.8
#>  4  21.4  3.22         22.4
#>  5  18.7  3.44         19.7
#>  6  18.1  3.46         19.1
#>  7  14.3  3.57         15.3
#>  8  24.4  3.19         25.4
#>  9  22.8  3.15         23.8
#> 10  19.2  3.44         20.2
#> # … with 22 more rows
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
