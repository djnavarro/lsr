# Contents of workspace

Prints out a simple summary of all the objects in the workspace

## Usage

``` r
who(expand = FALSE)
```

## Arguments

- expand:

  Should R "expand" data frames when listing variables? If
  `expand = TRUE`, variables inside a data frame are included in the
  output. The default is `FALSE`

## Value

`who` returns an object of class `whoList` which is just a data frame
with a dedicated print method.

## Details

The `who` function prints out some basic information about all variables
in the workspace. Specifically, it lists the names of all variables,
what class they are, and how big they are (see below for specifics). If
the `expand` argument is `TRUE` it will also print out the same
information about variables within data frames. See the examples below
to see what the output looks like.

The purpose for the function is to show more information than the
`objects` function (especially as regards the names of variables inside
data frames), but not to show as much detail as the `ls.str` function,
which is generally too verbose for novice users.

The "size" of an object is only reported for some kinds of object:
specifically, only those objects whose
[`mode`](https://lsr.djnavarro.net/reference/mode.md) is either
`numeric`, `character`, `logical`, `complex` or `list`. Nothing is
printed for any other kind of object. If the object has explicit
dimensions (e.g., data frames or matrices) then `who` prints out the
dimension sizes (e.g., "2 x 3" ). Otherwise the length of the object is
printed.

## See also

[`objects`](https://rdrr.io/r/base/ls.html),
[`ls.str`](https://rdrr.io/r/utils/ls_str.html)

## Examples

``` r
cats <- 4
mood <- "happy"
who()
#>    -- Name --   -- Class --   -- Size --
#>    cats         numeric       1         
#>    mood         character     1         

dataset <- data.frame(
  hi = c( "hello","cruel","world" ),
  pi = c( 3,1,4 )
)

who()
#>    -- Name --   -- Class --   -- Size --
#>    cats         numeric       1         
#>    dataset      data.frame    3 x 2     
#>    mood         character     1         
who(expand = TRUE)
#>    -- Name --   -- Class --   -- Size --
#>    cats         numeric       1         
#>    dataset      data.frame    3 x 2     
#>     $hi         character     3         
#>     $pi         numeric       3         
#>    mood         character     1         
```
