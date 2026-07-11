# Contents of workspace

Prints a summary of all objects in the workspace, showing each object's
name, class, and size.

## Usage

``` r
who(expand = FALSE)
```

## Arguments

- expand:

  Set to `TRUE` to also list the variables inside any data frames in the
  workspace. Defaults to `FALSE`.

## Value

Prints the workspace summary and invisibly returns the underlying data
(a data frame with columns `Name`, `Class`, and `Size`), which can be
assigned to a variable and inspected if needed.

## Details

Shows each object's name, class, and size. For objects with explicit
dimensions (e.g., data frames, matrices) the size is shown as rows x
columns; for other objects it is the length. Size is only shown for
objects whose mode is `numeric`, `character`, `logical`, `complex`, or
`list`.

Shows more information than [`objects`](https://rdrr.io/r/base/ls.html)
(especially for variables inside data frames) but less detail than
[`ls.str`](https://rdrr.io/r/utils/ls_str.html).

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
  hi = c("hello", "cruel", "world"),
  pi = c(3, 1, 4)
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
