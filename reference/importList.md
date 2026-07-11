# Import a list into the workspace

Copies each element of a list into a separate variable in the workspace,
using the list element names as variable names.

## Usage

``` r
importList(x, ask = TRUE)
```

## Arguments

- x:

  A list or data frame whose elements are to be imported as individual
  variables.

- ask:

  Set to `TRUE` (the default) to display the names of the variables that
  will be created and ask for confirmation before proceeding. Set to
  `FALSE` to import silently.

## Value

Called primarily for its side effect of creating variables in the
workspace. Invisibly returns `1` if variables were created, `0` if the
user declined.

## Details

Creates one variable per list element in the calling environment
(usually the global workspace). Element names that are not valid R
variable names are automatically converted using
[`make.names`](https://rdrr.io/r/base/make.names.html).

## See also

[`unlist`](https://rdrr.io/r/base/unlist.html),
[`attach`](https://rdrr.io/r/base/attach.html)

## Examples

``` r
values <- c(1, 2, 3, 4, 5)
group  <- c("group A", "group A", "group B", "group B", "group B")

# split() returns a named list: one element per group
grp_list <- split(values, group)

# import silently (no confirmation prompt)
importList(grp_list, ask = FALSE)

if (FALSE) {
  # interactive: shows variable names and asks for confirmation
  importList(grp_list)
}
```
