# Remove all objects

Removes all objects from the workspace

## Usage

``` r
rmAll(ask = TRUE)
```

## Arguments

- ask:

  Logical value indicating whether to ask user to confirm deletions.
  Default is `TRUE`

## Value

Invisibly returns 0 if no deletions are made, 1 if at least one deletion
is made.

## Details

The `rmAll` function provides a simple way of deleting all objects from
the workspace. It is almost equivalent to the usual
`rm(list = objects())` command. The only difference that it requires the
user to confirm the deletions first if `ask = TRUE`, after displaying a
list of the current objects in the worspace. This can occasionally be
useful for teaching purposes.

## See also

[`rm`](https://rdrr.io/r/base/rm.html)
