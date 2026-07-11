# Remove all objects from the workspace

Deletes all objects from the workspace, with an optional confirmation
prompt.

## Usage

``` r
rmAll(ask = TRUE)
```

## Arguments

- ask:

  Set to `TRUE` (the default) to display the current workspace contents
  and ask for confirmation before deleting. Set to `FALSE` to delete
  immediately without prompting.

## Value

Invisibly returns `1` if objects were deleted, `0` if the user declined
or the workspace was already empty.

## Details

Removes all objects from the workspace. When `ask = TRUE`, the list of
objects is printed and the user must type `y` to confirm before anything
is deleted. This is similar to `rm(list = objects())`, but with an
interactive safety check.

## See also

[`rm`](https://rdrr.io/r/base/rm.html),
[`objects`](https://rdrr.io/r/base/ls.html)

## Examples

``` r
if (FALSE) {
  # interactive: displays workspace contents and asks for confirmation
  rmAll()

  # non-interactive: deletes immediately without prompting
  rmAll(ask = FALSE)
}
```
