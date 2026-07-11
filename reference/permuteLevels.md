# Permute the levels of a factor

Reorder the levels of a factor into any order you specify.

## Usage

``` r
permuteLevels(x, perm, ordered = is.ordered(x), invert = FALSE)
```

## Arguments

- x:

  A factor.

- perm:

  An integer vector of the same length as `nlevels(x)`. When
  `invert = FALSE` (the default), `perm[k]` gives the index of the old
  level that should appear in position `k` of the new ordering. When
  `invert = TRUE`, `perm[k]` gives the new position to assign to the
  `k`-th old level.

- ordered:

  Set to `TRUE` to return an ordered factor. Defaults to
  `is.ordered(x)`, preserving the ordered status of the input.

- invert:

  Set to `TRUE` to apply the inverse of `perm`. See the `perm`
  description above and the examples below.

## Value

A factor with the same values as `x` but with the levels in the new
order specified by `perm`.

## Details

Similar to [`relevel`](https://rdrr.io/r/stats/relevel.html), but more
general: `relevel` can only move one level to the front, whereas
`permuteLevels` can place the levels in any order. This is useful when
you want to control the order in which levels appear on a plot axis or
in a table.

## See also

[`factor`](https://rdrr.io/r/base/factor.html),
[`relevel`](https://rdrr.io/r/stats/relevel.html),
[`order`](https://rdrr.io/r/base/order.html)

## Examples

``` r
# factor with levels a, b, c, d, e, f (in that order)
x <- factor(c(1, 4, 2, 2, 3, 3, 5, 5, 6, 6), labels = letters[1:6])
levels(x)
#> [1] "a" "b" "c" "d" "e" "f"

# move level e to position 1, c to position 2, b to 3, a to 4, d to 5, f to 6
permuteLevels(x, perm = c(5, 3, 2, 1, 4, 6))
#>  [1] a d b b c c e e f f
#> Levels: e c b a d f

# using invert = TRUE: move level a to position 5, b to 3, c to 2, etc.
permuteLevels(x, perm = c(5, 3, 2, 1, 4, 6), invert = TRUE)
#>  [1] a d b b c c e e f f
#> Levels: d c b e a f
```
