# Permute the levels of a factor

Apply an arbitrary permutation to the ordering of levels within a factor

## Usage

``` r
permuteLevels(x, perm, ordered = is.ordered(x), invert = FALSE)
```

## Arguments

- x:

  The factor to be permuted

- perm:

  A vector specifying the permutation

- ordered:

  Should the output be an ordered factor?

- invert:

  Use the inverse of `perm` to specify the permutation

## Value

Returns a factor with identical values, but with the ordering of the
factor levels shuffled.

## Details

This is a convenience function used to shuffle the order in which the
levels of a factor are specified. It is similar in spirit to the
`relevel` function, but more general. The `relevel` function only
changes the first level of the factor, whereas `permuteLevels` can apply
an arbitrary permutation. This can be useful for plotting data, because
some plotting functions will display the factor levels in the same order
that they appear within the factor.

The `perm` argument is a vector of the same length as `levels(x)`, such
that `perm[k]` is an integer that indicates which of the old levels
should be moved to position k. However, if `invert=TRUE`, the inverse
permutation is applied: that is, `perm[k]` is an integer specifying
where to move the k-th level of the original factor. See the examples
for more details.

## See also

[`factor`](https://rdrr.io/r/base/factor.html),
[`order`](https://rdrr.io/r/base/order.html),
[`relevel`](https://rdrr.io/r/stats/relevel.html)

## Examples

``` r
# original factor specifies the levels in order: a,b,c,d,e,f
x <- factor( c(1,4,2,2,3,3,5,5,6,6), labels=letters[1:6] )
print(x)
#>  [1] a d b b c c e e f f
#> Levels: a b c d e f

# apply permutation (5 3 2 1 4 6)... i.e., move 5th factor level (e)
# into position 1, move 3rd factor level (c) into position 2, etc
permuteLevels(x,perm = c(5,3,2,1,4,6))
#>  [1] a d b b c c e e f f
#> Levels: e c b a d f

# apply the inverse of permutation (5 3 2 1 4 6)... i.e., move 1st
# level (a) into position 5, move 2nd level (b) into position 3, etc
permuteLevels(x,perm = c(5,3,2,1,4,6),invert=TRUE)
#>  [1] a d b b c c e e f f
#> Levels: d c b e a f
```
