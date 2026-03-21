# Transpose a data frame

Transposes a data frame, converting variables to cases and vice versa

## Usage

``` r
tFrame(x)
```

## Arguments

- x:

  The data frame to be transposed.

## Value

The transposed data frame

## Details

The `tFrame` function is a convenience function that simply transposes
the input data frame and coerces the result back to a data frame. Apart
from a very small amount of exception handling, it is equivalent to
`as.data.frame(t(x))`. It exists simply because I sometimes find it
convenient when teaching statistics to discuss simple data handling
before going into details regarding coercion; similarly, since I
generally have students work with data frames before exposing them to
matrices, it is convenient to have a transpose function that returns a
data frame as output.

Naturally, the `tFrame` function should only be used when it is actually
sensible to think of the cases of `x` as variables in their own right.
In real life I expect that this maps almost perfectly onto those cases
where `x` could be a matrix just as easily as a data frame, so I don't
believe that `tFrame` is useful in real world data analysis. It is
intended as a teaching tool.

## See also

[`t`](https://rdrr.io/r/base/t.html)

## Examples

``` r
# Create a data frame that could sensibly be transposed...
Gf <- c(105, 119, 121, 98)   # fluid intelligence for 4 people
Gc <- c(110, 115, 119, 103)  # crystallised intelligence
Gs <- c(112, 102, 108, 99)   # speed of processing
dataset <- data.frame( Gf, Gc, Gs )
rownames(dataset) <- paste( "person", 1:4, sep="" )
print(dataset)
#>          Gf  Gc  Gs
#> person1 105 110 112
#> person2 119 115 102
#> person3 121 119 108
#> person4  98 103  99

# Now transpose it...
tFrame( dataset )
#>    person1 person2 person3 person4
#> Gf     105     119     121      98
#> Gc     110     115     119     103
#> Gs     112     102     108      99
```
