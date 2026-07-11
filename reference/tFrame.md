# Transpose a data frame

Transposes a data frame, swapping rows and columns, and returns the
result as a data frame.

## Usage

``` r
tFrame(x)
```

## Arguments

- x:

  A data frame to be transposed.

## Value

The transposed data frame.

## Details

Equivalent to `as.data.frame(t(x))`. Unlike applying
[`t`](https://rdrr.io/r/base/t.html) directly, `tFrame` ensures the
result is a data frame rather than a matrix. This makes sense when the
rows of `x` can be meaningfully treated as variables — for example, when
each row represents a measurement type and each column represents a
participant.

## See also

[`t`](https://rdrr.io/r/base/t.html)

## Examples

``` r
dataset <- data.frame(
  Gf = c(105, 119, 121, 98),   # fluid intelligence
  Gc = c(110, 115, 119, 103),  # crystallised intelligence
  Gs = c(112, 102, 108, 99)    # processing speed
)
rownames(dataset) <- paste0("person", 1:4)
dataset
#>          Gf  Gc  Gs
#> person1 105 110 112
#> person2 119 115 102
#> person3 121 119 108
#> person4  98 103  99

tFrame(dataset)
#>    person1 person2 person3 person4
#> Gf     105     119     121      98
#> Gc     110     115     119     103
#> Gs     112     102     108      99
```
