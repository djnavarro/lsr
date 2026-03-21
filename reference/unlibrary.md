# Unload a package

A wrapper function to [`detach`](https://rdrr.io/r/base/detach.html)
that removes a package from the search path, but takes a package name as
input similar to [`library`](https://rdrr.io/r/base/library.html).

## Usage

``` r
unlibrary(package)
```

## Arguments

- package:

  A package name, which may be specified with or without quotes.

## Value

Identical to `detach`.

## Details

Unloads a package. This is just a wrapper for the `detach` function.
However, the `package` argument is just the name of the package (rather
than the longer string that is required by the `detach` function), and –
like the `library` function – can be specified without quote marks. The
`unlibrary` function does not unload dependencies, only the named
package.

The name "unlibrary" is a bit of an abuse of both R terminology (in
which one has a library of packages) and the English language, but I
think it helps convey that the goal of the `unlibrary` function is to do
the opposite of what the `library` function does.

## See also

[`library`](https://rdrr.io/r/base/library.html),
[`require`](https://rdrr.io/r/base/library.html),
[`detach`](https://rdrr.io/r/base/detach.html)
