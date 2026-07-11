# Unload a package

Removes a package from the search path, using the same naming convention
as [`library`](https://rdrr.io/r/base/library.html).

## Usage

``` r
unlibrary(package)
```

## Arguments

- package:

  The name of the package to unload, with or without quotes.

## Value

Called for its side effect of removing the package from the search path.
Returns the result of [`detach`](https://rdrr.io/r/base/detach.html)
invisibly.

## Details

Calls [`detach`](https://rdrr.io/r/base/detach.html) on the named
package. Unlike `detach`, which requires the full `"package:name"`
string, `unlibrary` accepts the bare package name (with or without
quotes), matching the syntax of
[`library`](https://rdrr.io/r/base/library.html). Only the named package
is unloaded; dependencies are not affected.

## See also

[`library`](https://rdrr.io/r/base/library.html),
[`detach`](https://rdrr.io/r/base/detach.html)

## Examples

``` r
if (FALSE) {
  # after loading a package with library(), unload it with unlibrary()
  unlibrary(MASS)
}
```
