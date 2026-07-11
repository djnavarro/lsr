# CRAN submission comments — lsr 1.0.0

## Summary of changes since 0.5.2

This is a maintenance release. No new exported functions, no API changes.

**Bug fixes:**

- `oneSampleTTest()`: `conf.level` was not forwarded to `stats::t.test()`,
  so `$conf.int` always contained the 95% interval regardless of what the
  user requested.
- `independentSamplesTTest()`, `pairedSamplesTTest()`, `associationTest()`:
  passing a tibble as `data` caused spurious type-check failures. Fixed by
  coercing `data` to a plain data frame on entry.
- `correlate()`: the pairwise-correlation loop used `1:(n-1)`, which
  evaluates to `c(1, 0)` in R when `n == 1`, causing incorrect iteration.
  Fixed with `seq_len(n-1)`.

**Other changes:**

- Input validation hardened across many functions (informative errors for
  `NA` flag arguments, scalar `||` in `conf.level` guards, deprecated
  `class()` comparisons replaced throughout).
- `tibble` and `withr` added to `Suggests` (test infrastructure only).
- `Language: en-GB` added to DESCRIPTION.
- `LazyLoad` field removed from DESCRIPTION (deprecated).

---

## R CMD check results

Checked locally on R 4.6.1 (Ubuntu 24.04, x86_64):

```
0 errors | 0 warnings | 0 notes
```

---

## win-builder and mac-builder checks

Submitted to CRAN's own check infrastructure:

- `devtools::check_win_devel()` — 0 errors, 0 warnings, 2 notes (see below)
- `devtools::check_win_release()` — results pending
- `devtools::check_mac_release()` — macOS builder (mac.r-project.org) was
  returning HTTP 502 at time of submission; macOS coverage provided by rhub
  instead (see below)

**win-devel NOTE — URLs in README.md:**

Two pkgdown article links returned 404 because the documentation site had
not yet been rebuilt at time of checking:

```
Found the following (possibly) invalid URLs:
  URL: https://lsr.djnavarro.net/articles/commentary.html
  URL: https://lsr.djnavarro.net/articles/overview.html
```

These URLs are correct and will resolve once `dev` is merged into `main` and
the pkgdown workflow rebuilds the site — which is the first step of the
release. A final win-builder run will be done after the site is live and
before submitting to CRAN.

**win-devel NOTE — CITATION format:**

`inst/CITATION` used the deprecated `citEntry()` / `citHeader()` API. The
file has been removed; `citation("lsr")` now auto-generates a package
citation from DESCRIPTION, which is more appropriate (the book and the
package are distinct works). This note will not appear in the final
submission.

---

## rhub checks

Submitted to rhub v2 on the following platforms:

- `linux` — GitHub Actions ubuntu-latest, all R versions
- `macos-arm64` — GitHub Actions macos-latest (Apple Silicon)
- `windows` — GitHub Actions windows-latest
- `clang-asan` — R-devel on Ubuntu 22.04, ASAN + UBSAN

Results: https://github.com/djnavarro/lsr/actions/runs/29138944675

All four platforms passed (0 errors, 0 warnings, 0 notes).

Annotations seen in the run output:

- "data have zero variance" on linux, macos-arm64, and windows — this is
  expected; it is triggered by a `ciMean()` test that deliberately exercises
  the zero-variance warning path.
- Homebrew tap-trust and macOS runner migration messages on macos-arm64 — 
  GitHub Actions infrastructure noise unrelated to the package.

---

## Reverse dependencies

There are 5 packages on CRAN that depend on lsr:
**AOboot**, **autoBagging**, **calms**, **noisemodel**, **superb**.

**autoBagging** has pre-existing failures on CRAN (4 ERROR across 12 platforms)
that are unrelated to lsr — the errors exist against lsr 0.5.2 and have been
present for some time.

For the remaining four packages, we reviewed their source code to determine
which lsr functions they use and whether our changes could affect them:

| Package | lsr functions used | Verdict |
|---|---|---|
| AOboot | `wideToLong` | Unaffected — our change only adds validation for invalid inputs |
| calms | `cramersV` | Unaffected — only internal `inherits()` refactor, no behavioural change |
| noisemodel | `expandFactors` | Unaffected — our change only adds validation for invalid inputs |
| superb | `correlate`, `wideToLong`, `longToWide` | Unaffected — correlate loop fix only affects the edge case of a single numeric variable; other changes are validation-only |

No new failures are expected in any of these packages as a result of this
release.
