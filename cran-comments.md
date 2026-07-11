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
- `correlate()`: when a pair of variables had too few complete observations
  for `cor.test()` to run, the entire call aborted. The affected cell is now
  left as `NA` and remaining pairs are computed normally.
- `correlate()`, `goodnessOfFitTest()`, `associationTest()`: `options(warn = 2)`
  used internally was not guarded with `on.exit()`, so an unexpected error
  could leave the session's global warn level permanently elevated. Fixed.
- `cramersV()`: Yates' continuity correction was applied by default for 2×2
  tables, causing V to fall below 1 even for perfect association. `cramersV()`
  now always uses the Pearson chi-squared (`correct = FALSE`).
- `goodnessOfFitTest()`, `associationTest()`: unused factor levels were
  silently included in the test with zero observed cases, changing degrees of
  freedom and p-values without any indication. Both functions now warn and
  suggest `droplevels()`.
- `modeOf()`, `maxFreq()`: all-`NA` or zero-length input produced a cryptic
  base-R message. Both functions now warn and return `NA`.
- `wideToLong()`: when no column names contained the separator string, errors
  came from inside `stats::reshape()` with no hint of the cause. The function
  now checks early and gives a plain-English message.
- `importList()`: unnamed or partially-named lists produced a cryptic base-R
  error. The function now checks for missing names and gives an informative
  message. Empty lists now produce a message rather than silently returning.

**Other changes:**

- Input validation hardened across many functions (informative errors for
  `NA` flag arguments, scalar `||` in `conf.level` guards, deprecated
  `class()` comparisons replaced throughout).
- `tibble` and `withr` added to `Suggests` (test infrastructure only).
- `Language: en-GB` added to DESCRIPTION.
- `LazyLoad` field removed from DESCRIPTION (deprecated).
- `inst/CITATION` removed; `citation("lsr")` now auto-generates a package
  citation from DESCRIPTION.

---

## R CMD check results

Checked locally on R 4.6.1 (Ubuntu 24.04, x86_64):

```
0 errors | 0 warnings | 0 notes
```

---

## win-builder and mac-builder checks

Submitted to CRAN's own check infrastructure:

- `devtools::check_win_devel()` — results pending
- `devtools::check_win_release()` — results pending
- `devtools::check_mac_release()` — macOS builder (mac.r-project.org) was
  returning HTTP 502 at time of submission; macOS coverage provided by rhub
  instead (see below)

*(To be updated with final win-builder results before submission.)*

---

## rhub checks

Submitted to rhub v2 on the following platforms:

- `linux` — GitHub Actions ubuntu-latest, all R versions
- `macos-arm64` — GitHub Actions macos-latest (Apple Silicon)
- `windows` — GitHub Actions windows-latest
- `clang-asan` — R-devel on Ubuntu 22.04, ASAN + UBSAN

Results: https://github.com/djnavarro/lsr/actions/runs/29161721954

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
| AOboot | `wideToLong` | Unaffected — changes add validation for invalid inputs only |
| calms | `cramersV` | Low risk — `cramersV()` now uses Pearson chi-squared without continuity correction; V values for 2×2 tables will differ slightly from 0.5.2, but calms does not appear to test exact V values |
| noisemodel | `expandFactors` | Unaffected — changes add validation for invalid inputs only |
| superb | `correlate`, `wideToLong`, `longToWide` | Unaffected — correlate loop fix only affects the edge case of <2 numeric variables; other changes are validation-only |

No new failures are expected in any of these packages as a result of this
release.
