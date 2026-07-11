# lsr Maintenance Plan

This file records the overall maintenance plan for the package and the
current status of each stage. Update it as stages are completed.

------------------------------------------------------------------------

## Stages

### Stage 1 — Test suite quality ✅ Complete

**Goal:** Build a test suite thorough enough to support safe internal
hardening. No functional code changes.

**Completed:** 2026-07-10. PR merged into `dev`.

**Outcome:** - 28 test files, 448 assertions - All 29 exported functions
and all S3 print methods covered - Every test file covers: typical
usage, numeric correctness against a known reference, and expected
errors/warnings - Five latent bugs identified and documented (see Stage
2 targets below)

------------------------------------------------------------------------

### Stage 2 — Internal hardening ✅ Complete

**Goal:** Improve input validation and defensive coding so the package
continues to work reliably as R and its ecosystem evolve. Tests from
Stage 1 are in place to verify that no existing behaviour is broken.

**Completed:** 2026-07-10.

**Outcome:** - 486 assertions (up from 448); all new tests pass - All
five original targets from Stage 1 addressed - Full review of all 29
source files conducted; four additional groups of issues identified and
resolved: - **Group 1:** `NA` not checked for logical flag arguments —
fixed in `aad`, `modeOf`, `maxFreq`, `etaSquared`, `permuteLevels`,
`sortFrame`, `rmAll`, `who`; guards added from scratch to `rmAll` and
`who` - **Group 2:** `|` instead of `||` in multi-condition `conf.level`
guards, causing opaque “condition has length \> 1” errors — fixed in
`oneSampleTTest`, `independentSamplesTTest`, `pairedSamplesTTest` -
**Group 3:** Deprecated `class() ==` / `class() %in%` comparisons
replaced with [`inherits()`](https://rdrr.io/r/base/class.html),
[`is.name()`](https://rdrr.io/r/base/name.html),
[`is.matrix()`](https://rdrr.io/r/base/matrix.html),
[`is.numeric()`](https://rdrr.io/r/base/numeric.html) in `correlate`,
`associationTest`, `goodnessOfFitTest`, `ciMean` - **Group 4:** Missing
input validation added to `longToWide`, `expandFactors`, `wideToLong`
(including a `split = NA` guard folded in from the Group 1 pattern)

------------------------------------------------------------------------

### Stage 3 — Bug fixes ✅ Complete

**Goal:** Fix genuine defects where a fix is clearly safe.

**Approach:** Be conservative. Prefer doing nothing over introducing a
breaking change. One fix per PR; include or update a test for each fix.

**Completed:** 2026-07-11.

**Outcome:** - All five open GitHub issues reviewed and resolved - Three
confirmed bugs fixed: - `oneSampleTTest`: `conf.level` not forwarded to
[`stats::t.test()`](https://rdrr.io/r/stats/t.test.html), so `$conf.int`
always contained the 0.95 interval regardless of what the user requested
(PR \#12, closes \#9) - Tibble compatibility: `data[, col]` returning a
one-column tibble instead of a vector caused false type-check failures
in `independentSamplesTTest`, `pairedSamplesTTest`, and
`associationTest`; fixed by coercing `data` to a plain data frame on
entry (PR \#13, closes \#2) - `sortFrame` locale-sensitive test: the
mixed-case character sort test was commented out because results varied
across platforms; restored and stabilised by pinning `LC_COLLATE = "C"`
with
[`withr::with_locale()`](https://withr.r-lib.org/reference/with_locale.html)
(PR \#14, closes \#8) - Two issues closed as non-bugs: - `cohensD`
returning equal values for `method = "pooled"` and `method = "unequal"`
when group sizes are equal is a mathematical identity, not a defect
(closes \#7) - `methods` package failing to load under `Rscript` is
resolved by the existing `Imports: methods` declaration in DESCRIPTION
(closes \#1) - One missed Stage 2 hardening item fixed: `class() ==` and
`class() %in%` comparisons remaining in `modeOf` and `bars` replaced
with [`is.factor()`](https://rdrr.io/r/base/factor.html) and
[`inherits()`](https://rdrr.io/r/base/class.html), removing an R CMD
check NOTE (PR \#15) - `tibble` and `withr` added to `Suggests`;
`AGENTS.md` and `PLAN.md` added to `.Rbuildignore` - 501 assertions
total (up from 486)

------------------------------------------------------------------------

### Stage 4 — Release infrastructure ✅ Complete

**Goal:** Bring the changelog and version number up to date before any
further work.

**Completed:** 2026-07-11. Single commit to `dev`; no PR needed.

**Outcome:** - `Version` in `DESCRIPTION` bumped from `0.5.2` to
`0.5.2.9000` - `NEWS.md` created, documenting all changes since the
0.5.2 CRAN release (bug fixes, hardening, expanded test suite, new
`Suggests` entries) - The `^NEWS$` line in `.Rbuildignore` was already
absent; no action needed

------------------------------------------------------------------------

### Stage 5 — Test coverage review ✅ Complete

**Goal:** Identify lines not currently exercised by the test suite and
write targeted tests for the most important gaps. The aim is not 100%
coverage for its own sake — it is to ensure that non-trivial logic
branches are actually tested.

**Completed:** 2026-07-11.

**Outcome:** - Baseline coverage: 75.05% (501 assertions, from Stage
3) - Final coverage: 82.22% (556 assertions) -
[`correlate()`](https://lsr.djnavarro.net/reference/correlate.md) loop
bug fixed: `1:(n.cont-1)` → `seq_len(n.cont-1)` (and equivalent for the
pairwise sample-size loop), preventing incorrect iteration when fewer
than 2 numeric variables are present - New tests cover: `cohensD`
corrected method, `formula=` alias, paired warning; `correlate` numeric
vector / matrix inputs, single-vector edge case, `print(test=TRUE)`,
error paths; `pairedSamplesTTest` lme4-style `(1|id)` formula, one-sided
alternatives (both forms), unused factor levels, non-factor group,
non-factor id; `independentSamplesTTest` `one.sided` second-group
alternative, unused factor levels, non-factor group; `oneSampleTTest`
one-sided `"less"` alternative, all input-validation errors; `ciMean`
all input-validation errors, zero-variance warning, non-numeric matrix;
`cramersV` goodness-of-fit branch - Functions reaching 100%: `aad`,
`colCopy`, `cramersV`, `expandFactors`, `modeOf`, `oneSampleTTest`,
`permuteLevels`, `posthocPairwiseT`, `quantileCut`, `rowCopy`,
`sortFrame`, `tFrame`, `unlibrary` - Remaining gaps (accepted): -
`bars.R` (58%): complex plotting function — not tested per plan -
`rmAll.R` (37%): interactive `readline` branch — untestable in CI -
`pairedSamplesTTest.R` (71%), `cohensD.R` (69%),
`independentSamplesTTest.R` (76%): remaining uncovered lines are
workspace-lookup paths (calling without `data=`) that require
parent-frame manipulation awkward in testthat, and duplicative
formula-alias input combinations

------------------------------------------------------------------------

### Stage 6 — Roxygen documentation improvement ✅ Complete

**Goal:** Make the documentation more accurate, consistent, and
beginner-friendly across all 29 exported functions and their print
methods.

**Guiding principle — the reader is a novice:** The audience is
introductory statistics students encountering R for the first time.
Documentation must reflect this:

- **Avoid S3/OOP jargon.** “S3 method”, “dispatch”, “generic function”,
  “class attribute” mean nothing to a beginner. Rewrite in plain
  English.
- **Describe what the user sees, not what R does internally.** For
  functions whose primary purpose is printing (t-test functions,
  [`who()`](https://lsr.djnavarro.net/reference/who.md),
  [`correlate()`](https://lsr.djnavarro.net/reference/correlate.md),
  etc.), `@return` should describe the printed output — not lead with
  “returns an object of class X”.
- **`@examples` should be self-contained, immediately runnable, and use
  consistent code style** (spaces around `=`, spaces after commas, no
  extra spaces after `(` or before `)`). Fixing both the content and the
  code style of existing examples is explicitly within scope.

**Specific checks per file:** - `@description` — one clear sentence;
remove redundant hedges like “convenience function intended for
pedagogical purposes only”. - `@param` — all parameters documented;
plain language (not “a logical flag”, but “set to `TRUE` to …”). -
`@return` — highest priority for revision; describe what the user
sees. - `@details` — verify accuracy after Stages 1–3 changes. -
`@seealso` — cross-references relevant and complete. - `@examples` —
self-contained, runnable, and consistent code style; add examples where
missing; improve existing examples where style or clarity falls short. -
Print methods — minimal but plain-English documentation; note they are
called automatically when the object is printed, without using the word
“dispatch”.

**Approach:** One PR per function group. Run `devtools::document()`
after each batch to verify `.Rd` files regenerate cleanly.

#### Group 1 — T-tests ✅ Complete (PR \#17, 2026-07-11)

`oneSampleTTest`, `independentSamplesTTest`, `pairedSamplesTTest`,
`cohensD`, `print.TTest`

#### Group 2 — Hypothesis tests and effect sizes ✅ Complete (PR \#18, 2026-07-11)

`associationTest`, `goodnessOfFitTest`, `etaSquared`, `cramersV`,
`ciMean`, `correlate`, `posthocPairwiseT`, `bars`, `print.assocTest`,
`print.gofTest`, `print.correlate`

#### Group 3 — Data manipulation ✅ Complete (PR \#19, 2026-07-11)

`wideToLong`, `longToWide`, `expandFactors`, `permuteLevels`,
`quantileCut`, `sortFrame`, `tFrame`, `colCopy`, `rowCopy`

#### Group 4 — Workspace utilities ✅ Complete (PR \#20, 2026-07-11)

`who`, `modeOf`, `aad`, `maxFreq`, `rmAll`, `unlibrary`, `importList`,
`standardCoefs`, `print.whoList`

------------------------------------------------------------------------

### Stage 7 — Code linting ✅ Complete

**Goal:** Apply tidyverse style consistently across all 29 source files
in `R/`. All changes are cosmetic only — no logic, no API, no
return-value changes. The existing test suite and `R CMD check` are the
safety net.

**Completed:** 2026-07-11. PR \#21 merged into `dev`.

**Outcome:** - `styler::style_pkg(filetype = "R")` applied to all 29
`R/*.R` files and all 28 test files: spaces around operators, spaces
after commas, no extra spaces inside parentheses, 2-space indentation,
opening braces on same line - Dead code removed from `bars.R`:
commented-out `isColours()` function and its TODO note, commented-out
colour-validation checks, stray `# barLineColour ???` inline comment,
and `# par(old.par)` remnant - Dead code removed from `rowCopy.R`:
commented-out alternative implementation `# t(replicate(times,x))` -
Dead code removed from `correlate.R`: two commented-out
`ct <- cor.test(...)` lines superseded by the `getCT()` helper - `cttp`
→ `ct_result` in `correlate.R` (cryptic abbreviation) - `c` → `var_call`
in `who.R` `getWhoInfo()` (shadowed base
[`c()`](https://rdrr.io/r/base/c.html)) - `test-rmAll.R`: added
`inherits = FALSE` to [`exists()`](https://rdrr.io/r/base/exists.html)
calls to prevent false positives from session variables (pre-existing
test fragility) - 556 assertions pass; `R CMD check`: 0 errors, 0
warnings, 0 notes

------------------------------------------------------------------------

### Stage 8 — pkgdown site ✅ Complete

**Goal:** Build a polished documentation site with a well-organised
Reference section, an updated README, and two articles covering the
package from different angles.

#### Sub-task 8a — Structured References (`_pkgdown.yml`)

Replace the default alphabetical function list with four named groups:

| Group | Functions |
|----|----|
| Hypothesis tests | `oneSampleTTest`, `independentSamplesTTest`, `pairedSamplesTTest`, `associationTest`, `goodnessOfFitTest`, `posthocPairwiseT` |
| Effect sizes and descriptive statistics | `cohensD`, `etaSquared`, `cramersV`, `ciMean`, `correlate` |
| Data manipulation | `wideToLong`, `longToWide`, `expandFactors`, `permuteLevels`, `quantileCut`, `sortFrame`, `tFrame`, `colCopy`, `rowCopy` |
| Workspace utilities | `who`, `modeOf`, `maxFreq`, `aad`, `rmAll`, `unlibrary`, `importList`, `standardCoefs` |

Print methods are listed at the end of their parent function’s group.
Also add an `articles:` section to `_pkgdown.yml` pointing at the two
new articles.

#### Sub-task 8b — README rewrite

`README.Rmd` (which generates the pkgdown landing page) will be
rewritten to include: - An orienting description (what the package is,
who it is for, textbook link) - A brief motivating example showing the
beginner-friendly output format - A “Where to go next” section linking
to the articles and Reference page - Installation instructions (retained
from current README)

`README.md` is regenerated from `README.Rmd` and committed alongside it.

#### Sub-task 8c — Article 1: Guided overview (novice audience) ✅ Complete (PR \#22, 2026-07-11)

**File:** `vignettes/articles/overview.Rmd` (pkgdown-only; not built by
`R CMD check`)

**Audience:** Introductory statistics students; readers of the
accompanying textbook.

**Tone:** Informal, first-person, encouraging.

**Structure:** 1. Why this package exists 2. Setting up
([`library(lsr)`](https://github.com/djnavarro/lsr),
[`who()`](https://lsr.djnavarro.net/reference/who.md)) 3. Descriptive
statistics (`ciMean`, `aad`, `modeOf`, `correlate`) 4. Reshaping data
(`wideToLong`, `longToWide`) 5. Hypothesis testing — one worked example
each of the six test functions 6. Effect sizes (`cohensD`, `etaSquared`,
`cramersV`) 7. Where to go next (textbook, Article 2)

A single coherent toy dataset will be used throughout.

#### Sub-task 8d — Article 2: Critical commentary (intermediate audience) ✅ Complete (PR \#23, 2026-07-11)

**File:** `vignettes/articles/commentary.Rmd` (pkgdown-only)

**Audience:** Intermediate R users; those considering recommending the
package to students, or graduating students away from it.

**Tone:** Informal, first-person, candid; occasional expert asides.

**Structure:** 1. What this package is and isn’t (pedagogical scaffold,
not a research tool) 2. Hypothesis test wrappers — what they add, what
they give up; better alternatives (`infer`, `easystats`) 3. Effect sizes
— limitations; `effectsize` package as alternative 4. Correlation
matrices — why the built-in correction may not be enough; `correlation`
from `easystats` 5. Data reshaping — where the naming-convention
approach breaks down; `tidyr::pivot_longer` / `pivot_wider` 6. Plotting
— [`bars()`](https://lsr.djnavarro.net/reference/bars.md) limitations;
`ggplot2` as the right tool 7. Workspace utilities — why you probably
don’t need them in modern IDEs 8. The bottom line — honest assessment
and graduation path

**PR breakdown for Stage 8:**

| PR       | Content                                           |
|----------|---------------------------------------------------|
| Stage 8a | `_pkgdown.yml` reference groups + articles config |
| Stage 8b | `README.Rmd` rewrite + regenerated `README.md`    |
| Stage 8c | Article 1: guided overview                        |
| Stage 8d | Article 2: critical commentary                    |

------------------------------------------------------------------------

### Stage 9 — CRAN submission preparation ⬜ Pending

**Goal:** Ensure the package passes `R CMD check --as-cran` with 0
errors, 0 warnings, and 0 notes, and that all supporting materials
(version number, NEWS.md, DESCRIPTION, `cran-comments.md`) are ready for
submission.

**Version:** `1.0.0` — the API has been stable for years, the package is
feature-frozen, and the maintenance cycle has brought tests, internals,
docs, and site to a mature state. 1.0.0 communicates that accurately.

**Sub-tasks:**

- **9a — Version bump and NEWS finalisation**
  - Bump `Version` in `DESCRIPTION` from `0.5.2.9000` → `1.0.0`
  - Remove `LazyLoad: yes` from `DESCRIPTION` (field is deprecated)
  - Rename the `# lsr 0.5.2.9000` heading in `NEWS.md` to `# lsr 1.0.0`
  - Condense NEWS entries to user-visible changes (bug fixes +
    dependency additions); test-suite and internal changes are not
    typically included verbatim in a CRAN NEWS entry
  - Confirm `RoxygenNote` in `DESCRIPTION` matches the installed
    roxygen2 version (currently `8.0.0`; verify before submitting)
- **9b — DESCRIPTION audit**
  - Title: verify title case and CRAN policy compliance
  - Description: verify it doesn’t begin “This package …”, ends with a
    period, and doesn’t repeat the package name
  - URL field: `http://lsr.djnavarro.net/` is the package site — update
    scheme to `https://` if the site supports it
  - BugReports: already correct
- **9c — Spelling check**
  - Run `spelling::spell_check_package()` and review results
  - Add a `WORDLIST` file (via `spelling::update_wordlist()`) for
    correctly- spelled technical terms flagged by the checker (function
    names, proper nouns such as “Navarro”, “Cohen”, “Cramér”)
- **9d — URL check**
  - Run `urlchecker::url_check()` on the package
  - Fix or remove any broken or redirecting URLs in `.Rd` files and
    vignettes
- **9e — `R CMD check --as-cran` (local)**
  - Run `devtools::check(args = "--as-cran")` locally
  - Target: 0 errors, 0 warnings, 0 notes (the unavoidable “New
    submission” note on a version bump is acceptable)
- **9f — rhub checks**
  - Run `rhub::rhub_check()` to check on additional platforms beyond the
    CI matrix (Windows with rtools, various Linux distributions, macOS
    arm64)
  - Review any notes or warnings that appear only on specific platforms;
    rhub can surface encoding issues, compiler differences, and
    platform- specific NOTE triggers that local and GitHub CI checks
    miss
- **9g — Reverse dependency check**
  - Run `revdepcheck::revdep_check()` to check all CRAN packages that
    depend on lsr
  - Confirm that internal changes (hardened input validation, tibble
    coercion, `seq_len` fix in `correlate`) have not broken any
    downstream packages
  - Document the results in `cran-comments.md`
- **9h — CI check on GitHub Actions**
  - Push the prep branch to GitHub and confirm the `R-CMD-check.yaml`
    matrix passes cleanly on all three OS × R version combinations
- **9i — `cran-comments.md`**
  - Create `cran-comments.md` at the repo root (add to `.Rbuildignore`)
  - Include: summary of changes since 0.5.2; `R CMD check` result; any
    platform-specific rhub notes; reverse-dependency check results

**PR:** Single PR (`stage-9-cran-prep`) targeting `dev`. No functional
code changes; only version bumps, text, and supporting files.

------------------------------------------------------------------------

### Stage 10 — CRAN submission and post-release ⬜ Pending

**Goal:** Submit to CRAN, handle any reviewer feedback, and close out
the release on GitHub.

**Sub-tasks:**

- **10a — Merge `dev` → `main` and deploy pkgdown site**
  - Open and merge the `stage-9-cran-prep` PR into `dev`, then merge
    `dev` into `main`
  - The `pkgdown.yaml` GitHub Actions workflow will rebuild and deploy
    the site automatically; wait for it to complete
  - Verify the two article URLs that triggered a win-builder NOTE are
    now live: `https://lsr.djnavarro.net/articles/overview.html` and
    `https://lsr.djnavarro.net/articles/commentary.html`
- **10b — Final win-builder and mac-builder checks**
  - Re-run `devtools::check_win_devel()` and
    `devtools::check_win_release()` from `main`; both should now be 0
    errors, 0 warnings, 0 notes
  - Retry `devtools::check_mac_release()` (was returning HTTP 502 during
    Stage 9)
  - Update `cran-comments.md` with the clean results
- **10c — Submit to CRAN**
  - Submit via `devtools::submit_cran()` (runs `R CMD check --as-cran`
    one final time and uploads to CRAN’s web form)
  - Watch for the automatic confirmation email; respond if CRAN sends
    further questions
- **10d — Handle CRAN reviewer feedback (if any)**
  - Address any notes or requests from the CRAN team promptly
  - Small fixes go into a follow-up commit on the same branch (or a new
    `stage-9-cran-prep-v2` branch)
  - Do not merge into `main` until CRAN confirms acceptance
- **10e — Post-acceptance: tag and reopen dev**
  - Create a GitHub release tagged `v1.0.0`, with release notes drawn
    from the `NEWS.md` entry
- **10f — Re-open `dev` for future work**
  - Bump `Version` in `DESCRIPTION` to `1.0.0.9000`
  - Add a new `# lsr 1.0.0.9000` heading at the top of `NEWS.md`
  - Commit as “open dev for post-1.0.0 work”

------------------------------------------------------------------------

## Guiding principles

- The package is **stable and feature-frozen**. No new exported
  functions, no API changes, no new dependencies.
- All PRs target `dev`. `main` is only updated for CRAN releases.
- When in doubt, do less. A minimal, targeted change is always
  preferable to a broad refactor.
