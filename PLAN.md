# lsr Maintenance Plan

This file records the overall maintenance plan for the package and the
current status of each stage. Update it as stages are completed.

---

## Stages

### Stage 1 — Test suite quality ✅ Complete

**Goal:** Build a test suite thorough enough to support safe internal
hardening. No functional code changes.

**Completed:** 2026-07-10. PR merged into `dev`.

**Outcome:**
- 28 test files, 448 assertions
- All 29 exported functions and all S3 print methods covered
- Every test file covers: typical usage, numeric correctness against a
  known reference, and expected errors/warnings
- Five latent bugs identified and documented (see Stage 2 targets below)

---

### Stage 2 — Internal hardening ✅ Complete

**Goal:** Improve input validation and defensive coding so the package
continues to work reliably as R and its ecosystem evolve. Tests from
Stage 1 are in place to verify that no existing behaviour is broken.

**Completed:** 2026-07-10.

**Outcome:**
- 486 assertions (up from 448); all new tests pass
- All five original targets from Stage 1 addressed
- Full review of all 29 source files conducted; four additional
  groups of issues identified and resolved:
  - **Group 1:** `NA` not checked for logical flag arguments — fixed in
    `aad`, `modeOf`, `maxFreq`, `etaSquared`, `permuteLevels`,
    `sortFrame`, `rmAll`, `who`; guards added from scratch to `rmAll`
    and `who`
  - **Group 2:** `|` instead of `||` in multi-condition `conf.level`
    guards, causing opaque "condition has length > 1" errors — fixed in
    `oneSampleTTest`, `independentSamplesTTest`, `pairedSamplesTTest`
  - **Group 3:** Deprecated `class() ==` / `class() %in%` comparisons
    replaced with `inherits()`, `is.name()`, `is.matrix()`,
    `is.numeric()` in `correlate`, `associationTest`,
    `goodnessOfFitTest`, `ciMean`
  - **Group 4:** Missing input validation added to `longToWide`,
    `expandFactors`, `wideToLong` (including a `split = NA` guard
    folded in from the Group 1 pattern)

---

### Stage 3 — Bug fixes ✅ Complete

**Goal:** Fix genuine defects where a fix is clearly safe.

**Approach:** Be conservative. Prefer doing nothing over introducing a
breaking change. One fix per PR; include or update a test for each fix.

**Completed:** 2026-07-11.

**Outcome:**
- All five open GitHub issues reviewed and resolved
- Three confirmed bugs fixed:
  - `oneSampleTTest`: `conf.level` not forwarded to `stats::t.test()`,
    so `$conf.int` always contained the 0.95 interval regardless of what
    the user requested (PR #12, closes #9)
  - Tibble compatibility: `data[, col]` returning a one-column tibble
    instead of a vector caused false type-check failures in
    `independentSamplesTTest`, `pairedSamplesTTest`, and
    `associationTest`; fixed by coercing `data` to a plain data frame on
    entry (PR #13, closes #2)
  - `sortFrame` locale-sensitive test: the mixed-case character sort test
    was commented out because results varied across platforms; restored
    and stabilised by pinning `LC_COLLATE = "C"` with
    `withr::with_locale()` (PR #14, closes #8)
- Two issues closed as non-bugs:
  - `cohensD` returning equal values for `method = "pooled"` and
    `method = "unequal"` when group sizes are equal is a mathematical
    identity, not a defect (closes #7)
  - `methods` package failing to load under `Rscript` is resolved by the
    existing `Imports: methods` declaration in DESCRIPTION (closes #1)
- One missed Stage 2 hardening item fixed: `class() ==` and
  `class() %in%` comparisons remaining in `modeOf` and `bars` replaced
  with `is.factor()` and `inherits()`, removing an R CMD check NOTE
  (PR #15)
- `tibble` and `withr` added to `Suggests`; `AGENTS.md` and `PLAN.md`
  added to `.Rbuildignore`
- 501 assertions total (up from 486)

---

### Stage 4 — Release infrastructure ✅ Complete

**Goal:** Bring the changelog and version number up to date before any
further work.

**Completed:** 2026-07-11. Single commit to `dev`; no PR needed.

**Outcome:**
- `Version` in `DESCRIPTION` bumped from `0.5.2` to `0.5.2.9000`
- `NEWS.md` created, documenting all changes since the 0.5.2 CRAN release
  (bug fixes, hardening, expanded test suite, new `Suggests` entries)
- The `^NEWS$` line in `.Rbuildignore` was already absent; no action needed

---

### Stage 5 — Test coverage review ✅ Complete

**Goal:** Identify lines not currently exercised by the test suite and write
targeted tests for the most important gaps. The aim is not 100% coverage for
its own sake — it is to ensure that non-trivial logic branches are actually
tested.

**Completed:** 2026-07-11.

**Outcome:**
- Baseline coverage: 75.05% (501 assertions, from Stage 3)
- Final coverage: 82.22% (556 assertions)
- `correlate()` loop bug fixed: `1:(n.cont-1)` → `seq_len(n.cont-1)` (and
  equivalent for the pairwise sample-size loop), preventing incorrect
  iteration when fewer than 2 numeric variables are present
- New tests cover: `cohensD` corrected method, `formula=` alias, paired
  warning; `correlate` numeric vector / matrix inputs, single-vector edge
  case, `print(test=TRUE)`, error paths; `pairedSamplesTTest` lme4-style
  `(1|id)` formula, one-sided alternatives (both forms), unused factor levels,
  non-factor group, non-factor id; `independentSamplesTTest` `one.sided`
  second-group alternative, unused factor levels, non-factor group;
  `oneSampleTTest` one-sided `"less"` alternative, all input-validation
  errors; `ciMean` all input-validation errors, zero-variance warning,
  non-numeric matrix; `cramersV` goodness-of-fit branch
- Functions reaching 100%: `aad`, `colCopy`, `cramersV`, `expandFactors`,
  `modeOf`, `oneSampleTTest`, `permuteLevels`, `posthocPairwiseT`,
  `quantileCut`, `rowCopy`, `sortFrame`, `tFrame`, `unlibrary`
- Remaining gaps (accepted):
  - `bars.R` (58%): complex plotting function — not tested per plan
  - `rmAll.R` (37%): interactive `readline` branch — untestable in CI
  - `pairedSamplesTTest.R` (71%), `cohensD.R` (69%),
    `independentSamplesTTest.R` (76%): remaining uncovered lines are
    workspace-lookup paths (calling without `data=`) that require
    parent-frame manipulation awkward in testthat, and duplicative
    formula-alias input combinations

---

### Stage 6 — Roxygen documentation improvement 🔄 Current focus

**Goal:** Make the documentation more accurate, consistent, and
beginner-friendly across all 29 exported functions and their print methods.

**Guiding principle — the reader is a novice:** The audience is introductory
statistics students encountering R for the first time. Documentation must
reflect this:

- **Avoid S3/OOP jargon.** "S3 method", "dispatch", "generic function",
  "class attribute" mean nothing to a beginner. Rewrite in plain English.
- **Describe what the user sees, not what R does internally.** For functions
  whose primary purpose is printing (t-test functions, `who()`, `correlate()`,
  etc.), `@return` should describe the printed output — not lead with
  "returns an object of class X".
- **`@examples` should be self-contained, immediately runnable, and use
  consistent code style** (spaces around `=`, spaces after commas, no
  extra spaces after `(` or before `)`). Fixing both the content and the
  code style of existing examples is explicitly within scope.

**Specific checks per file:**
- `@description` — one clear sentence; remove redundant hedges like
  "convenience function intended for pedagogical purposes only".
- `@param` — all parameters documented; plain language (not "a logical flag",
  but "set to `TRUE` to ...").
- `@return` — highest priority for revision; describe what the user sees.
- `@details` — verify accuracy after Stages 1–3 changes.
- `@seealso` — cross-references relevant and complete.
- `@examples` — self-contained, runnable, and consistent code style; add
  examples where missing; improve existing examples where style or clarity
  falls short.
- Print methods — minimal but plain-English documentation; note they are
  called automatically when the object is printed, without using the word
  "dispatch".

**Approach:** One PR per function group. Run `devtools::document()` after
each batch to verify `.Rd` files regenerate cleanly.

#### Group 1 — T-tests ✅ Complete (PR #17, 2026-07-11)

`oneSampleTTest`, `independentSamplesTTest`, `pairedSamplesTTest`,
`cohensD`, `print.TTest`

#### Group 2 — Hypothesis tests and effect sizes ✅ Complete (PR #18, 2026-07-11)

`associationTest`, `goodnessOfFitTest`, `etaSquared`, `cramersV`,
`ciMean`, `correlate`, `posthocPairwiseT`, `bars`,
`print.assocTest`, `print.gofTest`, `print.correlate`

#### Group 3 — Data manipulation 🔄 Next

`wideToLong`, `longToWide`, `expandFactors`, `permuteLevels`,
`quantileCut`, `sortFrame`, `tFrame`, `colCopy`, `rowCopy`

#### Group 4 — Workspace utilities

`who`, `modeOf`, `aad`, `maxFreq`, `rmAll`, `unlibrary`,
`importList`, `standardCoefs`, `print.whoList`

---

## Guiding principles

- The package is **stable and feature-frozen**. No new exported functions,
  no API changes, no new dependencies.
- All PRs target `dev`. `main` is only updated for CRAN releases.
- When in doubt, do less. A minimal, targeted change is always preferable
  to a broad refactor.
