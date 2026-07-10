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

## Guiding principles

- The package is **stable and feature-frozen**. No new exported functions,
  no API changes, no new dependencies.
- All PRs target `dev`. `main` is only updated for CRAN releases.
- When in doubt, do less. A minimal, targeted change is always preferable
  to a broad refactor.
