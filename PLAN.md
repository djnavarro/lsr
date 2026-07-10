# lsr Maintenance Plan

This file records the overall maintenance plan for the package and the
current status of each stage. Update it as stages are completed.

---

## Stages

### Stage 1 — Test suite quality ✅ Complete

**Goal:** Build a test suite thorough enough to support safe internal
hardening. No functional code changes.

**Completed:** 2025-07-10. PR merged into `dev`.

**Outcome:**
- 28 test files, 448 assertions
- All 29 exported functions and all S3 print methods covered
- Every test file covers: typical usage, numeric correctness against a
  known reference, and expected errors/warnings
- Five latent bugs identified and documented (see Stage 2 targets below)

---

### Stage 2 — Internal hardening 🔄 Current focus

**Goal:** Improve input validation and defensive coding so the package
continues to work reliably as R and its ecosystem evolve. Tests from
Stage 1 are in place to verify that no existing behaviour is broken.

**Approach:** One narrow PR per issue. Do not refactor adjacent functions
unless there is a direct dependency. All changes must pass the full test
suite and `R CMD check`.

**Known targets (discovered during Stage 1):**

| Priority | Location | Issue |
|----------|----------|-------|
| 1 | `printTTest.R` | `print.TTest` has no `return()` call; returns `NULL` instead of `invisible(x)` as documented. Every other lsr print method returns `invisible(x)`. |
| 2 | `unlibrary.R` | `unlibrary("pkg")` (quoted string) fails because `deparse(substitute("pkg"))` produces nested quotes. Only the unquoted form works. |
| 3 | `modeOf.R`, `colCopy.R` | `is.vector(list(...))` is `TRUE` in R, so plain list inputs slip past several `is.vector()` guards. Affects `modeOf`, `maxFreq`, `colCopy`, `rowCopy`. |
| 4 | `importList.R` | `importList(ask = NA)` passes the `is(ask, "logical")` guard but errors later with an opaque message. Guard should also check `!is.na(ask)`. |
| 5 | `standardCoefs.R` | `aov` objects inherit from `lm` and pass the `is(x, "lm")` guard silently. Decide whether `aov` input should be accepted or rejected with an informative message. |

Each issue already has a test that will catch the fix.

---

### Stage 3 — Bug fixes ⏳ Pending

**Goal:** Fix genuine defects where a fix is clearly safe. This stage
begins only after Stage 2 is complete.

**Approach:** Be conservative. Prefer doing nothing over introducing a
breaking change. One fix per PR; include or update a test for each fix.

Known candidates will be identified during Stage 2 work.

---

## Guiding principles

- The package is **stable and feature-frozen**. No new exported functions,
  no API changes, no new dependencies.
- All PRs target `dev`. `main` is only updated for CRAN releases.
- When in doubt, do less. A minimal, targeted change is always preferable
  to a broad refactor.
