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

### Stage 3 — Bug fixes 🔄 Current focus

**Goal:** Fix genuine defects where a fix is clearly safe.

**Approach:** Be conservative. Prefer doing nothing over introducing a
breaking change. One fix per PR; include or update a test for each fix.

Known candidates will be identified during Stage 3 work.

---

## Guiding principles

- The package is **stable and feature-frozen**. No new exported functions,
  no API changes, no new dependencies.
- All PRs target `dev`. `main` is only updated for CRAN releases.
- When in doubt, do less. A minimal, targeted change is always preferable
  to a broad refactor.
