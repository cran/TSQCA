# TSQCA 1.2.0

*Release date: 2026-01-19*

## Bug Fixes

### CRITICAL: Fixed Intermediate Solution Extraction

**Problem:** When `dir.exp` is specified for intermediate solutions, the QCA package stores:
- `sol$solution` — Contains the **Parsimonious** solution
- `sol$i.sol$C1P1$solution` — Contains the true **Intermediate** solution

Previous versions of TSQCA incorrectly prioritized `sol$solution`, causing Parsimonious solutions to be extracted and displayed when Intermediate solutions were expected.

**Fix:** All solution extraction functions now correctly prioritize `sol$i.sol` when available:

- `get_n_solutions()` — Now checks `i.sol` first
- `qca_extract()` — Now checks `i.sol` first  
- `extract_solution_list()` — Now checks `i.sol` first
- `write_full_report()` — Fixed 3 locations
- `write_simple_report()` — Fixed 1 location

**Impact:** Users who specified `dir.exp` for intermediate solutions may have received incorrect results in:
- Report generation (`generate_report()`)
- Configuration charts
- Solution expression extraction

**Verification:** The `print(sol)` output was always correct because the QCA package's print method handles this correctly. Only programmatic extraction was affected.

## New Features

### Solution Type Display

Reports now explicitly display the solution type in the Analysis Overview section:

| Include | dir.exp | **Solution Type** |
|---------|---------|-------------------|
| `""` | any | Complex (Conservative) |
| `"?"` | `NULL` | Parsimonious |
| `"?"` | specified | **Intermediate** |

### QCA Package Output for Verification

Added optional raw QCA output section to reports for verification purposes:

```r
generate_report(result, "report.md", include_raw_output = TRUE)  # default
generate_report(result, "report.md", include_raw_output = FALSE) # disable
```

When enabled, each threshold's detailed results include:

```
#### QCA Package Output (for verification)

```
DEV*URB*LIT*STB + DEV*LIT*~IND*STB -> SURV
```

```

This allows researchers to verify that TSQCA's extraction matches the QCA package's native output.

## Migration Guide

If you used intermediate solutions (with `dir.exp`) in previous versions, we recommend re-running analyses to ensure correct results. Compare your new results with `print(minimize(...))` output for verification.

---

# TSQCA 1.1.0

*Release date: 2026-01-17*

## Bug Fixes

### CRITICAL: Fixed `dir.exp = NULL` Behavior

**Problem:** In v1.0.0, when `dir.exp = NULL` (the default), the package incorrectly converted it to `c(1, 1, ...)`, which forced intermediate solution calculation regardless of user intent.

**Fix:** `dir.exp = NULL` is now correctly passed to `QCA::minimize()` without modification.

## Breaking Changes

### Default Arguments Now Match QCA Package

To ensure consistency with the QCA package, default argument values have been changed:

| Argument | v1.0.0 Default | v1.1.0 Default | Effect |
|----------|---------------|---------------|--------|
| `include` | `"?"` | `""` | Complex solution (no logical remainders) |
| `dir.exp` | `NULL` → `c(1,1,...)` (bug) | `NULL` | No directional expectations |

**Result:** TSQCA now produces **complex solutions** by default, matching `QCA::minimize()` default behavior.

### Solution Type Summary

| Solution Type | How to Compute |
|--------------|----------------|
| **Complex** (default) | `include = ""`, `dir.exp = NULL` |
| **Parsimonious** | `include = "?"`, `dir.exp = NULL` |
| **Intermediate** | `include = "?"`, `dir.exp = c(1, 1, ...)` |

### Migration Guide

```r
# v1.0.0 (incorrect: intermediate solution by default due to bug)
result <- otSweep(dat, "Y", c("X1", "X2", "X3"), sweep_range = 7, thrX = thrX)

# v1.1.0: Complex solution (new default, QCA compatible)
result_comp <- otSweep(dat, "Y", c("X1", "X2", "X3"), sweep_range = 7, thrX = thrX)

# v1.1.0: Parsimonious solution (include = "?")
result_pars <- otSweep(dat, "Y", c("X1", "X2", "X3"), sweep_range = 7, thrX = thrX,
                       include = "?")

# v1.1.0: Intermediate solution (include = "?" + dir.exp)
result_int <- otSweep(dat, "Y", c("X1", "X2", "X3"), sweep_range = 7, thrX = thrX,
                      include = "?",
                      dir.exp = c(1, 1, 1))
```

## Documentation Improvements

### Enhanced Examples for All Sweep Functions

All four sweep functions (`otSweep`, `dtSweep`, `ctSweepS`, `ctSweepM`) now include examples demonstrating:

1. **Complex Solution** — default (QCA compatible)
2. **Parsimonious Solution** — using `include = "?"`
3. **Intermediate Solution** — using `include = "?"` with `dir.exp`

### Updated Parameter Documentation

The `@param dir.exp` and `@param include` documentation now clearly explains:
- Default behavior produces complex solutions (QCA compatible)
- `include = "?"` enables logical remainders for parsimonious/intermediate
- `dir.exp` specifies directional expectations for intermediate solutions

---

# TSQCA 1.0.0

*Release date: 2026-01-06*

## Changes

### Default Chart Level Changed to "term" (Fiss-style)

The default value for `chart_level` parameter has been changed from `"summary"` to `"term"`.

**Rationale:**
The solution-term level format (Fiss, 2011 notation) is the standard for academic publications, where each column represents one prime implicant (configuration). The previous default (`"summary"`) aggregated all configurations at each threshold into a single column, which obscured the distinction between different sufficient paths.

**Column header format updated:**
- Old format: `thrY=6_M1`
- New format: `thrY = 6 (M1)` (consistent with the paper format)

**Affected functions:**
- `generate_report()` — default `chart_level` is now `"term"`
- `generate_cross_threshold_chart()` — default `chart_level` is now `"term"`

**Migration:**
If you prefer the previous behavior (threshold-level summary), explicitly specify `chart_level = "summary"`:

```r
generate_report(result, "report.md", chart_level = "summary")
generate_cross_threshold_chart(result, conditions, chart_level = "summary")
```

---

# TSQCA 0.5.3

*Release date: 2026-01-03*

## New Features

### Solution-Term Level Configuration Charts (Fiss-style)

Added support for solution-term level configuration charts following Fiss (2011) notation. This feature allows generating charts where each column represents a single prime implicant (configuration), which is the standard format for academic publications.

**New parameter for `generate_report()`:**

* `chart_level` — Character. Either `"summary"` (default) or `"term"`.
  - `"summary"`: Threshold-level summaries where each column represents one threshold, showing all conditions that appear in any configuration at that threshold.
  - `"term"`: Solution-term level (Fiss-style) where each column represents one prime implicant (sufficient configuration). Recommended for academic publications.

**New functions:**

* `generate_cross_threshold_chart()` — Generate configuration charts from sweep results with `chart_level` option.
* `parse_solution_terms()` — Internal function to parse solution expressions into individual terms.
* `get_condition_status()` — Internal function to determine condition presence/absence in a term.
* `generate_term_level_chart()` — Internal function for term-level chart generation.
* `generate_threshold_level_chart()` — Internal function for threshold-level chart generation.

**Example:**

```r
# Threshold-level summary (default)
generate_report(result, "report.md", chart_level = "summary")

# Solution-term level (Fiss-style, recommended for publications)
generate_report(result, "report.md", chart_level = "term")
```

When the solution is `X3 + X1*X2`, the term-level chart will show two separate columns (`thrY=7_M1` for `X3` and `thrY=7_M2` for `X1*X2`), while the summary-level chart shows one column (`thrY=7`) with all three conditions marked.

---

# TSQCA 0.5.2

*Release date: 2026-01-03*

## Documentation Fixes

### Vignette Examples Corrected

Fixed non-working code examples in all vignettes (Tutorial and Reproducible, both EN/JA):

* Updated argument names from deprecated `Yvar`/`Xvars` to `outcome`/`conditions`
* Fixed `ctSweepM()` examples to use `sweep_list` parameter instead of old `sweep_vars`/`sweep_range`
* Added required `dat` parameter to all `generate_report()` examples
* Changed output from `head(res$summary)` to `summary(res)` for consistency with S3 methods
* Reduced sweep ranges (e.g., 6:9 → 6:8, 6:8 → 6:7) for faster example execution

### README Updates

* Updated all code examples to use new argument names (`outcome`, `conditions`)
* Added `dat` parameter to `generate_report()` examples
* Consistent sweep ranges across all examples

### Test Scripts Added

* `test_quick.R` — Minimal verification script (6 tests)
* `test_tutorial_code.R` — Comprehensive verification script (11 tests)

---

# TSQCA 0.5.1

*Release date: 2026-01-01*

## New Features

### Multiple Solutions Note in Configuration Charts

When multiple logically equivalent solutions (M1, M2, M3...) exist, configuration charts now automatically include a note explaining that M1 is displayed.

**New parameters for `generate_report()`:**

* `solution_note` — Logical. If TRUE (default), adds note when multiple solutions exist
* `solution_note_style` — `"simple"` (default) or `"detailed"` (includes EPIs)
* `solution_note_lang` — `"en"` (default) or `"ja"` for Japanese

**New parameters for `config_chart_from_paths()`:**

* `n_sol` — Number of equivalent solutions (triggers note if > 1)
* `solution_note` — Logical. Whether to add solution note
* `solution_note_style` — `"simple"` or `"detailed"`
* `epi_list` — Character vector of EPIs for detailed notes

**New exported functions:**

* `generate_solution_note()` — Generate solution note text
* `identify_epi()` — Identify Essential Prime Implicants from multiple solutions

**Example output (simple):**

```
*Note: 2 logically equivalent solutions were identified. This table presents configurations based on M1.*
```

**Example output (detailed with EPIs):**

```
*Note: 3 logically equivalent solutions were identified (M1-M3). This table presents configurations based on M1. All solutions share the essential prime implicants: A·B and C.*
```

**Example (Japanese):**

```
*注: 論理的に等価な2つの解が得られた。本表はM1に基づく構成を示す。*
```

---

# TSQCA 0.5.0

*Release date: 2025-12-31*

## New Features

### Configuration Chart Integration in Reports

Configuration charts are now automatically included in reports generated by `generate_report()`.

**New parameters for `generate_report()`:**

* `include_chart` — Logical. If TRUE (default), includes Fiss-style configuration charts
* `chart_symbol_set` — Symbol set: `"unicode"` (default), `"ascii"`, or `"latex"`

**Example:**

```r
# Generate report with configuration charts (default)
generate_report(result, "my_report.md", format = "full")

# Generate report without charts
generate_report(result, "my_report.md", include_chart = FALSE)

# Generate report with LaTeX symbols (for PDF/academic papers)
generate_report(result, "my_report.md", chart_symbol_set = "latex")
```

### Standalone Configuration Chart Functions

* `generate_config_chart()` — Generate chart from QCA solution object
* `config_chart_from_paths()` — Generate chart from path strings (e.g., "A*B*~C")
* `config_chart_multi_solutions()` — Generate separate charts for multiple solutions

**Features:**

* Three symbol sets: `"unicode"` (● / ⊗), `"ascii"` (O / X), `"latex"` ($\bullet$ / $\otimes$)
* Automatic condition extraction from solution paths
* Bilingual labels (English / Japanese)
* Markdown table output

## Breaking Changes (from 0.4.x)

### Terminology Correction

Fixed incorrect use of "Core Conditions" terminology:

| Old (incorrect) | New (correct) | Meaning |
|-----------------|---------------|---------|
| `extract_mode = "core"` | `extract_mode = "essential"` | Mode for extracting shared terms |
| `core_terms` | `essential_terms` | Terms in ALL solutions |
| `peripheral_terms` | `selective_terms` | Terms in SOME solutions |

**Migration:** Change `extract_mode = "core"` to `extract_mode = "essential"`.

## Documentation

* Added `docs/TSQCA_Terminology_Guide_EN.md` — English terminology guide
* Added `docs/TSQCA_Terminology_Guide_JA.md` — Japanese terminology guide
* Added `README_JP.md` — Japanese README

---

# TSQCA 0.4.2

## Terminology Correction (Breaking Change)

### Corrected QCA Terminology for Multiple Solutions

Fixed incorrect use of "Core Conditions" terminology. The terms that appear in ALL equivalent solutions (M1, M2, M3...) are now correctly called **Essential Prime Implicants** (EPI), following standard Boolean minimization terminology.

**Changed terms:**

| Old (incorrect) | New (correct) | Meaning |
|-----------------|---------------|---------|
| `extract_mode = "core"` | `extract_mode = "essential"` | Mode for extracting shared terms |
| `core_terms` | `essential_terms` | Terms in ALL solutions |
| `peripheral_terms` | `selective_terms` | Terms in SOME solutions |
| "Core Conditions" | "Essential Prime Implicants (EPI)" | Report labels |
| "Peripheral Terms" | "Selective Prime Implicants (SPI)" | Report labels |

**Why this matters:**

The term "Core Conditions" in QCA literature (Fiss, 2011) refers to conditions appearing in **both** parsimonious AND intermediate solutions—a comparison between solution *types*. This is distinct from terms shared across multiple equivalent solutions of the *same* type, which are properly called "Essential Prime Implicants" in Boolean algebra terminology.

**Migration:**

If you used `extract_mode = "core"` in previous versions, change to `extract_mode = "essential"`. The output structure is identical; only the names have changed for methodological accuracy.

## New Features

### Configuration Chart Generator

Added functions for generating Fiss-style configuration charts (Table 5 format) commonly used in QCA publications.

**New functions:**

* `generate_config_chart()` — Generate configuration chart from QCA solution object
* `config_chart_from_paths()` — Generate chart from path strings (e.g., "A*B*~C")
* `config_chart_multi_solutions()` — Generate separate charts for multiple solutions

**Features:**

* Three symbol sets: `"unicode"` (● / ⊗), `"ascii"` (O / X), `"latex"` ($\bullet$ / $\otimes$)
* Automatic condition extraction from solution paths
* Optional metrics rows (Consistency, Coverage, Unique Coverage)
* Bilingual labels (English / Japanese)
* Markdown table output for easy integration with reports

**Example:**

```r
# From QCA solution object
chart <- generate_config_chart(sol, symbol_set = "unicode")
cat(chart)

# From path strings
paths <- c("A*B*~C", "A*D")
chart <- config_chart_from_paths(paths)
cat(chart)
```

---

# TSQCA 0.4.1

## Bug Fixes

### Report Generation Improvements
* Fixed empty "Detailed Results" and "Cross-Threshold Comparison" sections for large threshold sweeps
* Added threshold limit (27 combinations) for detailed output in reports
  - When threshold combinations exceed 27, detailed per-threshold results are omitted with explanatory message
  - Users are directed to access details programmatically via `result$details`
* Affected functions: `generate_report()` for dtSweep and ctSweepM results

---

# TSQCA 0.4.0

## New Features

### S3 Methods
* Added S3 class system for all sweep function results
  - Class hierarchy: `otSweep_result`, `dtSweep_result`, `ctSweepS_result`, `ctSweepM_result` inherit from `tsqca_result`
* Added `print()` methods for all result types
  - Displays analysis overview: outcome, conditions, thresholds swept
  - Shows summary statistics: valid solutions, no solution, multiple solutions
* Added `summary()` methods for all result types
  - Displays analysis parameters and full results table
  - Notes when multiple solutions exist

## Changes

### Backward Compatibility
* All existing workflows continue to work unchanged
* Direct access to `$summary`, `$details`, and `$params` components still works
* When `return_details = FALSE`, returns plain data.frame without S3 class

---

# TSQCA 0.3.0

## New Features

### QCA-Compatible Argument Names
* Renamed `Yvar` to `outcome` and `Xvars` to `conditions` in all sweep functions
  - Follows QCA package naming conventions for consistency
  - Old argument names (`Yvar`, `Xvars`) are still supported with deprecation warnings

### Negated Outcome Support
* Added support for negated outcomes using tilde prefix (e.g., `outcome = "~Y"`)
  - Analyzes conditions sufficient for the absence of the outcome (Y < threshold)
  - Follows QCA package's `truthTable()` convention for negation
  - Works with all sweep functions: `otSweep()`, `dtSweep()`, `ctSweepS()`, `ctSweepM()`

### Enhanced Report Generation
* `generate_report()` now displays "(negated)" indicator when analyzing negated outcomes
* Supports both old and new parameter names for backward compatibility

## Changes

### Argument Names (Backward Compatible)
* `Yvar` → `outcome` (recommended)
* `Xvars` → `conditions` (recommended)
* Using old argument names will trigger a deprecation warning but will continue to work

### Parameter Storage
* `$params` now includes:
  - `outcome`: New argument name (also stores `~Y` notation if negated)
  - `conditions`: New argument name
  - `negate_outcome`: Boolean indicating if outcome was negated

## Migration Guide

```r
# Old syntax (still works, but shows deprecation warning)
result <- otSweep(dat, Yvar = "Y", Xvars = c("X1", "X2"), ...)

# New syntax (recommended)
result <- otSweep(dat, outcome = "Y", conditions = c("X1", "X2"), ...)

# Negated outcome (new feature)
result <- otSweep(dat, outcome = "~Y", conditions = c("X1", "X2"), ...)
```

---

# TSQCA 0.2.0

## New Features

### Multiple Solution Handling
* Added `extract_mode` parameter to all sweep functions (`otSweep()`, `dtSweep()`, `ctSweepS()`, `ctSweepM()`) with three options:
  - `"first"` (default): Returns only the first solution (M1), maintaining backward compatibility
  - `"all"`: Returns all intermediate solutions concatenated (e.g., "M1: A*B; M2: A*C")
  - `"essential"`: Returns essential prime implicants common to all solutions, plus peripheral and unique terms

* Added `get_n_solutions()` helper function to count the number of intermediate solutions

### Report Generation
* Added `generate_report()` function for automatic markdown report generation with two formats:
  - `"full"`: Comprehensive report including all analysis details, solution formulas, and fit measures
  - `"simple"`: Condensed format designed for journal manuscript supplementary materials

### Reproducibility
* All sweep functions now return analysis parameters in `$params` for full reproducibility
* Parameters include: variable names, thresholds, QCA settings (`incl.cut`, `n.cut`, `pri.cut`, `dir.exp`, `include`)

## Changes

### Default Value Updates
* Changed `return_details` default from `FALSE` to `TRUE` for better integration with `generate_report()`
* Changed `n.cut` default from `2` to `1` to align with QCA package conventions
* Changed `pri.cut` default from `0.5` to `0` to align with QCA package conventions

### Output Structure
* When `return_details = TRUE`, results are now accessed via `$summary` (e.g., `result$summary$expression`)
* Added `n_solutions` column when using `extract_mode = "all"` or `"essential"`
* Added `selective_terms` and `unique_terms` columns when using `extract_mode = "essential"`

## Documentation

* Updated README with new features section and usage examples
* Added new vignette sections:
  - "Handling Multiple Solutions" explaining essential vs. selective prime implicants
  - "Generating Reports" with workflow examples
  - "Best Practices" including computational complexity guidance
* Updated all code examples to reflect new default values and output structure

---
 
# TSQCA 0.1.2

* Initial release for paper submission
* Implemented four threshold sweep methods:
  - `ctSweepS()`: Single-condition X sweep (CTS-QCA)
  - `ctSweepM()`: Multi-condition X sweep (MCTS-QCA)
  - `otSweep()`: Outcome Y sweep (OTS-QCA)
  - `dtSweep()`: Two-dimensional X and Y sweep (DTS-QCA)
* Core QCA functions: `qca_bin()`, `qca_extract()`
* Sample dataset included

---

# TSQCA 0.1.1

* Bug fixes and documentation improvements

---

# TSQCA 0.1.0

* Initial development version
