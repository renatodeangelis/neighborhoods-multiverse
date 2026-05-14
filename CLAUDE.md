# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an R-based research project analyzing neighborhood effects using multiverse analysis. It is a co-authored project currently in the pre-analysis phase. The goal is to estimate the sensitivity of neighborhood effect estimates across a range of reasonable analytic choices (a "multiverse" of specifications).

## Data Storage

Data does **not** live in this repository:

| Data type | Location |
|-----------|----------|
| Non-sensitive data | Dropbox |
| Sensitive / PII data | Institutional Box only — never accessed through Claude |

Scripts should use relative paths or clearly documented path variables pointing to Dropbox/Box locations. Never hard-code absolute paths to data.

## R Conventions

- Use `tidyverse` style throughout (pipes, `dplyr`, `ggplot2`, `tidyr`)
- Use `=` for assignment, `|>` for pipes
- Tables and formatted output via `modelsummary`, `gt`, or `kableExtra`
- Keep data cleaning, analysis, and output scripts separate
- Use `here::here()` for file paths within the project

## Anticipated Project Structure

```
R/            # Analysis and helper scripts
data/         # Placeholder only; actual data lives in Dropbox/Box
output/
  figures/
  tables/
docs/         # Pre-analysis plan, notes, documentation
```

This structure may not exist yet — the project is in early setup. Add folders as needed.

## Multiverse Analysis Notes

The core analytic approach involves specifying a grid of defensible modeling choices (e.g., sample restrictions, covariate sets, outcome definitions, estimators) and running all combinations. Key design considerations:

- Define the multiverse specification grid early and document each dimension
- Scripts that run the full multiverse may be slow; consider checkpointing results
- Summary figures typically show coefficient distributions across specifications (e.g., specification curves)