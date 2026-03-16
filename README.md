# Prosocial Effort Task — Data Cleaning Pipeline

**FONDECYT David Huepe**
Diego Garrido · José Borquez
Viña del Mar, 2025

---

## Overview

This repository contains the data cleaning and preprocessing pipeline for a prosocial effort task, where participants decided whether to exert physical effort to earn credits for themselves (SELF) or for another person (OTHER). The task includes 48 experimental trials (24 SELF + 24 OTHER) and 1 attention check, with varying levels of reward (2, 6, 10 credits), difficulty (easy, hard), and effort (50%, 65%, 80%, 95%).

## Script

**`Script_Limpieza.R`** processes the raw Qualtrics export through five sequential stages, producing progressively refined datasets.

### Requirements

- R ≥ 4.0
- Packages: `readxl`, `tidyverse`

### Input

| File | Description |
|------|-------------|
| `Prosocial_effort_task_trini.xlsx` | Raw Qualtrics export with dual-row headers |

### Output pipeline

The script generates the following files in order:

| # | File | Format | Description |
|---|------|--------|-------------|
| 1 | `datos_limpios.csv` | Wide | Initial cleanup: resolved dual headers, removed practice/box trials, standardized IDs, assigned group labels |
| 2 | `datos_clean.csv` | Wide | Restructured columns separating condition, reward, difficulty, and effort per trial. More readable than `datos_limpios.csv` |
| 3 | `datos_final.csv` | Wide | Adds computed proportions: work rates (raw and adjusted), omission rates, and failure rates for SELF and OTHER conditions |
| 4 | `datos_long_models.csv` | Long | One row per participant × trial (48 rows per subject). Variables recoded for computational modeling (`decision`, `reward`, `effort`, `agent`, `success`, `grupo`) |
| 5 | `datos_analisis.csv` | Wide | Model-free proportions of accepting work by reward level, effort level, and agent (SELF/OTHER) |
| 6 | `datos_analisis_v2.csv` | Wide | Same as above, plus the proportion columns from `datos_final.csv` (work rates, omission rates, failure rates) |

### Variable coding (long format)

| Variable | Values |
|----------|--------|
| `decision` | 1 = work, 0 = rest, 2 = omission |
| `reward` | 1 = low (2 credits), 2 = medium (6), 3 = high (10) |
| `effort` | 1 = 50%, 2 = 65%, 3 = 80%, 4 = 95% |
| `agent` | 0 = SELF, 1 = OTHER |
| `success` | 0 = fail, 1 = success |
| `grupo` | 0 = Control, 1 = Experimental |
