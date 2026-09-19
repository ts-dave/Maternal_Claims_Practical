# Maternal Claims Analysis

This project cleans, standardizes, and audits maternal claims data for downstream analysis of patient journeys, facility patterns, and data quality issues.

## Project goal

The workflow is designed to:

- standardize raw column names
- normalize key text fields such as facility, district, type, and ownership
- parse and validate attendance dates
- check missingness and identify incomplete records
- create an analysis-ready claims dataset for further exploration

## Data

The source data are stored in the raw data folder:

- [data/raw/Final Final Data.csv](data/raw/Final Final Data.csv)

This file contains the original claims records used throughout the project.

## Repository structure

- [data/](data/) — raw and processed datasets
- [scripts/data_cleaning.R](scripts/data_cleaning.R) — main cleaning and validation script
- [output/](output/) — generated summaries and analysis outputs
- [reports/](reports/) — report files and documentation
- [.gitignore](.gitignore) — project-level ignore rules for local artifacts
- [README.md](README.md) — project overview and workflow

## Setup

This project uses R and the following packages:

```r
install.packages(c("tidyverse", "janitor", "lubridate"))
```

## Workflow

The main script performs the following steps:

1. Reads the raw claims dataset
2. Inspects dataset structure and missing values
3. Identifies a sample patient record using `NHISID`
4. Cleans column names with `janitor::clean_names()`
5. Standardizes key text fields with `str_squish()` and case normalization
6. Parses attendance dates using `lubridate::dmy()`
7. Audits rows with missing or invalid date values
8. Summarizes missingness across core variables

## Key files

- [scripts/data_cleaning.R](scripts/data_cleaning.R) — cleaning and validation workflow

## Notes

- Keep the raw dataset in [data/raw/](data/raw/) and avoid overwriting it.
- Use project-relative paths so the workflow is reproducible from any machine.
- Prefer rerunning the cleaning script from the project root rather than editing the raw file directly.
- Review missingness checks before building downstream analyses.

## Status

This project is in the data cleaning and validation stage.
