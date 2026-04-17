# Public Institutions Flagship Report — Reproducibility Package

This repository contains all the analysis produced for the Public Institutions (PI) Flagship Report. It is structured to adhere to the [World Bank Reproducible Research Repository guidelines](https://worldbank.github.io/wb-reproducible-research-repository/guidance_note_wb.html).

## Contents

1. [Overview](#overview)
2. [Data Availability](#data-availability)
3. [Instructions for Replicators](#instructions-for-replicators)
4. [List of Exhibits](#list-of-exhibits)
5. [Requirements](#requirements)
6. [Code Description](#code-description)
7. [Folder Structure](#folder-structure)

---

## Overview

This reproducibility package contains all code and documentation necessary to replicate the analysis in the Public Institutions Flagship Report. The package includes data-cleaning scripts, analysis scripts, and the code required to produce all tables and figures presented in the report.

Replicators should begin by reviewing the [Requirements](#requirements) section to ensure all necessary software and dependencies are installed, then follow the step-by-step instructions in [Instructions for Replicators](#instructions-for-replicators).

---

## Data Availability

- [ ] All data are publicly available.
- [ ] Some data cannot be made publicly available.
- [ ] No data can be made publicly available.

### Data Sources

Detailed information on each dataset used in the analysis is provided below. Fill in the information for each data file used:

- **Filename:** *Exact file name as shown on the source website*
- **Source:** *Name of the source website*
- **URL:** *Exact downloadable URL of the data used*
- **Access year:** *Date when the data was accessed*
- **Variable names (optional):** *Variable names used, if only a subset was downloaded*
- **License (optional):** *License under which the data is available*

### Statement about Rights

- [ ] I certify that the author(s) of the manuscript have legitimate access to and permission to use the data used in this manuscript.
- [ ] I certify that the author(s) of the manuscript have documented permission to redistribute/publish the data contained within this replication package. Appropriate permissions are documented in the LICENSE file.

---

## Instructions for Replicators

New users should follow these steps to run the package successfully:

1. Ensure all required software and dependencies are installed as listed in the [Requirements](#requirements) section.
2. If data files are not included in the reproducibility package, download them from the links provided in the [Data Sources](#data-sources) section and place them in the `data/` folder.
3. Update the root directory path in the main script:
   - Open `code/main.R` and set the `root_dir` variable (or equivalent) at the top of the file to point to your local copy of the repository.
4. Run the main script to execute all analysis steps in order:
   ```r
   source("code/main.R")
   ```

---

## List of Exhibits

The provided code reproduces:

- [ ] All numbers provided in text in the paper
- [ ] All tables and figures in the paper
- [ ] Selected tables and figures in the paper, as explained and justified below

| Exhibit name | Output filename | Script | Note |
|--------------|-----------------|--------|------|
| *Table 1*    | *table1.xlsx*   | *02_analysis.R (line X)* | *Found in outputs/tables/* |
| *Figure 1*   | *figure1.png*   | *02_analysis.R (line Y)* | *Found in outputs/figures/* |

---

## Requirements

### Computational Requirements

The analysis was developed and tested on the following system:

- **Operating System:** *e.g., Windows 10 / macOS 13 / Ubuntu 22.04*
- **Processor:** *e.g., Intel Core i7*
- **RAM:** *e.g., 16 GB*

### Software Requirements

- **R** *(version X.X.X)*
  - `tidyverse` *(version X.X.X)*
  - `haven` *(version X.X.X)*
  - `readxl` *(version X.X.X)*
  - *(add additional packages as needed)*

### Memory, Runtime, and Storage Requirements

- **Approximate runtime:** *e.g., X minutes on the above system*
- **Storage required:** *e.g., X GB (including raw and processed data)*

---

## Code Description

> **Note:** The script names below are illustrative placeholders. Update them to reflect the actual files in this repository.

The repository contains the following scripts:

- **`code/main.R`** — Sets file paths, installs required packages, and executes all other scripts in order.
- **`code/01_cleaning.R`** — Loads raw data, handles missing values, and saves cleaned datasets.
- **`code/02_analysis.R`** — Performs statistical analysis and generates all tables and figures.

Replicators can reproduce all results with a single command by running `main.R`.

---

## Folder Structure

```
pi-flagship-report/
├── code/
│   ├── main.R
│   ├── 01_cleaning.R
│   └── 02_analysis.R
├── data/
│   ├── raw/
│   └── clean/
├── outputs/
│   ├── figures/
│   └── tables/
├── LICENSE
└── README.md
```
