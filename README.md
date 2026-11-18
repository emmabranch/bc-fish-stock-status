# BC fish stock status: an exploratory rapid assessment toolkit

**Live site:** https://emmabranch.github.io/bc-fish-stock-status/

An exploratory, open-data project that compiles the status of ten British Columbia fish and shellfish stocks into a documented database, and produces standardised figures, a parameterised 2-page report for each stock, and a website.

> **Disclaimer.** This is a personal learning project. It is not an official stock assessment, it does not reproduce the methodology of any seafood recommendation program, and it makes no recommendations about what to buy or eat. Assessment time series end between 2018 and 2021 and may lag DFO's latest advice.

## What is in it

- **A small relational database** (`data/processed/`): one row per stock (`stocks.csv`), long-format time series (`timeseries.csv`), BC landings by species group (`landings.csv`), and a data dictionary covering every field (`data_dictionary.csv`).
- **A common indicator**: biomass divided by each stock's limit reference point (LRP), so stocks with different reference points can be compared. Values below 1 are below the limit.
- **Rule-based data gaps**: each stock is assigned a data tier (A to D) and a list of caveats generated from simple written rules (for example, "time series ends in 2018; the DFO zone is from 2024").
- **Cross-checks**: herring limits derived from RAM Legacy are compared with the values reported in DFO's 2024 survey (differences of 0.2% to 11.8%), and each stock's latest indicator is compared with its DFO 2024 status zone (7 of 7 consistent).
- **Figures**: biomass "stripes", Kobe plots (static and animated), and trend panels.
- **Reports**: a parameterised Quarto template (`reports/rapid_assessment.qmd`) that renders a 2-page PDF for any stock.
- **Website**: an interactive map with stock cards, an Explore page, a scrolling story, and a Methods and data page.

## Stocks and data tiers

| Tier | Meaning | Stocks |
|:--|:--|:--|
| A | Biomass and fishing pressure relative to MSY | Sablefish; Pacific halibut |
| B | Biomass relative to MSY only | Pacific cod (5ABCD) |
| C | Biomass and catch trends, no MSY ratios | Pacific herring: Strait of Georgia, West Coast of Vancouver Island, Central Coast, Prince Rupert District, Haida Gwaii |
| D | DFO status zone only, no time series compiled | Spot prawn; Dungeness crab |

## Data sources

| Source | Used for | Version | Licence |
|:--|:--|:--|:--|
| DFO Sustainability Survey for Fisheries | Status zones, reference point descriptions, management plans | 2024 results | Open Government Licence, Canada |
| RAM Legacy Stock Assessment Database | Assessment time series and reference points | v4.66, assessment-only | CC BY 4.0 |
| DFO Commercial Seafisheries Landings | Landings by species group (background only) | 1990 to 2024 | Open Government Licence, Canada |

Raw files are not committed. `R/01_download.R` downloads them into `data/raw/`.

## How to reproduce

Requires R (4.3 or later), Quarto (1.4 or later), and the packages installed by `R/00_setup.R`. From the project root, run the scripts in order:

1. `R/00_setup.R`: folders and packages
2. `R/01_download.R`: download raw data
3. `R/04_build_database.R`: build the tidy tables and data dictionary
4. `R/06_harmonise_lrp.R`: add the biomass-to-limit indicator and cross-checks
5. `R/07_make_figures.R`: figures
6. `R/08_map_cards.R`: standalone map and card preview
7. `R/09_render_reports.R`: one PDF per stock in `reports/output/`
8. `R/10_make_site_files.R` and `R/11_make_banner.R`: stock pages, card styles, and banner
9. `quarto render`: builds the website into `docs/`

Scripts `02_inspect.R` and `03_explore_stocks.R` are exploratory checks and are not needed to rebuild the outputs.

## Repository structure

```
R/                 analysis scripts and shared functions
data/processed/    tidy tables and data dictionary (committed)
data/raw/          downloaded source files (not committed)
figures/           saved figures
reports/           parameterised report template and PDF outputs
stocks/            one website page per stock (generated)
docs/              rendered website (served by GitHub Pages)
```

## Limitations

- RAM Legacy copies published assessments, so time series end between 2018 and 2021.
- Limit reference points used for the common indicator are approximate; some are derived rather than official.
- Point estimates only; assessment uncertainty is not shown.
- Map pins mark general areas, not stock boundaries.
- National landings data group several BC species together and are not used for stock-level conclusions.

## About

Built by Emma Branch, an epidemiologist and data analyst who is new to fisheries science, as a way to learn the field's core concepts through its public data. Corrections are welcome through GitHub issues.

## Citation for data

- Fisheries and Oceans Canada. Sustainability Survey for Fisheries, 2024 results. Open Government Portal.
- RAM Legacy Stock Assessment Database (2024). Version 4.66. Zenodo.
- Ricard, D., Minto, C., Jensen, O.P. and Baum, J.K. (2012). Evaluating the knowledge base and status of commercially exploited marine species with the RAM Legacy Stock Assessment Database. *Fish and Fisheries* 13(4): 380 to 398.
