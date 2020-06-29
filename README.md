# Antibiotic resistance spillover

[![DOI](https://zenodo.org/badge/142627876.svg)](https://zenodo.org/badge/latestdoi/142627876)

Code for "The role of 'spillover' in outpatient antibiotic resistance:
simulations and observations of US states and European countries" (preprint DOI [10.1101/536714](https://doi.org/10.1101/536714))

## Getting started

The code run using R (developed against version 3.6.0) and a number of packages
(see conda setup below).

[Snakemake](https://snakemake.readthedocs.io/en/stable/) is used for
convenience of executing the scripts but is not strictly necessary.

You can use [Conda](https://docs.conda.io/en/latest) to create a compute
environment:

```
conda create --name spillover r-base=3.6 python=3.6
conda activate spillover
python3 -m pip install snakemake
R -e "install.packages(scan('r-packages.txt', what=character()))"
```

## File structure

There are two directories with input empirical data:

- `data/` has antibiotic use and resistance data
- `db/` has adjacency and transportation flow data

This directory has the scripts required to re-run the analyses:

- `whn_sims.R` and `dtypes_sims.R` run the 2-population simulations and
  produced cached output, using `utils.R` as common resources
- `2pop_figs.R` and `tables.R` create figures and tables using the cached
  simulations
- `empirical_analysis.R` runs the analyses of US states and European countries

And there are 3 directories that hold the outputs:

- `cache/` holds cached simulation results
- `fig/` holds the output figures
- `results/` holds the output tables

## Output

- `fig/whn_2pop.pdf` and `dtypes_2pop.pdf` show the simulation results visually
- `results/2pop_reduction.tsv` shows the theoretical results in a table
- `fig/cross_sectional.pdf` shows the observational data before any analysis
- `fig/adjacency_plot.pdf` and `results/adjacency-results.tsv` show the
  adjacency analysis
- `fig/interaction_plot.pdf`, `fig/interactions_histogram.pdf`,
  `results/mantel-results.tsv` and `results/tile-table.tsv` show the
transportation analysis

## Author

Scott Olesen <olesen@hsph.harvard.edu>
