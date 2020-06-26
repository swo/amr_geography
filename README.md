# Antibiotic resistance spillover

Code for "The role of 'spillover' in outpatient antibiotic resistance:
simulations and observations of US states and European countries"

## To do

- Add Zenodo badge at top
- Figure out if want to include Python scraping scripts
- Clean up
    - one big snakemake for everything
    - data
    - db
    - analysis

## Getting started

The code run using R (developed against version 3.6.0) and packages:

- [tidyverse](https://tidyverse.tidyverse.org)
- [countrycode](https://github.com/vincentarelbundock/countrycode)
- [vegan](https://github.com/vegandevs/vegan)
- [cowplot](https://wilkelab.org/cowplot)
- [patchwork](https://github.com/thomasp85/patchwork)

[Snakemake](https://snakemake.readthedocs.io/en/stable/) is used for
convenience of executing the scripts but is not strictly necessary.

You can use [Conda](https://docs.conda.io/en/latest) to create a compute
environment:

```
conda create --name spillover r-base=3.6 python=3.6
conda activate spillover
python3 -m pip install snakemake
R -e "install.packages(c('tidyverse', 'countrycode', 'vegan', 'cowplot', 'patchwork'))"
```

## File structure


### Output


## Author

Scott Olesen <olesen@hsph.harvard.edu>
