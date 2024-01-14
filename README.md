# Materials for "Quantifying competition between two demersal fish species from spatiotemporal stomach content data"

This repository contains all the materials needed to reproduce the manuscript "Quantifying competition between two demersal fish species
from spatiotemporal stomach content data" by Max Lindmark, Federico Maioli, Sean C. Anderson, Mayya Gogina, Mattias Sköld, Valerio Bartolino, Mikael Ohlsson, Anna Eklöf and Michele Casini.


## Setup

To simply view the analyses, download the .qmd or .html files. To reproduce the results:

1. Fork the repository (or download a local version)

2. Open a new RStudio project and clone the fork (or open R and set your working directory of the cloned repository)

3. R-package dependencies and versions are handled with [`renv`](https://rstudio.github.io/renv/articles/renv.html). Simply run `renv::restore()` to install the correct versions of all the packages needed to replicate our results. Packages are installed in a stand-alone project library for this paper, and will not affect your installed R packages anywhere else. 

Once you've successfully run `renv::restore()` you can reproduce our results by running R/main-analysis/food_competition.qmd and R/main-analysis/diet_overlap.qmd.

Scripts for data preparation are found in prepare-data. Some files (e.g., environmental data) are too big for github, so currently only the cleaned and processed data are included here, which are formatted for this specific analysis. You can either view the .html files to see how raw data were handled or reach out to me to get the data needed for the scripts to run.