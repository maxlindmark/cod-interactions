# Materials for "Quantifying competition between two demersal fish species from spatiotemporal stomach content data"

This repository contains all the materials needed to reproduce the analysis in manuscript "Quantifying competition between two demersal fish species
from spatiotemporal stomach content data" by Max Lindmark, Federico Maioli, Sean C. Anderson, Mayya Gogina, Mattias Sköld, Valerio Bartolino, Mikael Ohlsson, Anna Eklöf and Michele Casini.

## Setup

To simply view the analyses, download the .qmd or .html files. To reproduce the results, download a zip of the repository and work locally within the RStudio project or just with R setting the project folder as your working directory. Alternatively, you can:

1. Fork the repository on GitHub

2. Create a new RStudio project and clone your fork

R-package dependencies and versions are handled with [`renv`](https://rstudio.github.io/renv/articles/renv.html). Simply run `renv::restore()` to install the correct versions of all the packages needed to replicate our results. Packages are installed in a stand-alone project library for this paper, and will not affect your installed R packages anywhere else. 

Once you've successfully run `renv::restore()` you can reproduce our results by running R/main-analysis/food_competition.qmd and R/main-analysis/diet_overlap.qmd. You don't need to run the data preparation scripts but can jump straight to these since a trimmed and cleaned data ready for analysis are included in this repository (data/clean). 

Scripts for data preparation are found in prepare-data. This repository contains all raw files needed except ERSST data because it's too big. The code to download it again is in the script though. You can also jump straight to the analysis-scripts using data in data/for-analysis, where output from data preparation scripts are stored.