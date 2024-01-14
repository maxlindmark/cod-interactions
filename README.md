# Materials for "Quantifying competition between two demersal fish species from spatiotemporal stomach content data"

This repository contains all the materials needed to reproduce the manuscript "Quantifying competition between two demersal fish species
from spatiotemporal stomach content data" by Max Lindmark, Federico Maioli, Sean C. Anderson, Mayya Gogina, Mattias Sköld, Valerio Bartolino, Mikael Ohlsson, Anna Eklöf and Michele Casini.




## Setup

In order to reproduce the results:

1. Fork the repository and clone to your machine (or download a local version)

2. Open R and set your working directory of the cloned repository (or just use RStudio projects)

3. This project is set up with [`renv`](https://rstudio.github.io/renv/articles/renv.html) to manage package dependencies. Inside R (and with your working directory set correctly) run `renv::restore()`. This will install the correct versions of all the packages needed to replicate our results. Packages are installed in a stand-alone project library for this paper, and will not affect your installed R packages anywhere else. 

Once you've successfully run `renv::restore()` you can reproduce our results by running R/main-analysis/food_competition.qmd and R/main-analysis/diet_overlap.qmd.