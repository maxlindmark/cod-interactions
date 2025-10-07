# Quantifying food competition between two demersal fish species from spatiotemporal stomach content data

This repository contains all the materials needed to reproduce the analysis in manuscript "Quantifying competition between two demersal fish species
from spatiotemporal stomach content data" by Max Lindmark, Federico Maioli, Sean C. Anderson, Mayya Gogina, Mattias Sköld, Valerio Bartolino, Mikael Ohlsson, Anna Eklöf and Michele Casini.

### Reproducing Results

To reproduce our results you can either:

1. Fork the repository, clone it, open a new RStudio project with version control, and paste the repo url

2. Download a zip and work locally on your computer

We use [`renv`](https://rstudio.github.io/renv/articles/renv.html) to manage package versions. Once you've downloaded the project, run `renv::restore()` in your current working directory. This will install the package versions we used when this repository was archived. Note that packages are installed in a stand-alone project library for this paper, and will not affect your installed R packages anywhere else! `renv` does *not* help with different versions of R. We used R version 4.3.2, and ran the analysis on a 24 GB Apple M2 Sequoia 15.6.1 laptop.

### Repository structure

`R`: code to prepare data, run models and make figures.

`data`: trawl survey & stomach content data.

`figures`: figures for paper are saved here
