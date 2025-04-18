# brachypode-r

This is the [R](https://www.r-project.org/) code used to perform the analyses of the [brachypode](https://github.com/rscherrer/brachypode) project.

## Description

The code base consists in a series of [RStudio](https://posit.co/download/rstudio-desktop/) projects, each running a specific analysis and numbered according to the order in which each analysis should be run (some analyses may produce data used as output in subsequent analyses). Each project contains scripts whose names are numbered, again, according to the order they should be run for full reproducibility. The `functions` folder contains code used across analyses. Certain project folders may contain their own local `functions` folder, for code that only they need. The simulation data are assumed to be in a `data/` folder, placed in the root of this repository, but not uploaded to GitHub (the data will be made available upon publication). Some projects possess their own local `data/` folder, to store data (in `.rds` files) generated during the analysis.