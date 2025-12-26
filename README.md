# brachypode-r

This is the [R](https://www.r-project.org/) code used to perform the analyses of the [brachypode](https://github.com/rscherrer/brachypode) project.

## Description

The code base consists in a series of [RStudio](https://posit.co/download/rstudio-desktop/) projects, each running a specific analysis and numbered according to the order in which each analysis should be run (some analyses may produce data used as output in subsequent analyses). Each project contains scripts whose names are numbered, again, according to the order they should be run for full reproducibility. The `functions/` folder contains code used across analyses. Certain project folders may contain their own local `functions/` folder, for code that only they need. The simulation data are assumed to be in a `data/` folder, placed in the root of this repository, but not uploaded to GitHub (the data will be made available upon publication). Some projects have their own local `data/` folder, to store intermediate files (in `.rds` files) generated during the analysis.

## Requirements

Analyses were run in [R](https://www.r-project.org/) version 4.2.2.

The following [CRAN](https://cran.r-project.org/) packages were used:

* [ggbeeswarm](https://cran.r-project.org/web/packages/ggbeeswarm/index.html) 0.7.2
* [ggh4x](https://teunbrand.github.io/ggh4x/) 0.3.0
* [ggnewscale](https://eliocamp.github.io/ggnewscale/) 0.5,1
* [ggridges](https://cran.r-project.org/web/packages/ggridges/index.html) 0.5.6
* [patchwork](https://patchwork.data-imaginist.com/) 1.2.0
* [pracma](https://cran.r-project.org/web/packages/pracma/index.html) 2.4.4
* [Rcpp](https://cran.r-project.org/web/packages/Rcpp/index.html) 1.0.12
* [rlang](https://rlang.r-lib.org/) 1.1.5
* [scales](https://scales.r-lib.org/) 1.3.0
* [scatterpie](https://cran.r-project.org/web/packages/scatterpie/index.html) 0.2.4
* [tidyverse](https://www.tidyverse.org/) 2.0.0

In addition the following non-CRAN package was used:

* [readsim](https://github.com/rscherrer/readsim) 0.1.0

## About

This code was developed on Ubuntu Linux 24.04 LTS, in [RStudio](https://posit.co/download/rstudio-desktop/) 2024.12.1 and [Visual Studio Code](https://code.visualstudio.com/) 1.99.0 ([R extension](https://marketplace.visualstudio.com/items/?itemName=REditorSupport.r) 2.8.5), with [R](https://www.r-project.org/) 4.3.3 compiled on Linux with [GCC](https://gcc.gnu.org/) 13.2.0. For code development the packages [devtools](https://devtools.r-lib.org/) 2.4.5 and [languageserver](https://cran.r-project.org/web/packages/languageserver/index.html) 0.3.16 were used. Occasional use was made of [ChatGPT](https://chatgpt.com/) and [GitHub Copilot](https://github.com/features/copilot) in the development of this code.

## Permissions

Copyright (c) 2025 Raphaël Scherrer (open source license will be added upon publication). This code comes with no guarantee whatsoever.
