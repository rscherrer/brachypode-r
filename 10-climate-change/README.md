## climate-change

Here we plot the results of our climate change experiment --- different simulations according to different climate change scenarios. The simulated data are assumed to be in the `../data/climate-change/` folder, which itself contains three folders:

* `standard/` contains the main sets of simulations run for the experiment 
* `long/` contains additional simulations that were run for a longer time
* `highmut/` contains simulations with varying mutation rates

Within each of these folders, simulations are grouped by the climate change scenario they were run under, in folders with descriptive names. Note that in the case of the `macro-mutations/` scenario, the external genetic architecture files loaded by the program during the simulations (and specifying some loci as having higher effect sizes than the rest) were generated a priori by running the R script `generate_architecture.R` located in `../data/climate-change/`.