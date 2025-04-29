## This script sources all the available functions. It does not
## make sure that the required packages are loaded. It should
## be run from within the various project folders.

# Load all the global functions
for (f in list.files("../functions", pattern = "\\.R$", full.names = TRUE, recursive = TRUE)) source(f)
