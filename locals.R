## This script loads local functions. It shoud be run from within
## an analysis project folder. It will not check that the required
## packages are loaded. It is assumed that a local functions/ folder
## is present in the project.

# Load all the local functions
for (f in list.files("./functions", full.names = TRUE)) source(f)