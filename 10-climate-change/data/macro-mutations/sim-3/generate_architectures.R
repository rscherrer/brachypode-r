# Script to generate a custom genetic architecture
n <- 100
locations <- sort(runif(n, 0, 1))
effects <- rep(0.1, n)
ii <- sample(n, 10, replace = FALSE)
effects[ii] <- 5
file <- "architecture.txt"
cat("1", file = file, sep = "\n") # number of chromosomes
cat("1", file = file, sep = "\n", append = TRUE) # chromsome ends
cat(n, file = file, sep = "\n", append = TRUE) # number of loci
cat(locations, file = file, sep = "\n", append = TRUE) # locus positions
cat(effects, file = file, sep = "\n", append = TRUE) # locus effect sizes
