#
# 
# Author: matthewkrachey
###############################################################################

library(Gilliam); 
source("~/Documents/workspace/Gilliam/GilliamFunctions.R")
library(stringr);library(functional)

pastes = Curry(paste, sep="")
run.simout = Curry(run.sim, niter1 = 100000)
Surv2

chainnumb = pastes('FixK', 1:8)

library(multicore)

mclapply(chainnumb, run.simout)

