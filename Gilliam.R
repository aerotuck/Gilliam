#
# 
# Author: matthewkrachey
###############################################################################

library(Gilliam); 
source("~/Documents/workspace/Gilliam/GilliamFunctions.R")
library(stringr);library(functional)

##################################################################################
##################################################################################
pastes = Curry(paste, sep="")
run.simout = Curry(run.sim, niter1 = 100000)
run.simstart = Curry(run.sim, niter1 = 20000, readstart1=FALSE, readstep1 = FALSE)
chainnumb = pastes('FixKFull', 1:8)

##################################################################################
##################################################################################
library(multicore)
mclapply(chainnumb, run.simstart)
mclapply(chainnumb, run.simout)

##################################################################################
##################################################################################
run.simout1 = Curry(run.sim, niter1 = 10000)
for(i in 1:10){ 
	mclapply(chainnumb, run.simout)
cat("Done with iteration :", i ,fill=TRUE)
}

##################################################################################
##################################################################################
mclapply(chainnumb, run.simout)
mclapply(chainnumb, Ncalc.mcapply )