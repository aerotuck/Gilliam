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
run.simstart = Curry(run.sim, niter1 = 10000, readstart1=FALSE, readstep1 = FALSE)
chainnumb = pastes('FixKFull', 1:6)


##################################################################################
##################################################################################
library(multicore)
mclapply(chainnumb, run.simstart)
#mclapply(chainnumb, run.simout)

##################################################################################
##################################################################################
run.simout1 = Curry(run.sim, niter1 = 10000)
for(i in 1:15){ 
	mclapply(chainnumb, run.simout1)
cat("Done with iteration :", i ,fill=TRUE)
}

##################################################################################
##################################################################################
mclapply(chainnumb, run.simout)
mclapply(chainnumb, Ncalc.mcapply)

setwd("~/Dropbox/Research/Consulting/Gilliam/EdWork/Samples/")
files = list.files()
files = files[str_detect(files, "FixKFull")]
lenuni = function(x) length(unique(x))
for(i in 1:length(files)){
	
	print(files[i])
	temp= read.csv(files[i])
	print(apply(temp,2,lenuni))
	print(apply(temp,2,mean))
	cat("----------------------------------------------",fill=T)
}



