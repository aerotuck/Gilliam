# TODO: Add comment
# 
# Author: matthewkrachey
###############################################################################

library(Gilliam); 
library(stringr);library(functional)
NPopEst =function(b00, b0l, g00, g0l, csi, Nobserved, Times).Call("NPopEst",b00, b0l, g00, g0l, csi, Nobserved, Times,package="Gilliam")
pastes = Curry(paste, sep="")
setwd("~/Dropbox/Research/Consulting/Gilliam/EdWork/Samples/")

###  Read in some test data.
cc1 <- as.matrix(read.csv("~/Dropbox/Research/Consulting/Gilliam/EdWork/cc.csv", header=FALSE))
ci1 <- as.matrix(read.csv("~/Dropbox/Research/Consulting/Gilliam/EdWork/ci.csv", header=FALSE))
lc1 <- as.matrix(read.csv("~/Dropbox/Research/Consulting/Gilliam/EdWork/lc.csv", header=FALSE))
li1 <- as.matrix(read.csv("~/Dropbox/Research/Consulting/Gilliam/EdWork/li.csv", header=FALSE))
ti1 <- as.matrix(read.csv("~/Dropbox/Research/Consulting/Gilliam/EdWork/ti.csv", header=FALSE))
tc1 <- as.matrix(read.csv("~/Dropbox/Research/Consulting/Gilliam/EdWork/tc.csv", header=FALSE))
uc1 <- as.matrix(read.csv("~/Dropbox/Research/Consulting/Gilliam/EdWork/uc.csv", header=FALSE))
ui1 <- as.matrix(read.csv("~/Dropbox/Research/Consulting/Gilliam/EdWork/ui.csv", header=FALSE))

## Set Times
citime1 <- c(245,308,366,430,492,554,611,638,652,708,796,835,863,983,1019,1058)/1058 
cctime1 <- c(611,638,652,708,796,835,863,983,1019,1058)/1058
litime1 <- c(0,65,128,185,245,308,366,430,492,554,611,638,652,708,796,835,863,983,1019,1058)/1058
lctime1 <- c(0,65,128,185,245,308,366,430,492,554,611,638,652,708,796,835,863,983,1019,1058)/1058
titime1 <- c(245,308,366,430,491,554,611,638,652,708,796,835,863,983,1019,1058)/1058
tctime1 <- c(245,611,638,652,708,796,835,863,983,1019,1058)/1058
uctime1 <- c(0,65,128,185,245,308,366,430,492,554,611,652,708,796,835,863,983,1019,1058)/1058
uitime1 <- c(0,65,128,185,245,308,366,430,492,554,611,652,708,796,835,863,983,1019,1058)/1058

files = list.files()
files2 = files[str_detect(files, "FixK")]
files2 = na.omit(files2[str_detect(files2, "Pop")==FALSE])

parameters = c("b00","bl0","g00","gl0","csi1")
reader = function(param,n,col) read.csv(pastes("Mod1.sample.",param,".chain.FixK",n,".csv"))[,col]
reader11 = Curry(reader, n=1, col=1)
trial.data = NPopEst(reader11("b00"), reader11("bl0"), reader11("g00"), reader11("gl0"), reader11("csi1"), nrow(cc1), cctime1)

