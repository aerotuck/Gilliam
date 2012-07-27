# TODO: Add comment
# 
# Author: matthewkrachey
###############################################################################
require(Gilliam)
run.sim = function(chainnumber1, readstart1=TRUE, readstep1=TRUE, savestep1 = TRUE, niter1= 1000){
	setwd("~/Dropbox/Research/Consulting/Gilliam/EdWork") 
	
	
###  Read in some test data.
cc1 <- as.matrix(read.csv("cc.csv", header=FALSE))
ci1 <- as.matrix(read.csv("ci.csv", header=FALSE))
lc1 <- as.matrix(read.csv("lc.csv", header=FALSE))
li1 <- as.matrix(read.csv("li.csv", header=FALSE))
ti1 <- as.matrix(read.csv("ti.csv", header=FALSE))
tc1 <- as.matrix(read.csv("tc.csv", header=FALSE))
uc1 <- as.matrix(read.csv("uc.csv", header=FALSE))
ui1 <- as.matrix(read.csv("ui.csv", header=FALSE))

######## Need a time vector, will call ti
citime1 <- c(245,308,366,430,492,554,611,638,652,708,796,835,863,983,1019,1058)/1058    ## I added the 492 values to make dimensions correct.  Needs checked!!!
cctime1 <- c(611,638,652,708,796,835,863,983,1019,1058)/1058
litime1 <- c(0,65,128,185,245,308,366,430,492,554,611,638,652,708,796,835,863,983,1019,1058)/1058
lctime1 <- c(0,65,128,185,245,308,366,430,492,554,611,638,652,708,796,835,863,983,1019,1058)/1058
titime1 <- c(245,308,366,430,491,554,611,638,652,708,796,835,863,983,1019,1058)/1058
tctime1 <- c(245,611,638,652,708,796,835,863,983,1019,1058)/1058
uctime1 <- c(0,65,128,185,245,308,366,430,492,554,611,652,708,796,835,863,983,1019,1058)/1058
uitime1 <- c(0,65,128,185,245,308,366,430,492,554,611,652,708,796,835,863,983,1019,1058)/1058

### Determine the numbers of rows in each data set
ccnobs1 <- nrow(cc1)
cinobs1 <- nrow(ci1)
tcnobs1 <- nrow(tc1)
tinobs1 <- nrow(ti1)
lcnobs1 <- nrow(lc1)
linobs1 <- nrow(li1)
ucnobs1 <- nrow(uc1)
uinobs1 <- nrow(ui1)

### Determine the numbers of columns in each data set
ccntimes1 <- ncol(cc1)
cintimes1 <- ncol(ci1)
tcntimes1 <- ncol(tc1)
tintimes1 <- ncol(ti1)
lcntimes1 <- ncol(lc1)
lintimes1 <- ncol(li1)
ucntimes1 <- ncol(uc1)
uintimes1 <- ncol(ui1)

if(readstart1 == TRUE){
	b00 <- as.vector(t(read.csv(paste('StartStep/Mod1.start.b00.chain.', chainnumber1, '.txt', sep=""),header=TRUE)))
	b01m <- as.vector(t(read.csv(paste('StartStep/Mod1.start.b01m.chain.', chainnumber1, '.txt', sep=""),header=TRUE)))
	b01s <- as.vector(t(read.csv(paste('StartStep/Mod1.start.b01s.chain.',  chainnumber1, '.txt', sep=""),header=TRUE)))
	bl0 <- as.vector(t(read.csv(paste('StartStep/Mod1.start.bl0.chain.', chainnumber1, '.txt', sep=""),header=TRUE)))
	bl1m <- as.vector(t(read.csv(paste('StartStep/Mod1.start.bl1m.chain.', chainnumber1, '.txt', sep=""),header=TRUE)))
	bl1s <- as.vector(t(read.csv(paste('StartStep/Mod1.start.bl1s.chain.', chainnumber1, '.txt', sep=""),header=TRUE)))
	g00 <- as.vector(t(read.csv(paste('StartStep/Mod1.start.g00.chain.', chainnumber1, '.txt', sep=""),header=TRUE)))
	gl0 <- as.vector(t(read.csv(paste('StartStep/Mod1.start.gl0.chain.', chainnumber1, '.txt', sep=""),header=TRUE)))
	#k1 <- as.vector(t(read.csv(paste('StartStep/Mod1.start.k1.chain.', chainnumber1, '.txt', sep=""),header=TRUE)))
	#k0 <- as.vector(t(read.csv(paste('StartStep/Mod1.start.k0.chain.', chainnumber1, '.txt', sep=""),header=TRUE)))
	csi1 <- as.vector(t(read.csv(paste('StartStep/Mod1.start.csi1.chain.', chainnumber1, '.txt', sep=""),header=TRUE)))
	sigma1 <- as.vector(t(read.csv(paste('StartStep/Mod1.start.sigma1.chain.', chainnumber1, '.txt', sep=""),header=TRUE)))
	g01 <- as.vector(t(read.csv(paste('StartStep/Mod1.start.g01.chain.', chainnumber1, '.txt', sep=""),header=TRUE)))
	gl1 <- as.vector(t(read.csv(paste('StartStep/Mod1.start.gl1.chain.', chainnumber1, '.txt', sep=""),header=TRUE)))
	gl1sigma <- as.vector(t(read.csv(paste('StartStep/Mod1.start.gl1sigma.chain.', chainnumber1, '.txt', sep=""),header=TRUE)))
	g01sigma <- as.vector(t(read.csv(paste('StartStep/Mod1.start.g01sigma.chain.', chainnumber1, '.txt', sep=""),header=TRUE)))
	#  k1b1 <- as.vector(t(read.csv(paste('StartStep/Mod1.start.k1b1.chain.', chainnumber1, '.txt', sep=""),header=TRUE)))
	# k1m1 <- as.vector(t(read.csv(paste('StartStep/Mod1.start.k1m1.chain.', chainnumber1, '.txt', sep=""),header=TRUE)))
	# k1s1 <- as.vector(t(read.csv(paste('StartStep/Mod1.start.k1s1.chain.', chainnumber1, '.txt', sep=""),header=TRUE)))
	#k0m1 <- as.vector(t(read.csv(paste('StartStep/Mod1.start.k0m1.chain.', chainnumber1, '.txt', sep=""),header=TRUE)))
	#k0s1 <- as.vector(t(read.csv(paste('StartStep/Mod1.start.k0s1.chain.', chainnumber1, '.txt', sep=""),header=TRUE)))
}else{
	####  Set up initial values
	b00 <- rep(0,8)
	b01m <- rep(1.00,2)
	b01s <- rep(1.0,2)
	bl0 <- rep(0,8)
	bl1m <- rep(0,2)
	bl1s <- rep(1,2)
	g00 <- rep(0,8)
	gl0 <- rep(0,8)
	k1 <- 0.00254868*1058			# Growth Rate
	k0 <- rep(0,8)
	csi1 <- .5*rep(1,8)  				# Probability of birth
	sigma1 <- rep(1,8)
	g01 <- rep(0,2)
	gl1 <- rep(0,2)
	gl1sigma <- rep(1,2)
	g01sigma <- rep(1,2) 
	k1b1 <- rep(1,2)#/0.00254868
	k1m1 <- rep(1,2)*log(0.00254868*1058)
	k1s1 <- rep(1,2)
	k0m1 <- rep(1,2)*log(0.00254868*1058)
	k0s1 <- rep(1,2)
}

k1 <- 0.00254868*1058   
L01 <- 0.0			# Lower Bound on length
Linf1 <- 75		# Upper Bound on length
#Linf1[1] <- quantile(cc1[cc1>0],0.95)
#Linf1[2] <- quantile(ci1[ci1>0],0.95)
#Linf1[3] <- quantile(lc1[lc1>0],0.95)
#Linf1[4] <- quantile(li1[li1>0],0.95)
#Linf1[5] <- quantile(tc1[tc1>0],0.95)
#Linf1[6] <- quantile(ti1[ti1>0],0.95)
#Linf1[7] <- quantile(uc1[uc1>0],0.95)
#Linf1[8] <- quantile(ui1[ui1>0],0.95)
indexc1 <- c(0,2,4,6)
indexi1 <- c(1,3,5,7)
b01priorm <- rep(0,2)
b01priors <- 1*rep(1,2)
bl1priorm <- rep(0,2)
bl1priors <- 1*rep(1,2)
k1priorm <- rep(0.002,2)
k1priors <- c(10,0.1)
k0priorm <- rep(0,2)
k0priors <- c(10,0.1)
k1a1 <- rep(1,2)
k1a2 <- rep(1,2)
k1b2 <- rep(1,2)#/0.00254868
k0 <- rep(1,8)/0.00254868

## Prior distribution values
csibeta1 <- 2.0			# Prior distribution parameters for csi1 using the beta distribution
csibeta2 <- 2.0			# Prior distribution parameters for csi1 using the beta distribution
lambda1 <- 1.0			# Prior scale for inv-chisquare distribution
df1 <- 3.0                 	# Prior degrees of freedom for the inv-chisquare distribution
gl1priorm <- rep(0.1,2)         # Prior distribution mean for the gl1 parameter >>> gl0 parameters
gl1priors <- rep(1,2)         # Prior distribution standard deviation for the gl1 parameter >>> gl0 parameters
g01priorm <- rep(0.1,2)         # Prior distribution mean for the g01 parameter >>> g00 parameters
g01priors <- rep(1,2)          # Prior distribution standard deviation for the g01 parameter >>> g00 parameters


if(readstep1 == TRUE){
	## Read in Step Values
	b00step <- as.vector(t(read.csv(paste('StartStep/Mod1.step.b00.chain.', chainnumber1, '.txt', sep=""),header=TRUE)))
	bl0step <- as.vector(t(read.csv(paste('StartStep/Mod1.step.bl0.chain.', chainnumber1, '.txt', sep=""),header=TRUE)))
	g00step <- as.vector(t(read.csv(paste('StartStep/Mod1.step.g00.chain.', chainnumber1, '.txt', sep=""),header=TRUE)))		
	gl0step <- as.vector(t(read.csv(paste('StartStep/Mod1.step.gl0.chain.', chainnumber1, '.txt', sep=""),header=TRUE)))
	k1step <- as.vector(t(read.csv(paste('StartStep/Mod1.step.k1.chain.', chainnumber1, '.txt', sep=""),header=TRUE)))
	csi1step <- as.vector(t(read.csv(paste('StartStep/Mod1.step.csi1.chain.', chainnumber1, '.txt', sep=""),header=TRUE)))
	# k0step <- as.vector(t(read.csv(paste('StartStep/Mod1.step.k0.chain.', chainnumber1, '.txt', sep=""),header=TRUE)))
}else{
	#### Set up step values if none are provided
	b00step <- rep(1,8)*0.001  # Previously .0001
	bl0step <- rep(1,8)*0.0001
	g00step <- rep(1,8)*0.00001
	gj0step <- 0.0001*rep(1,8)
	gl0step <- 0.0001*rep(1,8)
	k1step <- rep(1,8)*0.0001
	k0step <- rep(1,8)*0.0001
	csi1step <- rep(1,8)*0.01
	k1b2step <- rep(1,2)*0.01
}
k1b2step <- rep(1,2)*0.1

################

Sijcc <- matrix(0,ccnobs1,ccntimes1)   # Storage for the Survival Function
Lijcc <- matrix(0,ccnobs1,ccntimes1)   # Storage for the length function
Dijcc <- matrix(0,ccnobs1,ccntimes1)   # Storage for the detection function
ones1cc <- matrix(1,ccnobs1,ccntimes1)  # Matrix of ones for constant birth rate.

Sijci <- matrix(0,cinobs1,cintimes1)   # Storage for the Survival Function
Lijci <- matrix(0,cinobs1,cintimes1)   # Storage for the length function
Dijci <- matrix(0,cinobs1,cintimes1)   # Storage for the detection function
ones1ci <- matrix(1,cinobs1,cintimes1)  # Matrix of ones for constant birth rate.

Sijlc <- matrix(0,lcnobs1,lcntimes1)   # Storage for the Survival Function
Lijlc <- matrix(0,lcnobs1,lcntimes1)   # Storage for the length function
Dijlc <- matrix(0,lcnobs1,lcntimes1)   # Storage for the detection function
ones1lc <- matrix(1,lcnobs1,lcntimes1)  # Matrix of ones for constant birth rate.

Sijli <- matrix(0,linobs1,lintimes1)   # Storage for the Survival Function
Lijli <- matrix(0,linobs1,lintimes1)   # Storage for the length function
Dijli <- matrix(0,linobs1,lintimes1)   # Storage for the detection function
ones1li <- matrix(1,linobs1,lintimes1)  # Matrix of ones for constant birth rate.

Sijtc <- matrix(0,tcnobs1,tcntimes1)   # Storage for the Survival Function
Lijtc <- matrix(0,tcnobs1,tcntimes1)   # Storage for the length function
Dijtc <- matrix(0,tcnobs1,tcntimes1)   # Storage for the detection function
ones1tc <- matrix(1,tcnobs1,tcntimes1)  # Matrix of ones for constant birth rate.

Sijti <- matrix(0,tinobs1,tintimes1)   # Storage for the Survival Function
Lijti <- matrix(0,tinobs1,tintimes1)   # Storage for the length function
Dijti <- matrix(0,tinobs1,tintimes1)   # Storage for the detection function
ones1ti <- matrix(1,tinobs1,tintimes1)  # Matrix of ones for constant birth rate.

Sijuc <- matrix(0,ucnobs1,ucntimes1)   # Storage for the Survival Function
Lijuc <- matrix(0,ucnobs1,ucntimes1)   # Storage for the length function
Dijuc <- matrix(0,ucnobs1,ucntimes1)   # Storage for the detection function
ones1uc <- matrix(1,ucnobs1,ucntimes1)  # Matrix of ones for constant birth rate.

Sijui <- matrix(0,uinobs1,uintimes1)   # Storage for the Survival Function
Lijui <- matrix(0,uinobs1,uintimes1)   # Storage for the length function
Dijui <- matrix(0,uinobs1,uintimes1)   # Storage for the detection function
ones1ui <- matrix(1,uinobs1,uintimes1)  # Matrix of ones for constant birth rate.


csi2cc <- csi1[1]*ones1cc  # Create a constant birth rate matrix
csi2ci <- csi1[2]*ones1ci  # Create a constant birth rate matrix
csi2lc <- csi1[3]*ones1lc  # Create a constant birth rate matrix
csi2li <- csi1[4]*ones1li  # Create a constant birth rate matrix
csi2tc <- csi1[5]*ones1tc  # Create a constant birth rate matrix
csi2ti <- csi1[6]*ones1ti  # Create a constant birth rate matrix
csi2uc <- csi1[7]*ones1uc  # Create a constant birth rate matrix
csi2ui <- csi1[8]*ones1ui  # Create a constant birth rate matrix


### Create the matrix of indicators of when a fish is alive, birth, mortality and starting points.
### Creates a list with ad1, ab1, t0i1, t0i2, nobsperstream1[1]
ccad1 <- ab1ad1setup(cc1, cctime1, k1, Linf1, L01)   
ciad1 <- ab1ad1setup(ci1, citime1, k1, Linf1, L01)   
lcad1 <- ab1ad1setup(lc1, lctime1, k1, Linf1, L01)   
liad1 <- ab1ad1setup(li1, litime1, k1, Linf1, L01)   
tcad1 <- ab1ad1setup(tc1, tctime1, k1, Linf1, L01)   
tiad1 <- ab1ad1setup(ti1, titime1, k1, Linf1, L01)   
ucad1 <- ab1ad1setup(uc1, uctime1, k1, Linf1, L01)   
uiad1 <- ab1ad1setup(ui1, uitime1, k1, Linf1, L01)   
ad1s <- list(ccad1$ad1,ciad1$ad1,lcad1$ad1,liad1$ad1,tcad1$ad1,tiad1$ad1,ucad1$ad1,uiad1$ad1)
ab1s <- list(ccad1$ab1,ciad1$ab1,lcad1$ab1,liad1$ab1,tcad1$ab1,tiad1$ab1,ucad1$ab1,uiad1$ab1)
c0i1s <- list(ccad1$t0i1,ciad1$t0i1,lcad1$t0i1,liad1$t0i1,tcad1$t0i1,tiad1$t0i1,ucad1$t0i1,uiad1$t0i1)

### Number of alive fish at each time point.
ccnobsalive1 <- apply(ccad1$t0i1, 2, sum)
cinobsalive1 <- apply(ciad1$t0i1, 2, sum)
lcnobsalive1 <- apply(lcad1$t0i1, 2, sum)
linobsalive1 <- apply(liad1$t0i1, 2, sum)
tcnobsalive1 <- apply(tcad1$t0i1, 2, sum)
tinobsalive1 <- apply(tiad1$t0i1, 2, sum)
ucnobsalive1 <- apply(ucad1$t0i1, 2, sum)
uinobsalive1 <- apply(uiad1$t0i1, 2, sum)

### Create survival, length and detection function
Lijcc <- VonBertLM(cctime1, ccad1$t0i1, ccad1$t0i2, k1, L01, Linf1)
Lijci <- VonBertLM(citime1, ciad1$t0i1, ciad1$t0i2, k1, L01, Linf1)
Lijlc <- VonBertLM(lctime1, lcad1$t0i1, lcad1$t0i2, k1, L01, Linf1)
Lijli <- VonBertLM(litime1, liad1$t0i1, liad1$t0i2, k1, L01, Linf1)
Lijtc <- VonBertLM(tctime1, tcad1$t0i1, tcad1$t0i2, k1, L01, Linf1)
Lijti <- VonBertLM(titime1, tiad1$t0i1, tiad1$t0i2, k1, L01, Linf1)
Lijuc <- VonBertLM(uctime1, ucad1$t0i1, ucad1$t0i2, k1, L01, Linf1)
Lijui <- VonBertLM(uitime1, uiad1$t0i1, uiad1$t0i2, k1, L01, Linf1)

### Create the variances for the length matrices
sigma1[1] = sqrt(sum((cc1-Lijcc)[cc1>0.0]^2)/length(cc1[cc1>0.0]))
sigma1[2] = sqrt(sum((ci1-Lijci)[ci1>0.0]^2)/length(ci1[ci1>0.0]))
sigma1[3] = sqrt(sum((lc1-Lijlc)[lc1>0.0]^2)/length(lc1[lc1>0.0]))
sigma1[4] = sqrt(sum((li1-Lijli)[li1>0.0]^2)/length(li1[li1>0.0]))
sigma1[5] = sqrt(sum((tc1-Lijtc)[tc1>0.0]^2)/length(tc1[tc1>0.0]))
sigma1[6] = sqrt(sum((ti1-Lijti)[ti1>0.0]^2)/length(ti1[ti1>0.0]))
sigma1[7] = sqrt(sum((uc1-Lijuc)[uc1>0.0]^2)/length(uc1[uc1>0.0]))
sigma1[8] = sqrt(sum((ui1-Lijui)[ui1>0.0]^2)/length(ui1[ui1>0.0]))

###  Create the survival matrices
Sijcc <- Surv2(Sijcc, b00[1], bl0[1], cctime1, cc1, Lijcc)  
Sijci <- Surv2(Sijci, b00[2], bl0[2], citime1, ci1, Lijci)
Sijlc <- Surv2(Sijlc, b00[3], bl0[3], lctime1, lc1, Lijlc)
Sijli <- Surv2(Sijli, b00[4], bl0[4], litime1, li1, Lijli)
Sijtc <- Surv2(Sijtc, b00[5], bl0[5], tctime1, tc1, Lijtc)
Sijti <- Surv2(Sijti, b00[6], bl0[6], titime1, ti1, Lijti)
Sijuc <- Surv2(Sijuc, b00[7], bl0[7], uctime1, uc1, Lijuc)
Sijui <- Surv2(Sijui, b00[8], bl0[8], uitime1, ui1, Lijui)
Sdats <- list(Sijcc,Sijci,Sijlc,Sijli,Sijtc,Sijti,Sijuc,Sijui)
S0Mortlike <- 0
for(i in 1:8){
	#print(i)
	S0Mortlike <- S0Mortlike + mortality2(ad1s[[i]], ab1s[[i]], Sdats[[i]]) #Currently evaluates to -inf...
	#print(S0Mortlike)
}

###  Create the detection matrices
Dijcc <- Detect2(cc1, Lijcc, g00[1], gl0[1]) 
Dijci <- Detect2(ci1, Lijci, g00[2], gl0[2])
Dijlc <- Detect2(lc1, Lijlc, g00[3], gl0[3])
Dijli <- Detect2(li1, Lijli, g00[4], gl0[4])
Dijtc <- Detect2(tc1, Lijtc, g00[5], gl0[5])
Dijti <- Detect2(ti1, Lijti, g00[6], gl0[6])
Dijuc <- Detect2(uc1, Lijuc, g00[7], gl0[7])
Dijui <- Detect2(ui1, Lijui, g00[8], gl0[8])
Ddats <- list(Dijcc, Dijci, Dijlc, Dijli, Dijtc, Dijti, Dijuc, Dijui)
D0caplike <- 0
for(ind in 1:8) D0caplike <- D0caplike + capture2(c0i1s[[ind]], ab1s[[ind]], ad1s[[ind]], Ddats[[ind]])

### Start packaging up the various items needed later.
indexc1 <- c(1,3,5,7)
indexi1 <- c(2,4,6,8)
csis <- list(csi2cc, csi2ci, csi2lc, csi2li, csi2tc, csi2ti, csi2uc, csi2ui)
datasets <- list(cc1, ci1, lc1, li1, tc1, ti1, uc1, ui1)
Ldats <- list(Lijcc, Lijci, Lijlc, Lijli, Lijtc, Lijti, Lijuc, Lijui)
nobsperstream1 <- c(ccnobs1, cinobs1, lcnobs1, linobs1, tcnobs1, tinobs1, ucnobs1, uinobs1)

### Start building the likelihood and priors
C0birthlike <- 0
for(i in 1:8) C0birthlike <- C0birthlike + birth1(ab1s[[i]], csis[[i]])
g00prior <- sum(dnorm(g00[indexc1], g01[1], g01sigma[1],log=TRUE)) + sum(dnorm(gl0[indexc1], gl1[1], gl1sigma[1],log=TRUE))
g00prior <- g00prior + sum(dnorm(g00[indexi1], g01[2], g01sigma[2],log=TRUE)) +sum(dnorm(gl0[indexi1], gl1[2], gl1sigma[2],log=TRUE))
g01prior <- sum(dnorm(g01[1], g01priorm[1], g01priors[1],log=TRUE)) + dchisqinvlogdVS(g01sigma[1]^2, df1)
g01prior <- g01prior + sum(dnorm(g01[2], g01priorm[2], g01priors[2],log=TRUE)) + dchisqinvlogdVS(g01sigma[2]^2, df1)
gl1prior <- sum(dnorm(gl1[1], gl1priorm[1], gl1priors[1],log=TRUE)) + dchisqinvlogdVS(gl1sigma[1]^2, df1)
gl1prior <- gl1prior + sum(dnorm(gl1[2], gl1priorm[2], gl1priors[2],log=TRUE)) + dchisqinvlogdVS(gl1sigma[2]^2, df1)
b00prior <- sum(dnorm(b00[indexc1],b01m[1], b01s[1],log=TRUE)) + sum(dnorm(b01m[1],b01priorm[1],b01priors[1],log=TRUE)) + sum(dchisqinvlogdVS(b01s[1]^2 + lambda1, df1 + 1))
b00prior <- b00prior + sum(dnorm(b00[indexi1],b01m[2], b01s[2],log=TRUE)) + sum(dnorm(b01m[2],b01priorm[2],b01priors[2],log=TRUE)) + sum(dchisqinvlogdVS(b01s[2]^2 + lambda1, df1 + 1))
bl0prior <- sum(dnorm(bl0[indexc1],bl1m[1], bl1s[1],log=TRUE)) + sum(dnorm(bl1m[1],bl1priorm[1],bl1priors[1],log=TRUE)) + sum(dchisqinvlogdVS(bl1s[1]^2 + lambda1, df1 + 1))
bl0prior <- bl0prior + sum(dnorm(bl0[indexi1],bl1m[2], bl1s[2],log=TRUE)) + sum(dnorm(bl1m[2],bl1priorm[2],bl1priors[2],log=TRUE)) + sum(dchisqinvlogdVS(bl1s[2]^2 + lambda1, df1 + 1))
#k0prior <- sum(dnorm(k0[indexc1],k0m1[1],k0s1[1],log=TRUE)) + sum(dnorm(k0m1[1],k0priorm,k0priors,log=TRUE))
#k0prior <- k0prior + sum(dnorm(k0[indexi1],k0m1[2],k0s1[2],log=TRUE)) + sum(dnorm(k0m1[2],k0priorm,k0priors,log=TRUE))
#k0prior <- k0prior + sum(dnorm(k1[indexc1],k1m1[1],k1s1[1],log=TRUE)) + sum(dnorm(k1m1[1],k1priorm,k1priors,log=TRUE))
#k0prior <- k0prior + sum(dnorm(k1[indexi1],k1m1[2],k1s1[2],log=TRUE)) + sum(dnorm(k1m1[2],k1priorm,k1priors,log=TRUE))
k0prior=0
csiprior <- sum(dbeta(csi1,csibeta1,csibeta2,log=TRUE))
L0Like1 <- 0
for(i in 1:8) L0Like1 <- L0Like1 + sum(LengthLikeR(datasets[[1]], Ldats[[1]], sigma1[[1]]))
sigma1prior <- dchisqinvlogdVV(sigma1[1]^2 + lambda1, df1 + nobsperstream1[1])

### Get an initial look at what things will look like
totalpost0 <- D0caplike + S0Mortlike + g00prior + b00prior + bl0prior + k0prior + L0Like1 + C0birthlike + csiprior + sigma1prior  
totalpost0 

### Create a list that has the input values we will need to run the model
list_in1 <- list("csiprior" = csiprior,
		"csi2cc" = csi2cc, "csi2ci" = csi2ci, "csi2lc" = csi2lc, "csi2li" = csi2li,
		"csi2tc" = csi2tc, "csi2ti" = csi2ti, "csi2uc" = csi2uc, "csi2ui" = csi2ui,
		"C0birthlike" = C0birthlike,
		"csi1" = csi1, "csi1step" = csi1step, "csibeta1" = csibeta1, "csibeta2" = csibeta2,
		"ccab1" = ccad1$ab1, "ciab1" = ciad1$ab1, "lcab1" = lcad1$ab1, "liab1" = liad1$ab1,
		"tcab1" = tcad1$ab1, "tiab1" = tiad1$ab1, "ucab1" = ucad1$ab1, "uiab1" = uiad1$ab1,
		"b00" = b00,
		"bl0" = bl0,
		"b00step" = b00step, "bl0step" = bl0step,
		"b01m" = b01m, "bl1m" = bl1m,
		"b01s" = b01s, "bl1s" = bl1s,
		"b00prior" = b00prior, "bl0prior" = bl0prior,
		"b01priorm"=b01priorm, "b01priors" = b01priors,
		"bl1priorm"=bl1priorm, "bl1priors" = bl1priors,
		"S0Mortlike" = S0Mortlike,
		"totalpost0" = totalpost0,
		"g00"=g00, "gl0"=gl0,"gl1"=gl1,"gl0step"=gl0step,"g00step"=g00step,
		"g01m"=g01, "g00prior"=g00prior,"gl0prior"=gl1prior,
		"g01priorm"=g01priorm,"g01priors"=g01priors,
		"gl1priorm"=gl1priorm,"gl1priors"=gl1priors,
		"g01prior"=g01prior, "lambda1"=lambda1,"df1"=df1,
		"ccDij"=Dijcc,"ciDij"=Dijci,"lcDij"=Dijlc,"liDij"=Dijli,
		"tcDij"=Dijtc,"tiDij"=Dijti,"ucDij"=Dijuc,"uiDij"=Dijui,
		"g01s"=g01sigma,"gl1sigma"=gl1sigma,"D0caplike"=D0caplike,
		"cct0i1"=ccad1$t0i1,"cit0i1"=ciad1$t0i1,"lct0i1"=lcad1$t0i1,"lit0i1"=liad1$t0i1,
		"tct0i1"=tcad1$t0i1,"tit0i1"=tiad1$t0i1,"uct0i1"=ucad1$t0i1,"uit0i1"=uiad1$t0i1,
		"cct0i2"=ccad1$t0i2,"cit0i2"=ciad1$t0i2,"lct0i2"=lcad1$t0i2,"lit0i2"=liad1$t0i2,
		"tct0i2"=tcad1$t0i2,"tit0i2"=tiad1$t0i2,"uct0i2"=ucad1$t0i2,"uit0i2"=uiad1$t0i2,
		"cc1" = cc1, "ccSij" = Sijcc, "ccLij" = Lijcc, "ccad1" = ccad1$ad1, "cctime1" = ccad1$t0i2,
		"ci1" = ci1, "ciSij" = Sijci, "ciLij" = Lijci, "ciad1" = ciad1$ad1, "citime1" = ciad1$t0i2,
		"lc1" = lc1, "lcSij" = Sijlc, "lcLij" = Lijlc, "lcad1" = lcad1$ad1, "lctime1" = lcad1$t0i2,
		"li1" = li1, "liSij" = Sijli, "liLij" = Lijli, "liad1" = liad1$ad1, "litime1" = liad1$t0i2,
		"tc1" = tc1, "tcSij" = Sijtc, "tcLij" = Lijtc, "tcad1" = tcad1$ad1, "tctime1" = tcad1$t0i2,
		"ti1" = ti1, "tiSij" = Sijti, "tiLij" = Lijti, "tiad1" = tiad1$ad1, "titime1" = tiad1$t0i2,
		"uc1" = uc1, "ucSij" = Sijuc, "ucLij" = Lijuc, "ucad1" = ucad1$ad1, "uctime1" = ucad1$t0i2,
		"ui1" = ui1, "uiSij" = Sijui, "uiLij" = Lijui, "uiad1" = uiad1$ad1, "uitime1" = uiad1$t0i2,
		"DetectNcc" = ccnobsalive1, "DetectNci" = cinobsalive1, "DetectNlc" = lcnobsalive1, 
		"DetectNli" = linobsalive1, "DetectNtc" = tcnobsalive1, "DetectNti" = tinobsalive1, 
		"DetectNuc" = ucnobsalive1, "DetectNui" = uinobsalive1, "k0"=k0, #"k0step"=k0step, "k0m1"=k0m1, "k0s1"=k0s1,
		"k1"=k1,#"k1step"=k1step,"k0prior"=k0prior,"k1a1"=k1a1, "k1b1"=k1b1, "k1m1"=k1m1, "k1s1"=k1s1,
		"L01"=L01, "Linf1"=Linf1, #"k1priorm"=k1priorm, "k1priors"=k1priors, "k0priorm"=k0priorm, "k0priors"=k0priors,
		"L0Like1"=L0Like1,#"k1b1step"=k1b2step, "k1b2"=k1b2, "k1a2"=k1a2,
		"sigma1"=sigma1,"sigma1prior"=sigma1prior
)

#######################################################################################################################
#######################################################################################################################
#######################################################################################################################

sim1results <- csi_sample1(list_in1,niter1)

#################################################################################################################
###  Get New Start Values (essentially the last values from the chain).
b00 <- sim1results$b00[niter1,]
b01m <- sim1results$b01m[niter1,1:2]
b01s <- sim1results$b01m[niter1,3:4]
bl0 <- sim1results$bl0[niter1,]
bl1m <- sim1results$bl1m[niter1,1:2]
bl1s <- sim1results$bl1m[niter1,3:4]
g00 <- sim1results$g00[niter1,]
gl0 <- sim1results$gl0[niter1,]
#k1 <- sim1results$k1[niter1,]
csi1 <- sim1results$csi1[niter1,]
sigma1 <- sim1results$sigma1[niter1,]
gl1sigma <- sim1results$gl1m[niter1,3:4]
g01sigma <- sim1results$g01m[niter1,3:4]
#k1m1 <- sim1results$k1b1[niter1,1:2]
#k1s1 <- sim1results$k1b1[niter1,3:4]
gl1 <- sim1results$gl1m[niter1,1:2]
g01 <- sim1results$g01m[niter1,1:2]

#################################################################################################################
###  Get New Step Values (Calculated from the ends of the chains).
stepsamp1 <- (niter1 + lastiterforstep1):niter1
b00step <- new_step1(sim1results$b00, stepsamp1, b00step)
bl0step <- new_step1(sim1results$bl0, stepsamp1, bl0step)
g00step <- new_step1(sim1results$g00, stepsamp1, g00step)
gl0step <- new_step1(sim1results$gl0, stepsamp1, gl0step)
#k1step <- new_step1(sim1results$k1, stepsamp1, k1step)
csi1step <- new_step1(sim1results$csi1, stepsamp1, csi1step)

#################################################################################################################
#################################################################################################################
#################################################################################################################
###   W r i t e   s t u f f   o u t
#################################################################################################################
###  Write out the samples
write.csv(sim1results$b00, paste('Samples/Mod1.sample.b00.chain.', chainnumber1, '.csv', sep=""),  row.names=FALSE)
write.csv(sim1results$b01m[,1:2], paste('Samples/Mod1.sample.b01m.chain.', chainnumber1, '.csv', sep=""), row.names=FALSE)
write.csv(sim1results$b01m[,3:4], paste('Samples/Mod1.sample.b01s.chain.', chainnumber1, '.csv', sep=""), row.names=FALSE)
write.csv(sim1results$bl0, paste('Samples/Mod1.sample.bl0.chain.', chainnumber1, '.csv', sep=""), row.names=FALSE)
write.csv(sim1results$bl1m[,1:2], paste('Samples/Mod1.sample.bl1m.chain.', chainnumber1, '.csv', sep=""), row.names=FALSE)
write.csv(sim1results$bl1m[,3:4], paste('Samples/Mod1.sample.bl1s.chain.', chainnumber1, '.csv', sep=""), row.names=FALSE)
write.csv(sim1results$g00, paste('Samples/Mod1.sample.g00.chain.', chainnumber1, '.csv', sep=""), row.names=FALSE)
write.csv(sim1results$g01m[,1:2], paste('Samples/Mod1.sample.g01m.chain.', chainnumber1, '.csv', sep=""), row.names=FALSE)
write.csv(sim1results$g01m[,3:4], paste('Samples/Mod1.sample.g01s.chain.', chainnumber1, '.csv', sep=""), row.names=FALSE)
write.csv(sim1results$gl0, paste('Samples/Mod1.sample.gl0.chain.', chainnumber1, '.csv', sep=""), row.names=FALSE)
write.csv(sim1results$gl1m[,1:2], paste('Samples/Mod1.sample.gl1m.chain.', chainnumber1, '.csv', sep=""), row.names=FALSE)
write.csv(sim1results$gl1m[,3:4], paste('Samples/Mod1.sample.gl1s.chain.', chainnumber1, '.csv', sep=""), row.names=FALSE)
write.csv(sim1results$csi1, paste('Samples/Mod1.sample.csi1.chain.', chainnumber1, '.csv', sep=""), row.names=FALSE)
#write.csv(sim1results$k1, paste('Samples/Mod1.sample.k1.chain.', chainnumber1, '.csv', sep=""), row.names=FALSE)

write.csv(sim1results$sigma1, paste('Samples/Mod1.sample.sigma1.chain.', chainnumber1, '.csv', sep=""), row.names=FALSE)
############################################################
############################################################
############################################################
############################################################

write.csv(sim1results$PopEstcc, paste('Samples/Mod1.sample.PopCC.chain.', chainnumber1, '.csv', sep=""), row.names=FALSE)

write.csv(sim1results$PopEstci, paste('Samples/Mod1.sample.PopCI.chain.', chainnumber1, '.csv', sep=""), row.names=FALSE)

write.csv(sim1results$PopEstlc, paste('Samples/Mod1.sample.PopLC.chain.', chainnumber1, '.csv', sep=""), row.names=FALSE)

write.csv(sim1results$PopEstli, paste('Samples/Mod1.sample.PopLI.chain.', chainnumber1, '.csv', sep=""), row.names=FALSE)

write.csv(sim1results$PopEsttc, paste('Samples/Mod1.sample.PopTC.chain.', chainnumber1, '.csv', sep=""), row.names=FALSE)

write.csv(sim1results$PopEstti, paste('Samples/Mod1.sample.PopTI.chain.', chainnumber1, '.csv', sep=""), row.names=FALSE)

write.csv(sim1results$PopEstuc, paste('Samples/Mod1.sample.PopUC.chain.', chainnumber1, '.csv', sep=""), row.names=FALSE)

write.csv(sim1results$PopEstui, paste('Samples/Mod1.sample.PopUI.chain.', chainnumber1, '.csv', sep=""), row.names=FALSE)





#################################################################################################################
###  Write out the start values
write.csv(b00, paste('StartStep/Mod1.start.b00.chain.', chainnumber1, '.txt', sep=""),  row.names=FALSE)
write.csv(b01m, paste('StartStep/Mod1.start.b01m.chain.', chainnumber1, '.txt', sep=""),  row.names=FALSE)
write.csv(b01s, paste('StartStep/Mod1.start.b01s.chain.',  chainnumber1, '.txt', sep=""),  row.names=FALSE)
write.csv(bl1m, paste('StartStep/Mod1.start.bl1m.chain.', chainnumber1, '.txt', sep=""),  row.names=FALSE)
write.csv(bl1s, paste('StartStep/Mod1.start.bl1s.chain.', chainnumber1, '.txt', sep=""),  row.names=FALSE)
write.csv(g00, paste('StartStep/Mod1.start.g00.chain.', chainnumber1, '.txt', sep=""),  row.names=FALSE)
write.csv(gl0, paste('StartStep/Mod1.start.gl0.chain.', chainnumber1, '.txt', sep=""),  row.names=FALSE)
write.csv(bl0, paste('StartStep/Mod1.start.bl0.chain.', chainnumber1, '.txt', sep=""),  row.names=FALSE)
#write.csv(k1, paste('StartStep/Mod1.start.k1.chain.', chainnumber1, '.txt', sep=""),  row.names=FALSE)
write.csv(csi1, paste('StartStep/Mod1.start.csi1.chain.', chainnumber1, '.txt', sep=""),  row.names=FALSE)
write.csv(sigma1, paste('StartStep/Mod1.start.sigma1.chain.', chainnumber1, '.txt', sep=""),  row.names=FALSE)
write.csv(g01, paste('StartStep/Mod1.start.g01.chain.', chainnumber1, '.txt', sep=""), row.names=FALSE)
write.csv(gl1, paste('StartStep/Mod1.start.gl1.chain.', chainnumber1, '.txt', sep=""), row.names=FALSE)
write.csv(gl1sigma, paste('StartStep/Mod1.start.gl1sigma.chain.', chainnumber1, '.txt', sep=""), row.names=FALSE)
write.csv(g01sigma, paste('StartStep/Mod1.start.g01sigma.chain.', chainnumber1, '.txt', sep=""), row.names=FALSE)
#write.csv(k1m1, paste('StartStep/Mod1.start.k1m1.chain.', chainnumber1, '.txt', sep=""), row.names=FALSE)
#write.csv(k1s1, paste('StartStep/Mod1.start.k1s1.chain.', chainnumber1, '.txt', sep=""), row.names=FALSE)

#################################################################################################################
###  Write out the step values
if( savestep1 == TRUE ){
	write.csv(b00step, paste('StartStep/Mod1.step.b00.chain.', chainnumber1, '.txt', sep=""),  row.names=FALSE)
	write.csv(bl0step, paste('StartStep/Mod1.step.bl0.chain.', chainnumber1, '.txt', sep=""),  row.names=FALSE)
	write.csv(g00step, paste('StartStep/Mod1.step.g00.chain.', chainnumber1, '.txt', sep=""),  row.names=FALSE)
	write.csv(gl0step, paste('StartStep/Mod1.step.gl0.chain.', chainnumber1, '.txt', sep=""),  row.names=FALSE)
	write.csv(k1step, paste('StartStep/Mod1.step.k1.chain.', chainnumber1, '.txt', sep=""),  row.names=FALSE)
	write.csv(csi1step, paste('StartStep/Mod1.step.csi1.chain.', chainnumber1, '.txt', sep=""),  row.names=FALSE)
}
}

units = c("cc","ci","lc", "li", "tc", "ti", "uc", "ui")

### Convenience function for reading in the csvs. Curried within the function below
reader = function(param,n,col,chainnumber1) read.csv(pastes("Mod1.sample.",param,".chain.",chainnumber1, ".",n,".csv"))[,col]





new_step1 <- function(samples1, stepsamp1, oldstep){
	temp1 <- apply(samples1[stepsamp1,],2,sd)/2
	temp2 <- ifelse(temp1 == 0, oldstep/2, temp1)
	return(temp2)
}

##############################################################
###  Other functions we need for the setup
################################################################
## t0 generation
## length    length of individual at time t
## time
## k         LVB growth parameter
## L0
## Linf
t0gen <- function(length,time,k,L0,Linf){
	result1 <- time + 1/k*log((Linf-length)/(Linf-L0))
	return(result1)
}

################################################################
## t0 generation for logistic function
## length    length of individual at time t
## time
## k0        growth parameter
## k1        growth parameter
## L0
## Linf
t0genlogistic <- function(length1, time, k0, k1, Linf, L0){
	LR1 <- length1/Linf
	result1 <- time + (log(.1/.9) - k0)/k1  
	return(result1)
}

##################################################################################################
ab1ad1setup <- function(d1, time1, k1, Linf1, L01){
	t0i1 <- ifelse(d1>0.0,1,0)		# Determine where a fish is present
	nrow1 <- nrow(d1)			# Determine the number of rows
	ncol1 <- ncol(d1)			# Determine the number of columns
	ab1 = matrix(0, nrow1, ncol1)		# Set up a container or the birth matrix
	ad1 = matrix(0, nrow1, ncol1)         # Set up a container for the mortality matrix
	t0i2 = rep(1, nrow1)	                # Storage for t0
	nobsperstream1 = sum(t0i1)            # counter for the number of observations in the stream.
	for(i in 1:nrow1){
		if(t0i1[i,1] == 1){
			bornflag1 = 1
			ab1[i,1] = 1 
		}else{
			bornflag1 = 0
		}
		aliveflag1 = 1
		ad1[i,1] = 1
		for(j in 1:(ncol1-1)){
			if(t0i1[i,j] == 0 & t0i1[i,j+1] == 1){
				t0i2[i] = t0gen(d1[i,j+1],time1[j], k1, Linf1, L01)
				bornflag1 = 1      # Switch flag if the fish was born
			}
			ab1[i,j+1] = bornflag1
			if(t0i1[i,j] == 1 & t0i1[i,j+1] == 0){
				aliveflag1 = 0    # Switch flag if the fish has died
			}
			ad1[i,j+1] = aliveflag1
		}
	}
	res1 <- list("ad1"=ad1, "ab1"=ab1, "t0i1"=t0i1, "t0i2"=t0i2, "nobsperstream1" = nobsperstream1)
	return(res1)
}

################################################################### 
##  Von-Bertalanffy curve
##  Linf1   Asymptotic length
##  L01     Lowest detectable limit
##  k1      growth rate
##  t1      time now
##  t0      time at birth
VonBert <- function(t1, t0, k1, L01, Linf1){
	diff1 <- t1 - t0
	result1 <- Linf1 - (Linf1 - L01)*exp(-k1*(t1-t0))
	if(result1 < 0.0){
		result = 0.0
	}else{
		result = result1
	}
	return(result)
}

##################################################################################################
VonBertLM <- function(ti, t0i1, t0i2, k1, L01, Linf1){
	nrow1 <- nrow(t0i1)
	ncol1 <- ncol(t0i1)
	Lij = matrix(0, nrow1, ncol1)
	for(i in 1:nrow1){
		for(j in 1:ncol1){
			#if(t0i1[i,j] == 1){
			Lij[i,j] = VonBert(ti[j], t0i2[i], k1, L01, Linf1)  ### This needs to be changed to reflect the correct times.
			#}
		}
	}
	return(Lij)
}

################################################################### 
##  Logistic for growth curve
##  Linf1   Asymptotic length
##  L01     Lowest detectable limit
##  k0      growth offset
##  k1      growth rate
##  t1      time now
##  t0      time at birth
logistic1AB <- function(ti, t0i1, k0, k1, L01, Linf1){
	diff1 <- ti-t0i1;
	x1 <- k0 + k1*diff1;
	result1 <- L01 + (Linf1 - L01)*exp(x1)/(1+exp(x1))
	return(result1)
} 

##################################################################################################
logisticLM <- function(ti, t0i1, t0i2, k0, k1, L01, Linf1){
	nrow1 <- nrow(t0i1)
	ncol1 <- ncol(t0i1)
	Lij = matrix(0, nrow1, ncol1)
	for(i in 1:nrow1){
		for(j in 1:ncol1){
			#if(t0i1[i,j] == 1){
			Lij[i,j] = logistic1AB(ti[j], t0i2[i], k0, k1, L01, Linf1)  ### This needs to be changed to reflect the correct times.
			#}
		}
	}
	return(Lij)
}

##################################################################################################
ab1ad1setuplogistic <- function(d1, time1, k0, k1, Linf1, L01){
	t0i1 <- ifelse(d1>0.0,1,0)		# Determine where a fish is present
	nrow1 <- nrow(d1)			# Determine the number of rows
	ncol1 <- ncol(d1)			# Determine the number of columns
	ab1 = matrix(0, nrow1, ncol1)		# Set up a container or the birth matrix
	ad1 = matrix(0, nrow1, ncol1)         # Set up a container for the mortality matrix
	t0i2 = rep(1, nrow1)	                # Storage for t0
	nobsperstream1 = sum(t0i1)            # counter for the number of observations in the stream.
	for(i in 1:nrow1){
		if(t0i1[i,1] == 1){
			bornflag1 = 1
			ab1[i,1] = 1 
		}else{
			bornflag1 = 0
		}
		aliveflag1 = 1
		ad1[i,1] = 1
		for(j in 1:(ncol1-1)){
			if(t0i1[i,j] == 0 & t0i1[i,j+1] == 1){
				t0i2[i] = t0genlogistic(d1[i,j+1],time1[j], k0, k1, Linf1, L01)
				bornflag1 = 1      # Switch flag if the fish was born
			}
			ab1[i,j+1] = bornflag1
			if(t0i1[i,j] == 1 & t0i1[i,j+1] == 0){
				aliveflag1 = 0    # Switch flag if the fish has died
			}
			ad1[i,j+1] = aliveflag1
		}
	}
	res1 <- list("ad1"=ad1, "ab1"=ab1, "t0i1"=t0i1, "t0i2"=t0i2, "nobsperstream1"=nobsperstream1)
	return(res1)
}



### Function to calculate N. Currently this is a post-hoc calculation, but it's pretty fast so it shouldn't be a big deal.
N.Calc = function(prefix, chain.numb){

	setwd("~/Dropbox/Research/Consulting/Gilliam/EdWork/Samples/")

outname = paste(prefix,chain.numb, sep="")

col = which(units == stream)

###  Read in asome test data.
cc1 <- as.matrix(read.csv("~/Dropbox/Research/Consulting/Gilliam/EdWork/cc.csv", header=FALSE))
ci1 <- as.matrix(read.csv("~/Dropbox/Research/Consulting/Gilliam/EdWork/ci.csv", header=FALSE))
lc1 <- as.matrix(read.csv("~/Dropbox/Research/Consulting/Gilliam/EdWork/lc.csv", header=FALSE))
li1 <- as.matrix(read.csv("~/Dropbox/Research/Consulting/Gilliam/EdWork/li.csv", header=FALSE))
ti1 <- as.matrix(read.csv("~/Dropbox/Research/Consulting/Gilliam/EdWork/ti.csv", header=FALSE))
tc1 <- as.matrix(read.csv("~/Dropbox/Research/Consulting/Gilliam/EdWork/tc.csv", header=FALSE))
uc1 <- as.matrix(read.csv("~/Dropbox/Research/Consulting/Gilliam/EdWork/uc.csv", header=FALSE))
ui1 <- as.matrix(read.csv("~/Dropbox/Research/Consulting/Gilliam/EdWork/ui.csv", header=FALSE))

length.chains = nrow(cc1)
## Set Times
citime1 <- c(245,308,366,430,492,554,611,638,652,708,796,835,863,983,1019,1058)/1058 
cctime1 <- c(611,638,652,708,796,835,863,983,1019,1058)/1058
litime1 <- c(0,65,128,185,245,308,366,430,492,554,611,638,652,708,796,835,863,983,1019,1058)/1058
lctime1 <- c(0,65,128,185,245,308,366,430,492,554,611,638,652,708,796,835,863,983,1019,1058)/1058
titime1 <- c(245,308,366,430,491,554,611,638,652,708,796,835,863,983,1019,1058)/1058
tctime1 <- c(245,611,638,652,708,796,835,863,983,1019,1058)/1058
uctime1 <- c(0,65,128,185,245,308,366,430,492,554,611,652,708,796,835,863,983,1019,1058)/1058
uitime1 <- c(0,65,128,185,245,308,366,430,492,554,611,652,708,796,835,863,983,1019,1058)/1058

files = list.files(treatment, )
files2 = files[str_detect(files, "FixK")]
files2 = na.omit(files2[str_detect(files2, "Pop")==FALSE])

parameters = c("b00","bl0","g00","gl0","csi1")

timer = c("cctime1","citime1","lctime1","litime1", "tctime1", "titime1", "uctime1", "uitime1")
output.data = matrix(0, nrows=nrow(cc1),ncols=8)

for(i in 1:8){
reader11 = Curry(reader, n=chain.numb, col=i)
output.data[,i]= NPopEst(reader11("b00"), reader11("bl0"), reader11("g00"), reader11("gl0"), reader11("csi1"), length.chains, get(timer[i]))
}
write.csv(output.data, paste('StartStep/Mod1.start.N.chain.',outname, '.txt', sep=""),  row.names=FALSE)
}

NPopEst =function(b00, b0l, g00, g0l, csi, Nobserved, Times).Call("NPopEst",b00, b0l, g00, g0l, csi, Nobserved, Times,package="Gilliam")
Surv2 = function( Sij, b0,  b1,  time1,  Dij,  Lij) .Call("Surv2",  Sij, b0,  b1,  time1,  Dij,  Lij,package="Gilliam")
LengthLike2 = function(Diji, Liji, sigma1) .Call("LengthLikeR",Diji, Liji, sigma1, package="Gilliam")
mortality2 = function(  ab1,  ad1,  S1) .Call("mortalityR", ab1,  ad1,  S1, package="Gilliam")
Surv3prep = function( Inlist) .Call("Surv3Prep", Inlist, package="Gilliam")

  multmat = function(n).Call("multmat",n , package="Gilliam")
  indmat = function( n).Call("indmat",n , package="Gilliam")
  dchisqinvlogd1 = function( X,  v1).Call("dchisqinvlogd1R",X, v1 , package="Gilliam")
  dchisqinvlogdVV= function( X,  v1).Call("dchisqinvlogdVVR",X, v1 , package="Gilliam")
  dchisqinvlogdVS= function( X,  v1).Call("dchisqinvlogdVSR",X,v1 , package="Gilliam")
  Detect2= function( Diji,  Liji,  g00,  gl0).Call("DetectR",  Diji,  Liji,  g00,  gl0, package="Gilliam")
  birth1= function(  ab1,  csi2).Call("birthR",ab1,  csi2 , package="Gilliam")
  capture2= function( x1,  ab1,  ad1,  Dij).Call("captureR", x1,  ab1,  ad1,  Dij, package="Gilliam")
  
  csi_sample1=function( list_in1,  nsim1).Call("csi_sample1", list_in1,  nsim1, package="Gilliam")