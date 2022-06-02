#---------------------install/load packages----------------------
# packages necessary to perform simulation study

if (!require("devtools")) install.packages("devtools")
#library(devtools) 
#install_github("LeonardV/restriktor")
#library(restriktor)
if (!require("restriktor")) install.packages("restriktor") # install this package first (once)
library(restriktor)
#
if (!require("MASS")) install.packages("MASS") # install this package first (once)
library(MASS)   # to call mvrnorm


#----------------ADJUST, DOES NOT REMAIN THE SAME DURING EVERY NEW SIMULATION--------------------

#specify number of simulations
nsim <- 2 # 1000

# Paste a condition here; wrt p, hypos etc.
# In this file, we replicate HT89 and do not inspect multiple r2 and rho values - this is done in another file.
p <- 7
ratio <- c(1, 2, 3, 0, 0, 0, 0)
ratio_name <- "ratio1230000"
ratio_title <- paste("Ratio of effects is 1:2:3:0:0:0:0")
length(ratio) == p
# Here, beta <- ratio will be used.


# Sim 0 - like HT89 do
HyposetName <- "HTset0"
H01 <- "X2 == 0; X3 == 0; X4 == 0; X5 == 0; X6 == 0; X7 == 0"
H02 <- "X3 == 0; X4 == 0; X5 == 0; X6 == 0; X7 == 0"
H03 <- "X4 == 0; X5 == 0; X6 == 0; X7 == 0"
H04 <- "X5 == 0; X6 == 0; X7 == 0"
H05 <- "X6 == 0; X7 == 0"
H06 <- "X7 == 0"
nrhypos <- 6
indexHtrue <- 3
#
##cat(paste0("H0", 1:nrhypos, ","))
goric_m <- "goric(fit.lm, H01, H02, H03, H04, H05, H06, type = 'goric')"
goricc_m <- "goric(fit.lm, H01, H02, H03, H04, H05, H06, type = 'goricc')"


# Sim 1
HyposetName <- "HTset1"
H01 <- "X3 == 0; X4 == 0; X5 == 0; X6 == 0; X7 == 0"
H02 <- "X4 == 0; X5 == 0; X6 == 0; X7 == 0"
H03 <- "X5 == 0; X6 == 0; X7 == 0"
H04 <- "X6 == 0; X7 == 0"
H05 <- "X7 == 0"
nrhypos <- 5
indexHtrue <- 2
#
##cat(paste0("H0", 1:nrhypos, ","))
goric_m <- "goric(fit.lm, H01, H02, H03, H04, H05, type = 'goric')"
goricc_m <- "goric(fit.lm, H01, H02, H03, H04, H05, type = 'goricc')"





set.seed(123) # to obtain same results every time you run the same simulation


#----------------DO NOT ADJUST, REMAINS THE SAME DURING EVERY NEW SIMULATION--------------------

#-------------EFFECT SIZE, CORRELATIE EN SAMPLE SIZE------------

f2 <- c(0.02, 0.15, 0.35) # effect sizes f2
r2 <- f2 / (1+f2) # transfrom f2 to R2 # R2 = .0196, .13, .26
r2 <- c(r2, .933) # Special for this simulation, since HT89 use this r2 (then beta = ratio)
rho <- c(0, 0.25, 0.5) # correlation between X's/predictors -- all set equal to rho
# In this file, onlt r2 = .933 and rho = 0 will be used
N <- c(10, 20, 50, 80, 160, 250) # sample size  # c(10, 20)


#dataframe for simulation results (true hypothesis rate per n) in one table
goricresult <- data.frame(criterion = NA, thr = NA, meanWeight = NA, Htrue = NA, bestH_thr = NA, bestH_meanW = NA, npred = NA, rpred = NA, ninf = NA, n = NA, cor_pop = NA, ES_pop = NA, meanES = NA)

for(n in N){
  #for(tellerR2 in 1:length(r2)){
    #for(tellerRho in 1:length(rho)){
      tellerR2 <- length(r2) # r2 = 0.933
      tellerRho <- 1 # rho = 0
#----------------------------GORIC SIMULATIES -----------------------------

#RUN SIMULATIONS AFTER EVERY ADJUSTMENT

# Make variables for output from simulation
goric <- array(NA, c(nsim, (nrhypos+1)))
goricweights <- array(NA, c(nsim, (nrhypos+1)))
goricc <- array(NA, c(nsim, (nrhypos+1)))
goriccweights <- array(NA, c(nsim, (nrhypos+1)))
r2approx <- array(NA, c(nsim, (1)))

#matrix voor correlatie tussen variabelen
sigma <- matrix(rho[tellerRho], ncol=p, nrow = p)
for(i in 1:p){
  sigma[i,i] <- 1
}


# General formula to calculate population regression coefficients (betas)
betas <- NA
fun_calcBeta <- function(x){sum <- 0; for(i in 1:p){for(j in 1:p){sum <- sum + ratio[i]*x * ratio[j]*x * sigma[i,j]}}; sum - r2[tellerR2]}
x <- uniroot(fun_calcBeta, lower=0, upper=100)$root 
betas <- ratio*x
#betas # population value for beta (notably: standardized beta)
#
#Check:
#sum <- 0; for(i in 1:p){for(j in 1:p){sum <- sum + (ratio[i]*x) * (ratio[j]*x) * sigma[i,j]}}; sum 
#r2[tellerR2]
#
#betas <- ratio # In this file, we replicate HT89 and thus use this
# Btw, because of this we will also not standardize X and y below.

betas_check <- 0 # As a check to see how simulation went

# Generate goric values = AIC values in case of equalities
# And generate goricc values = AICc values in case of equalities
for (i in 1:nsim) {
  cat("iteration =", p, "... =", i, "\n")
  
  # generate X, that is, create sample of predictor variables
  X <- mvrnorm(n, rep(0,p), sigma, empirical=FALSE)
  #
  # Generate error variable based on desired r2 (because all vars are standardized, var(resid)=1-R2)
  # So, not variance of 1 every time but 1-R2.
  var.epsilon <- 1-r2[tellerR2]
  error <- rnorm(n, sd=sqrt(var.epsilon))
  #
  # Make y based on true coefficients and desired r2
  intercept <- 0
  y <- intercept + X %*% betas + error
  #
  X <- scale(X) # We need this when comparing betas with GORIC(C).
  y <- scale(y) # Not per se needed, but is nice to check the estimated standardized beta values.
  
  #fit model
  fit.lm <- lm(y ~ 1 + X) # regression model, with intercept
  # As a check to see how simulation went:
  r2approx[i,] <- summary(lm(fit.lm))$r.squared
  betas_check <- betas_check + coef(fit.lm)
  
  # GORIC and GORICC
  goricm <- eval(parse(text=goric_m))
  goriccm <- eval(parse(text=goricc_m))
  #
  goric[i,] <- goricm$result[, 4]
  goricweights[i,] <- goricm$result[, 5]
  #
  goricc[i,] <- goriccm$result[, 4]
  goriccweights[i,] <- goriccm$result[, 5]
}

# As check to see how simulation went
# Compare r2approx with r2
#CHECK WHETHER ESTIMATED R2 OF SAMPLE MATCHES WITH POPULATION R2
#mean(r2approx[,1])  # estimated based on n and nsim
#r2[tellerR2]        # population
#
## Compare betas
#betas_check / nsim # estimated based on n and nsim
#betas              # population


#TRUE HYPOTHESIS RATE GORIC/AIC
#sum(apply(goric[,indexHtrue] < goric[,-indexHtrue], 1, all)) / nsim # true hypothesis rate
HRgoric <- matrix(NA, nrow = 1, ncol = (nrhypos))
for(i in 1:(nrhypos+1)){
  if(nrhypos == 1){
    HRgoric[i] <- sum(goric[,i] <= goric[,-i]) / nsim
  }else{
    HRgoric[i] <- sum(apply(goric[,i] <= goric[,-i], 1, all)) / nsim # how often is each hypothesis chosen
  }
}
#HRgoric
#HRgoric[indexHtrue] # true hypothesis rate
#
#
#TRUE HYPOTHESIS RATE GORICc
HRgoricc <- matrix(NA, nrow = 1, ncol = (nrhypos))
for(i in 1:(nrhypos+1)){
  if(nrhypos == 1){
    HRgoricc[i] <- sum(goricc[,i] <= goricc[,-i]) / nsim
  }else{
    HRgoricc[i] <- sum(apply(goricc[,i] <= goricc[,-i], 1, all)) / nsim # how often is each hypothesis chosen
  }
}
#HRgoricc
#HRgoricc[indexHtrue] # true hypothesis rate


meanGoricweights <- apply(goricweights, 2, mean) # mean of goric weights
meanGoricweights <- apply(goricweights, 2, mean, na.rm=T) # mean of goric weights# voor als er NA (= missings) zijn
#meanGoricweights
meangoriccweights <- apply(goriccweights, 2, mean) #mean of goricc weights
meangoriccweights <- apply(goriccweights, 2, mean, na.rm=T) #mean of goricc weights voor als er missings zijn
#meangoriccweights


#table with true hypothesis rate per n=sample size for goric and goricc
goricresult[nrow(goricresult) + 1,]   = list("aic", HRgoric[indexHtrue], meanGoricweights[indexHtrue], indexHtrue, which.max(HRgoric), which.max(meanGoricweights), length(ratio), sum(ratio), length(ratio[ratio != 0]), n, rho[tellerRho], r2[tellerR2], mean(r2approx[,1]))
goricresult[nrow(goricresult) + 1,]   = list("aicc", HRgoricc[indexHtrue], meangoriccweights[indexHtrue], indexHtrue, which.max(HRgoricc), which.max(meangoriccweights), length(ratio), sum(ratio), length(ratio[ratio != 0]), n, rho[tellerRho], r2[tellerR2], mean(r2approx[,1]))
#goricresult
#goriccresult


} # End of loop for n
#}} # End of loops for r2 and rho

goricresult <- goricresult[-1,] # Note: delete first row here, because it consists of NAs
#goriccresult[-1,] # Note: delete first row here, because it consists of NAs
goricresult # Gives table of results

name <- paste0("Output_aic_", ratio_name, "_", HyposetName, ".rds")
# Save an object to a file
saveRDS(goricresult, file = name)
# Restore the object
goricresult <- readRDS(name) 

