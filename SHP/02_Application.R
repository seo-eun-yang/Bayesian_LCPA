################################################################################
######################### Application LCPA - SHP Data ##########################
############################# General Application ##############################
################################################################################

# =============================================================================
# Data: SHP
# Waves: 2017 -> 2020 -> 2023
# N = 5,125 (921 18-34 / 2415 35-59 / 1789 60+)
# =============================================================================

if(!require(dplyr)) install.packages("dplyr")
if(!require(readr)) install.packages("readr")
library(dplyr)
library(purrr)
library(tibble)
library(stringr)
library(forcats)
library(readr)
library(ggplot2)
library(haven)


## Load CAT_LVM_BAYESIAN as instructed in README.md!

source("CATLVM_BAYESIAN/CAT_LVM_BAYESIAN.r")
source("CATLVM_BAYESIAN/CAT_LVM_MCMC_prelim.r")
source("CATLVM_BAYESIAN/CAT_LVM_MCMC.r")
source("CATLVM_BAYESIAN/Time_Series_Plots.r")
### Funtions needed to calculate LCA
source("CATLVM_BAYESIAN/LCA/LCA_MCMC_prelim.r")
source("CATLVM_BAYESIAN/LCA/LCA_istep.r")
source("CATLVM_BAYESIAN/LCA/LCA_assign_subjects.r")
source("CATLVM_BAYESIAN/LCA/LCA_pstep.r")
source("CATLVM_BAYESIAN/LCA/LCA_pstep_gamma_beta.r")
source("CATLVM_BAYESIAN/LCA/LCA_time_plots.r")
### Funtions needed to calculate LTA
source("CATLVM_BAYESIAN/LTA/LTA_MCMC_prelim.r")
source("CATLVM_BAYESIAN/LTA/LTA_istep.r")
source("CATLVM_BAYESIAN/LTA/LTA_assign_subjects.r")
source("CATLVM_BAYESIAN/LTA/LTA_pstep.r")
source("CATLVM_BAYESIAN/LTA/LTA_pstep_delta_beta.r")
source("CATLVM_BAYESIAN/LTA/LTA_pstep_tau_beta.r")
source("CATLVM_BAYESIAN/LTA/LTA_time_plots.r")
source("CATLVM_BAYESIAN/LTA/LTA_patt.r")
### Funtions needed to calculate LCPA
source("CATLVM_BAYESIAN/LCPA/LCPA_MCMC_prelim.r")
source("CATLVM_BAYESIAN/LCPA/LCPA_istep.r")
source("CATLVM_BAYESIAN/LCPA/LCPA_assign_subjects.r")
source("CATLVM_BAYESIAN/LCPA/LCPA_pstep.r")
source("CATLVM_BAYESIAN/LCPA/LCPA_pstep_gam_pf_beta.r")
source("CATLVM_BAYESIAN/LCPA/LCPA_pstep_eta_beta.r")
source("CATLVM_BAYESIAN/LCPA/LCPA_time_plots.r")
source("CATLVM_BAYESIAN/CAT_LVM_BAYESIAN_ORIGINAL.r")
source("CATLVM_BAYESIAN/CAT_LVM_MCMC_ORIGINAL.r")
# ML Functions
source("CATLVM_ML/CAT_LVM_ML.r")
source("CATLVM_ML/CAT_LVM_prelim.r")
source("CATLVM_ML/CAT_LVM_EM.r")
source("CATLVM_ML/LCA_prelim.r")
source("CATLVM_ML/LTA_prelim.r")
source("CATLVM_ML/LCPA_prelim.r")
source("CATLVM_ML/LCPA_estep.r")
source("CATLVM_ML/LCPA_post_marg.r")
source("CATLVM_ML/LCPA_mstep.r")
source("CATLVM_ML/LCPA_mstep.r")
source("CATLVM_ML/LCPA_mstep_gam_pf_beta.r")
source("CATLVM_ML/Matrix_Solve.r")
source("CATLVM_ML/LCPA_mstep_eta_beta.r")

### Load data ### 
data_lcpa <- read.csv("shp_panel_cleaned.csv")

nrow(data_lcpa)

nkase <- nrow(data_lcpa)
data <- list()

data$is.grp <- TRUE
data$group  <- data_lcpa$age

data$lcpa$si <- list()
nkase <- nrow(data_lcpa)
data$lcpa$si <-array(1,c(nkase,5,3)) 

data$lcpa$si[, 1, 1] <- data_lcpa$AFFPOL_17
data$lcpa$si[, 2, 1] <- data_lcpa$IDEOEX_17
data$lcpa$si[, 3, 1] <- data_lcpa$ISSPOL_17
data$lcpa$si[, 4, 1] <- data_lcpa$TRUST_17
data$lcpa$si[, 5, 1] <- data_lcpa$GENDER_17

# Wave 2 (2020)
data$lcpa$si[, 1, 2] <- data_lcpa$AFFPOL_20
data$lcpa$si[, 2, 2] <- data_lcpa$IDEOEX_20
data$lcpa$si[, 3, 2] <- data_lcpa$ISSPOL_20
data$lcpa$si[, 4, 2] <- data_lcpa$TRUST_20
data$lcpa$si[, 5, 2] <- data_lcpa$GENDER_20

# Wave 3 (2023)
data$lcpa$si[, 1, 3] <- data_lcpa$AFFPOL_23
data$lcpa$si[, 2, 3] <- data_lcpa$IDEOEX_23
data$lcpa$si[, 3, 3] <- data_lcpa$ISSPOL_23
data$lcpa$si[, 4, 3] <- data_lcpa$TRUST_23
data$lcpa$si[, 5, 3] <- data_lcpa$GENDER_23

cat("NAs in data$lcpa$si:", sum(is.na(data$lcpa$si)), "\n") 
cat("Unique Values in Array:", unique(as.vector(data$lcpa$si)), "\n")
cat("nkase:", nkase, "\n")
cat("Array N:", dim(data$lcpa$si)[1], "\n") 

###################################
######## Parameter Setting ########
################################### 
#nburn <- 100
#niter <- 1000
niter_list <- c(5000)

for(N in niter_list){  
  niter <- N
  nburn <- niter*0.4
  nkase <- nrow(data_lcpa)
  nstr <- 3 # subgroups
  nc <- 1 # count latent classes
  nt <- 3 # waves
  ns <- 3 # count latent status (e.g. high/medium/low)
  npf <- 3 # no latent status-profiles
  nsi <- 5 # items
  nrc <- c(2, 2, 2, 2, 2)
  
  data$lcpa$cov$gam.pf <- NULL
  
  model <- list()
  model$lcpa$is.lcpa <- TRUE
  model$lcpa$ns <- 3
  model$lcpa$npf <- 3
  model$lcpa$nrs <- c(2, 2, 2, 2, 2)
  
  constraint <- list()
  constraint$lcpa$EQUAL$BIG.RHO$TIME <- TRUE 
  constraint$lcpa$EQUAL$BIG.RHO$GROUP <- FALSE
  constraint$lcpa$EQUAL$BIG.RHO$CLASS <- FALSE 
  
  ## ADD GAMMA COVARIATES TO THE LCPA ##
  rel <- data_lcpa$religion
  sex <- data_lcpa$sex
  
  model$ncov <- list()
  model$ncov$gam.pf <- 2 
  data$lcpa$cov$gam.pf <- matrix(c(rel, sex),
                                 nrow = nkase,
                                 ncol = 2)
  
  ## STARTING VALUE SETUP ## 
  iteration <- list()
  iteration$burn.in <- nburn
  iteration$niter   <- niter 
  
  ## First RUN: ML to find priors for gamma ## 
  starval <- list()
  starval$is.random <- FALSE 
  LCPA.REG.1.ML <- cat.lvm(data=data, model=model, starval=starval,
                           constraint=constraint)
  
  ## Transfer prior ##
  prior <- list()
  prior$lcpa$cov$beta$gam.pf <- LCPA.REG.1.ML$prior$lcpa$cov$beta$gam.pf
  
  ## ADD ETA COVARIATES TO THE LCPA ##
  ## STARTING VALUE SETUP ## 
  starval <- list()
  starval$is.random <- TRUE
  constraint$lcpa$ETA <- LCPA.REG.1.ML$constraint$lcpa$ETA
  
  ## Second RUN: ML to find priors for eta ##
  ## This is required to get prior$lcpa$cov$beta$eta ##
  LCPA.REG.2.ML <- cat.lvm(data=data, model=model, starval=starval,
                           constraint=constraint)
  
  ## Transfer prior ##
  prior <- list()
  prior$lcpa$cov$beta$gam.pf <- LCPA.REG.2.ML$prior$lcpa$cov$beta$gam.pf
  prior$lcpa$cov$beta$eta <- LCPA.REG.2.ML$prior$lcpa$cov$beta$eta
  
  ## STARTING VALUE SETUP ##
  ## Using parameters estimated from ML method as a starting values
  starval <- list()
  starval$is.random <- FALSE
  starval$param <- LCPA.REG.2.ML$param
  
  ## Third RUN: MCMC with chains##  
  iteration <- list()
  iteration$burn.in <- nburn
  iteration$niter   <- niter
  iteration$plot    <- TRUE  
  
  LCPA.REG.2.MCMC <- cat.lvm.bayesian.with.chains(  
    data = data, 
    model = model, 
    starval = starval,
    constraint = constraint, 
    prior = prior,
    iteration=iteration,
    save.chains = TRUE 
  ) 
  
  saveRDS(LCPA.REG.2.MCMC, paste("application/LCPA_SHP_",N,".rds",sep=""))
}
