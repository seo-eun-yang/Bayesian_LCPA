################################################################################
######################### Application LCPA - TIES Data #########################
############################ Posterior Calculation #############################
################################################################################

  ################################################
  # Load R-packages, set up directory, load Data #
  ################################################

library(dplyr)
library(readr)

LCPA.REG.2.MCMC <- readRDS("LCPA_SAN_COV_50000.rds")

  ###########################
  # Extract complete Chains #
  ###########################

use_chain <- TRUE 

rho <- LCPA.REG.2.MCMC$param$lcpa$big.rho
eta <- LCPA.REG.2.MCMC$param$lcpa$eta
pi  <- LCPA.REG.2.MCMC$param$lcpa$gam.pf


if(use_chain) {
  rho_chain <- LCPA.REG.2.MCMC$param.chain$lcpa$big.rho
  eta_chain <- LCPA.REG.2.MCMC$param.chain$lcpa$eta
  pi_chain  <- LCPA.REG.2.MCMC$param.chain$lcpa$gam.pf
  
  n_iter <- length(rho_chain)
  cat("Using", n_iter, "MCMC iterations\n")
}

  #######################################
  # Functions for Posterior Calculation #
  #######################################

# Likelihood of one Itemset given Stage
stage_likelihood <- function(y_it, rho_s_t) {
  lik <- 1
  for(j in seq_along(y_it)) {
    if(!is.na(y_it[j])) {
      prob <- rho_s_t[j, y_it[j]]
      if(is.na(prob) || prob < 1e-10) prob <- 1e-10
      lik <- lik * prob
    }
  }
  return(lik)
}

# Stage-Posterior given Profil
stage_posterior_given_pf <- function(y_it, rho, eta_k_t, t, g) {
  n_stage <- length(eta_k_t)
  post <- numeric(n_stage)
  
  for(s in seq_len(n_stage)) {
    rho_s_t <- rho[, , s, t, , g]
    post[s] <- eta_k_t[s] * stage_likelihood(y_it, rho_s_t)
  }
  
  total <- sum(post)
  if(total > 0) {
    return(post / total)
  } else {
    return(rep(1/n_stage, n_stage))
  }
}

# Profile-Posterior for one person
profile_posterior <- function(y_i, rho, eta, pi, g) {
  n_time  <- dim(y_i)[2]
  n_prof  <- dim(eta)[3]
  n_stage <- dim(eta)[1]
  
  post <- numeric(n_prof)
  
  for(k in seq_len(n_prof)) {
    lik <- 1
    
    for(t in seq_len(n_time)) {
      tmp <- 0
      
      for(s in seq_len(n_stage)) {
        rho_s_t <- rho[, , s, t, , g]
        tmp <- tmp + eta[s, t, k, , g] * stage_likelihood(y_i[, t], rho_s_t)
      }
      
      if(tmp < 1e-100) tmp <- 1e-100
      lik <- lik * tmp
    }
    
    post[k] <- pi[k, , g] * lik
  }
  
  total <- sum(post)
  if(total > 0) {
    return(post / total)
  } else {
    return(rep(1/n_prof, n_prof))
  }
}

  #############################
  # Calculate Stage Posterior #
  #############################

cat("\n=== Computing Stage Posteriors ===\n")

setwd("/projects/ocrproject/latentmodels/SY")
ties_proc <- read_csv2("application/TIESv4-1_cleaned.csv")

### Exclude missing cases ###
nrow(ties_proc)
ties_proc <- ties_proc %>% 
  filter(!is.na(major_power))
nrow(ties_proc)

nkase <- nrow(ties_proc)
data <- list() 
data$lcpa$si <- list()
data$lcpa$si <- array(1, c(nkase, 5, 3))

# Wave 1 — Threat phase
data$lcpa$si[,1,1] <- ties_proc$item1_w1
data$lcpa$si[,2,1] <- ties_proc$item2_w1
data$lcpa$si[,3,1] <- ties_proc$item3_w1
data$lcpa$si[,4,1] <- ties_proc$item4_w1
data$lcpa$si[,5,1] <- ties_proc$item5_w1

# Wave 2 — Imposition phase
data$lcpa$si[,1,2] <- ties_proc$item1_w2
data$lcpa$si[,2,2] <- ties_proc$item2_w2
data$lcpa$si[,3,2] <- ties_proc$item3_w2
data$lcpa$si[,4,2] <- ties_proc$item4_w2
data$lcpa$si[,5,2] <- ties_proc$item5_w2

# Wave 3 — Resolution phase
data$lcpa$si[,1,3] <- ties_proc$item1_w3
data$lcpa$si[,2,3] <- ties_proc$item2_w3
data$lcpa$si[,3,3] <- ties_proc$item3_w3
data$lcpa$si[,4,3] <- ties_proc$item4_w3
data$lcpa$si[,5,3] <- ties_proc$item5_w3

Y   <- data$lcpa$si

nkase   <- dim(Y)[1]
n_time  <- dim(Y)[3]
n_stage <- dim(eta)[1]
n_group <- dim(rho)[6]
n_prof  <- dim(eta)[3]

cat("n_subjects:", nkase, "\n")
cat("n_stages:", n_stage, "\n")
cat("n_profiles:", n_prof, "\n")
cat("n_groups:", n_group, "\n")
cat("n_timepoints:", n_time, "\n\n")

# Initialize Arrays
stage_post <- array(
  NA_real_,
  dim = c(nkase, n_stage, n_time, n_group),
  dimnames = list(
    Case  = NULL,
    Stage = dimnames(eta)[[1]],
    Time  = dimnames(eta)[[2]],
    Group = dimnames(rho)[[6]]
  )
)

profile_post <- array(
  NA_real_,
  dim = c(nkase, n_prof, n_group),
  dimnames = list(
    Case    = NULL,
    Profile = paste0("Profile.", 1:n_prof),
    Group   = dimnames(rho)[[6]]
  )
)

# ==================================================
# Option A: with posterior means (faster)
# ==================================================

if(!use_chain) {
  
  cat("Using posterior means...\n")
  
  for(g in seq_len(n_group)) {
    
    if(g == 1) cat("Processing Group", g, "...\n")
    
    for(i in seq_len(nkase)) {
      
      if(i %% 200 == 0) cat("  Subject", i, "/", nkase, "\n")
      
      # Profile posterior for this Person
      pf_post <- profile_posterior(Y[i, , ], rho, eta, pi, g)
      profile_post[i, , g] <- pf_post
      
      # Stage posterior for each Timepoint
      for(t in seq_len(n_time)) {
        tmp <- numeric(n_stage)
        
        # Marginalize over Profiles
        for(k in seq_along(pf_post)) {
          sp <- stage_posterior_given_pf(
            Y[i, , t], 
            rho, 
            eta[, t, k, , g], 
            t, 
            g
          )
          tmp <- tmp + pf_post[k] * sp
        }
        
        # Normalize
        total <- sum(tmp)
        if(total > 0) {
          stage_post[i, , t, g] <- tmp / total
        } else {
          stage_post[i, , t, g] <- rep(1/n_stage, n_stage)
        }
      }
    }
  }
  
  # ==================================================
  # Option B: with MCMC Chain (slower)
  # ==================================================
  
} else {
  
  cat("Using MCMC chain (averaging over", n_iter, "iterations)...\n")
  
  # Temporal Array for each Iteration
  stage_post_temp <- array(0, dim = c(nkase, n_stage, n_time, n_group))
  profile_post_temp <- array(0, dim = c(nkase, n_prof, n_group))
  
  for(iter in 1:n_iter) {
    
    if(iter %% 50 == 0) cat("  Iteration", iter, "/", n_iter, "\n")
    
    # Extract Parameter for this Iteration
    rho_iter <- rho_chain[[iter]]
    eta_iter <- eta_chain[[iter]]
    pi_iter  <- pi_chain[[iter]]
    
    for(g in seq_len(n_group)) {
      for(i in seq_len(nkase)) {
        
        # Profile posterior
        pf_post <- profile_posterior(Y[i, , ], rho_iter, eta_iter, pi_iter, g)
        profile_post_temp[i, , g] <- profile_post_temp[i, , g] + pf_post
        
        # Stage posterior
        for(t in seq_len(n_time)) {
          tmp <- numeric(n_stage)
          
          for(k in seq_along(pf_post)) {
            sp <- stage_posterior_given_pf(
              Y[i, , t], 
              rho_iter, 
              eta_iter[, t, k, , g], 
              t, 
              g
            )
            tmp <- tmp + pf_post[k] * sp
          }
          
          stage_post_temp[i, , t, g] <- stage_post_temp[i, , t, g] + tmp 
        }
      }
    }
  }
  
  stage_post_temp <- stage_post_temp / n_iter
  profile_post_temp <- profile_post_temp / n_iter
  
  for(g in seq_len(n_group)) {
    for(i in seq_len(nkase)) {
      for(t in seq_len(n_time)) {
        tmp <- stage_post_temp[i, , t, g]
        total <- sum(tmp)
        if(total > 0) {
          stage_post_temp[i, , t, g] <- tmp / total
        } else {
          stage_post_temp[i, , t, g] <- rep(1/n_stage, n_stage)
        }
      }
    }
  }
  
  stage_post <- stage_post_temp
  profile_post <- profile_post_temp
  
}

N <- length(rho_chain)

saveRDS(
  list(stage_post = stage_post,profile_post = profile_post),
  paste("/projects/ocrproject/latentmodels/posteriors_san_cov_", N, ".rds",sep="")
)

cat("\nDone!\n\n")

