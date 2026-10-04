################################################################################
######################### Application LCPA - TIES Data #########################
############################### Class Selection ################################
################################################################################

LCPA_test <- readRDS("SAN_GOF/LCPA_SAN_C2_P2.rds")
data_lcpa <- read.csv2("TIESv4-1_cleaned.csv")

data <- list()
nkase       <- nrow(data_lcpa)

data_si      <- array(NA, c(nkase, 5, 3))

data_si[,1,1] <- data_lcpa$item1_w1
data_si[,2,1] <- data_lcpa$item2_w1
data_si[,3,1] <- data_lcpa$item3_w1
data_si[,4,1] <- data_lcpa$item4_w1
data_si[,5,1] <- data_lcpa$item5_w1

data_si[,1,2] <- data_lcpa$item1_w2
data_si[,2,2] <- data_lcpa$item2_w2
data_si[,3,2] <- data_lcpa$item3_w2
data_si[,4,2] <- data_lcpa$item4_w2
data_si[,5,2] <- data_lcpa$item5_w2

data_si[,1,3] <- data_lcpa$item1_w3
data_si[,2,3] <- data_lcpa$item2_w3
data_si[,3,3] <- data_lcpa$item3_w3
data_si[,4,3] <- data_lcpa$item4_w3
data_si[,5,3] <- data_lcpa$item5_w3

group_vec <- rep(1, nkase)

cat("Range data_si:", range(data_si, na.rm=TRUE), "\n")
cat("NAs:", sum(is.na(data_si)), "\n")
cat("Unique values:", sort(unique(as.vector(data_si))), "\n")

# ============================================================

compute_loglik_lcpa <- function(big_rho, eta, gam_pf, data_si, group_vec){
  
  nkase  <- dim(data_si)[1]
  nitem  <- dim(data_si)[2]
  ntime  <- dim(data_si)[3]
  nc     <- dim(eta)[1]
  npf    <- dim(eta)[3]
  
  loglik_total <- 0
  
  for(i in 1:nkase){
    g <- group_vec[i]
    
    lik_i <- 0
    
    for(s in 1:npf){
      
      gamma_s <- gam_pf[s, 1, g]
      prob_seq <- 1
      
      for(t in 1:ntime){
        
        prob_t <- 0
        
        for(c in 1:nc){
          
          # [nc, ntime, npf, 1, ngroup]
          eta_cts <- eta[c, t, s, 1, g]
          
          prob_items <- 1
          
          for(m in 1:nitem){
            y_imt <- data_si[i, m, t]
            if(!is.na(y_imt)){
              # [nitem, ncat, nc, ntime, 1, ngroup]
              prob_items <- prob_items * big_rho[m, y_imt, c, t, 1, g]
            }
          }
          
          prob_t <- prob_t + eta_cts * prob_items
        }
        
        prob_seq <- prob_seq * prob_t
      }
      
      lik_i <- lik_i + gamma_s * prob_seq
    }
    
    if(lik_i <= 0) lik_i <- 1e-300
    loglik_total <- loglik_total + log(lik_i)
  }
  
  return(loglik_total)
}

# ============================================================
# DIC Calculation
# ============================================================

compute_dic <- function(LCPA_obj, data_si, group_vec){
  
  chains <- LCPA_obj$param.chain$lcpa
  niter  <- length(chains$big.rho)
  
  cat(paste0("Computing log-likelihood for ", niter, " iterations...\n"))
  
  loglik_chain <- numeric(niter)
  
  for(iter in 1:niter){
    if(iter %% 500 == 0) cat(paste0("  Iteration ", iter, "/", niter, "\n"))
    
    loglik_chain[iter] <- compute_loglik_lcpa(
      big_rho   = chains$big.rho[[iter]],
      eta       = chains$eta[[iter]],
      gam_pf    = chains$gam.pf[[iter]],
      data_si   = data_si,
      group_vec = group_vec
    )
  }
  
  # D_bar = mean Deviance over all Iterations
  D_bar <- mean(-2 * loglik_chain)
  
  # D_hat = Deviance at posterior mean
  loglik_at_mean <- compute_loglik_lcpa(
    big_rho   = LCPA_obj$param$lcpa$big.rho,
    eta       = LCPA_obj$param$lcpa$eta,
    gam_pf    = LCPA_obj$param$lcpa$gam.pf,
    data_si   = data_si,
    group_vec = group_vec
  )
  D_hat <- -2 * loglik_at_mean
  
  # Effective number of parameter
  p_D <- D_bar - D_hat
  
  # DIC
  DIC <- D_hat + 2 * p_D   
  
  return(list(
    DIC          = DIC,
    p_D          = p_D,
    D_bar        = D_bar,
    D_hat        = D_hat,
    loglik_mean  = loglik_at_mean,
    loglik_chain = loglik_chain
  ))
}

# ============================================================
# Calculate parameter count
# ============================================================

count_params_lcpa <- function(nc, npf, nitem=5, ncat=2, ntime=3, ngroup=1){
  # big.rho: nitem * (ncat-1) * nc  
  n_rho <- nitem * (ncat - 1) * nc * ngroup
  # eta: (nc-1) * ntime * npf  per group
  n_eta <- (nc - 1) * ntime * npf * ngroup
  # gam.pf: (npf-1) per group
  n_gamma <- (npf - 1) * ngroup
  
  return(n_rho + n_eta + n_gamma)
}

# ============================================================
# Apply model 
# ============================================================

dic_result <- compute_dic(LCPA_test, data_si, group_vec)

# CHANGE based on input .rds file here! (nc = number classes, npf = number profiles)
n_params <- count_params_lcpa(nc=2, npf=2)
n        <- nrow(data_lcpa)

BIC_equiv <- -2 * dic_result$loglik_mean + log(n) * n_params
AIC_equiv <- -2 * dic_result$loglik_mean + 2 * n_params

cat("\n========================================\n")
cat(paste0("DIC:        ", round(dic_result$DIC, 2), "\n"))
cat(paste0("p_D:        ", round(dic_result$p_D, 2), "\n"))
cat(paste0("D_bar:      ", round(dic_result$D_bar, 2), "\n"))
cat(paste0("D_hat:      ", round(dic_result$D_hat, 2), "\n"))
cat(paste0("LogLik:     ", round(dic_result$loglik_mean, 2), "\n"))
cat(paste0("N params:   ", n_params, "\n"))
cat(paste0("AIC_equiv:  ", round(AIC_equiv, 2), "\n"))
cat(paste0("BIC_equiv:  ", round(BIC_equiv, 2), "\n"))
cat("========================================\n")


# ============================================================
#Save results - run first time
# ============================================================

gof_table <- data.frame(
  Dataset    = character(),
  N_Classes  = integer(),
  N_Profiles = integer(),
  N_Params   = integer(),
  LogLik     = numeric(),
  DIC        = numeric(),
  p_D        = numeric(),
  AIC_equiv  = numeric(),
  BIC_equiv  = numeric(),
  stringsAsFactors = FALSE
)

# ============================================================
# Add results
# ============================================================

add_gof_result <- function(table, dataset, nc, npf, dic_result, n, ngroup=1){
  
  n_params  <- count_params_lcpa(nc=nc, npf=npf, ngroup=ngroup)
  aic       <- -2 * dic_result$loglik_mean + 2 * n_params
  bic       <- -2 * dic_result$loglik_mean + log(n) * n_params
  
  new_row <- data.frame(
    Dataset    = dataset,
    N_Classes  = nc,
    N_Profiles = npf,
    N_Params   = n_params,
    LogLik     = round(dic_result$loglik_mean, 2),
    DIC        = round(dic_result$DIC, 2),
    p_D        = round(dic_result$p_D, 2),
    AIC_equiv  = round(aic, 2),
    BIC_equiv  = round(bic, 2),
    stringsAsFactors = FALSE
  )
  
  return(rbind(table, new_row))
}

# ============================================================

# CHANGE based on input .rds file here! (nc = number classes, npf = number profiles)
gof_table <- add_gof_result(
  table      = gof_table,
  dataset    = "SAN",
  nc         = 2,
  npf        = 2,
  dic_result = dic_result,
  n          = nrow(data_lcpa)
)

print(gof_table)

out_dir <- "/projects/ocrproject/latentmodels/FINAL/SAN_GOF"

saveRDS(gof_table,
        file.path(out_dir, "GOF_Table_C2_P2.rds"))
