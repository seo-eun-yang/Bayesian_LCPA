################################################################################
######################### Application LCPA - TIES Data #########################
########################### Parameter Distribution #############################
################################################################################

# =============================================================================
# Compare true vs. estimated parameters for Sanctions 
# =============================================================================

if(!require(patchwork)) install.packages("patchwork")
library(patchwork) 
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr) 

source("Data_generator.R")
source("Parameter_Comparison.R")

# =============================================================================
# SANCTIONS RECOVERY
# =============================================================================

nobs = c(50000)

for(N in nobs){
  cat(paste("=== SANCTIONS PARAMETER RECOVERY N=",N,"===\n",sep="")) 
  lcpa_san_sim <- readRDS(paste("LCPA_SAN_",N,".rds",sep=""))
  param_est_san  <- extract_param_for_sim(lcpa_san_sim)
  
  es_eta = draw_plots_eta(param_est_san$eta, paste('Estimated Eta (N=',N,')',sep=""))
  es_eta
  ggsave(paste("Diagnostics/Profiles_Estimated","_N",N,".png",sep=""),
         width = 15,           
         height = 5,          
         dpi = 300)
  
  es_rho = draw_plots_rho(param_est_san$big.rho[,2,,'Time.1',1,1],paste('Estimated rho (N=',N,')',sep=""))
  es_rho
  ggsave(paste("Diagnostics/Dist_Classes","_N",N,".png",sep=""),
         width = 8,           
         height = 6,          
         dpi = 300)
  
  df_gam = rbind(data.frame(prob=param_est_san$gam.pf[,,1],
                            model='Estimated',profile=paste('Profile.',1:3,sep="")))
  rownames(df_gam)=1:nrow(df_gam) 
  
  draw_barplot_gam(df_gam,paste('(N=',N,')',sep=""))
  ggsave(paste("Diagnostics/Dist_Profile","_N",N,".png",sep=""),
         width = 8,           
         height = 6,          
         dpi = 300)
  
}
