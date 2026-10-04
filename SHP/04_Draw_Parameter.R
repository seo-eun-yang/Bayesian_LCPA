################################################################################
######################### Application LCPA - SHP Data ##########################
########################### Parameter Distribution #############################
################################################################################

# =============================================================================
# Compare estimated parameters by group
# =============================================================================

if(!require(patchwork)) install.packages("patchwork")
library(patchwork) 
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)

source("Data_generator.R")
source("Parameter_Comparison.R")

draw_plots_eta <- function(data, obj) {
  df_long <- as.data.frame.table(data)
  colnames(df_long) <- c("Stage", "Time", "Profile", "Probability")
  
  save_plots <- list()
  
  for (k in 1:3) {
    df_plot <- df_long %>% filter(Profile == paste0("Profile.", k))
    
    p1 <- ggplot(df_plot, aes(x = Time, y = Probability, color = Stage, group = Stage)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      scale_y_continuous(limits = c(0, 1)) +
      theme_minimal() +
      labs(
        title    = paste0("Profile.", k),
        subtitle = obj,
        x        = "Time Point",
        y        = "Probability",
        color    = "Stage"
      ) +
      theme(legend.position = "right")
    
    save_plots[[k]] <- p1
    if (k == 1) { ps <- p1 } else { ps <- ps + p1 }
  }
  
  return(ps + plot_layout(guides = "collect"))
}

nobs <- LCPA$iteration$niter
groups <- 1:LCPA$model$lcpa$npf

out_dir <- "/projects/ocrproject/latentmodels/FINAL/SHP_5000/Diagnostics"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

draw_plots_eta <- function(data, obj) {
  df_long <- as.data.frame.table(data)
  colnames(df_long) <- c("Stage", "Time", "Profile", "Probability") 
  
  save_plots <- list()
  
  for (k in 1:3) {
    df_plot <- df_long %>% filter(Profile == paste0("Profile.", k))
    
    p1 <- ggplot(df_plot, aes(x = Time, y = Probability, color = Stage, group = Stage)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      scale_y_continuous(limits = c(0, 1)) +
      theme_minimal() +
      labs(
        title    = paste0("Profile.", k),
        subtitle = obj,
        x        = "Time Point",
        y        = "Probability",
        color    = "Stage"
      ) +
      theme(legend.position = "right")
    
    save_plots[[k]] <- p1
    
    if (k == 1) {
      ps <- p1
    } else {
      ps <- ps + p1
    }
  }
  
  return(ps + plot_layout(guides = "collect"))
}


for(N in nobs){
  
  cat(paste0("=== PARAMETER RECOVERY N = ", N, " ===\n"))
  
  lcpa_san_sim <- readRDS(
    paste0("/projects/ocrproject/latentmodels/LCPA_SHP_FINAL_", N, ".rds")
  )
  
  param_est_san <- extract_param_for_sim(lcpa_san_sim)
  
  for(g in groups){
    
    cat(paste0("---- Group ", g, " ----\n"))
    
    # Eta by group
    eta_g <- param_est_san$eta[, , , 1, g]
    
    es_eta <- draw_plots_eta(
      eta_g,
      paste0("Estimated Eta (N = ", N, ", Group ", g, ")")
    )
    
    print(es_eta)
    
    ggsave(
      filename = paste0(out_dir, "/SHP_Profiles_Estimated_N", N, "_Group", g, ".png"),
      plot = es_eta,
      width = 15,
      height = 5,
      dpi = 300
    )
    
    # Rho by group
    rho_g <- param_est_san$big.rho[, 2, , 1, 1, g]
    
    es_rho <- draw_plots_rho(
      rho_g,
      paste0("Estimated Rho (N = ", N, ", Group ", g, ")")
    )
    
    print(es_rho)
    
    ggsave(
      filename = paste0(out_dir, "/SHP_Dist_Classes_N", N, "_Group", g, ".png"),
      plot = es_rho,
      width = 8,
      height = 6,
      dpi = 300
    )
    
    # Gamma/profile distribution by group
    df_gam <- data.frame(
      prob = as.vector(param_est_san$gam.pf[, 1, g]),
      model = "Estimated",
      profile = paste0("Profile.", 1:3)
    )
    
    es_gam <- draw_barplot_gam(
      df_gam,
      paste0("(N = ", N, ", Group ", g, ")")
    )
    
    print(es_gam)
    
    ggsave(
      filename = paste0(out_dir, "/SHP_Dist_Profile_N", N, "_Group", g, ".png"),
      plot = es_gam,
      width = 8,
      height = 6,
      dpi = 300
    )
  }
}

