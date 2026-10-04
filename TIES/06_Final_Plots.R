################################################################################
######################### Application LCPA - ANES Data #########################
################################# Final Plots ##################################
################################################################################

  ################################################
  # Load R-packages, set up directory, load Data #
  ################################################

library(dplyr)
library(ggplot2)
library(ggpattern)

LCPA.REG.2.MCMC <- readRDS("Data/LCPA_SAN_50000.rds")

ties_proc <- read_csv2("Data/TIESv4-1_cleaned.csv")

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

##################### 
# Rearrange big.rho #
#####################

rho <- LCPA.REG.2.MCMC$param$lcpa$big.rho

rho_long <- as.data.frame.table(
  rho[, 2, , , ,], responseName = "Pr_item2")

colnames(rho_long) <- c("Item", "Stage", "Time", "Pr_item2")

item_labels <- c(
  "sItem.1" = "Direct\nAction",
  "sItem.2" = "Instrument\nBreadth",
  "sItem.3" = "Economic\nPressure",
  "sItem.4" = "Sender\nCommitment",
  "sItem.5" = "Multilateral\nParticipation"
)

rho_long$Item_label <- factor(
  rho_long$Item,
  levels = names(item_labels),
  labels = item_labels)

stage_labels <- c(
  "Stage.1" = "Class 1",
  "Stage.2" = "Class 2", 
  "Stage.3" = "Class 3")


######################################
# Create Item Responsibility Barplot #
######################################

# Map latent classes
rho_long$Stage_mapped <- as.character(rho_long$Stage)

rho_long$Stage_mapped[rho_long$Stage == "Stage.1"] <- "Direct Coercion"
rho_long$Stage_mapped[rho_long$Stage == "Stage.2"] <- "Broad Coercion"
rho_long$Stage_mapped[rho_long$Stage == "Stage.3"] <- "Economic Pressure"

rho_long$Stage_mapped <- factor(rho_long$Stage_mapped,
                                levels = c("Direct Coercion", "Broad Coercion", "Economic Pressure"))

# Plot 
item_san <- ggplot(rho_long, aes(x       = Stage_mapped, 
                                       y       = Pr_item2, 
                                       fill    = Item_label,
                                       pattern = Item_label)) +
  geom_bar_pattern(
    stat            = "identity",
    position        = position_dodge(width = 0.85),
    color           = "black",
    linewidth       = 0.3,
    pattern_density = 0.25,
    pattern_spacing = 0.04,
    pattern_fill    = "black",
    pattern_colour  = "black"
  ) +
  scale_fill_manual(
    name = "Item",
    values = c(
      "Direct\nAction"            = "#2C3E7A",
      "Instrument\nBreadth"         = "#5B8DB8",
      "Economic\nPressure"          = "#A8C4D4",
      "Sender\nCommitment"          = "#D4A96A",
      "Multilateral\nParticipation" = "#8B4513"
    )
  ) +
  scale_pattern_manual(
    name = "Item",
    values = c(
      "Direct\nAction"            = "none",
      "Instrument\nBreadth"         = "stripe",
      "Economic\nPressure"          = "crosshatch",
      "Sender\nCommitment"          = "circle",
      "Multilateral\nParticipation" = "wave"
    )
  ) +
  guides(
    fill = guide_legend(
      override.aes = list(
        fill    = c("#2C3E7A", "#5B8DB8", "#A8C4D4", "#D4A96A", "#8B4513"),
        pattern = c("none", "stripe", "crosshatch", "circle", "wave"),
        color   = rep("black", 5),
        size    = rep(0.3, 5)
      )
    ),
    pattern = guide_none()
  ) +
  labs(
    x     = "Latent Class",
    y     = "Probability",
    title = "Item Response Probabilities by Latent Class"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x      = element_text(size = 11),
    axis.text.y      = element_text(size = 11),
    axis.title       = element_text(face = "bold", size = 12),
    legend.title     = element_text(face = "bold", size = 11),
    legend.text      = element_text(size = 10),
    plot.title       = element_text(face = "bold", hjust = 0.5, size = 13),
    panel.border     = element_rect(color = "black", fill = NA),
    legend.position  = "right"
  )
item_san

ggsave(
  filename = "Figures/Item_Response_FINAL.png",
  plot = item_san,
  width = 7,
  height = 8.5,
  dpi = 300
)

################################################ 
# Prepare Class-Membership by Profile Timeline #
################################################

eta <- LCPA.REG.2.MCMC$param$lcpa$eta

eta_long <- as.data.frame(as.table(eta))
eta_long[,4] <- NULL
eta_long[,4] <- NULL
colnames(eta_long) <- c("Class", "Time", "Profile", "Value")  

eta_long$Class   <- gsub("Stage=|Stage\\.", "", as.character(eta_long$Class))
eta_long$Time    <- gsub("Time=|Time\\.", "", as.character(eta_long$Time))
eta_long$Profile <- gsub("Profile=|Profile\\.", "", as.character(eta_long$Profile))

eta_long$Class   <- factor(eta_long$Class, 
                           levels = c("1","2","3"),
                           labels = c("Class 1", "Class 2", "Class 3"))

eta_long$Time    <- factor(eta_long$Time, 
                           levels = c("1","2","3"),
                           labels = c("Threat", "Imposition", "Resolution"))

eta_long$Profile <- factor(eta_long$Profile, 
                           levels = c("1","2","3"),
                           labels = c("Profile 1", "Profile 2", "Profile 3"))

eta_long$Class_mapped <- as.character(eta_long$Class)


############################################### 
# Create Class-Membership by Profile Timeline #
###############################################

eta_long$Profile_mapped <- as.character(eta_long$Profile)

eta_long$Profile_mapped <- factor(eta_long$Profile_mapped,
                                  levels = c("Profile 1", "Profile 2", "Profile 3"))

eta_long$Class_mapped <- as.character(eta_long$Class)

eta_long$Class_mapped[eta_long$Class == "Class 1"] <- "Direct Coercion"
eta_long$Class_mapped[eta_long$Class == "Class 2"] <- "Broad Coercion"
eta_long$Class_mapped[eta_long$Class == "Class 3"] <- "Economic\nPressure"

eta_long$Class_mapped <- factor(eta_long$Class_mapped,
                                levels = c("Direct Coercion", 
                                           "Broad Coercion", 
                                           "Economic\nPressure"))
# Plot 
time_san <- ggplot(eta_long, aes(x        = Time, 
                                 y        = Value, 
                                 color    = Class_mapped, 
                                 shape    = Class_mapped,
                                 linetype = Class_mapped,
                                 group    = Class_mapped)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~ Profile_mapped, scales = "free_y") +
  scale_color_manual(
    name = "Latent Class",
    values = c(
      "Direct Coercion" = "#2C5F8A",   
      "Broad Coercion" = "#7B7B7B",   
      "Economic\nPressure" = "#D35400"    
    )
  ) +
  scale_shape_manual(
    name = "Latent Class",
    values = c(
      "Direct Coercion" = 16,
      "Broad Coercion" = 17,
      "Economic\nPressure" = 15
    )
  ) +
  scale_linetype_manual(
    name = "Latent Class",
    values = c(
      "Direct Coercion" = "solid",
      "Broad Coercion" = "solid",
      "Economic\nPressure" = "solid"
    )
  ) +
  labs(
    x     = "Time Period",
    y = expression(paste("Probability of Class Membership (", eta, ")")),
    title = "Latent Class Membership Probabilities by Profile"
  ) +
  scale_x_discrete(limits = c("Threat", "Imposition", "Resolution"))+
  theme_classic(base_size = 12) +
  theme(
    strip.text       = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "white", color = "black"),
    panel.border     = element_rect(color = "black", fill = NA),
    axis.title       = element_text(face = "bold", size = 12),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y      = element_text(size = 11),
    plot.title       = element_text(face = "bold", hjust = 0.5, size = 11),
    legend.title     = element_text(face = "bold", size = 9),
    legend.text      = element_text(size = 9),
    legend.position  = "top"
  )
time_san

ggsave(
  filename = "Figures/Class_Membership_TIES.pdf",
  plot = time_san, 
  width    = 5.2,
  height   = 4.8,
  units    = "in",
  dpi      = 300
) 


################################
# Check Structure of Parameter #
################################

beta_list       <- LCPA.REG.2.MCMC$param.chain$lcpa$beta.gam.pf
beta_array_full <- simplify2array(beta_list)
beta_array_full <- aperm(beta_array_full, c(5, 1, 2, 3, 4))
beta_array_full <- beta_array_full[,,,1,]


##############################
# Get Statistics from Chains #
##############################

profiles  <- c("Profile.1", "Profile.2", "Profile.3")
cov_names <- c("Intercept", "Cold War", "Power")

results_rc <- data.frame()

for(g in 1:3){
  for(p in 1:3){
    for(cov in 2:3){
      
      samples <- beta_array_full[, cov, p]
      
      # Probability of Direction (pd)
      prob_positive <- mean(samples > 0)
      prob_negative <- mean(samples < 0)
      pd <- max(prob_positive, prob_negative)
      
      results_rc <- rbind(
        results_rc,
        data.frame(
          Profile   = profiles[p],
          Covariate = cov_names[cov],
          Mean      = mean(samples),
          CI_lo     = quantile(samples, 0.025),
          CI_hi     = quantile(samples, 0.975),
          PD        = pd
        )
      )
    }
  }
}


################################
# Map Substantive Profile Names
################################

results_rc$Profile_mapped <- NA

results_rc$Profile_mapped[
  results_rc$Profile == "Profile.1"
] <- "Sustained Broad Coercion"

results_rc$Profile_mapped[
  results_rc$Profile == "Profile.2"
] <- "Imposition-Centered Direct Coercion"

results_rc$Profile_mapped[
  results_rc$Profile == "Profile.3"
] <- "Direct-to-Economic Pressure"


results_rc$Profile_mapped <- factor(
  results_rc$Profile_mapped,
  levels = c(
    "Sustained Broad Coercion",
    "Imposition-Centered Direct Coercion",
    "Direct-to-Economic Pressure"
  )
)


################################
# Remove Reference Profile
################################

# Profile 1 = reference category
plot_data <- results_rc %>%
  filter(Profile != "Profile.1")


################################
# Create Covariate Effect Plot
################################

cov <- ggplot(
  plot_data,
  aes(
    x = Profile_mapped,
    y = Mean,
    color = Profile_mapped,
    shape = Profile_mapped
  )
) +
  
  geom_point(size = 4) +
  
  geom_errorbar(
    aes(
      ymin = CI_lo,
      ymax = CI_hi
    ),
    width = 0.2,
    linewidth = 0.8
  ) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black",
    linewidth = 0.8
  ) +
  
  facet_wrap(
    ~ Covariate,
    scales = "free_y"
  ) +
  
  scale_color_manual(
    name = "Latent Profile",
    values = c(
      "Imposition-Centered Direct Coercion" = "#7B7B7B",
      "Direct-to-Economic Pressure"         = "#D35400"
    ),
    labels = c(
      "Imposition-Centered Direct Coercion" =
        "Profile 2: Imposition-Centered\nDirect Coercion",
      "Direct-to-Economic Pressure" =
        "Profile 3: Direct-to-Economic Pressure"
    )
  ) +
  
  scale_shape_manual(
    name = "Latent Profile",
    values = c(
      "Imposition-Centered Direct Coercion" = 17,
      "Direct-to-Economic Pressure"         = 15
    ),
    labels = c(
      "Imposition-Centered Direct Coercion" =
        "Profile 2: Imposition-Centered\nDirect Coercion",
      "Direct-to-Economic Pressure" =
        "Profile 3: Direct-to-Economic Pressure"
    )
  ) +
  
  scale_x_discrete(
    labels = c(
      "Imposition-Centered Direct Coercion" =
        "Profile 2\nImposition-Centered\nDirect Coercion",
      "Direct-to-Economic Pressure" =
        "Profile 3\nDirect-to-Economic\nPressure"
    )
  ) +
  
  labs(
    title = "Covariate Associations with Profile Membership",
    subtitle = paste0(
      "Coefficients relative to Profile 1 ",
      "(Sustained Broad Coercion); 95% credible intervals"
    ),
    y = "Coefficient (log-odds)",
    x = "Latent Profile"
  ) +
  
  theme_classic(base_size = 12) +
  
  theme(
    legend.position = "bottom",
    
    plot.title = element_text(
      face = "bold",
      hjust = 0.5,
      size = 13
    ),
    
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 10,
      color = "gray40"
    ),
    
    strip.text = element_text(
      face = "bold",
      size = 11
    ),
    
    strip.background = element_rect(
      fill = "white",
      color = "black"
    ),
    
    panel.border = element_rect(
      color = "black",
      fill = NA
    ),
    
    axis.title = element_text(
      face = "bold",
      size = 10
    ),
    
    axis.text.x = element_text(
      size = 8,
      lineheight = 0.9
    ),
    
    axis.text.y = element_text(size = 8),
    
    legend.title = element_text(
      face = "bold",
      size = 10
    ),
    
    legend.text = element_text(size = 9)
  )

cov


################
# Save Figure
################

ggsave(
  filename = "Figures/Covariate_Effects_TIES.pdf",
  plot = cov,
  width = 6.5,
  height = 4.8,
  units = "in",
  dpi = 300
)

# ============================================================
# Distribution Plot - LCPA Profile Sizes (SANCTION, no groups)
# ============================================================ 

gam.pf <- LCPA.REG.2.MCMC$param$lcpa$gam.pf

dist_df <- data.frame(
  Profile = factor(
    paste0("Profile ", 1:3),
    levels = paste0("Profile ", 1:3)
  ),
  Probability = as.numeric(gam.pf[, 1, 1])
)

# Profile colors
profile_colors_san <- c(
  "Profile 1" = "#2C5F8A",
  "Profile 2" = "#7B7B7B",
  "Profile 3" = "#D35400"
)

# Substantive trajectory labels
profile_labels_san <- c(
  "Profile 1" = "Profile 1\nSustained Broad Coercion",
  "Profile 2" = "Profile 2\nImposition-Centered\nDirect Coercion",
  "Profile 3" = "Profile 3\nDirect-to-Economic\nPressure"
)

# Plot
p_dist <- ggplot(
  dist_df,
  aes(
    x = Profile,
    y = Probability,
    fill = Profile
  )
) +
  geom_bar(
    stat = "identity",
    color = "black",
    linewidth = 0.3
  ) +
  geom_text(
    aes(
      label = scales::percent(
        Probability,
        accuracy = 0.1
      )
    ),
    vjust = -0.4,
    size = 3.5
  ) +
  scale_fill_manual(
    name = "Latent Profile",
    values = profile_colors_san
  ) +
  scale_x_discrete(
    labels = profile_labels_san
  ) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 1)
  ) +
  labs(
    x = "Latent Profile",
    y = "Proportion",
    title = "Profile Size Distribution",
    subtitle = "LCPA | TIES Sanctions Application"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(
      size = 10,
      lineheight = 0.9
    ),
    axis.text.y = element_text(size = 11),
    axis.title = element_text(
      face = "bold",
      size = 12
    ),
    plot.title = element_text(
      face = "bold",
      hjust = 0.5,
      size = 13
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 10,
      color = "gray40"
    ),
    panel.border = element_rect(
      color = "black",
      fill = NA
    ),
    legend.position = "none"
  )

p_dist

ggsave(
  filename = "Figures/Profile_Distribution_TIES.pdf",
  plot = p_dist,
  width = 6.5,
  height = 4.2,
  units = "in",
  dpi = 300
)