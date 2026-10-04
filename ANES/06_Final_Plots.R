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

LCPA.REG.2.MCMC <- readRDS("Data/LCPA_ANES_5000.rds")

data_lcpa <- read.csv("Data/anes_panel_cleaned.csv")

data <- list()

data$is.grp <- TRUE
data$group  <- data_lcpa$age

data$lcpa$si <- list()
nkase <- nrow(data_lcpa)
data$lcpa$si <-array(1,c(nkase,5,3)) 

data$lcpa$si[,1,1] <- data_lcpa$AFFPOL_16
data$lcpa$si[,2,1] <- data_lcpa$IDEOEX_16
data$lcpa$si[,3,1] <- data_lcpa$ISSPOL_16
data$lcpa$si[,4,1] <- data_lcpa$TRUST_16
data$lcpa$si[,5,1] <- data_lcpa$AUTSOC_16

data$lcpa$si[,1,2] <- data_lcpa$AFFPOL_20
data$lcpa$si[,2,2] <- data_lcpa$IDEOEX_20
data$lcpa$si[,3,2] <- data_lcpa$ISSPOL_20
data$lcpa$si[,4,2] <- data_lcpa$TRUST_20
data$lcpa$si[,5,2] <- data_lcpa$AUTSOC_20

data$lcpa$si[,1,3] <- data_lcpa$AFFPOL_24
data$lcpa$si[,2,3] <- data_lcpa$IDEOEX_24
data$lcpa$si[,3,3] <- data_lcpa$ISSPOL_24
data$lcpa$si[,4,3] <- data_lcpa$TRUST_24
data$lcpa$si[,5,3] <- data_lcpa$AUTSOC_24


##################### 
# Rearrange big.rho #
#####################

rho <- LCPA.REG.2.MCMC$param$lcpa$big.rho

rho_long <- as.data.frame.table(
  rho[, 2, , , ,], responseName = "Pr_item2")

colnames(rho_long) <- c("Item", "Stage", "Time", "Group", "Pr_item2")

item_labels <- c(
  "sItem.1" = "Affective\nPolarization",
  "sItem.2" = "Ideological\nExtremity",
  "sItem.3" = "Issue\nExtremity",
  "sItem.4" = "Social\nDistrust",
  "sItem.5" = "Authoritarian\nValues"
)

rho_long$Item_label <- factor(
  rho_long$Item,
  levels = names(item_labels),
  labels = item_labels)

group_labels <- c(
  "Group.1" = "Age: 18-34",
  "Group.2" = "Age: 35-59",
  "Group.3" = "Age: 60+")

stage_labels <- c(
  "Stage.1" = "Class 1",
  "Stage.2" = "Class 2", 
  "Stage.3" = "Class 3")


###################################### 
# Create Item Responsibility Barplot #
######################################

rho_long$Stage_mapped <- as.character(rho_long$Stage)
rho_long$Group <- as.character(rho_long$Group)

# Group 1
idx <- rho_long$Group == "Group.1"
rho_long$Stage_mapped[idx & rho_long$Stage == "Stage.1"] <- "Authoritarian–Affective\n-Polarization"
rho_long$Stage_mapped[idx & rho_long$Stage == "Stage.2"] <- "Ideological–Affective\n-Polarization"
rho_long$Stage_mapped[idx & rho_long$Stage == "Stage.3"] <- "Low-Polarization"

# Group 2
idx <- rho_long$Group == "Group.2"
rho_long$Stage_mapped[idx & rho_long$Stage == "Stage.1"] <- "Low-Polarization"
rho_long$Stage_mapped[idx & rho_long$Stage == "Stage.2"] <- "Ideological–Affective\n-Polarization"
rho_long$Stage_mapped[idx & rho_long$Stage == "Stage.3"] <- "Authoritarian–Affective\n-Polarization"

# Group 3
idx <- rho_long$Group == "Group.3"
rho_long$Stage_mapped[idx & rho_long$Stage == "Stage.1"] <- "Ideological–Affective\n-Polarization"
rho_long$Stage_mapped[idx & rho_long$Stage == "Stage.2"] <- "Low-Polarization"
rho_long$Stage_mapped[idx & rho_long$Stage == "Stage.3"] <- "Authoritarian–Affective\n-Polarization"

# Desired Order
rho_long$Stage_mapped <- factor(rho_long$Stage_mapped,
                                levels = c("Authoritarian–Affective\n-Polarization",  
                                           "Ideological–Affective\n-Polarization",
                                           "Low-Polarization"))


# Plot 

library(ggpattern)

item_combo <- ggplot(rho_long, aes(x       = Stage_mapped, 
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
  facet_wrap(~ Group, ncol = 1, labeller = labeller(Group = group_labels)) +
  scale_fill_manual(
    name = "Item",
    values = c(
      "Affective\nPolarization"      = "#2C3E7A",   
      "Ideological\nExtremity"       = "#5B8DB8",   
      "Issue\nExtremity"          = "#A8C4D4",   
      "Social\nDistrust"             = "#D4A96A",   
      "Authoritarian\nValues" = "#8B4513"    
    )
  ) +
  scale_pattern_manual(
    name = "Item",
    values = c(
      "Affective\nPolarization"      = "none",
      "Ideological\nExtremity"       = "stripe",
      "Issue\nExtremity"          = "crosshatch",
      "Social\nDistrust"             = "circle",
      "Authoritarian\nValues" = "wave"
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
    title = "Item Response Probabilities by Latent Class and Age Group"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x      = element_text(size = 11),
    axis.text.y      = element_text(size = 11),
    axis.title       = element_text(face = "bold", size = 12),
    legend.title     = element_text(face = "bold", size = 11),
    legend.text      = element_text(size = 10),
    plot.title       = element_text(face = "bold", hjust = 0.5, size = 13),
    strip.text       = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "white", color = "black"),
    panel.border     = element_rect(color = "black", fill = NA),
    legend.position  = "right"
  )

item_combo

ggsave(
  filename = "Figures/Item_Response.png",
  plot = item_combo,
  width = 7.1,
  height = 3.6,
  dpi = 600
)

################################################ 
# Prepare Class-Membership by Profile Timeline #
################################################

eta <- LCPA.REG.2.MCMC$param$lcpa$eta

eta_long <- as.data.frame(as.table(eta))
eta_long[,4] <- NULL
colnames(eta_long) <- c("Class", "Time", "Profile", "Group", "Value")  

eta_long$Class   <- gsub("Stage=|Stage\\.", "", as.character(eta_long$Class))
eta_long$Time    <- gsub("Time=|Time\\.", "", as.character(eta_long$Time))
eta_long$Profile <- gsub("Profile=|Profile\\.", "", as.character(eta_long$Profile))
eta_long$Group   <- gsub("Group=|Group\\.", "", as.character(eta_long$Group))

eta_long$Class   <- factor(eta_long$Class, 
                           levels = c("1","2","3"),
                           labels = c("Class 1", "Class 2", "Class 3"))

eta_long$Time    <- factor(eta_long$Time, 
                           levels = c("1","2","3"),
                           labels = c("2016", "2020", "2024"))

eta_long$Profile <- factor(eta_long$Profile, 
                           levels = c("1","2","3"),
                           labels = c("Profile 1", "Profile 2", "Profile 3"))

eta_long$Group   <- factor(eta_long$Group, 
                           levels = c("1","2","3"), 
                           labels = c("Age: 18-34", "Age: 35-59", "Age: 60+"))

eta_long$Class_mapped <- as.character(eta_long$Class)


############################################### 
# Create Class-Membership by Profile Timeline #
###############################################

# Group 18-34
idx <- eta_long$Group == "Age: 18-34"
eta_long$Class_mapped[idx & eta_long$Class == "Class 1"] <- "Authoritarian–Affective\n-Polarization"
eta_long$Class_mapped[idx & eta_long$Class == "Class 2"] <- "Ideological–Affective\n-Polarization"
eta_long$Class_mapped[idx & eta_long$Class == "Class 3"] <- "Low-Polarization"

# Group 35-59
idx <- eta_long$Group == "Age: 35-59"
eta_long$Class_mapped[idx & eta_long$Class == "Class 1"] <- "Low-Polarization"
eta_long$Class_mapped[idx & eta_long$Class == "Class 2"] <- "Ideological–Affective\n-Polarization"
eta_long$Class_mapped[idx & eta_long$Class == "Class 3"] <- "Authoritarian–Affective\n-Polarization"

# Group 60+
idx <- eta_long$Group == "Age: 60+"
eta_long$Class_mapped[idx & eta_long$Class == "Class 1"] <- "Ideological–Affective\n-Polarization"
eta_long$Class_mapped[idx & eta_long$Class == "Class 2"] <- "Low-Polarization"
eta_long$Class_mapped[idx & eta_long$Class == "Class 3"] <- "Authoritarian–Affective\n-Polarization"

eta_long$Class_mapped <- factor(eta_long$Class_mapped,
                                levels = c("Authoritarian–Affective\n-Polarization", "Ideological–Affective\n-Polarization", "Low-Polarization"))

eta_long$Profile_mapped <- as.character(eta_long$Profile)

# Age 18-34
idx <- eta_long$Group == "Age: 18-34"
eta_long$Profile_mapped[idx & eta_long$Profile == "Profile 1"] <- "Profile 2"
eta_long$Profile_mapped[idx & eta_long$Profile == "Profile 2"] <- "Profile 1"
eta_long$Profile_mapped[idx & eta_long$Profile == "Profile 3"] <- "Profile 3"

# Age 35-59
idx <- eta_long$Group == "Age: 35-59"
eta_long$Profile_mapped[idx & eta_long$Profile == "Profile 1"] <- "Profile 1"
eta_long$Profile_mapped[idx & eta_long$Profile == "Profile 2"] <- "Profile 3"
eta_long$Profile_mapped[idx & eta_long$Profile == "Profile 3"] <- "Profile 2"

# Age 60+
idx <- eta_long$Group == "Age: 60+"
eta_long$Profile_mapped[idx & eta_long$Profile == "Profile 1"] <- "Profile 3"
eta_long$Profile_mapped[idx & eta_long$Profile == "Profile 2"] <- "Profile 1"
eta_long$Profile_mapped[idx & eta_long$Profile == "Profile 3"] <- "Profile 2"

eta_long$Profile_mapped <- factor(eta_long$Profile_mapped,
                                  levels = c("Profile 1", "Profile 2", "Profile 3"))

# Plot 
time <- ggplot(eta_long, aes(x        = Time, 
                             y        = Value, 
                             color    = Class_mapped, 
                             shape    = Class_mapped,
                             linetype = Class_mapped,
                             group    = Class_mapped)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  facet_grid(Profile_mapped ~ Group, scales = "free_y") +
  scale_color_manual(
    name = "Latent Class",
    values = c(
      "Authoritarian–Affective\n-Polarization" = "#DC143C",   
      "Low-Polarization"     = "#7B7B7B",   
      "Ideological–Affective\n-Polarization"      = "#003399" 
    )
  ) +
  scale_shape_manual(
    name = "Latent Class",
    values = c(
      "Authoritarian–Affective\n-Polarization" = 16,
      "Low-Polarization"     = 17,
      "Ideological–Affective\n-Polarization"      = 15
    )
  ) +
  scale_linetype_manual(
    name = "Latent Class",
    values = c(
      "Authoritarian–Affective\n-Polarization" = "solid",
      "Low-Polarization"     = "solid",
      "Ideological–Affective\n-Polarization"      = "solid"
    )
  ) +
  labs(
    x     = "Time Period",
    y = expression(paste("Probability of Class Membership (", eta, ")")),
    title = "Latent Class Membership Probabilities by Profile"
  ) +
  theme_classic(base_size = 12) +
  theme(
    strip.text       = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "white", color = "black"),
    panel.border     = element_rect(color = "black", fill = NA),
    axis.title       = element_text(face = "bold", size = 12),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y      = element_text(size = 11),
    plot.title       = element_text(face = "bold", hjust = 0.5, size = 11),
    legend.title     = element_text(face = "bold", size = 8.5),
    legend.text      = element_text(size = 8.5),
    legend.position  = "top"
  )
time

ggsave(
  filename = "Figures/Class_Membership_ANES.pdf",
  plot = time,
  width = 5.2,
  height = 5.8,
  units = "in",
  dpi = 300
)

  ################################
  # Check Structure of Parameter #
  ################################

beta_list       <- LCPA.REG.2.MCMC$param.chain$lcpa$beta.gam.pf
beta_array_full <- simplify2array(beta_list)
beta_array_full <- aperm(beta_array_full, c(5, 1, 2, 3, 4))
beta_array_full <- beta_array_full[,,,1,]

  ###############################
  # Recenter for each iteration #
  ###############################

# Reference: Conservative-Profil for each group
#   Group 1 (18-34): Profile 2 = Conservative
#   Group 2 (35-59): Profile 1 = Conservative
#   Group 3 (60+):   Profile 2 = Conservative

beta_recentered <- beta_array_full

# Group 1 (18-34)
ref_g1 <- beta_array_full[, , 2, 1]
for(p in 1:3){
  beta_recentered[, , p, 1] <- beta_array_full[, , p, 1] - ref_g1
}

# Group 2 (35-59)
ref_g2 <- beta_array_full[, , 1, 2]
for(p in 1:3){
  beta_recentered[, , p, 2] <- beta_array_full[, , p, 2] - ref_g2
}

# Group 3 (60+)
ref_g3 <- beta_array_full[, , 2, 3]
for(p in 1:3){
  beta_recentered[, , p, 3] <- beta_array_full[, , p, 3] - ref_g3
}

  #########################################
  # Get Statistics from recentered chains #
  #########################################

groups   <- c("Age: 18-34", "Age: 35-59", "Age: 60+")
profiles <- c("Profile.1",  "Profile.2",  "Profile.3")
cov_names <- c("Intercept", "Religion", "Sex")

results_rc <- data.frame()

for(g in 1:3){
  for(p in 1:3){
    for(cov in 2:3){  # nur Religion und Sex
      samples <- beta_recentered[, cov, p, g]
      
      # Get Bayesian p-value
      prob_positive <- mean(samples > 0)
      prob_negative <- mean(samples < 0)
      pd <- max(prob_positive, prob_negative)  
      p_val <- 2 * (1 - pd)    
      
      sig <- ifelse(p_val < 0.001, "***",
                    ifelse(p_val < 0.01,  "**",
                           ifelse(p_val < 0.05,  "*",
                                  ifelse(p_val < 0.1,   ".", ""))))
      
      results_rc <- rbind(results_rc, data.frame(
        Group     = groups[g],
        Profile   = profiles[p],
        Covariate = cov_names[cov],
        Mean      = mean(samples),
        CI_lo     = quantile(samples, 0.025),
        CI_hi     = quantile(samples, 0.975),
        P_Value   = p_val,
        Sig       = sig
      ))
    }
  }
}

  ##########################################
  # Assign each Profile with correct label #
  ##########################################

results_rc$Profile_mapped <- NA

# Age 18-34: P1=Conservative, P2=Liberal, P3=Moderate
idx <- results_rc$Group == "Age: 18-34"
results_rc$Profile_mapped[idx & results_rc$Profile == "Profile.1"] <- "Low-Polarization"
results_rc$Profile_mapped[idx & results_rc$Profile == "Profile.2"] <- "Authoritarian–Affective\n-Polarization"
results_rc$Profile_mapped[idx & results_rc$Profile == "Profile.3"] <- "Ideological–Affective\n-Polarization"

# Age 35-59: P1=Liberal, P2=Conservative, P3=Moderate
idx <- results_rc$Group == "Age: 35-59"
results_rc$Profile_mapped[idx & results_rc$Profile == "Profile.1"] <- "Authoritarian–Affective\n-Polarization"
results_rc$Profile_mapped[idx & results_rc$Profile == "Profile.2"] <- "Ideological–Affective\n-Polarization"
results_rc$Profile_mapped[idx & results_rc$Profile == "Profile.3"] <- "Low-Polarization"

# Age 60+: P1=Liberal, P2=Conservative, P3=Moderate
idx <- results_rc$Group == "Age: 60+"
results_rc$Profile_mapped[idx & results_rc$Profile == "Profile.1"] <- "Ideological–Affective\n-Polarization"
results_rc$Profile_mapped[idx & results_rc$Profile == "Profile.2"] <- "Authoritarian–Affective\n-Polarization"
results_rc$Profile_mapped[idx & results_rc$Profile == "Profile.3"] <- "Low-Polarization"

results_rc$Profile_mapped <- factor(results_rc$Profile_mapped,
                                    levels = c("Authoritarian–Affective\n-Polarization", "Low-Polarization", "Ideological–Affective\n-Polarization"))

# Filter Conservative
plot_data <- results_rc %>% filter(Profile_mapped != "Authoritarian–Affective\n-Polarization")


  ################################
  # Create Covariate Effect Plot #
  ################################

# Plot Final

cov <- ggplot(plot_data, aes(x = Profile_mapped, y = Mean,
                             color = Group, shape = Group)) +
  geom_point(size = 4, position = position_dodge(width = 0.6)) +
  geom_errorbar(
    aes(ymin = CI_lo, ymax = CI_hi),
    width     = 0.2,
    linewidth = 0.8,
    position  = position_dodge(width = 0.6)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "black", linewidth = 0.8) +
  facet_wrap(~ Covariate, scales = "free_y") +
  scale_color_manual(
    name = "Age Group",
    values = c(
      "Age: 18-34" = "#2C5F8A",   
      "Age: 35-59" = "#7B7B7B",   
      "Age: 60+"   = "#D35400"    
    )
  ) +
  scale_shape_manual(
    name = "Age Group",
    values = c(
      "Age: 18-34" = 16,
      "Age: 35-59" = 17,
      "Age: 60+"   = 15
    )
  ) +
  labs(
    title    = "Covariate Effects on Profile Membership by Age Group",
    subtitle = "Coefficients relative to Authoritarian–Affective Polarization profile;\n95% credible intervals",
    y        = "Coefficient (log-odds)",
    x        = "Latent Profile"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position  = "bottom",
    plot.title       = element_text(face = "bold", hjust = 0.5, size = 13),
    plot.subtitle    = element_text(hjust = 0.5, size = 10, color = "gray40"),
    strip.text       = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "white", color = "black"),
    panel.border     = element_rect(color = "black", fill = NA),
    axis.title       = element_text(face = "bold", size = 10),
    axis.text        = element_text(size = 8),
    legend.title     = element_text(face = "bold", size = 10),
    legend.text      = element_text(size = 10)
  )
cov 

ggsave(
  filename = "Figures/Covariate_Effects_ANES.pdf",
  plot = cov,
  width    = 5.2,
  height   = 4.8,
  units    = "in",
  dpi      = 300
)


# ============================================================
# Distribution Plot - LCPA Profile Sizes by Group 
# ============================================================ 
gam.pf <- LCPA.REG.2.MCMC$param$lcpa$gam.pf

dist_df <- data.frame()
for(G in 1:3){
  for(p in 1:3){
    dist_df <- rbind(dist_df, data.frame(
      Group        = paste0("Group.", G),
      Profile_orig = paste0("Profile.", p),
      Probability  = gam.pf[p, 1, G]
    ))
  }
}


dist_df$Profile_mapped <- NA

# Age 18-34
idx <- dist_df$Group == "Group.1"
dist_df$Profile_mapped[idx & dist_df$Profile_orig == "Profile.1"] <- "Profile 2"
dist_df$Profile_mapped[idx & dist_df$Profile_orig == "Profile.2"] <- "Profile 1"
dist_df$Profile_mapped[idx & dist_df$Profile_orig == "Profile.3"] <- "Profile 3"

# Age 35-59
idx <- dist_df$Group == "Group.2"
dist_df$Profile_mapped[idx & dist_df$Profile_orig == "Profile.1"] <- "Profile 1"
dist_df$Profile_mapped[idx & dist_df$Profile_orig == "Profile.2"] <- "Profile 3"
dist_df$Profile_mapped[idx & dist_df$Profile_orig == "Profile.3"] <- "Profile 2"

# Age 60+
idx <- dist_df$Group == "Group.3"
dist_df$Profile_mapped[idx & dist_df$Profile_orig == "Profile.1"] <- "Profile 3"
dist_df$Profile_mapped[idx & dist_df$Profile_orig == "Profile.2"] <- "Profile 1"
dist_df$Profile_mapped[idx & dist_df$Profile_orig == "Profile.3"] <- "Profile 2"

dist_df$Profile_mapped <- factor(dist_df$Profile_mapped,
                                 levels = c("Profile 1", "Profile 2", "Profile 3"))
dist_df$Group <- factor(dist_df$Group,
                        levels = paste0("Group.", 1:3),
                        labels = c("Age: 18-34", "Age: 35-59", "Age: 60+"))

# Plot 

profile_colors <- c(
  "Profile 1" = "#DC143C",  
  "Profile 2" = "#7B7B7B",   
  "Profile 3" = "#003399"   
)

p_dist <- ggplot(dist_df, aes(x = Profile_mapped, y = Probability, 
                              fill = Profile_mapped)) +
  geom_bar(stat = "identity",
           color = "black", linewidth = 0.3) +
  geom_text(aes(label = scales::percent(Probability, accuracy = 0.1)),
            vjust = -0.4, size = 3.5) +
  scale_fill_manual(
    name   = "Latent Profile",
    values = profile_colors,
    labels = c(
      "Profile 1" = "Profile 1 (Authoritarian–Affective\n-Polarization)",
      "Profile 2" = "Profile 2 (Low-Polarization)",
      "Profile 3" = "Profile 3 (Ideological–Affective\n-Polarization)"
    ) 
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  facet_wrap(~ Group, ncol = 3) +
  labs(
    x        = "Latent Profile",
    y        = "Proportion",
    title    = "Profile Size Distribution by Age Group",
    subtitle = "LCPA | ANES Final Model\nProfile 1 = Authoritarian–Affective-Polarization, Profile 2 = Low-Polarization, \nProfile 3 = Ideological–Affective-Polarization"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text        = element_text(size = 9),
    axis.title       = element_text(face = "bold", size = 12),
    plot.title       = element_text(face = "bold", hjust = 0.5, size = 13),
    plot.subtitle    = element_text(hjust = 0.5, size = 10, color = "gray40"),
    strip.text       = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "white", color = "black"),
    panel.border     = element_rect(color = "black", fill = NA),
    #legend.position  = "bottom",
    legend.title     = element_text(face = "bold", size = 11),
    legend.text      = element_text(size = 10),
    legend.position  = "none" 
  )

ggsave(
  filename = "Figures/Profile_Distribution_ANES.pdf",
  plot = p_dist,
  width = 6.5,
  height = 3.8,
  units = "in",  
  dpi = 300
)



