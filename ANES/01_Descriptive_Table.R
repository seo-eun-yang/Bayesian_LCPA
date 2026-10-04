################################################################################
######################### Application LCPA - ANES Data #########################
############################## Descriptive Table ###############################
################################################################################

## Install & Load Packages

if(!require(cat)) install.packages("cat") 
library(cat)

## Get data

anes_panel <- read.csv("anes_panel.csv", sep = ",", header = TRUE)
anes_panel_cleaned <- read.csv("anes_panel_cleaned.csv", sep = ",", header = TRUE)

## Run 00_Prepare_Data.R!

## Get relevant Variables 

# Age
data_table <- data.frame(
  age_16 = anes$V161267[complete_idx]
)
data_table$age_16 <- ifelse(data_table$age_16 == -9 | data_table$age_16 == -8, NA, data_table$age_16)

# Religion
data_table$rel_16 <- anes_panel_cleaned$religion

# Sex
data_table$sex_16 <- anes_panel_cleaned$sex

# Items
data_table$affpol_16 <- anes_panel_cleaned$AFFPOL_16
data_table$ideopol_16 <- anes_panel_cleaned$IDEOEX_16
data_table$issuepol_16 <- anes_panel_cleaned$ISSPOL_16
data_table$socdis_16 <- anes_panel_cleaned$TRUST_16
data_table$authsoc_16 <- anes_panel_cleaned$AUTSOC_16


## Wave 2 - 2020

# Age
data_table$age_20 <- anes$V201507x[complete_idx]
data_table$age_20 <- ifelse(data_table$age_20 == -9 | data_table$age_20 == -8, NA, data_table$age_20)

# Religion
data_table$rel_20 <- anes$V201433[complete_idx]
data_table$rel_20 <- ifelse(data_table$rel_20 == -9, NA, data_table$rel_20)
data_table$rel_20 <- ifelse(data_table$rel_20 == 1 | data_table$rel_20 == 2, 1, 0)

# Sex
data_table$sex_20 <- anes$V201600[complete_idx]
data_table$sex_20 <- ifelse(data_table$sex_20 == -9, NA, data_table$sex_20)
data_table$sex_20 <- ifelse(data_table$sex_20 == 1, 0, 1)

# Items
data_table$affpol_20 <- anes_panel_cleaned$AFFPOL_20
data_table$ideopol_20 <- anes_panel_cleaned$IDEOEX_20
data_table$issuepol_20 <- anes_panel_cleaned$ISSPOL_20
data_table$socdis_20 <- anes_panel_cleaned$TRUST_20
data_table$authsoc_20 <- anes_panel_cleaned$AUTSOC_20


## Wave 3 - 2024

# Age 
data_table$age_24 <- anes$V241458x[complete_idx]
data_table$age_24 <- ifelse(data_table$age_24 == -2, NA, data_table$age_24)

# Religion
data_table$rel_24 <- anes$V241420[complete_idx]
data_table$rel_24 <- ifelse(data_table$rel_24 == -9, NA, data_table$rel_24)
data_table$rel_24 <- ifelse(data_table$rel_24 == 1 | data_table$rel_24 == 2, 1, 0)

# Sex
data_table$sex_24 <- anes$V241550 [complete_idx]
data_table$sex_24 <- ifelse(data_table$sex_24 %in% c(-9, 3), NA, data_table$sex_24)
data_table$sex_24 <- ifelse(data_table$sex_24 == 1, 0, 1)

# Items
data_table$affpol_24 <- anes_panel_cleaned$AFFPOL_24
data_table$ideopol_24 <- anes_panel_cleaned$IDEOEX_24
data_table$issuepol_24 <- anes_panel_cleaned$ISSPOL_24
data_table$socdis_24 <- anes_panel_cleaned$TRUST_24
data_table$authsoc_24 <- anes_panel_cleaned$AUTSOC_24

## Age Groups based on Wave 1
data_table$age_group <- cut(data_table$age_16,
                            breaks = c(17, 34, 59, Inf),
                            labels = c("18-34", "35-59", "60+"))


## Create Table 

pct <- function(x, val) round(mean(x == val, na.rm = TRUE) * 100, 1)

# Table for each Wave
make_wave_row <- function(df, wave, age_var, rel_var) {
  data.frame(
    Wave          = wave,
    N             = sum(!is.na(df[[age_var]])),
    Mean_Age      = round(mean(df[[age_var]], na.rm = TRUE), 1),
    Pct_Female    = pct(df$sex_16, 1),
    Pct_Religious = pct(df[[rel_var]], 1),
    Pct_AffPol    = pct(df[[paste0("affpol_", wave)]], 2),
    Pct_IdeoPol   = pct(df[[paste0("ideopol_", wave)]], 2),
    Pct_IssuePol  = pct(df[[paste0("issuepol_", wave)]], 2),
    Pct_SocDis    = pct(df[[paste0("socdis_", wave)]], 2),
    Pct_AuthSoc   = pct(df[[paste0("authsoc_", wave)]], 2)
  )
}

# For each Wave and Age Group
groups <- c("18-34", "35-59", "60+")
waves  <- list(
  list(wave = "16", age_var = "age_16", rel_var = "rel_16"),
  list(wave = "20", age_var = "age_20", rel_var = "rel_20"),
  list(wave = "24", age_var = "age_24", rel_var = "rel_24")
)

rows <- list()
for(g in groups) {
  df_g <- data_table %>% filter(age_group == g)
  for(w in waves) {
    row <- make_wave_row(df_g, w$wave, w$age_var, w$rel_var)
    row$Age_Group <- g
    rows[[length(rows) + 1]] <- row
  }
}

desc_table <- do.call(rbind, rows) %>%
  select(Age_Group, Wave, N, Mean_Age, Pct_Female, Pct_Religious,
         Pct_AffPol, Pct_IdeoPol, Pct_IssuePol, Pct_SocDis, Pct_AuthSoc) %>%
  arrange(Age_Group, Wave)

# Wave Labels
desc_table$Wave <- recode(desc_table$Wave, "16" = "2016", "20" = "2020",
                          "24" = "2024")

# Rename columns
colnames(desc_table) <- c( "Age Group", "Wave", "N", "Mean Age", "% Female", 
                           "% Religious", "% Affective Pol.", 
                           "% Ideol. Extreme", "% Issue Pol.", 
                           "% Social Distrust", "% Traditional")

# LaTeX Table
desc_table %>%
  kbl(format   = "latex",
      booktabs = TRUE,
      caption  = "Descriptive Statistics by Age Group and Wave (ANES)",
      label    = "tab:desc_anes",
      digits   = 1) %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  collapse_rows(columns = 1, latex_hline = "major", valign = "middle") %>%
  add_header_above(c(" " = 4,
                     "Covariates" = 2,
                     "Polarization Items" = 5))

