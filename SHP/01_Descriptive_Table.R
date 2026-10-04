################################################################################
######################### Application LCPA - SHP Data #########################
############################## Descriptive Table ###############################
################################################################################

if(!require(dplyr))      install.packages("dplyr")
if(!require(kableExtra)) install.packages("kableExtra")
library(dplyr)
library(kableExtra)

# Load Data

shp_panel_cleaned <- read.csv("shp_panel_cleaned.csv")

years <- 1999:2023
files <- sprintf("data/raw/shp/shp%02d_p_user.dta", years %% 100)
shp_data <- map(files, read_dta)
names(shp_data) <- paste0("shp", years)


# Get Relevant Variables 

## Wave 1 - 2017
data_table <- data.frame(
  age_17 = merged_df$age17[complete_idx]
)
data_table$age_17 <- ifelse(data_table$age_17 < 18, NA, data_table$age_17)

# Religion 
data_table$rel_17 <- shp_panel_cleaned$religion

# Sex 
data_table$sex_17 <- shp_panel_cleaned$sex

# Items
data_table$affpol_17  <- shp_panel_cleaned$AFFPOL_17
data_table$ideopol_17 <- shp_panel_cleaned$IDEOEX_17
data_table$issuepol_17 <- shp_panel_cleaned$ISSPOL_17
data_table$socdis_17  <- shp_panel_cleaned$TRUST_17
data_table$gender_17  <- shp_panel_cleaned$GENDER_17


## Wave 2 - 2020

# Age
data_table$age_20 <- shp_data$shp2020$age20[
  match(common_ids, shp_data$shp2020$idpers)]
data_table$age_20 <- ifelse(data_table$age_20 < 18, NA, data_table$age_20)

# Religion
rel_20 <- shp_data$shp2020$p20n50[
  match(common_ids, shp_data$shp2020$idpers)]
rel_20 <- ifelse(rel_20 %in% c(-2, -1), NA, rel_20)
data_table$rel_20 <- ifelse(rel_20 %in% c(1, 2), 1, 0)

# Sex
data_table$sex_20 <- shp_panel_cleaned$sex   # time-invariant

# Items
data_table$affpol_20  <- shp_panel_cleaned$AFFPOL_20
data_table$ideopol_20 <- shp_panel_cleaned$IDEOEX_20
data_table$issuepol_20 <- shp_panel_cleaned$ISSPOL_20
data_table$socdis_20  <- shp_panel_cleaned$TRUST_20
data_table$gender_20  <- shp_panel_cleaned$GENDER_20


## Wave 3 - 2023

# Age
data_table$age_23 <- shp_data$shp2023$age23[
  match(common_ids, shp_data$shp2023$idpers)]
data_table$age_23 <- ifelse(data_table$age_23 < 18, NA, data_table$age_23)

# Religion
rel_23 <- shp_data$shp2023$p23n50[
  match(common_ids, shp_data$shp2023$idpers)]
rel_23 <- ifelse(rel_23 %in% c(-2, -1), NA, rel_23)
data_table$rel_23 <- ifelse(rel_23 %in% c(1, 2), 1, 0)

# Sex
data_table$sex_23 <- shp_panel_cleaned$sex   # time-invariant

# Items
data_table$affpol_23  <- shp_panel_cleaned$AFFPOL_23
data_table$ideopol_23 <- shp_panel_cleaned$IDEOEX_23
data_table$issuepol_23 <- shp_panel_cleaned$ISSPOL_23
data_table$socdis_23  <- shp_panel_cleaned$TRUST_23
data_table$gender_23  <- shp_panel_cleaned$GENDER_23


# Age Groups based on Wave 1 
data_table$age_group <- cut(data_table$age_17,
                            breaks = c(17, 34, 59, Inf),
                            labels = c("18-34", "35-59", "60+"))


# Create Table

pct <- function(x, val) round(mean(x == val, na.rm = TRUE) * 100, 1)

make_wave_row <- function(df, wave, age_var, rel_var) {
  data.frame(
    Wave          = wave,
    N             = nrow(df),
    Mean_Age      = round(mean(df[[age_var]], na.rm = TRUE), 1),
    Pct_Female    = pct(df$sex_17, 1),
    Pct_Religious = pct(df[[rel_var]], 1),
    Pct_AffPol    = pct(df[[paste0("affpol_",   wave)]], 2),
    Pct_IdeoPol   = pct(df[[paste0("ideopol_",  wave)]], 2),
    Pct_IssuePol  = pct(df[[paste0("issuepol_", wave)]], 2),
    Pct_SocDis    = pct(df[[paste0("socdis_",   wave)]], 2),
    Pct_Gender    = pct(df[[paste0("gender_",   wave)]], 2)
  )
}

groups <- c("18-34", "35-59", "60+")
waves  <- list(
  list(wave = "17", age_var = "age_17", rel_var = "rel_17"),
  list(wave = "20", age_var = "age_20", rel_var = "rel_20"),
  list(wave = "23", age_var = "age_23", rel_var = "rel_23")
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
         Pct_AffPol, Pct_IdeoPol, Pct_IssuePol, Pct_SocDis, Pct_Gender) %>%
  arrange(Age_Group, Wave)

desc_table$Wave <- recode(desc_table$Wave,
                          "17" = "2017",
                          "20" = "2020",
                          "23" = "2023")

colnames(desc_table) <- c("Age Group", "Wave", "N", "Mean Age",
                          "% Female", "% Religious",
                          "% Partisan Id.", "% Ideol. Extreme",
                          "% Issue Pol.", "% Social Distrust",
                          "% Gender Role Att.")

# LaTeX Table 
desc_table %>%
  kbl(format   = "latex",
      booktabs = TRUE,
      caption  = "Descriptive Statistics by Age Group and Wave (SHP)",
      label    = "tab:desc_shp",
      digits   = 1) %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  collapse_rows(columns = 1, latex_hline = "major", valign = "middle") %>%
  add_header_above(c(" " = 4,
                     "Covariates" = 2,
                     "Polarization Items" = 5)) %>%
  save_kable("output/tables/desc_shp.tex")