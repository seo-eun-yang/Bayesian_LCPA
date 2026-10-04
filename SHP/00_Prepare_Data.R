################################################################################
######################### Application LCPA - ANES Data #########################
############################### Data Preparation ###############################
################################################################################

## Install & Load Packages

if(!require(dplyr))   install.packages("dplyr")
if(!require(haven))   install.packages("haven")
if(!require(purrr))   install.packages("purrr")
if(!require(readr))   install.packages("readr")
if(!require(tibble))  install.packages("tibble")
if(!require(stringr)) install.packages("stringr")
if(!require(ggplot2)) install.packages("ggplot2")

library(dplyr); library(haven); library(purrr)
library(readr); library(tibble); library(stringr); library(ggplot2)

## Load data 

years <- 1999:2023
files <- sprintf("data/raw/shp/shp%02d_p_user.dta", years %% 100)
shp_data <- map(files, read_dta)
names(shp_data) <- paste0("shp", years)


## Select Waves and Exclude Proxy Interviews

df_2017 <- shp_data$shp2017 %>%
  filter(status17 == 0) %>%
  select(idpers, sex17, age17,
         p17p66, p17p68,          # Affective polarization
         p17p10,                  # Ideological extremity
         p17p13, p17p15, p17p17,  # Issue polarization (3 items)
         p17p45,                  # Social distrust
         p17d92,                  # Gender role attitudes
         p17n50)                  # Religion (Wave 1 only)

df_2020 <- shp_data$shp2020 %>%
  filter(status20 == 0) %>%
  select(idpers,
         p20p66, p20p68,
         p20p10,
         p20p13, p20p15, p20p17,
         p20p45,
         p20d92)

df_2023 <- shp_data$shp2023 %>%
  filter(status23 == 0) %>%
  select(idpers,
         p23p66, p23p68,
         p23p10,
         p23p13, p23p15, p23p17,
         p23p45,
         p23d92)

## Merge Waves

common_ids <- Reduce(intersect, list(df_2017$idpers, df_2020$idpers, 
                                     df_2023$idpers))

df_2017_f <- df_2017 %>% dplyr::filter(idpers %in% common_ids)
df_2020_f <- df_2020 %>% dplyr::filter(idpers %in% common_ids)
df_2023_f <- df_2023 %>% dplyr::filter(idpers %in% common_ids)

merged_df <- df_2017_f %>%
  dplyr::inner_join(df_2020_f, by = "idpers") %>%
  dplyr::inner_join(df_2023_f, by = "idpers") 


# ---- Item 1: Affective Polarization (Partisan Identity) ----
# Strong/moderate partisan attachment = high (2); no attachment = low (1)

recode_affpol <- function(close, strength) {
  case_when(
    is.na(close) | is.na(strength)          ~ NA_real_,
    close == 1 & strength %in% c(1, 2)      ~ 2,   # partisan, strong/moderate
    close == 2 | strength == 3              ~ 1,   # no partisan
    TRUE                                    ~ NA_real_
  )
}

affpol <- tibble(
  AFFPOL_17 = recode_affpol(merged_df$p17p66, merged_df$p17p68),
  AFFPOL_20 = recode_affpol(merged_df$p20p66, merged_df$p20p68),
  AFFPOL_23 = recode_affpol(merged_df$p23p66, merged_df$p23p68)
)


# ---- Item 2: Ideological Extremity ----
# 0-10 left-right scale: 0-2 or 8-10 = extreme (2); 3-7 = moderate (1)

recode_ideol <- function(x) {
  x <- ifelse(x %in% c(-5, -4, -3, -2, -1), NA, x)
  ifelse(x %in% c(0:2, 8:10), 2,
         ifelse(x %in% 3:7, 1, NA))
}

ideoex <- tibble(
  IDEOEX_17 = recode_ideol(merged_df$p17p10),
  IDEOEX_20 = recode_ideol(merged_df$p20p10),
  IDEOEX_23 = recode_ideol(merged_df$p23p10)
)


# ---- Item 3: Issue Polarization ----
# Mean index of 3 policy items (social spending, equal opportunities, taxation)
# Mean > 1.67 = issue-polarized (2); remainder = moderate (1)

recode_policy_item <- function(x) {
  x <- ifelse(x %in% c(-3, -2, -1), NA, x)
  case_when(
    is.na(x)   ~ NA_real_,
    x == 2     ~ 1,          # "neither" = moderate
    x %in% c(1, 3) ~ 2,     # extreme positions
    TRUE       ~ NA_real_
  )
}

compute_isspol <- function(v1, v2, v3) {
  idx <- rowMeans(cbind(
    recode_policy_item(v1),
    recode_policy_item(v2),
    recode_policy_item(v3)
  ), na.rm = TRUE)
  case_when(
    is.nan(idx) ~ NA_real_,
    idx > 1.67  ~ 2,
    TRUE        ~ 1
  )
}

isspol <- tibble(
  ISSPOL_17 = compute_isspol(merged_df$p17p13, merged_df$p17p15, merged_df$p17p17),
  ISSPOL_20 = compute_isspol(merged_df$p20p13, merged_df$p20p15, merged_df$p20p17),
  ISSPOL_23 = compute_isspol(merged_df$p23p13, merged_df$p23p15, merged_df$p23p17)
)


# ---- Item 4: Social Distrust ----
# 0-10 interpersonal trust scale: 0-4 = distrust (2); 5-10 = trust (1)

recode_trust <- function(x) {
  x <- ifelse(x %in% c(-2, -1), NA, x)
  ifelse(x %in% 5:10, 1,
         ifelse(x %in% 0:4, 2, NA))
}

trust <- tibble(
  TRUST_17 = recode_trust(merged_df$p17p45),
  TRUST_20 = recode_trust(merged_df$p20p45),
  TRUST_23 = recode_trust(merged_df$p23p45)
)


# ---- Item 5: Gender Role Attitudes ----
# 0-10 scale: 0-6 = autonomy-oriented (1); 7-10 = traditional-authoritarian (2)

recode_gender <- function(x) {
  x <- ifelse(x %in% c(-5, -4, -3, -2, -1), NA, x)
  ifelse(x %in% 0:6, 1,
         ifelse(x %in% 7:10, 2, NA))
}

gender <- tibble(
  GENDER_17 = recode_gender(merged_df$p17d92),
  GENDER_20 = recode_gender(merged_df$p20d92),
  GENDER_23 = recode_gender(merged_df$p23d92)
)


## Combine All Items 

shp_panel_cleaned <- bind_cols(affpol, ideoex, isspol, trust, gender)


## Covariates 

# Sex: 0 = male, 1 = female 
shp_panel_cleaned$sex <- ifelse(
  df_2017$sex17[match(common_ids, df_2017$idpers)] == 1, 0, 1)

# Religion: 1 = important, 0 = not important 
rel_raw <- merged_df$p17n50
rel_raw <- ifelse(rel_raw %in% c(-2, -1), NA, rel_raw)
shp_panel_cleaned$religion <- ifelse(rel_raw %in% c(1, 2), 1, 0)

# Age group (Wave 1): 1 = 18-34, 2 = 35-59, 3 = 60+
age_raw <- merged_df$age17
age_raw <- ifelse(age_raw < 18, NA, age_raw)
shp_panel_cleaned$age <- case_when(
  age_raw <= 34 ~ 1L,
  age_raw <= 59 ~ 2L,
  age_raw >= 60 ~ 3L,
  TRUE          ~ NA_integer_
)


## Exclude Cases with Missing Covariates 

complete_idx <- complete.cases(
  shp_panel_cleaned[, c("sex", "religion", "age")]
)
shp_panel_cleaned <- shp_panel_cleaned[complete_idx, ]

## Save

write_csv(shp_panel_cleaned, "shp_panel_cleaned.csv")


