################################################################################
######################### Application LCPA - ANES Data #########################
############################### Data Preparation ###############################
################################################################################

## Install & Load Packages

if(!require(dplyr)) install.packages("dplyr")
library(dplyr)
if(!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)


## Load data 

anes <- read.csv("anes_panel.csv", sep = ",", header = TRUE)


## Exclude Missing Values from last wave

anes <- anes[!is.na(anes$V240001), ]


## Create Items 

# ---- Item 1: Affective Polarization ----
# Absolute difference between feeling thermometer scores for Democratic and Republican parties
# 1 = low (< 50 points), 2 = high (>= 50 points)

vars_dem <- c("V161086", "V201151", "V241156")
vars_rep <- c("V161087", "V201152", "V241157")

ft_clean <- anes %>%
  select(all_of(c(vars_dem, vars_rep))) %>%
  mutate(across(everything(),
                ~ ifelse(. %in% c(-9, -99, -3, -2, -1, -4, -5), NA, .)))

affpol <- ft_clean %>%
  mutate(
    AFFPOL_16 = ifelse(
      !is.na(V161086) & !is.na(V161087),
      ifelse(abs(V161086 - V161087) >= 50, 2, 1), NA),
    AFFPOL_20 = ifelse(
      !is.na(V201151) & !is.na(V201152),
      ifelse(abs(V201151 - V201152) >= 50, 2, 1), NA),
    AFFPOL_24 = ifelse(
      !is.na(V241156) & !is.na(V241157),
      ifelse(abs(V241156 - V241157) >= 50, 2, 1), NA)
  ) %>%
  select(AFFPOL_16, AFFPOL_20, AFFPOL_24)

# ---- Item 2: Ideological Extremity ----
# 7-point liberal-conservative self-placement scale
# 1-2 or 6-7 = extreme (2); 3-5 = moderate (1)

recode_7pt_extreme <- function(x) {
  x <- ifelse(x %in% c(-9, -99, -3, -2, -1, -4, -5), NA, x)
  ifelse(x %in% c(1, 2, 6, 7), 2,
         ifelse(x %in% 3:5, 1, NA))
}

ideoex <- anes %>%
  select(V161126, V201200, V241177) %>%
  mutate(
    IDEOEX_16 = recode_7pt_extreme(V161126),
    IDEOEX_20 = recode_7pt_extreme(V201200),
    IDEOEX_24 = recode_7pt_extreme(V241177)
  ) %>%
  select(IDEOEX_16, IDEOEX_20, IDEOEX_24)

# ---- Item 3: Issue Polarization ----
# 7-point government spending scale
# 1-2 or 6-7 = issue-polarized (2); 3-5 = moderate (1)

isspol <- anes %>%
  select(V161178, V201246, V241239) %>%
  mutate(
    ISSPOL_16 = recode_7pt_extreme(V161178),
    ISSPOL_20 = recode_7pt_extreme(V201246),
    ISSPOL_24 = recode_7pt_extreme(V241239)
  ) %>%
  select(ISSPOL_16, ISSPOL_20, ISSPOL_24)

# ---- Item 4: Social Distrust ----
# Standard interpersonal trust item (5 categories)
# 1-3 = trust (1); 4-5 = distrust (2)

recode_trust <- function(x) {
  x <- ifelse(x %in% c(-9, -99, -3, -2, -1, -4, -5), NA, x)
  ifelse(x %in% 1:3, 1,
         ifelse(x %in% 4:5, 2, NA))
}

trust <- anes %>%
  select(V161219, V201237, V241234) %>%
  mutate(
    TRUST_16 = recode_trust(V161219),
    TRUST_20 = recode_trust(V201237),
    TRUST_24 = recode_trust(V241234)
  ) %>%
  select(TRUST_16, TRUST_20, TRUST_24)

# ---- Item 5: Authoritarian Socialization ----
# Child-rearing values index (4 forced-choice pairs per wave)
# Items coded: 1 = autonomy, 2 = authoritarian
# Item 3 is reverse-coded (1 = authoritarian, 2 = autonomy)
# Row mean > 0.5 = authoritarian (2); <= 0.5 = autonomy/tie (1)
# NA only when all 4 items are missing

create_auth_score <- function(data, var_names) {
  temp <- data %>%
    select(all_of(var_names)) %>%
    mutate(across(everything(), 
                  ~ ifelse(. %in% c(-9, -8, -99, -3, -2, -1, -4, -5), 
                           NA, .)))
  
  auth_rec <- temp %>%
    mutate(
      !!var_names[1] := case_when(
        .data[[var_names[1]]] == 1 ~ 0,
        .data[[var_names[1]]] == 2 ~ 1,
        TRUE ~ NA_real_),
      !!var_names[2] := case_when(
        .data[[var_names[2]]] == 1 ~ 0,
        .data[[var_names[2]]] == 2 ~ 1,
        TRUE ~ NA_real_),
      !!var_names[3] := case_when(        # reverse coded
        .data[[var_names[3]]] == 1 ~ 1,
        .data[[var_names[3]]] == 2 ~ 0,
        TRUE ~ NA_real_),
      !!var_names[4] := case_when(
        .data[[var_names[4]]] == 1 ~ 0,
        .data[[var_names[4]]] == 2 ~ 1,
        TRUE ~ NA_real_)
    )
  
  auth_mean <- rowMeans(auth_rec, na.rm = TRUE)
  n_valid   <- rowSums(!is.na(auth_rec))
  
  case_when(
    n_valid == 0     ~ NA_real_,
    auth_mean > 0.5  ~ 2,
    auth_mean <= 0.5 ~ 1,
    TRUE             ~ NA_real_
  )
}

autsoc <- tibble(
  AUTSOC_16 = create_auth_score(anes, 
                                c("V162239", "V162240", "V162241", "V162242")),
  AUTSOC_20 = create_auth_score(anes, 
                                c("V202266", "V202267", "V202268", "V202269")),
  AUTSOC_24 = create_auth_score(anes, 
                                c("V242260", "V242261", "V242262", "V242263"))
)

# Combine All Items 
anes_panel_cleaned <- bind_cols(
  affpol, ideoex, isspol, trust, autsoc
)


## Add Group and Covariables

# Sex: 0 = male, 1 = female 

anes_panel_cleaned$sex <- anes$V161342
anes_panel_cleaned$sex <- ifelse(anes_panel_cleaned$sex %in% c(-9, 3), NA, 
                                 anes_panel_cleaned$sex)
anes_panel_cleaned$sex <- ifelse(anes_panel_cleaned$sex == 1, 0, 1)

# Religion: 0 = not important, 1 = important 

anes_panel_cleaned$religion <- anes$V161241
anes_panel_cleaned$religion <- ifelse(anes_panel_cleaned$religion %in% c(-9, -8), NA, 
                                      anes_panel_cleaned$religion)
anes_panel_cleaned$religion <- ifelse(anes_panel_cleaned$religion == 2, 0 , 1)

# Age: 1 = 18-34, 2 = 35-59, 3 = 60+

age_groups <- ifelse(anes$V161267x == -2, NA, anes$V161267x)
age_groups <- ifelse(age_groups == 2 | age_groups == 3 | age_groups == 4, 1, age_groups)
age_groups <- ifelse(age_groups == 5 | age_groups == 6 | age_groups == 7 |
                       age_groups == 8 | age_groups == 9, 2, age_groups)
age_groups <- ifelse(age_groups == 10 | age_groups == 11 | age_groups == 12 |
                       age_groups == 13, 3, age_groups)
anes_panel_cleaned$age <- age_groups


## no missing values in covariables 

cov_df <- data.frame(
  sex = anes_panel_cleaned$sex,
  rel = anes_panel_cleaned$religion,
  age = anes_panel_cleaned$age
)

complete_idx <- complete.cases(cov_df)
cov_df <- cov_df[complete_idx, ]
anes_panel_cleaned <- anes_panel_cleaned[complete_idx, , ]

summary(anes_panel_cleaned)
str(anes_panel_cleaned)


## Add raw Ideology score for plots 

anes_panel_cleaned$ideology_w1 <- anes$V161126[complete_idx]
anes_panel_cleaned$ideology_w1 <- ifelse(
  anes_panel_cleaned$ideology_w1 %in% c(-9, -8, 99), NA, 
  anes_panel_cleaned$ideology_w1)


## Save 

saveRDS(anes_panel_cleaned, "data/processed/anes_panel_cleaned.rds")

