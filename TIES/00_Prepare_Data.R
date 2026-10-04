################################################################################
######################### Application LCPA - TIES Data #########################
############################### Data Preparation ###############################
################################################################################

## Install & Load Packages

if(!require(dplyr)) install.packages("dplyr")
library(dplyr)
if(!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
if(!require(readr)) install.packages("readr")
library(readr)

## Load data

ties <- read_csv2("Data/TIESv4-1.csv")


## Get covariables

# Major Powers: USA (2), UK (200), France (220), Russia (365), China (710), EU (1000)

major_power_codes <- c(2, 200, 220, 365, 710, 1000)

ties <- ties %>%
  mutate(
    major_power = if_else(
      primarysender %in% major_power_codes, 1L, 0L
    )
  )

# Cold War: 0 = Cold War (< 1992), 1 = Post-Cold War (>=1992)

ties <- ties %>%
  mutate(
    major_power = case_when(
      primarysender %in% major_power_codes ~ 1L,   # Major Power
      !is.na(primarysender)                ~ 0L,   # Minor/Middle Power
      TRUE                                 ~ NA_integer_  
    )
  )


## Item construction (procedural waves)

ties_proc <- ties %>%
  mutate(
    
    # ----------------------------------------------------------
    # ITEM 1: Coercive Action
    # W1: Threat issued? | W2: Sanctions imposed? | W3: Compliance?
    # ----------------------------------------------------------
    item1_w1 = if_else(threat == 1, 2L, 1L),
    item1_w2 = if_else(imposition == 1, 2L, 1L),
    item1_w3 = case_when(
      finaloutcome %in% c(1,2,5,6,7,10) ~ 2L,  # compliance
      finaloutcome %in% c(3,4,8,9)      ~ 1L,  # stalemate/failure
      TRUE ~ NA_integer_),
    
    # ----------------------------------------------------------
    # ITEM 2: Instrument Breadth
    # W1: Multiple types threatened? | W2: Multiple types imposed? | W3: Sender settlement favorable?
    # ----------------------------------------------------------
    item2_w1 = if_else(str_detect(as.character(sanctiontypethreat), " "), 2L, 1L),
    item2_w2 = if_else(str_detect(as.character(sanctiontype), " "), 2L, 1L),
    item2_w3 = case_when(
      settlementnaturesender >= 6 ~ 2L,
      settlementnaturesender <  6 ~ 1L,
      TRUE ~ NA_integer_),
    
    # ----------------------------------------------------------
    # ITEM 3: Economic Pressure
    # W1: Anticipated costs high? | W2: Realized costs high? | W3: Target settlement favorable?
    # ----------------------------------------------------------
    item3_w1 = case_when(
      anticipatedtargetcosts == 3        ~ 2L,
      anticipatedtargetcosts %in% c(1,2) ~ 1L,
      TRUE ~ NA_integer_),
    item3_w2 = case_when(
      targetcosts == 3        ~ 2L,
      targetcosts %in% c(1,2) ~ 1L,
      TRUE ~ NA_integer_),
    item3_w3 = case_when(
      settlementnaturetarget >= 6 ~ 2L,
      settlementnaturetarget <  6 ~ 1L,
      TRUE ~ NA_integer_),
    
    # ----------------------------------------------------------
    # ITEM 4: Sender Commitment
    # W1: Strong commitment? | W2: Sender costs high? | W3: Sender outcome favorable?
    # ----------------------------------------------------------
    item4_w1 = case_when(
      scommit == 3        ~ 2L,
      scommit %in% c(1,2) ~ 1L,
      TRUE ~ NA_integer_),
    item4_w2 = case_when(
      sendercosts >= 2 ~ 2L,
      sendercosts == 1 ~ 1L,
      TRUE ~ NA_integer_),
    item4_w3 = case_when(
      settlementnaturesender >= 7 ~ 2L,
      settlementnaturesender <  7 ~ 1L,
      TRUE ~ NA_integer_),
    
    # ----------------------------------------------------------
    # ITEM 5: Multilateral Participation
    # W1: IO-backed? | W2: Multiple senders? | W3: Negotiated settlement?
    # ----------------------------------------------------------
    item5_w1 = if_else(institution == 1, 2L, 1L, missing = 1L),
    item5_w2 = if_else(!is.na(sender2), 2L, 1L),
    item5_w3 = case_when(
      finaloutcome %in% c(5, 10) ~ 2L,
      !is.na(finaloutcome)       ~ 1L,
      TRUE ~ NA_integer_)
  )

# Combine All Items 
ties_cleaned <- ties_proc %>%
  select(item1_w1, item1_w2, item1_w3, item2_w1, item2_w2, item2_w3,
         item3_w1, item3_w2, item3_w3, item4_w1, item4_w2, item4_w3,
         item5_w1, item5_w2, item5_w3, coldwar_era, major_power)


write_csv2(ties_cleaned, "TIESv4-1_cleaned.csv")
