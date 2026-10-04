################################################################################
######################### Application LCPA - TIES Data #########################
############################## Descriptive Table ###############################
################################################################################


### Load data ### 

ties_proc <- read_csv2("TIESv4-1_cleaned.csv")

### Exclude missing cases ###
nrow(ties_proc)
ties_proc <- ties_proc %>% 
  filter(!is.na(major_power))
nrow(ties_proc)


library(dplyr)
library(kableExtra)

# ============================================================
# Descriptive Statistics - SANCTIONS (no groups, 3 waves)
# ============================================================

# Wave labels
wave_labels <- c("1" = "Threat", "2" = "Imposition", "3" = "Resolution")

# ============================================================
# Covariates
# ============================================================
cov_stats <- data.frame(
  Wave      = c("Threat", "Imposition", "Resolution"),
  N         = rep(nrow(ties_proc), 3),
  Pct_ColdWar     = rep(round(mean(ties_proc$coldwar_era == 1, na.rm=TRUE) * 100, 1), 3),
  Pct_MajorPower  = rep(round(mean(ties_proc$major_power == 1, na.rm=TRUE) * 100, 1), 3)
)

# ============================================================
# Item proportions per wave (% = 2)
# ============================================================
item_stats <- data.frame(
  Wave = c("Threat", "Imposition", "Resolution"),
  
  Pct_Item1 = c(
    round(mean(ties_proc$item1_w1 == 2, na.rm=TRUE) * 100, 1),
    round(mean(ties_proc$item1_w2 == 2, na.rm=TRUE) * 100, 1),
    round(mean(ties_proc$item1_w3 == 2, na.rm=TRUE) * 100, 1)
  ),
  Pct_Item2 = c(
    round(mean(ties_proc$item2_w1 == 2, na.rm=TRUE) * 100, 1),
    round(mean(ties_proc$item2_w2 == 2, na.rm=TRUE) * 100, 1),
    round(mean(ties_proc$item2_w3 == 2, na.rm=TRUE) * 100, 1)
  ),
  Pct_Item3 = c(
    round(mean(ties_proc$item3_w1 == 2, na.rm=TRUE) * 100, 1),
    round(mean(ties_proc$item3_w2 == 2, na.rm=TRUE) * 100, 1),
    round(mean(ties_proc$item3_w3 == 2, na.rm=TRUE) * 100, 1)
  ),
  Pct_Item4 = c(
    round(mean(ties_proc$item4_w1 == 2, na.rm=TRUE) * 100, 1),
    round(mean(ties_proc$item4_w2 == 2, na.rm=TRUE) * 100, 1),
    round(mean(ties_proc$item4_w3 == 2, na.rm=TRUE) * 100, 1)
  ),
  Pct_Item5 = c(
    round(mean(ties_proc$item5_w1 == 2, na.rm=TRUE) * 100, 1),
    round(mean(ties_proc$item5_w2 == 2, na.rm=TRUE) * 100, 1),
    round(mean(ties_proc$item5_w3 == 2, na.rm=TRUE) * 100, 1)
  )
)

# ============================================================
# Combine
# ============================================================
desc_san <- cbind(cov_stats, item_stats[, -1])

# ============================================================
# Table
# ============================================================
desc_san %>%
  kbl(
    caption  = "Descriptive Statistics by Sanction Phase (TIES v4.1)",
    col.names = c("Phase", "N",
                  "% Cold War", "% Major Power",
                  "% Coercive Action", "% Instrument Breadth",
                  "% Economic Pressure", "% Sender Commitment",
                  "% Multilateral Part."),
    booktabs = TRUE,
    label    = "tab:desc_san"
  ) %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  add_header_above(c(" " = 2,
                     "Covariates" = 2,
                     "Sanction Items" = 5)) %>%
  footnote(
    general = "Source: Threat and Imposition of Economic Sanctions (TIES) v4.1",
    general_title = "",
    footnote_as_chunk = TRUE
  )
