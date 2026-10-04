# Identifying Latent States and Trajectories with Bayesian Latent Class Profile Analysis


## Software Requirements

### R
R version 4.5.2 (2025-10-31)

### External Software (required, not on CRAN)
The core estimation relies on **CAT_LVM_BAYESIAN**, which must be downloaded and sourced manually before running any estimation scripts.

Download from: https://kustatlab.korea.ac.kr/home/softwares
Version: 0.9.0 alpha (Chung, n.d.)

### Revised Functions (available in folder "Functions")

This folder stores the original functions by Chung (n.d.) `CAT_LVM_BAYESIAN_ORIGINAL.r` and `CAT_LVM_MCMC_ORIGINAL.r` as well as our revised versions `CAT_LVM_BAYESIAN.r` and `CAT_LVM_MCMC.r`. Plese replace and add those functions to the folder downloaded in the previous step.

The file `Data_generator.R` contains an LCPA simulation data generator that creates synthetic datasets from estimated model parameters, enabling parameter recovery checks and robustness tests.

The file `Parameter_Comparison.R` contains parameter recovery diagnostics for the LCPA simulation, computing absolute differences between true and estimated parameters (gam.pf, eta, big.rho) and visualizing recovery quality via profile probability comparisons, eta trajectory plots, and item response probability bar charts across groups and waves.

### R Packages (available on CRAN)

See invidiual R files at the top.


## Repository Structure

```
Replication/
├── README.md
├── Functions/                       
│   ├── CAT_LVM_BAYESIAN_ORIGINAL.r
│   ├── CAT_LVM_MCMC_ORIGINAL.r
│   ├── CAT_LVM_BAYESIAN.r
│   ├── CAT_LVM_MCMC.r
│   ├── Data_generator.R
│   └── Parameter_Comparison.R
├── ANES/
│   ├── 00_Prepare_Data.R
│   ├── 01_Descriptive_Table.R
│   ├── 02_Application.R
│   ├── 03_Trace_Plots_Posterior.R
│   ├── 04_Draw_Parameter.R
│   ├── 05_Posterior_Calculation.R
│   ├── 06_Final_Plots.R
│   ├── Data/
│   │   ├── posteriors_anes.rds
│   │   ├── LCPA_ANES_5000.rds
│   │   ├── anes_panel.csv
│   │   └── anes_panel_cleaned.csv
│   ├── Diagnostics/
│   │   ├── Posterior_Trace_Plots/
│   │   ├── Parameter_Distribution/
│   │   └── Model_Selection/
│   └── Figures/
├── SHP/
│   ├── 00_Prepare_Data.R
│   ├── 01_Descriptive_Table.R
│   ├── 02_Application.R
│   ├── 03_Trace_Plots_Posterior.R
│   ├── 04_Draw_Parameter.R
│   ├── 05_Posterior_Calculation.R
│   ├── 06_Final_Plots.R
│   ├── Data/
│   │   ├── posteriors_shp.rds
│   │   └── LCPA_SHP_5000.rds
│   ├── Diagnostics/
│   │   ├── Posterior_Trace_Plots/
│   │   ├── Parameter_Distribution/
│   │   └── Model_Selection/
│   └── Figures/
└── TIES/
    ├── 00_Prepare_Data.R
    ├── 02_Application.R
    ├── 03_Trace_Plots_Posterior.R
    ├── 04_Draw_Parameter.R
    ├── 05_Posterior_Calculation.R
    ├── 06_Final_Plots.R
    ├── Data/
    │   ├── posteriors_san.rds
    │   ├── LCPA_SAN_50000.rds
    │   ├── TIESv4-1.csv
    │   └── TIESv4-1_cleaned.csv
    ├── Diagnostics/
    │   ├── Posterior_Trace_Plots/
    │   ├── Parameter_Distribution/
    │   └── Model_Selection/
    └── Figures/
```

## Data Availability
- TIES: https://sanctions.web.unc.edu/ 
  (free download after registration)
- ANES: https://electionstudies.org/data-center/2016-2020-2024-panel-merged-study/ 
  (free download after registration)
- SHP: https://www.swissubase.ch/en/catalogue/studies/6097/21345/overview 
  (requires data access agreement)
  
TIES and ANES datasets can be found in the folder "Data". 


## Workflow

| Script | Description |
|--------|-------------|
| `00_Prepare_Data.R` | Recodes raw ANES/SHP/TIES data into binary LCPA indicators | 
| `01_Descriptive_Table.R` | Computes descriptive statistics by age group (for ANES/SHP) and wave | 
| `02_Application.R` | Runs Bayesian LCPA estimation via CAT_LVM_BAYESIAN; saves full MCMC chains; 3 Class 3 Profile Setting | 
| `03_Trace_Plots_Posterior.R` | Generates MCMC trace plots for convergence assessment | 
| `04_Draw_Parameter.R` | Draws and summarizes posterior parameter estimates | 
| `05_Posterior_Calculation.R` | Computes chain-averaged individual-level posteriors and Bayesian p-values |
| `06_Final_Plots.R` | Produces all manuscript figures | 


## Saved Model Objects

Pre-estimated model objects are provided to allow reproduction of all figures and tables without re-running the full MCMC estimation:

- `LCPA_[DATASET]_[NITER].rds` — full MCMC output including post-burn-in chains (e.g., `LCPA_ANES_5000.rds`, `LCPA_TIES_50000.rds`)
- `posteriors_[DATASET].rds` — chain-averaged individual-level posterior probabilities for profile and class membership


## Diagnostics

All diagnostic materials are stored in the `Diagnostics` folder for each Application:

- `Posterior Trace Plots` — posterior trace plots for all model parameters, used to assess MCMC convergence visually
- `Parameter Distribution` — posterior distribution plots for primary measurement parameters (rho), secondary measurement parameters (eta), and profile prevalences (gamma)
- `Model Selection` — fitted LCPA objects for all tested class-profile combinations (C = 2–4, S = 2–4) estimated without covariates, named `LCPA_[DATASET]_C[C]_P[S]_nocov.rds`; goodness-of-fit tables (BIC, DIC) for each specification are saved as `GOF_Table_C[C]_P[S]_nocov.rds`; the script `LCPA_Class_Selection_nocov.R` was used to estimate all specifications and compute fit indices




