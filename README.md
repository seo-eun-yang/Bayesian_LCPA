# Replication Materials: Bayesian Latent Class Profile Analysis

This repository contains replication materials for:

**Yang, Seo Eun, and Sara Fürnkranz. "Identifying Latent States and
Trajectories with Bayesian Latent Class Profile Analysis."**

## Overview

Political processes often unfold through sequences of stages, but the
substantive object of interest is not always only the state occupied at
a particular stage. It may also be the trajectory through which those
states develop.

This paper introduces **Bayesian latent class profile analysis (LCPA)**
to political science as a framework for distinguishing two forms of
latent heterogeneity:

-   **Latent classes** represent qualitatively distinct states at
    particular stages.
-   **Latent profiles** represent common trajectories through those
    states across the full sequence.

Unlike approaches centered on adjacent transitions, LCPA identifies
profiles over the full sequence without imposing a first-order Markov
structure.

The paper also extends existing Bayesian LCPA by carrying posterior
parameter uncertainty into individual class/profile classification and
posterior inference for covariate associations.

## Applications

The framework is demonstrated using two substantively and structurally
distinct applications.

### 1. Political Polarization

The first application analyzes political polarization using three-wave
panel data from:

-   the **American National Election Studies (ANES), 2016--2024**, and
-   the **Swiss Household Panel (SHP), 2017--2023**.

The analysis identifies multidimensional latent polarization states and
trajectories through those states and examines differences across age
groups.

### 2. Economic Sanctions

The second application analyzes sanctions episodes using the **Threat
and Imposition of Sanctions (TIES) 4.0** dataset.

Here, stages correspond to theoretically defined phases of coercive
bargaining:

**Threat → Imposition → Resolution**

The application illustrates that LCPA can be used not only for repeated
observations over time but also for substantively ordered stages of a
political process.

## Methodological Contributions

The repository supports the paper's two main methodological
contributions:

1.  Introducing Bayesian LCPA as a general framework for identifying
    latent political states and trajectories through those states.
2.  Extending Bayesian LCPA by incorporating posterior parameter
    uncertainty into individual classification and posterior-based
    inference for covariate associations.

Posterior inference is based on retained post-burn-in MCMC draws rather
than conditioning classification and covariate inference on a single set
of posterior point estimates.

## Replication Materials

The files in this repository provide the code and supporting materials
used to reproduce the analyses reported in the manuscript, including the
political-polarization and economic-sanctions applications.

Please consult the comments and file organization in the repository for
the execution order and application-specific inputs.

## Data Availability

The empirical applications rely on data obtained from their original
providers and subject to their respective access requirements:

-   **American National Election Studies (ANES)** --- 2016--2020--2024
    Panel Study
-   **Swiss Household Panel (SHP)** --- 2017, 2020, and 2023 waves
-   **Threat and Imposition of Sanctions (TIES) 4.0**

Because access and redistribution conditions are determined by the
original data providers, users should obtain any restricted source data
directly from those providers before running the corresponding
replication scripts.

## Citation

If you use these materials, please cite:

> Yang, Seo Eun, and Sara Fürnkranz. "Identifying Latent States and
> Trajectories with Bayesian Latent Class Profile Analysis."

A complete journal citation and DOI can be added here once publication
information is finalized.

## Authors

**Seo Eun Yang**\
Northeastern University

**Sara Fürnkranz**\
Northeastern University

## Status

These materials accompany the manuscript and may be updated as the
article proceeds through publication and archival replication.
