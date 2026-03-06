library(here)      # For relative paths
library(brms)      # Bayesian modeling
library(tidybayes) # For post-processing
library(cmdstanr)  # MCMC sampling
library(lavaan)    # Internal consistency
library(dagitty)   # Specifying & Analyzing DAGs
library(ggdag)     # Drawing DAGs
library(ggtext)    # Fonts for graphs 
library(showtext)  # To display fonts 
library(posterior) # Extract posterior draws
library(MASS)      # To simulate multivariate normal distributions
library(tidyverse) # Data wrangling


# Import Data  -----------------------------------------------------------------
source(here("R/import.R"))


# Config -----------------------------------------------------------------------
## Set the theme for graphs
source(here("R/theme.R"))


# Causal Modeling --------------------------------------------------------------
## Create a simple DAG of the core variables (for printing)
source(here("R/dag-core.R"))

## Create a complete DAG to analyze for adjustment sets
source(here("R/dag-full.R"))


# Examine Measures -------------------------------------------------------------
## Calculate the internal consistency of the scales used
source(here("R/internal-consistency.R"))


# Descriptive Stats ------------------------------------------------------------



# Compile Stan Model -----------------------------------------------------------
model <- cmdstan_model("Stan/model.stan")


# Prepare Data -----------------------------------------------------------------
## Standardize the continuous variables and center the binary variables
source(here("R/prepare-data.R"))


# Test the model with simulated data -------------------------------------------
## Simulate Data
source(here("R/simulate-data.R"))

## Fit the stan model to the simulated data
source(here("R/fit-simulation.R"))

## Inspect if results recover simulation inputs


# Prior Predictive Check -------------------------------------------------------
source(here("R/prior-predictive-check.R"))


# Fit the model to the real data -----------------------------------------------
source(here("R/fit.R"))


# Check model diagnostics ------------------------------------------------------
source(here("R/diagnostics.R")) # add trace rank plots, output


# Posterior Predictive Check ---------------------------------------------------
source(here("R/posterior-predictive-check.R")) # add outputs


# Summarize Posterior -----------------------------------------------------
source(here("R/summarize-posterior.R"))


# Plot Posterior ----------------------------------------------------------
source(here("R/plot-posterior.R"))


