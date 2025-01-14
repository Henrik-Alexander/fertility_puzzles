######################################
# Purpose: Cohort component method
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mgp.de
# Date: 08.08.2024
#####################################

# Data preparation -----------------------

rm(list = ls()); gc(T)

# Load the web-scraping
source("U:/packages/wpp/R/wpp_scraping.R")

# Load the functions for the cohort component method
source("functions/cohort_component_projections.R")

# Load the DemoTools package
# install.packages("remotes)
#install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
#remotes::install_github("timriffe/DemoTools")
library(DemoTools)

 # Define the projection horizon
end_year <- 2023
start_year <- 2023
country <- "France"

# Define the age-groups
age_groups <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
  "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79",
  "80-84", "85-89", "90-94", "95-99", "100+")

# Cohort component method ------------------------------------------

# World
un_observed <- cohort_component_method(region = "World", horizon = 75)
un_counter <- cohort_component_method(region = "World", horizon = 75, replacement=T)
world <- list("oberved"=un_observed, "counterfactual"=un_counter)

Sys.sleep(10)


# Cohort component projection
us_observed <- cohort_component_method("United States of America", 75)
us_counter <- cohort_component_method("United States of America", 75, replacement=T)
us <- list("oberved"=us_observed, "counterfactual"=us_counter)

Sys.sleep(10)


# Germany
de_observed <- cohort_component_method("Germany", 75)
de_counter <- cohort_component_method("Germany", 75, replacement=T)
germany <- list("oberved"=de_observed, "counterfactual"=de_counter)

Sys.sleep(10)


# Finland
fi_observed <- cohort_component_method("Finland", 75)
fi_counter <- cohort_component_method("Finland", 75, replacement=T)
finland <- list("oberved"=fi_observed, "counterfactual"=fi_counter)

# Make the comparisons of population trend
df <- list(world, us, germany, finland)
countries <- c("World", "United States", "Germany", "Finland")

# Plot the data
pdf(file = "figures/panel_momentum_growth.pdf", width = 10, height = 8)
par(mfrow=c(2, 2), font.lab = 2, oma = c(0, 0, 0, 0), mar =c(4, 4, 2, 1))
for (i in seq_along(df)) {
  compare(df[[i]]$oberved, df[[i]]$counterfactual)
  title(countries[i])
}
dev.off()

# Save the results
save(df, file = "results/wpp_nrr_projection.Rda")

## Projection the mean age at childbearing ----------------------------

# Mean age of the population
mean_age_pop <- function(pop) {
  sum(seq(2.5, 102.5, by = 5) * pop) / sum(pop)
}

# Plot the data
pdf("figures/panel_momentum_age.pdf", width = 10, height = 8)
par(mfrow=c(2, 2), font.lab = 2, oma = c(0, 0, 0, 0), mar =c(4, 4, 2, 2))
for (i in 1:4) {
  compare(df[[i]]$oberved, df[[i]]$counterfactual,
          f = mean_age_pop, label = "Mean age of the population", 
          legend_position = "right")
  title(countries[i])
}
dev.off()


# Apply the function
compare(un_observed, un_counter, f = mean_age_pop, label = "Mean age of the population")
compare(fi_observed, fi_counter, f = mean_age_pop, label = "Mean age of the population")
compare(de_observed, de_counter, f = mean_age_pop, label = "Mean age of the population")
compare(us_observed, us_counter, f = mean_age_pop, label = "Mean age of the population")


## Project the share of working age population ----------------------

# Define working ages
working_ages <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44",
                  "45-49", "50-54", "55-59", "60-64")


# Function to estimate the share of the working age population
share_working <- function(pop, working = age_groups %in% working_ages) {
  100 * sum(pop[working]) / sum(pop)
}

# Plot the data
pdf("figures/panel_momentum_working_age.pdf", width = 10, height = 8)
par(mfrow=c(2, 2), font.lab = 2, oma = c(0, 0, 0, 0), mar =c(3, 4, 2, 0.5),
    family="serif", cex.axis=1.3, cex.main=2, cex.lab=1.3)
for (i in seq_along(df)) {
  compare(df[[i]]$oberved, df[[i]]$counterfactual, f = share_working, label = "Share working age (%)", 
          legend_position = "right")
  title(countries[i], line = 0.5)
}
dev.off()


# Project share of working age
compare(un_observed, un_counter, f = share_working, label = "Share working (%)")
compare(fi_observed, fi_counter, f = share_working, label = "Share working (%)")
compare(de_observed, de_counter, f = share_working, label = "Share working (%)")
compare(us_observed, us_counter, f = share_working, label = "Share working (%)")

## Plot the data ----------------------------------------------------

# Plot the development of age groups
plot_age_groups <- function (observed, counterfactual) {
  select_ages <- c("0-4", "35-39", "100+")
  tmp <- un_observed$males + un_observed$females
  tmp <- tmp[select_ages, ]
  if (max(tmp) > 1e+6) tmp <- tmp / 1e+6
  set.seed(123)
  colors <- sample(colors(distinct=T), size = 20)
  years <- as.numeric(colnames(tmp))
  henrik_plot(range(years), range(tmp), xlab = "Years", "Population (in million)")
  for (i in 1:nrow(tmp)) {
    lines(years, tmp[i, ], col = colors[i])
  }
  # Legend
  legend("right", inset = 0.05, legend = rownames(tmp), col = colors[1:nrow(tmp)], lty=1)
}

### END #########################