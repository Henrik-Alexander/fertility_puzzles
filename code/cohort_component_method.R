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

## Functions -------------------------------------------------------

# Function to extract lower age
lower_age <- function(agegroup) as.numeric(str_extract(agegroup, "^[0-9]+"))
upper_age <- function(agegroup) ifelse(agegroup == "100+", Inf,  as.numeric(str_extract(agegroup, "[0-9]+$")))

# Clean the life table data
clean_lt <- function(lt) {
  lt$ageStart <- lt$Age
  lt$ageEnd <- lt$ageStart + lt$AgeInt - 1
  lt$ageLabel <- ifelse(lt$ageStart < 5, "0-4", 
                        ifelse(is.na(lt$ageEnd), "100+", paste0(lt$ageStart, "-", lt$ageEnd)))
  Lx <- tapply(lt$nLx, lt$ageLabel, sum)
  data.frame("ageLabel"=unique(lt$ageLabel),
             "nLx"=Lx[unique(lt$ageLabel)],
             "Tx"=rev(cumsum(rev(Lx[unique(lt$ageLabel)]))),
             "ageStart"=lower_age(unique(lt$ageLabel)),
             "ageEnd"=upper_age(unique(lt$ageLabel)))
}

# Clean data
clean_data <- function(wpp_data) {
  wpp_data$year <- as.numeric(wpp_data$timeLabel)
  wpp_data <- wpp_data[wpp_data$variantLabel == "Median", ]
  return(wpp_data[, c("location", "sex", "ageLabel", "ageStart", "ageEnd", "value")])
}

# Function to create df
create_df <- function(mx, fx, pop, sex = "f") {
  
  # Create life table
  lt <- lt_abridged(nMx=mx$value, Age=mx$ageStart, sex=sex, mod=F, axmethod= "un", radix=1)
  lt <- clean_lt(lt)
  
  # Combine fertility and mortality
  df <- merge(lt, fx, by = c("ageLabel", "ageStart", "ageEnd"), all.x = T)
  df <- df[order(df$ageStart), ]
  
  # Combine the data
  data.frame("ageLabel"=df$ageLabel,
             "value_fx"=df$value,
             "nLx"=df$nLx,
             "Tx"=df$Tx,
             "fx"=df$value)
}


# Create the migration scenario
estimate_number_migrants <- function(country, total_pop, year) {
  
  # Get the data
  mig_df <- load_time_series(country, 
                             indicator = "Crude rate of net migration",
                             start = year,
                             end = year)
  
  # Create the migration data 
  mig_df <- mig_df[mig_df$variant == "Median", ]
  
  # Estimate the total number of migrants
  return((total_pop * mig_df$value) / 1000)
  
}

# Cohort component method
project <- function(pop_f, pop_m, age=age_groups, mx_m, mx_f, fx, srb=1.05) {
  
  # Get the length of the data
  n <- length(age)
  
  # Create df
  df <- create_df(mx_f, fx, pop_f)
  
  # Estimate the population in 5 years
  pop_f_t <- df$value_pop[1:(n-1)] * df$nLx[2:n] / df$nLx[1:(n-1)]
  pop_f_t[length(pop_f_t)] <- (df$value_pop[n] + df$value_pop[n-1]) * (df$Tx[n] / df$Tx[n-1])
  pop_f_t <- c(NA, pop_f_t)
  
  # Estimate the births
  Births <- 5 * df$value_fx * (df$value_pop + pop_f_t)/2
  B <- sum(Births, na.rm = T)
  B_f <- B * 1 / (1 + srb)
  
  # Create the first age category
  pop_f_t[1] <- B_f * (df$nLx[1]) / (5*1)

  # Create df for men
  df <- create_df(mx_m, fx, pop_m, sex = "m")
  
  # Estimate the population in 5 years
  pop_m_t <- df$value_pop[1:(n-1)] * df$nLx[2:n] / df$nLx[1:(n-1)]
  pop_m_t[length(pop_m_t)] <- (df$value_pop[n] + df$value_pop[n-1]) * (df$Tx[n] / df$Tx[n-1])
  pop_m_t <- c(NA, pop_m_t)
  
  # Assign the births
  B_m <- B * srb / (1 + srb)
  pop_m_t[1] <- B_m * (df$nLx[1]) / (5*1)

  # Combine the data
  data.frame("Age"=df$ageLabel, "Females"=pop_m_t, "Males"=pop_f_t)
  
}


# Project the fertiliy and mortality rate
project_vital <- function(country, year) {
  
  # Load the rates
  mx <- load_time_series(country=country, indicator="Age specific mortality rate m(x,n) - abridged", start=year, end=year)
  fx <- load_time_series(country=country, indicator="Fertility rates by age of mother (5-year)", start=year, end=year)
  pop <- load_time_series(country=country, indicator="Population by 5-year age groups and sex", start=2020, end=2020)
  
  # Clean the data
  mx <- clean_data(mx)
  fx <- clean_data(fx)
  pop <- clean_data(pop)
  
  # Combine the first two age groups of the mortality table
  mx_f <- mx[mx$sex=="Female", c("ageStart", "ageEnd", "value")]
  mx_m <- mx[mx$sex=="Male", c("ageStart", "ageEnd", "value")]
  
  # Get the counterfactual TFR
  df <- create_df(mx_f, fx)
  nrr <- sum(1 / (1 + srb) * df$nLx * df$fx, na.rm = T)
  fx$value_count <- fx$value / nrr

  # Combine the results
  results <- list(mortality = mx, 
                  obs_fx = fx$value,
                  rep_fx = fx$valu_count)
  
    return(result)
  
}



# Cohort component projection
cohort_component_method <- function(country, start_year, horizon, momentum=F, srb=1.05) {
  if (horizon %% 5 != 0) stop("The horizon is not divisible by 5, but we are working with 5-year age groups.")
  
  # Range of indicators
  pop <- load_time_series(country=country, indicator="Population by 5-year age groups and sex", start=start_year, end=start_year) 
  mx <- load_time_series(country=country, indicator="Age specific mortality rate m(x,n) - abridged", start=start_year, end=start_year)
  fx <- load_time_series(country=country, indicator="Fertility rates by age of mother (5-year)", start=start_year, end=start_year)
  
  # Clean the data
  pop <- clean_data(pop)
  mx <- clean_data(mx)
  fx <- clean_data(fx)
  
  # Clean the population data
  pop_f <- pop$value[pop$sex=="Female"]
  pop_m <- pop$value[pop$sex=="Male"]
  
  # Combine the first two age groups of the mortality table
  mx_f <- mx[mx$sex=="Female", c("ageStart", "ageEnd", "value")]
  mx_m <- mx[mx$sex=="Male", c("ageStart", "ageEnd", "value")]
  
  # Clean the fertility data
  fx$value <- fx$value / 1000
  
  # Observed tfr
  tfr <- sum(fx$value * 5, na.rm = T)
  
  cat("TFR observed:", tfr, "\n")
  
  # Population momentum
  if (momentum) {
    df <- create_df(mx_f, fx, pop_m)
    nrr <- sum(1 / (1 + srb) * df$nLx * df$fx, na.rm = T)
    fx$value <- fx$value / nrr
    tfr <- sum(fx$value * 5, na.rm = T)
    cat("TFR counterfactual:", tfr, "\n")
  }
  
  # Create the containers
  projection_m <- projection_f <- matrix(NA, nrow=length(pop_f), ncol=(horizon/5)+1)
  colnames(projection_m) <- colnames(projection_f) <- seq(start_year, (start_year + horizon), by = 5)
  rownames(projection_m) <- rownames(projection_f) <- pop$ageLabel[pop$sex=="Female"]
  
  # Assign the jump-off population
  projection_m[, 1] <- pop_m
  projection_f[, 1] <- pop_f
  
  # Project the data
  for (i in 1:(horizon/5)) {
    year <- (5 * i) + start_year
    cat("Year:", year, "\n")
    
    # Project the population
    projection <- project(projection_f[, i],
                          projection_m[, i],
                          mx_m = mx_m, 
                          mx_f = mx_f,
                          fx=fx,
                          srb = srb)
    
    # Get the number of migrants
    total_pop <- sum(projection$Males) + sum(projection$Males)
    if (year <= 2100) {
      migrants <- estimate_number_migrants(country = country,
                                           total_pop = total_pop,
                                           year = year)
      
    } else {
      migrants <- estimate_number_migrants(country = country,
                                           total_pop = total_pop,
                                           year = 2100)
    }

    
    projection_m[, i+1] <- projection$Males + 5 * migrants / (nrow(projection) * 2)
    projection_f[, i+1] <- projection$Females + 5 * migrants / (nrow(projection) * 2)
    
  }
  
  # Create the results
  projection <- list("males" = projection_m,
                     "females" = projection_f,
                     "tfr" = tfr)
  
  # Return the results
  return(projection)
  
}

# Estimate the trend in the data
compare <- function(observed, counterfactual, f = sum, label = "Population size", legend_position = "bottomleft") {
  # Combine male and female data
  obs <- observed$males + observed$females
  cou <- counterfactual$females + counterfactual$males
  
  # Apply the function
  obs <- apply(obs, 2, FUN = f)
  cou <- apply(cou, 2, FUN = f)
  
  # Adjust downwards
  if (max(obs) > 1e+6) {
    obs <- obs / 1e+6
    cou <- cou / 1e+6
    label <- paste(label, "(in million)")
  }
  # Plot the result
  years <- as.numeric(names(obs))
  henrik_plot(xlim=range(years), ylim=range(c(obs, cou)),
              xlab = "Year", ylab = label)
  lines(x=years, y=obs, lwd =2)
  lines(x=years, y=cou, lty=2, lwd = 2)
  legend(legend_position, inset =  0.02,
         legend = c(paste("Observed, TFR =", round(observed$tfr, 2)),
                    paste("Replacement fertility, TFR =",  round(counterfactual$tfr, 2))), 
         lty = c(1, 2), lwd = 2, bg = "white", text.font = 2)
}


# Cohort component method ------------------------------------------

# World
un_observed <- cohort_component_method("World", 2023, 100)
un_counter <- cohort_component_method("World", 2023, 100, momentum=T)
world <- list("oberved"=un_observed, "counterfactual"=un_counter)

# Cohort component projection
us_observed <- cohort_component_method("United States of America", 2023, 100)
us_counter <- cohort_component_method("United States of America", 2023, 100, momentum=T)
us <- list("oberved"=us_observed, "counterfactual"=us_counter)

# Germany
de_observed <- cohort_component_method("Germany", 2023, 100)
de_counter <- cohort_component_method("Germany", 2023, 100, momentum=T)
germany <- list("oberved"=de_observed, "counterfactual"=de_counter)

# Finland
fi_observed <- cohort_component_method("Finland", 2023, 100)
fi_counter <- cohort_component_method("Finland", 2023, 100, momentum=T)
finland <- list("oberved"=fi_observed, "counterfactual"=fi_counter)

# Make the comparisons of population trend
df <- list(world, us, germany, finland)
countries <- c("World", "United States", "Germany", "Finland")

# Plot the data
pdf(file = "figures/panel_momentum_growth.pdf", width = 10, height = 8)
par(mfrow=c(2, 2), font.lab = 2, oma = c(0, 0, 0, 0), mar =c(4, 4, 2, 2))
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
par(mfrow=c(2, 2), font.lab = 2, oma = c(0, 0, 0, 0), mar =c(4, 4, 2, 2))
for (i in seq_along(df)) {
  compare(df[[i]]$oberved, df[[i]]$counterfactual, f = share_working, label = "Share working age (in %)", 
          legend_position = "right")
  title(countries[i])
}
dev.off()


# Project share of working age
compare(un_observed, un_counter, f = share_working, label = "Share working (in %)")
compare(fi_observed, fi_counter, f = share_working, label = "Share working (in %)")
compare(de_observed, de_counter, f = share_working, label = "Share working (in %)")
compare(us_observed, us_counter, f = share_working, label = "Share working (in %)")

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

### END ##########################
