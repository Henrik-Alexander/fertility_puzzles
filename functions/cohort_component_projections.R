
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
  
  # Combine fertility and mortality
  df <- merge(lt, fx, by = c("ageLabel", "ageStart", "ageEnd"), all.x = T)
  df <- df[order(df$ageStart), ]
  
  # Combine the data
  data.frame("ageLabel"=df$ageLabel,
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
project <- function(pop_f, pop_m, df, srb=1.05, replacement) {
  
  # Get the length of the data
  n <- nrow(df)
  
  # Check if momentum
  if (replacement) {
    fertility <- df$fx_count
  } else {
    fertility <- df$fx
  }
  
  cat("TFR=", sum(fertility*5, na.rm = T), "\n")
  
  # Create df
  df$value_pop <- pop_f
  
  # Estimate the population in 5 years
  pop_f_t <- df$value_pop[1:(n-1)] * df$nLx[2:n] / df$nLx[1:(n-1)]
  pop_f_t[length(pop_f_t)] <- (df$value_pop[n] + df$value_pop[n-1]) * (df$Tx[n] / df$Tx[n-1])
  pop_f_t <- c(NA, pop_f_t)
  
  # Estimate the births
  Births <- 5 * fertility * (df$value_pop + pop_f_t)/2
  B <- sum(Births, na.rm = T)
  B_f <- B * 1 / (1 + srb)
  
  # Create the first age category
  pop_f_t[1] <- B_f * (df$nLx[1]) / (5*1)
  
  # Create df for men
  df$value_pop <- pop_m
  
  # Estimate the population in 5 years
  pop_m_t <- df$value_pop[1:(n-1)] * df$nLx_m[2:n] / df$nLx_m[1:(n-1)]
  pop_m_t[length(pop_m_t)] <- (df$value_pop[n] + df$value_pop[n-1]) * (df$Tx_m[n] / df$Tx_m[n-1])
  pop_m_t <- c(NA, pop_m_t)
  
  # Assign the births
  B_m <- B * srb / (1 + srb)
  pop_m_t[1] <- B_m * (df$nLx[1]) / (5*1)
  
  # Combine the data
  data.frame("Age"=df$ageLabel, "Females"=pop_m_t, "Males"=pop_f_t)
  
}


# Project the mortality rate
project_mortality <- function(year, country, srb = 1.05) {
  
  # Load the rates
  mx <- load_time_series(country=country, indicator="Age specific mortality rate m(x,n) - abridged", start=year, end=year)
  
  # Clean the data
  mx <- clean_data(mx)
  
  # Combine the first two age groups of the mortality table
  mx_f <- mx[mx$sex=="Female", c("ageStart", "ageEnd", "value")]
  mx_m <- mx[mx$sex=="Male", c("ageStart", "ageEnd", "value")]
  
  
  # Create life table
  lt <- clean_lt(lt_abridged(nMx=mx_f$value, Age=mx_f$ageStart, sex=sex, mod=F, axmethod= "un", radix=1))
  
  # Attach the male mortality for men
  lt_male <- clean_lt(lt_abridged(nMx=mx_m$value, Age=mx_m$ageStart, sex="m", mod=F, axmethod= "un", radix=1))
  
  # Attach the male life table
  lt$nLx_m <- lt_male$nLx
  lt$Tx_m <- lt_male$Tx
  
  # Assign identifiers
  lt$country <- country
  lt$year <- year
  
  Sys.sleep(1)
  
  return(lt)
  
}

# Project and clean the fertility data
project_fertility <- function(country = "France", year = 2023) {
  
  # Load the fertility data
  fx <- load_time_series(country=country, indicator="Fertility rates by age of mother (5-year)", start=year, end=year)
  
  # Clean the data
  fx <- clean_data(fx)
  
  # Clean the fertility data
  fx$fx <- fx$value / 1000
  
  # Assign identifiers
  fx$country <- country
  fx$year <- year
  
  return(fx[, names(fx) != "value"])
}


# Combine mortality and fertility projection
project_vital <- function(lt, fx, srb = 1.05) {
  # Get the counterfactual TFR
  df <- merge(lt, fx, all.x = T)
  
  # Estimate the counterfactual TFR
  nrr <- sum(1 / (1 + srb) * df$nLx * df$fx, na.rm = T)
  df$fx_count <- df$fx / nrr
  
  return(df[order(df$ageStart), ])
}

# Function to estimate the TFR
estimate_tfr <- function(data, asfr = "fx") {
  sum(data[[asfr]] * 5, na.rm = T)
}


# Cohort component projection
cohort_component_method <- function(region, horizon=50, replacement=F, srb=1.05, start_year = 2023) {
  if (horizon %% 5 != 0) stop("The horizon is not divisible by 5, but we are working with 5-year age groups.")
  if (start_year + horizon > 2100) warning("Projections can only go until 2100!")
  
  cat(region)
  
  # Create the year vector
  years <- seq(start_year, end_year+horizon, by = 5)
  
  # Range of indicators
  pop <- load_time_series(country = region, 
                          indicator="Population by 5-year age groups and sex",
                          start=start_year, 
                          end=start_year)
  
  # Clean the population data
  pop <- clean_data(pop)
  pop_f <- pop$value[pop$sex == "Female"]
  pop_m <- pop$value[pop$sex == "Male"]
  
  # Project the vital statistics data
  lts <- lapply(years, project_mortality, country = region)
  fxs <- project_fertility(country = region)
  fxs <- fxs[, names(fxs) != "year"]
  vital <- lapply(lts, FUN = project_vital, fx = fxs)
  
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
    projection <- project(pop_f = projection_f[, i],
                          pop_m = projection_m[, i],
                          df = vital[[i]],
                          srb = srb,
                          replacement = replacement)
    
    # Get the number of migrants
    total_pop <- sum(projection$Males) + sum(projection$Males)
    if (year <= 2100) {
      migrants <- estimate_number_migrants(country = region,
                                           total_pop = total_pop,
                                           year = year)
      
    } else {
      migrants <- estimate_number_migrants(country = region,
                                           total_pop = total_pop,
                                           year = 2100)
    }
    
    
    projection_m[, i+1] <- projection$Males + 5 * migrants / (nrow(projection) * 2)
    projection_f[, i+1] <- projection$Females + 5 * migrants / (nrow(projection) * 2)
    
    
    # Pause for access
    Sys.sleep(1)
  }
  
  # Create the results
  projection <- list("males" = projection_m,
                     "females" = projection_f,
                     "tfr observed" = sapply(vital, estimate_tfr, "fx"),
                     "tfr replacement" = sapply(vital, estimate_tfr, "fx_count")
  )
  
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
  
  # colors
  col_cou <- "#998ec3"
  col_obs <- "#f1a340"
  

  
  # Adjust downwards
  if (max(obs) > 1e+6) {
    obs <- obs / 1e+6
    cou <- cou / 1e+6
    label <- paste(label, "(m)")
  }
  # Plot the result
  years <- as.numeric(names(obs))
  
  # Filter the data
  years <- years[years <= 2100]
  obs <- obs[1:length(years)]
  cou <- cou[1:length(years)]
  
  henrik_plot(xlim=range(years), ylim=range(c(obs, cou)),
              xlab = "Year", ylab = label)
  lines(x=years, y=obs, lwd=3, col = col_obs)
  lines(x=years, y=cou, lwd=3, col = col_cou)
  
  # Create the labels
  text(y=obs[5], x=years[5], pos=1, adj=0.5, labels="freeze-rate", col=col_obs, cex=1.5)
  text(y=cou[12], x=years[12], pos=4, adj=1, labels="replacement", col=col_cou, cex=1.5)
  
  
  # legend(legend_position, inset =  0.02,
  #        main = "Fertility:",
  #        legend = c("Freeze-rate",
  #                   "Replacemeny"), 
  #        lty = c(1, 2), lwd = 2, bg = "white", text.font = 2, cex = 0.5)
}
