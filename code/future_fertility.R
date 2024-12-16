# 
rm(list = ls()); gc(T)

library

# Load the web-scraping
source("U:/packages/wpp/R/wpp_scraping.R")

# Years
years <- 2022:2070

# Countries
countries <- c("Finland", "United States of America")

# Set the indicator
indicator <- "Total fertility rate"


### Functions --------------------------------

# Set max year
project_fertility <- function(country, years =  2022:205) {
  last_year <- 2022
  df <- data.frame(year = years, tfr = rep(NA, length = length(years)))
  while (last_year < max(years)) {
    
    # Load the fertility rates for the future
    fert <- load_time_series(country,
                             indicator,
                             start = last_year,
                             end = max(years))
    
    # Clean the data
    fert <- fert[fert$variant == "Median", c("timeLabel", "location", "value")]
    fert$year <- as.numeric(fert$timeLabel)
    df$tfr[df$year %in% fert$year] <- fert$value
    
    
    # Reset last year
    last_year <- max(fert$year)
    
  }
  
  return(df)
}

# Projections ------------------------------------------

# Projection 
fert_fin <- project_fertility("Finland", years)
fert_usa <- project_fertility("United States of America", years)
fert_deu <- project_fertility("Germany", years)

# Plot the data
pdf("figures/fertility_forecast_un.pdf",
    height = 5, width = 8)
henrik_plot(xlim = range(years), 
            ylim = c(1.2, 1.8),
            xlab = "Years", 
            ylab = "Total Fertility Rate",
            title = "UN fertility forecasts")
lines(fert_fin$year, fert_fin$tfr, lwd = 2)
lines(fert_usa$year, fert_usa$tfr, lwd = 2, lty = 2)
lines(fert_deu$year, fert_deu$tfr, lwd = 2, lty = 3)
text(x = 2040, y = 1.68, label = "United States")
text(x = 2040, y = 1.53, label = "Germany")
text(x = 2040, y = 1.4, label = "Finland")
dev.off()

### END ##########################################
