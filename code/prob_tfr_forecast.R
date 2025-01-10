library(bayesTFR)

# Get the graphic setting
source("U:/packages/wpp/R/wpp_scraping.R")

# Functions -----------------------------

# Ploting function
plot_un_projection <- function(country, prediction) {
  tmp <- get.tfr.trajectories(pred, country)
  years <- as.numeric(rownames(tmp))
  plot(NA, xlim = range(years),
       ylim = c(1.1, 2.3),
       axes = FALSE,
       xaxs = "i",
       ann = FALSE,
       main = ifelse(country == "United States of America", paste("The", country), country))
  
  # Create the axis
  axis(1, at = seq(2015, 2100, by = 5), labels = F, lwd = 3)
  axis(1, at = seq(2015, 2100, by = 5), tick = F, line = -0.5)
  axis(1, at = min(years):max(years), labels = F, lwd = .1)
  axis(2, at = seq(1, 2.5, by = .25), labels = F, lwd = 3)
  axis(2, at = seq(1, 2.5, by = .25), tick = F, line = -.25, las = 1)
  axis(2, at = seq(1.1, 2.3, by = .05), labels = F, lwd = .1)
  
  # Create the labels
  title(country, cex = 1, font = 1, family = "serif")
  title(ylab = "Total fertility rate", 
        cex = 2, line = 2.75, font = 2, family = "serif")
  title(xlab = "Year", line = 1.5, cex = 3)
  
  # Draw the lines
  for (i in 1:ncol(tmp)) {
    lines(x = years, y = tmp[, i], col = "grey80")
  }
  lines(years, apply(tmp, 1, median), col = "red", lwd = 2.5)
  lines(years, apply(tmp, 1, min), lwd = 1.5)
  lines(years, apply(tmp, 1, max), lwd = 1.5)
  
}



# Data preparations ---------------------

# This command produces output data such as in the directory ex-data
sim.dir <- tempfile()

# Phase II MCMCs
m <- run.tfr.mcmc(nr.chains=1, iter=60, output.dir=sim.dir, seed=1, verbose=TRUE)

# Phase III MCMCs (not included in the package)
m3 <- run.tfr3.mcmc(sim.dir=sim.dir, nr.chains=2, iter=100, thin=1, seed=1, verbose=TRUE, replace.output = T)

# Prediction
pred <- tfr.predict(m, burnin=30, burnin3=50, verbose=TRUE, replace.output = T)

# Get the predictions for Finland, USA and Germany
summary(pred, country='Ghana')
unlink(sim.dir, recursive=TRUE)

# Plot the results ----------------------

# Define countries
countries <- c("Finland", "United States of America", "Germany")

# Get the TFR trajectories
pdf(file = "figures/un_prob_projections.pdf", height = 5, width = 12)
par(mfrow=c(1, 3), omi = c(0, 0, 0, 0), mar = c(3, 4, 2, 0.5))
lapply(countries, plot_un_projection, prediction = pred)
dev.off()


### END ################################
