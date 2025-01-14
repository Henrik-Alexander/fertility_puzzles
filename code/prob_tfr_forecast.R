library(bayesTFR)

# Get the graphic setting
source("U:/packages/wpp/R/wpp_scraping.R")

# Functions -----------------------------

# Ploting function
plot_un_projection <- function(country, prediction) {
  
  # Extract the data
  tmp <- get.tfr.trajectories(prediction, country)
  years <- as.numeric(rownames(tmp))
  
  # Create the canvas
  plot.new()
  plot.window(xlim = range(years),
              ylim = c(1.1, 2.3),
              xaxs = "i")
  
  # Create the axis
  axis(1, at = seq(2010, 2100, by = 10), labels = F, lwd = 2)
  axis(1, at = seq(2010, 2100, by = 10), tick = F, line = -0.5)
  axis(1, at = min(years):max(years), labels = F, lwd = .1, tcl = -.3)
  axis(2, at = seq(1, 2.5, by = .25), labels = F, lwd = 2)
  axis(2, at = seq(1, 2.5, by = .25), tick = F, line = -.25, las = 1)
  axis(2, at = seq(1.1, 2.3, by = .05), labels = F, lwd = .1, tcl = -.3)
  
  # Create the labels
  title(main = ifelse(country=="United States of America", paste("The", country), country),
        cex.main = 1.5, line = 0.5)
  title(ylab = "Total fertility rate", 
        cex.lab = 1.5, line = 2.75)
  title(xlab = "Year", line = 1.5, cex.lab = 1.5)
  
  # Create the confidence bands
  maxs <- apply(tmp, 1, max)
  mins <- apply(tmp, 1, min)
  
  # Draw the lines
  polygon(c(years,rev(years)),c(mins,rev(maxs)),col = "grey85", border = FALSE)
  for (i in 1:ncol(tmp)) {
    lines(x = years, y = tmp[, i], col=grey(0.2, 0.8))
  }
  lines(years, apply(tmp, 1, median), col = "red",lwd=5)
  #lines(years, mins, lwd = 2)
  #lines(years, maxs, lwd = 2)
  #grid()

}

# Extract the previous years ------------

# Load the past fertility data
tfr <- load_time_series("Germany", indicator = "Total fertility rate", start=2000, end=2018)


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

# Plot the results ----------------------

# Define countries
countries <- c("Finland", "United States of America", "Germany")

# Get the TFR trajectories
pdf(file = "figures/un_prob_projections.pdf", height = 5, width = 12)
par(mfrow=c(1, 3), omi = c(0, 0, 0, 0), mar = c(3, 4, 3, 0.5), 
    family = "serif", cex.axis=1.3,
    font.lab = 2)
lapply(countries, plot_un_projection, prediction = pred)
dev.off()


### END ################################
