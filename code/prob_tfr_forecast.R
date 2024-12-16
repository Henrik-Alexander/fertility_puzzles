library(bayesTFR)

# Get the graphic setting
source("U:/packages/wpp/R/wpp_scraping.R")

# Functions -----------------------------

# Ploting function
plot_un_projection <- function(country, prediction) {
  tmp <- get.tfr.trajectories(prediction, country)
  years <- as.numeric(rownames(tmp))
  henrik_plot(xlim = range(years),
              ylim = c(1.1, 2.3),
              xlab = "Year",
              ylab = "Total fertility rate",
              title = paste("UN forecast for", country))
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
m3 <- run.tfr3.mcmc(sim.dir=sim.dir, nr.chains=2, iter=100, thin=1, seed=1, verbose=TRUE)

# Prediction
pred <- tfr.predict(m, burnin=30, burnin3=50, verbose=TRUE)

# Get the predictions for Finland, USA and Germany
summary(pred, country='Ghana')
unlink(sim.dir, recursive=TRUE)

# Plot the results ----------------------


# Get the TFR trajectories
pdf(file = "figures/un_prob_projections.pdf", height = 5, width = 12)
countries <- c("Finland", "United States of America", "Germany")
par(mfrow=c(1, 3))
lapply(countries, plot_un_projection, prediction = pred)
dev.off()


### END ################################
