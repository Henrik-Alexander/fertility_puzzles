#####################
# Project: Thesis
# Purpose: Plot the fertility trend
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 05.12.2024
######################

library(readxl)
library(tidyverse)
library(HMDHFDplus)

# Theme set
theme_set(theme_test(base_size = 14, base_family = "serif"))

# Load the log-in details for the HFD
path_hfd <- "U:/data/global/human_fertility_database/"


### Function ----------------------------------

# Load the data
df <- read.table(file.path(path_hfd, "tfrRR.txt"), skip = 2, header = T)

# Filter year after 1969
df <- df[df$Year >= 2000, ]

anglosaxon <- c("GBRTENW", "USA", "CAN", "IRL")
eastasian <- c("JPN", "KOR", "TWN")
easteurope <- c("CZE", "EST", "HUN", "LTU", "BLR", "RUS", "POL")
germanspeak <- c("DEUTNP", "DEUTW", "DEUTE", "AUT", "CHE")
mediterranean <- c("FRATNP", "PRT", "ITA", "ESP")
nordics <- c("SWE", "NOR", "FIN", "DNK", "ICE")


# Recode the variables
df$group <- ""
df$group[df$Code %in% anglosaxon] <- "Anglo-Saxon"
df$group[df$Code %in% eastasian] <- "East-Asian"
df$group[df$Code %in% easteurope] <- "Eastern Europe"
df$group[df$Code %in% germanspeak] <- "German-Speaking"
df$group[df$Code %in% mediterranean] <- "Mediteranean"
df$group[df$Code %in% nordics] <- "Nordics"
df <- df[df$group != "", ]

### Preparations ------------------------------


# Country studies
countries <- c("FIN", "FRATNP", "DEUTNP", "NOR", "USA")
c_labs <- c("Finland", "France", "Germany","Norway", "United States")
country_groups <- unique(df$group)
study_countries <- df[df$Code %in% countries, ]
study_countries$label <- ""

for (i in seq_along(countries)) {
  study_countries$label[study_countries$Code == countries[i]] <- c_labs[i]
}


### Base R plots -----------------------------






plot_countrygroup <- function(group, dataset=df, label_data=study_countries) {

  # Plot the grey lines
  tmp <- dataset[dataset$group == group, ]
  countries <- unique(tmp$Code)
  
  
  # Create the base plot
  plot.new()
  plot.window(xlim = c(2000, 2027),
              ylim = range(dataset$TFR),
              xaxs = "i")

  # Plot the lines
  for(country in countries) {
  lines(x = tmp$Year[tmp$Code == country],
        y = tmp$TFR[tmp$Code == country],
        col = "grey25",
        lwd = 2)
  }
  
  # Color of the line
  cdf <- label_data[label_data$Code %in% countries & study_countries$group==group, ]
  for (country in unique(cdf$Code)) {
    tmp2 <- cdf[cdf$Code == country, ]
    lines(x = tmp2$Year,
          y = tmp2$TFR,
          col = cols[colour_count], 
          lwd = 3)
    points(x = tmp2$Year,
          y = tmp2$TFR,
          col = cols[colour_count],
          cex = 1.5,
          pch = 14 + colour_count)
    text(x = max(tmp2$Year),
         y = tmp2$TFR[tmp2$Year == max(tmp2$Year)],
         label = sub(" ", "\n", unique(tmp2$label)),
         pos = ifelse(group =="Nordics", 4, 3),
         offset = ifelse(group == "Nordics", 0.1, 0.8),
         cex = 1.7,
         col = cols[colour_count])
    colour_count <<- colour_count + 1
  }
  
  # Draw the axis
  axis(1, at = seq(2000, 2030, by = 5), lwd = 2, line = -0.5)
  axis(1, at = 2000:2027, lwd = 0.5, label = F, tcl = -0.2, line = -0.5)
  axis(2, at = seq(0.5, 2.5, by = 0.5), lwd = 2, line = 0, las = 1)
  axis(2, at = seq(0.9, 2.5, by = 0.1), tcl = -0.3, label = F, line = 0)
  
  # Labels
  title(main = paste(group, "countries"),  line = 0.2)
  title(ylab = "Total fertility rate", xlab = "Year", line = 2.5)

}

# Apply the function across countries
pdf(file = "figures/panel_fertility_trend.pdf", height=7, width=12)
# Create a colour vector
cols <- viridis::cividis(n = 6)
colour_count <- 1
# Set 
par(family = "serif", font = 1, cex.main = 1.5, cex.axis = 1.2, cex.lab = 1.5,
    mar = c(4, 4, 2, 2), mfrow = c(2, 3))
lapply(country_groups, plot_countrygroup)
dev.off()

### GGPLOT plot ------------------------------




# Plot the time-series
ggplot(df, aes(x=Year, y=TFR, group=Code)) +
  geom_vline(xintercept = 2008.5, col = "red") +
  geom_line(col = "darkgrey") +
  geom_line(data=subset(df, Code %in% countries), aes(col = Code)) +
  geom_point(data=subset(df, Code %in% countries), aes(col = Code)) +
  geom_text_repel(data=subset(study_countries, Code %in% countries & Year == 2017), 
            aes(col = Code, label = label), box.padding = 2, size = 5, family = "serif") +
  scale_colour_viridis_d("Country") +
  #scale_shape_discrete("Country") +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0.8, 2.5), xlim = c(2000, 2020)) +
  guides(colour = "none", shape = "none") + 
  facet_wrap(~ group) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.spacing.x = unit(1, "cm")
  )
  
ggsave(last_plot(), filename = "figures/panel_fertility.pdf",
       height = 12, width = 20, unit = "cm")

### END ##################################################
