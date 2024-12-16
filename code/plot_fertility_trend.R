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
study_countries <- df[df$Code %in% countries, ]
study_countries$label <- ""
for (i in seq_along(countries)) {
  study_countries$label[study_countries$Code == countries[i]] <- c_labs[i]
  
}

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

S### END ##################################################
