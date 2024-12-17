# Code for reproducing results in the Thesis
This repository contains the [R](https://cran.r-project.org/) for reproducing the results in my thesis "The fertility puzzle". In case of questions, feel free to drop me an <schubert@demogr.mpg.de>.


# Structure
1. `code/plot_fertility_trend.R`: Produces the figure in the introduction that displays the fertility trends
2. `code/cohort_component_method.R`: contains code to make the cohort component projection
3. `code/prob_tfr_forecast.R`: contains code to reproduce the figure in the discussion section on the future fertility trends

## 1. Plot fertility trend
This file visualises the fertility in the largest countries in the [Human Fertility Database](https://www.humanfertility.org/). The countries are split over panels according to their geographic region: Anglo-Saxon, East-Asian, Eastern Europe, German-speaking, Mediterranean, and the Nordics. Furthermore, highlighted countries are Finland, France, Norway, and the United States.


![Time trend of the total fertility rate across countries grouped by geographic-cultural similarities in the period between 2000 and 2020. *Source:* Data is obtained from the Human Fertility Database.](./figures/panel_fertility.pdf)

## 2. Cohort component method
The cohort component method is widely used to project population structures and sizes. The cohort component methods allows to account for age structure. In this case, I used a cohort component method that has three dimensions: year (2023-2098), age (0-100+) and sex (male and female).

### Scenarios
As discussed in the thesis, I model two different scenarios:

1. **Freeze rate**: The freeze rate approach holds the current observed fertility rates (2023) constant for the projection horizon.

2. **Replacement-fertility**: The second scenario is a replacement fertility scenario, which adjusts the age-specific fertility rates so that the net replacement rate is 1. I proceeded as follows. First, I used the UN projection of the age-specific fertility rates to estimate the observed fertility schedule (see Scenario 1). Then, I estimate the observed net reproductive rate, 

$$NRR=\sum_{i=1}^n \frac{1}{1 + SRB} \cdot _nL_x \cdot f_x,$$

where $SRB$ is the sex ratio at birth, $_nL_x$ is the lived person-years in the life table and $f_x$ is the age-specific fertility rate. Afterwards, I obtain the adjusted age-specific fertility rates though dividing the observed rates by the net reproductive rate: $f_x^{NRR=1}= \frac{f_x}{NRR}$.


## 3. Future fertility

![This figure displays the median, min, max and the Monte Carlo forecasts of the total fertility rate by the United Nations population division.](./figures/un_prob_projections.pdf)


