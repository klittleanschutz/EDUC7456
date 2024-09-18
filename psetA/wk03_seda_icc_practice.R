#wk03 educ 7456 ICC practice

library(tidyverse)
library(lme4)
library(sjPlot)

rm(list=ls())

psetA_seda <- read.csv(file = "psetA/seda_psetA.csv")

psetA_data <- drop_na(psetA_seda)

ri_model <- lmer(avg_math ~ 1 + (1 | stateabb), data = psetA_data)

summary(ri_model)

