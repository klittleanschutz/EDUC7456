# EDUC7456 wk02 regression practice

# Article: https://www.pnas.org/doi/full/10.1073/pnas.2312249121
# Data: https://osf.io/vfxjp/

library(vtable)
library(sjPlot)
library(readxl)
library(tidyverse)

# load data -----

dee24 <- read_excel(path = "Chronic_Absenteeism_202310.xlsx", range = "A1:O52")

# subset to variables of interest -----

wk02_data <- select(dee24, 
                    ST, State, CAR_1819, CAR_2122, COVID19_Rate, REMOTE_2020)

# drop states with missing data -----

wk02_data <- drop_na(wk02_data)

# add your code here...

# Part 1 Question 1 -----

wk02_data$CAR_CHANGE <- wk02_data$CAR_2122-wk02_data$CAR_1819

# Part 1 Question 2 -----

sumtable(wk02_data)

# Part 1 Question 4 -----

wk02_data %>%
  ggplot(aes(x=REMOTE_2020, y=CAR_CHANGE)) +
  geom_point(size=2) +
  geom_smooth(method="lm", se=FALSE) +
  xlab("Prevalence of Remote Learning in 2020-21") +
  ylab("Increase in Chronic Absenteeism\n(2018-19 to 2021-22)") +
  ylim(0,25) +
  labs(caption = "Source: Dee (2024); N=41.") +
  ggtitle("Increase in Chronic Absenteeism versus
          \nTime Spent in Remote Learning Across States") +
  theme_bw(base_size=20) +
  theme(legend.position="none")

# Part 1 Question 5 -----

model1 <- lm(CAR_CHANGE ~ REMOTE_2020, data = wk02_data)
tab_model(model1)

# Part 2 Question 1 -----

model2 <- lm(CAR_CHANGE ~ REMOTE_2020 + COVID19_Rate, data = wk02_data)
tab_model(model1, model2)

# Extensions -----

# Standardized models
model1z <- lm(scale(CAR_CHANGE) ~ scale(REMOTE_2020), data = wk02_data)
model2z <- lm(scale(CAR_CHANGE) ~ scale(REMOTE_2020) + scale(COVID19_Rate),
              data = wk02_data)

tab_model(model1z, model2z)

# Customize table

tab_model(model1, model2,
          digits=2,
          show.se=TRUE,
          show.ci=FALSE,
          string.est="B",
          string.se="SE",
          p.style="star",
          dv.labels = c("Model 1", "Model 2"))

