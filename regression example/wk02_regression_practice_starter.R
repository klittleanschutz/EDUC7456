# EDUC7456 wk02 regression practice

# Article: https://www.pnas.org/doi/full/10.1073/pnas.2312249121
# Data: https://osf.io/vfxjp/

setwd("C:/Users/littkate/Desktop/EDUC7456")

library(vtable)
library(sjPlot)
library(readxl)
library(tidyverse)

# load data -----

dee24 <- read_excel(path = "regression example/Chronic_Absenteeism_202310.xlsx", range = "A1:O52")

# subset to variables of interest -----

wk02_data <- select(dee24, ST, State, CAR_1819, CAR_2122, COVID19_Rate, REMOTE_2020)

# drop states with missing data -----

wk02_data <- drop_na(wk02_data)

# RQ1 Did states with higher prevalence of 
#remote instruction in 2021 experience larger increases in chronic absenteeism rates from 2019 to 2022

# y is CAR_2122- CAR_1819
# x1 = REMOTE_2020

wk02_data<-wk02_data%>%
            mutate(CAR_CHANGE = CAR_2122-CAR_1819)

sumtable(wk02_data)

# There are 41 states with non missing data
# plot 
plot(wk02_data$REMOTE_2020, wk02_data$CAR_CHANGE)

change_remote<-lm(CAR_CHANGE ~ REMOTE_2020, data = wk02_data)

plot(wk02_data$REMOTE_2020, wk02_data$CAR_CHANGE)
abline(change_remote, col = "blue")

tab_model(change_remote)

summary(change_remote)

#Interpretation:
# The model suggests that for each unit change in remote learning in 2022, 
# an 11.53% increase in absenteeism can be observed