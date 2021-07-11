library(tidyverse)
library(glmnet)

load("./data/spouseData.RData")


x <- model.matrix(stm ~ ., spouse.dta)[,-1]
y <- swiss$stm


summary(lm(stm ~ sstm + centage + educ_yrs + hhspend_log + children + adl + distress +
             gender * state  + residency + sep_q5 + caste + bmi + cardio + hhsmoke +
             vigorous,
           data = spouse.dta))
