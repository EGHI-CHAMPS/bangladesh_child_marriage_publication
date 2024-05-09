###########
### PURPOSE: logistic regression examining perinatal outcomes and maternal age
### DOI: https://doi.org/10.1371/journal.pone.0288746
###########


###### PACKAGES ######
library(tidyverse)
library(nnet)



###### DATA ######
### import data
dat<-read.csv("data/child_marriage/repo/perinatal_outcomes.csv")

### format
dat<-dat %>%
  mutate(outcome=factor(outcome, levels=c("None","Stillbirth","Early neonatal death")),
         age_group=factor(age_group, levels=c("18-34 Years","13-15 Years","16-17 Years")),
         wealth_quintile=factor(wealth_quintile))

### subset nulliparious women
par0<-dat %>%
  filter(parity=="0")



###### MULTINOMIAL MODELS ######
### all mothers, adjust for maternal age
multi <- nnet::multinom(outcome ~ age_group, data=dat)
summary(multi)
multi

ci<-confint(multi, level=0.95)
exp(coef(multi))
exp(ci)

### only nulliparious, adjusted for maternal age & household wealth
first <- nnet::multinom(outcome ~ age_group + wealth_quintile, data=par0)

ci<-confint(first, level=0.95)
exp(coef(first))
exp(ci)





