###########
### PURPOSE: survival analysis examining age at marriage and time to pregnancy
### DOI: https://doi.org/10.1371/journal.pone.0288746
###########



###### PACKAGES ######
library(survival)
library(tidyverse)
library(viridis)
library(ggfortify)
library(survminer)



###### DATA ######
### import data
dat<-read.csv("data/child_marriage/repo/time_to_pregnancy.csv")

### format
dat<-dat %>%
  mutate(age_group=factor(age_group),
         status_bin=ifelse(status=="Censored", 1,
                            ifelse(status=="Pregnancy", 2, NA)))

table(dat$status_bin, dat$status, useNA="ifany")

### survival fit
km_grp_fit <- survfit(Surv(time, status_bin) ~ age_group, data=dat)
print(km_grp_fit)
quantile(km_grp_fit)
summary(km_grp_fit, times=365)



###### CUMULTIVE INCIDENCE ######
### plot
(a<-ggsurvplot(km_grp_fit,
               data=dat,
               palette=c("#35B779FF","#31688EFF","#440154FF"),
               conf.int=T,
               pval=F,
               legend.labs=c("12-15 years","16-17 years","18-34 years"),
               break.time.by=100,
               xlab="Days since marriage",
               ylab="Cumulative incidence",
               risk.table.col = "strata", 
               surv.scale ="percent",
               legend = "bottom",
               legend.title = "",
               ggtheme = theme_bw(), 
               fun = "event"))

(b<-a$plot + geom_vline(xintercept = 365, color="grey60", linetype="longdash",size=1))

### log-rank test
surv_diff <- survdiff(Surv(time, status_bin) ~ age_group, rho=0, data = dat)
surv_diff
print(surv_diff, 5)


