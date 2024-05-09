###########
### PURPOSE: create figures for 1) distribution of age at marriage by year and 2) mean/sd age at marriage by year
### DOI: https://doi.org/10.1371/journal.pone.0288746
###########



###### PACKAGES ######
library(tidyverse)
library(viridis)



###### DATA ######
### import data
dat<-read.csv("data/child_marriage/repo/marriage_age_by_year.csv")
names(dat)

### create age at marriage groups
summary(dat$marriage_age)

dat<-dat %>%
  mutate(agegrp=ifelse(marriage_age<=12, 0,
                        ifelse(marriage_age>=13 & marriage_age<=15, 1,
                               ifelse(marriage_age>=16 & marriage_age<=17, 2,
                                      ifelse(marriage_age>=18, 3, NA)))))

table(dat$agegrp, useNA="ifany")
tapply(dat$marriage_age, dat$agegrp, summary)

### create dataset - proportion of marriage by age group
num<-dat %>%
  group_by(agegrp, marriage_year) %>%
  summarize(n=n())
den<-dat %>%
  group_by(marriage_year) %>%
  summarize(d=n())
ageprop<-num %>%
  left_join(den, by=c("marriage_year")) %>%
  mutate(prop=n/d*100)

### create dataset - average age at marriage by year
agemean<-dat %>% 
  group_by(marriage_year) %>% 
  summarize(age=mean(marriage_age, na.rm=T), 
            sd=sd(marriage_age, na.rm=T))



###### FIGURES ######
ggplot(ageprop, aes(x=marriage_year, y=prop, fill=as.factor(agegrp))) + geom_bar(stat="identity") +
    scale_fill_viridis(discrete=T, option="viridis",
                       name="Age (years)",
                       labels=c(expression(""<="12"), "13-15", "16-17", expression("">="18"))) +
    labs(x="Year", y="Proportion of marriages (%)") +
    scale_x_continuous(breaks=c(1990, 2000, 2010, 2019)) +
    theme_bw()+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.title=element_text(size=10),
          legend.text=element_text(size=8))

ggplot() + geom_point(data=agemean, aes(x=marriage_year, y=age)) +
    geom_errorbar(data=agemean, aes(x=marriage_year, ymin = (age-sd), ymax = (age+sd), width=.5)) +
    scale_x_continuous(name="Year", breaks=c(1990, 2000, 2010, 2019), limits=c(1989,2020)) +
    scale_y_continuous(name="Mean age (SD)", limits=c(10,25)) +
    theme_bw() +
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)))

