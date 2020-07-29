#This script provides data fitting of van Genuchten curve for each soil included in the study
#Data from Arnold et al. were fitted with Durner (two Van Genuchten curves), fitted parameter adapted form Ghezzehei 2019
# clear working space
rm(list=ls())

library(ggplot2)
library(reshape2)
library(dplyr)
library(minpack.lm)

mpdata <- read.csv(file = '6datasets_mp.csv')

test<-select(filter(mpdata,id=="wet_mid"), c(id,mvol, mwp))  #subsetting data by ID

#=======nsl fitting =======
x<-test$mwp
mvol<-test$mvol
dat<-data.frame(x=x)
dat$y<-mvol
dat$logmp<-log(x)

curve.nls = nlsLM(mvol ~ I(((1+(a*x)^n)^(1/n-1))*(thaS-thaR)+thaR),
                     start=list(a=1.3,
                                n=1.5,
                                thaS=0.5,
                                thaR=0.01),
                     data = dat)
coef(curve.nls)
summary(curve.nls)
#=======plot nsl fitting results=======

mp<-seq(0,10000)
logmp<-log(mp)

plot(mvol ~ logmp, data = dat)
lines(logmp, predict(curve.nls, newdata = data.frame(x = 0:10000)))

#====this part of code were used for samples with less than 5 observations, thaR was set to 0==
curve.nls = nlsLM(mvol ~ I(((1+(a*x)^n)^(1/n-1))*thaS),
                  start=list(a=1.3,
                             n=1.5,
                             thaS=0.5
                             ),
                  data = dat)
coef(curve.nls)
summary(curve.nls)