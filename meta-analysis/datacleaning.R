rm(list=ls())
library(nlme)
library(lme4)
library(ggplot2)
library(ggeffects)
library(car)
library(usdm) #for vifstep
library(MuMIn) #for dredge
library(geoR) #for box-cox
library(bbmle) # for AICctab
library(MASS)
source("/users/kharouba/google drive/UBC/summarySE.R")
source("/users/kharouba/google drive/UBC/multiplot.R")

#setwd("/users/kharouba/google drive/uOttawa/research/meta-analysis")


newdata<-read.csv("/users/kharouba/google drive/uOttawa/research/synchrony_fitness/papers/HMK088_SI.csv", header=TRUE, na.strings="NA", as.is=TRUE)

lol<-with(newdata, aggregate(half_fall, by=list(year), FUN=median, na.rm=T))
names(lol)[1]<-"year"; names(lol)[2]<-"cat_date"
lol2<-with(newdata, aggregate(April_hatch_date, by=list(year), FUN=median, na.rm=T))
names(lol2)[1]<-"year"; names(lol2)[2]<-"bird_date"

lol3<-with(newdata, aggregate(num_fledged, by=list(year), FUN=sum, na.rm=T))
names(lol3)[1]<-"year"; names(lol3)[2]<-"num_fled"

tot<-merge(lol, lol2)
tot2<-merge(tot, lol3)
tot2$mismatch<-with(tot2, bird_date-cat_date)

write.csv(tot2, "/users/kharouba/google drive/uOttawa/research/synchrony_fitness/papers/HMK088_phenodata.csv")


newdata<-read.csv("/users/kharouba/google drive/uOttawa/research/synchrony_fitness/papers/HMK024_greattit1.csv", header=TRUE, na.strings="NA", as.is=TRUE)

lol<-with(newdata, aggregate(SYN_SYNCHRONY, by=list(YEAR), FUN=mean, na.rm=T))


newdata<-read.csv("/users/kharouba/google drive/uOttawa/research/synchrony_fitness/papers/HMK024_willowtit1.csv", header=TRUE, na.strings="NA", as.is=TRUE)
lol<-with(newdata, aggregate(SYN_SYNCHRONY, by=list(YEAR), FUN=mean, na.rm=T))
