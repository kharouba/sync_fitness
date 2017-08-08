#Started July 2017
rm(list=ls()) 
options(stringsAsFactors = FALSE)
 
setwd("/users/kharouba/google drive/uOttawa/research/synchrony_fitness/")

# libraries
library(ggplot2)
library(rstan)
library(shinystan)
library(grid)
library(nlme)
library(dplyr)
library(ggrepel)
library(lme4)
source("/users/kharouba/google drive/UBC/multiplot.R")

#get data
rawlong <- read.csv("analysis/input/rawfitness.csv", header=TRUE)
rawtaxa <- read.csv("analysis/input/rawtaxa.csv", header=TRUE)

rawlong<-subset(rawlong, phenodiffunits!="correlation coefficient")

rawtaxa$int<-with(rawtaxa, paste(studyid,genus,species,genus2,species2))

#for intxn with mult sites: chose site with biggest sample size
trans2<-subset(rawlong, studyid!="HMK023")
sub<-subset(rawlong, studyid=="HMK023" & site=="nopporo")
rawlong<-rbind(trans2,sub)

#now join
slim<-rawlong[,c("studyid","phenodiff","genus","species","genus2","species2","fitnessvalue","fitnesstype")]
slim$int<-with(slim, paste(studyid,genus,species,genus2,species2))

total<-merge(rawtaxa[,c("studyid","intid","int")], slim, by=c("studyid","int"))

#####   Q1- test of match-mismatch hypothesis #####
# chose best fitness IF multiple
HMK011- survival1= fledged chicks
HMK026- reproduction
HMK029- demographic
HMK030- reproduction
HMK016- survival
HMK050- demographic
HMK002- demographic1
HMK042- fitness for intid-29
HMK025- fitness; need to choose because only set of years where mismatch is not standardized
HMK046- demographic (cant do fitness because inverse i.e. loss)
HMK054- fitness
AO001-  reproductive
HMK059- demographic1 (used in analysis)

trans2<-subset(total, studyid!="HMK011")
sub<-subset(total, studyid=="HMK011" & fitnesstype=="survival1")
total<-rbind(trans2,sub)

trans2<-subset(total, studyid!="HMK026")
sub<-subset(total, studyid=="HMK026" & fitnesstype=="reproduction")
total<-rbind(trans2,sub)

trans2<-subset(total, studyid!="HMK029")
sub<-subset(total, studyid=="HMK029" & fitnesstype=="demographic")
total<-rbind(trans2,sub)

trans2<-subset(total, studyid!="HMK030")
sub<-subset(total, studyid=="HMK030" & fitnesstype=="reproduction")
total<-rbind(trans2,sub)

trans2<-subset(total, studyid!="HMK016")
sub<-subset(total, studyid=="HMK016" & fitnesstype=="survival")
total<-rbind(trans2,sub)

trans2<-subset(total, studyid!="HMK050")
sub<-subset(total, studyid=="HMK050" & fitnesstype=="demographic")
total<-rbind(trans2,sub)

trans2<-subset(total, studyid!="HMK002")
sub<-subset(total, studyid=="HMK002" & fitnesstype=="demographic1")
total<-rbind(trans2,sub)

trans2<-subset(total, intid!="int29")
sub<-subset(total, intid=="int29" & fitnesstype=="fitness")
total<-rbind(trans2,sub)

trans2<-subset(total, studyid!="HMK025")
sub<-subset(total, studyid=="HMK025" & fitnesstype=="fitness")
total<-rbind(trans2,sub)

trans2<-subset(total, studyid!="HMK046")
sub<-subset(total, studyid=="HMK046" & fitnesstype=="demographic")
total<-rbind(trans2,sub)

trans2<-subset(total, studyid!="HMK054")
sub<-subset(total, studyid=="HMK054" & fitnesstype=="fitness")
total<-rbind(trans2,sub)

trans2<-subset(total, studyid!="AO001")
sub<-subset(total, studyid=="AO001" & fitnesstype=="reproductive")
total<-rbind(trans2,sub)

trans2<-subset(total, studyid!="HMK059")
sub<-subset(total, studyid=="HMK059" & fitnesstype=="demographic1")
total<-rbind(trans2,sub)


#center mismatch and fitness
total<-na.omit(total)
sup<-aggregate(total["phenodiff"], total[c("studyid", "intid")], FUN=mean, na.action=na.omit); names(sup)[3]<-"phenodiffmean"

total2<-merge(total, sup, by=c("studyid","intid"))
total2$phenodiff_center<-with(total2, phenodiff/phenodiffmean)

sups<-aggregate(total["fitnessvalue"], total[c("studyid", "intid")], FUN=mean, na.action=na.omit); names(sups)[3]<-"fitnessmean"
total3<-merge(total2, sups, by=c("studyid","intid"))
total3$fitness_center<-with(total3, fitnessvalue/fitnessmean)

sds<-aggregate(total["fitnessvalue"], total[c("studyid", "intid")], FUN=sd); names(sds)[3]<-"fitness_sd"
total4<-merge(total3, sds, by=c("studyid","intid"))

total4$fitness_z<-with(total4, (fitnessvalue-fitnessmean)/fitness_sd)


# FITNESS: only for intxns where NEGATIVE effect of mismatch predicted i.e. consumers in resource-consumer intxns (ANY SPP WITH POSITIVE ROLE), exclude competition, parasitism but include pollinator

go<-subset(total4, intid!="int38" & studyid!="HMK039" & studyid!="HMK052" & studyid!="HMK050" & studyid!="HMK018" & studyid!="HMK024" & studyid!="HMK026" & intid!="int17") # get rid of HMK018 because residuals, HMK024 because no raw data; HMK050 because same as HMK054; HMK026 because same as HMK030
# exclude int17 because mismatch index => not measured in days!

#double check count per intxn
go$count<-1
sun<-aggregate(go["count"], go[c("studyid", "intid")], FUN=sum)

#Analysis
# Approach 1s- exclude studies where changing values on mismatch axis means 2 things

RECHECK THESE INTXNS
yano<-subset(go, intid=="int37" | intid=="int1" | intid=="int4" | intid=="int5" | intid=="int6" | intid=="int30" | intid=="int14" | intid=="int15" | intid=="int17" | intid=="int22" | intid=="int23" | intid=="int24" | intid=="int33") #n=13 interactions: 37, 1, 4, 5, 6, 30, 14, 15, 17, 22, 23, 24, 33

# Approach 2- only take part of axis where food available (i.e. positive)
go$count<-1
yano<-subset(go, phenodiff>0)
sun<-aggregate(yano["count"], yano[c("studyid", "intid")], FUN=sum)
names(sun)[3]<-"totalcount"
yano2<-merge(yano, sun, by=c("studyid", "intid"))
yano3<-subset(yano2, totalcount>4)
yano2<-yano3

m1<-lmer(fitness_z~phenodiff+ (1|intid), data=yano2); summary(m1)
m2<-lmer(fitness_z~phenodiff+ (phenodiff|intid), data=yano2); summary(m2)

dad<-read.csv("analysis/input/studies_geog.csv", header=TRUE, na.strings="NA", as.is=TRUE)
yano3<-merge(yano2, dad[,c("studyid","eco1","eco2")], by="studyid")
yano2<-subset(yano3, eco1!="terrestrial")
yano2<-subset(yano3, eco1=="terrestrial")


N<-nrow(yano2)
y<-yano2$fitness_z
year<-yano2$phenodiff
Nint<-length(unique(yano2$intid)); Nint
intxn<-as.numeric(as.factor(yano2$intid))



##Approach 3- only take part of axis where negative
go$count<-1
yano<-subset(go, phenodiff<0)
sun<-aggregate(yano["count"], yano[c("studyid", "intid")], FUN=sum)
names(sun)[3]<-"totalcount"
yano2<-merge(yano, sun, by=c("studyid", "intid"))
yano3<-subset(yano2, totalcount>4)
yano<-yano3

N<-nrow(yano)
y<-yano$fitness_z
year<-yano$phenodiff
Nint<-length(unique(yano$intid)); Nint
intxn<-as.numeric(as.factor(yano$intid))


## Approach 4- take intxns with negative and positive mismatch
super<-subset(go, intid!="int4" & intid!="int5" & intid!="int6" & intid!="int30" &  intid!="int15" & intid!="int17" & intid!="int22" & intid!="int23" & intid!="int24" & intid!="int33") 
su<-aggregate(super["phenodiff"], super[c("studyid", "intid")], FUN=range); nrow(su) # check no positive datasets; 
super$year2<-(super$phenodiff_center)^2

dad<-read.csv("analysis/input/studies_geog.csv", header=TRUE, na.strings="NA", as.is=TRUE)
super2<-merge(super, dad[,c("studyid","eco1","eco2")], by="studyid")
super3<-subset(super2, eco1!="terrestrial")
super3<-subset(super2, eco1=="terrestrial")

N<-nrow(super)
y<-super$fitness_z
year<-super$phenodiff_center
year2<-super$year2
Nint<-length(unique(super$intid)); Nint
intxn<-as.numeric(as.factor(super$intid))

#which random effects model for quad?
m1<-lmer(fitness_z~phenodiff_center + I(phenodiff_center^2)+(1|intid), data=super); summary(m1) #only random intercepts
m2<-lmer(fitness_z~phenodiff_center + I(phenodiff_center^2)+(0+phenodiff_center|intid), data=super); summary(m2) #only random slopes
m3<-lmer(fitness_z~phenodiff_center+ I(phenodiff_center^2)+ (1+phenodiff_center|intid), REML=FALSE, data=super); summary(m3) #both intercepts and slopes
AIC(m1,m2,m3)

#which random effects model for linear?
m1<-lmer(fitness_z~phenodiff_center +(1|intid), data=super); summary(m1) #only random intercepts
m2<-lmer(fitness_z~phenodiff_center +(0+phenodiff_center|intid), data=super); summary(m2) #only random slopes
m3<-lmer(fitness_z~phenodiff_center+ (1+phenodiff_center|intid), REML=FALSE, data=super); summary(m3) #both intercepts and slopes
AIC(m1,m2,m3)

#quad or linear
m1<-lmer(fitness_z~phenodiff_center +(0+phenodiff_center|intid), data=super); summary(m1)
m2<-lmer(fitness_z~phenodiff_center + I(phenodiff_center^2)+(0+phenodiff_center|intid), data=super); summary(m2)
AIC(m1,m2)
confint(m1) #profile confidence intervals

fit.model<-stan("analysis/stanmodels/twolevelrandomeffects.stan", data=c("N","Nint","y","intxn","year"), iter=16000, chains=4)
print(fit.model, pars = c("mu_a", "mu_b", "sigma_y", "a", "b"))

fit.model<-stan("analysis/stanmodels/twolevelrandomslopes_quad.stan", data=c("N","Nint","y","intxn","year", "year2"), iter=8000, chains=4)
print(fit.model, pars = c("mu_b1", "mu_b2", "sigma_y", "a", "b1"))

fit.model<-stan("analysis/stanmodels/twolevelrandomslope.stan", data=c("N","Nint","y","intxn","year"), iter=16000, chains=4)
print(fit.model, pars = c("mu_b", "sigma_y", "a", "b"))
summary(fit.model, pars="mu_b")[[1]]
asdf<-summary(fit.model, pars="b")[[1]]
mean(asdf[1:21]; 

#fit.model<-stan("analysis/stanmodels/twolevelrandomintercept.stan", data=c("N","Nint","y","intxn","year"), iter=6000, chains=4)
#print(fit.model, pars = c("mu_a", "sigma_y", "a", "b"))

#overall figure
goo<-extract(fit.model)
mean(colMeans(goo$a))
ggplot(yano2, aes(y=fitness_z, x=phenodiff, colour=as.factor(intid)))+geom_hline(yintercept=0, linetype="dashed")+geom_point()+geom_smooth(method="lm", se=FALSE)+theme_bw()+theme(legend.position="none", axis.title.x = element_text(size=15), axis.text.x=element_text(size=15), axis.text.y=element_text(size=15), axis.title.y=element_text(size=15, angle=90))+geom_abline(slope=-0.02, intercept=0.45, size=0.75)+ylab("Fitness (z-score)")+xlab("mismatch (days)")

a<-ggplot(subset(yano3, eco1=="terrestrial"), aes(y=fitness_z, x=phenodiff, colour=as.factor(intid)))+geom_point()+geom_smooth(method="lm", se=FALSE)+theme_bw()+theme(legend.position="none", axis.title.x = element_text(size=15), axis.text.x=element_text(size=15), axis.text.y=element_text(size=15), axis.title.y=element_text(size=15, angle=90))+geom_abline(slope=-0.04, intercept=0.45, size=0.75)+ylab("Fitness (z-score)")+xlab("mismatch (days)")
ggplot(subset(yano3, eco1!="terrestrial"), aes(y=fitness_z, x=phenodiff, colour=as.factor(intid)))+geom_point()+geom_smooth(method="lm", se=FALSE)+theme_bw()+theme(legend.position="none", axis.title.x = element_text(size=15), axis.text.x=element_text(size=15), axis.text.y=element_text(size=15), axis.title.y=element_text(size=15, angle=90))+geom_abline(slope=-0.02, intercept=0.45, size=0.75)+ylab("Fitness (z-score)")+xlab("mismatch (days)")

#quadratic
asdf<-summary(fit.model, pars="mu_b2")
a<-ggplot(super, aes(y=fitness_z, x=phenodiff, colour=as.factor(intid)))+geom_vline(xintercept=0, linetype="dashed")+geom_hline(yintercept=0, linetype="dashed")+geom_point(size=1)+geom_smooth(method="lm", se=FALSE, formula=y~x+I(x^2))+theme_bw()+theme(legend.position="none", axis.title.x = element_text(size=15), axis.text.x=element_text(size=15), axis.text.y=element_text(size=15), axis.title.y=element_text(size=15, angle=90))+ylab("Fitness (z-score)")+xlab("mismatch (days)")
b<-ggplot(super, aes(y=fitness_z, x=phenodiff, colour=as.factor(intid)))+geom_vline(xintercept=0, linetype="dashed")+geom_hline(yintercept=0, linetype="dashed")+geom_point(size=1)+geom_smooth(method="lm", se=FALSE)+theme_bw()+theme(legend.position="none", axis.title.x = element_text(size=15), axis.text.x=element_text(size=15), axis.text.y=element_text(size=15), axis.title.y=element_text(size=15, angle=90))+ylab("")+xlab("mismatch (days)")
multiplot(a,b, cols=2)

#non-bayesian
1|unit= random intercept
x= random slope
0+x|unit = random regression coefficient without corresponding random intercept
1+x|unit= random intercept AND slope (or just x|unit)
m1<-lmer(fitness_z~phenodiff + (1+phenodiff|intid), data=yano); summary(m1)
m2<-lmer(fitness_z~phenodiff + (0+phenodiff|intid), data=yano); summary(m2)
m3<-lmer(fitness_z~phenodiff + (1|intid), data=yano); summary(m3)
m4<-lmer(fitness_z~1 + (1|intid), data=yano); summary(m4)
AIC(m1,m2, m3, m4)

mo2<-update(m1,~.-1); anova(m1,mo2)




