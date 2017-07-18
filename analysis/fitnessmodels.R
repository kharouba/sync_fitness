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

#get data
rawlong <- read.csv("analysis/input/rawfitness.csv", header=TRUE)
rawtaxa <- read.csv("analysis/input/rawtaxa.csv", header=TRUE)

rawlong<-subset(rawlong, phenodiffunits!="correlation coefficient")

rawtaxa$int<-with(rawtaxa, paste(studyid,genus,species,genus2,species2))


slim<-rawlong[,c("studyid","phenodiff","genus","species","genus2","species2","fitnessvalue","fitnesstype")]
slim$int<-with(slim, paste(studyid,genus,species,genus2,species2))

total<-merge(rawtaxa[,c("studyid","intid","int")], slim, by=c("studyid","int"))

#Q1- test of match-mismatch hypothesis
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

trans2<-subset(total, intid!="29")
sub<-subset(total, intid=="29" & fitnesstype=="fitness")
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


! TAKE MEAN BEFORE OR AFTER ABSOLUTE VALUE ??
#center mismatch and fitness
total<-na.omit(total)
sup<-aggregate(total["phenodiff"], total[c("studyid", "intid")], FUN=mean, na.action=na.omit); names(sup)[3]<-"phenodiffmean"

total2<-merge(total, sup, by=c("studyid","intid"))
total2$phenodiff_center<-with(total2, phenodiff/phenodiffmean)

sups<-aggregate(total["fitnessvalue"], total[c("studyid", "intid")], FUN=mean, na.action=na.omit); names(sups)[3]<-"fitnessmean"
total3<-merge(total2, sups, by=c("studyid","intid"))
total3$fitness_center<-with(total3, fitnessvalue/fitnessmean)

# FITNESS: only for intxns where NEGATIVE effect of mismatch predicted i.e. consumers in resource-consumer intxns (ANY SPP WITH POSITIVE ROLE), exclude competition, parasitism but include pollinator
go<-subset(total3, intid!="38" & studyid!="HMK039" & studyid!="HMK052" & studyid!="HMK018" & studyid!="HMK024") # get rid of HMK018 because residuals, HMK024 because no raw data; include HMK050 for now- even if standardized

# Just magnitude
go2<-subset(go, studyid!="HMK003")
sub<-subset(go, studyid=="HMK003" & phenodiff>0)
sub$phenodiff<-with(sub, -(phenodiff))
sub1<-subset(go, studyid=="HMK003" & phenodiff<0)
sub1$phenodiff<-abs(sub1$phenodiff)
go<-rbind(go2, sub, sub1)

doPlot <- function(sel_name) {
   subby <- go[go$studyid == sel_name,]
   ggobj <- ggplot(data=subset(subby, phenodiff>0), aes(x=phenodiff, y=fitnessvalue)) +
       geom_point(size=3) + facet_wrap(~intid)+geom_smooth(method="lm")+theme_bw()+ theme(legend.position="none",axis.title.x =element_text(size=17), axis.text.x=element_text(size=17), axis.text.y=element_text(size=17), axis.title.y=element_text(size=17, angle=90))+ylab("fitness")+xlab("mismatch")
   print(ggobj)
   ggsave(sprintf("graphs/int%s_mag.pdf", sel_name))
}
#formula=y~x+I(x^2),
lapply(unique(go$studyid), doPlot)

ggplot(go, aes(y=fitness_center, x=phenodiff_center, colour=as.factor(intid)))+geom_point()

# make a f(x), which I adapted from one I found online
# and use lapply
doPlot <- function(sel_name) {
   subby <- go[go$studyid == sel_name,]
   ggobj <- ggplot(data=subby, aes(x=phenodiff_center, y=fitnessvalue)) +
       geom_point(size=3) + facet_wrap(~intid)+geom_smooth(method="lm")+theme_bw()+ theme(legend.position="none",axis.title.x =element_text(size=17), axis.text.x=element_text(size=17), axis.text.y=element_text(size=17), axis.title.y=element_text(size=17, angle=90))+ylab("fitness")+xlab("mismatch")
   print(ggobj)
   ggsave(sprintf("graphs/int%s_center.pdf", sel_name))
}
#formula=y~x+I(x^2),
lapply(unique(go$studyid), doPlot)

doPlot <- function(sel_name) {
   subby <- go[go$studyid == sel_name,]
   ggobj <- hist(subby$fitnessvalue)
   print(ggobj)
   ggsave(sprintf("graphs/hist_int%s.pdf", sel_name))
}
lapply(unique(go$studyid), doPlot)
