#########################
#Q2- Has mismatched changed over recent decades?


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


slim2<-rawlong[,c("studyid","year","phenodiff","genus","species","genus2","species2")]
slim2$int<-with(slim2, paste(studyid,genus,species,genus2,species2))

newya<-merge(rawtaxa[,c("studyid","intid","int")], slim2, by=c("studyid","int"))
newya<-na.omit(newya)
newya<-unique(newya[,c("studyid","intid","year","phenodiff","genus","species","genus2","species2")])
newya$year2<-with(newya, round(year, digits=0))

#for now, same as fitness data
newya2<-subset(newya, intid!="int38" & studyid!="HMK039" & studyid!="HMK052" & studyid!="HMK050" & studyid!="HMK018" & studyid!="HMK024" & studyid!="HMK026" & intid!="int17") # get rid of HMK018 because residuals, HMK024 because no raw data; HMK050 because same as HMK054; HMK026 because same as HMK030



#Hinge
#1980- 3 groups of species: those with enough data <1980, those with some data but not enough <1980, and those with no data <1980
pre<-subset(newya2, year2<=1980)
pre$count<-1
sss<- aggregate(pre["count"], pre[c("studyid", "intid")], FUN=sum)
sss2<-subset(sss, count>=5);sss2 #datasets with enough data pre climate change
sss2$speciesid<-1:nrow(sss2) #number datas

hinge<-merge(newya2, sss2, by=c("studyid", "intid"))
hinge_pre<-subset(hinge, year<=1980); hinge_pre$newyear<-1980
hinge_post<-subset(hinge, year>1980); hinge_post$newyear<-hinge_post$year
hinges<-rbind(hinge_pre, hinge_post)

hinge_non<-subset(sss, count<5) #datasets with NOT enough data pre climate change (n=21)
hinge_non$speciesid<-1:nrow(hinge_non) #number datas
hinge_non2<-merge(newya2, hinge_non, by=c("studyid", "intid"))
hinge_non2$newyear<-hinge_non2$year
sums<-rbind(hinges, hinge_non2)

letstry<-anti_join(newya2, sums) #groups of intxns without any data below 1980
letstry$newyear<-letstry$year
letstry2<-rbind(letstry, sums[,c("studyid", "intid","year","phenodiff","genus","species","genus2","species2","year2","newyear")])
newya2<-letstry2
newya2$yr1980 <- newya2$newyear-1980


newya2$count<-1
sun<- aggregate(newya2["count"], newya2[c("studyid", "intid")], FUN=sum)


N<-nrow(newya2)
y<-abs(newya2$phenodiff)
year<-newya2$yr1980
Nint<-length(unique(newya2$intid)); Nint
intxn<-as.numeric(as.factor(newya2$intid))

ggplot(newya2, aes(y=abs(phenodiff), x=yr1980, colour=as.factor(intid)))+geom_point()+geom_smooth(method="lm", se=FALSE)


mis.model<-stan("analysis/stanmodels/twolevelrandomeffects.stan", data=c("N","Nint","y","intxn","year"), iter=16000, chains=4)
print(fit.model, pars = c("mu_a", "mu_b", "sigma_y", "a", "b"))


# check- weird doubling in Neff- I think it has to do with newyear vs yr1980
mis.model<-stan("analysis/stanmodels/twolevelrandomslope.stan", data=c("N","Nint","y","intxn","year"), iter=6000, chains=4)
print(mis.model, pars = c("mu_b", "sigma_y", "a", "b"))
summary(mis.model, pars="mu_b")[1]


fh.sim <- extract(mis.model)# 
it1000 <- matrix(0, ncol=3000, nrow=Nint) #2000 iterations for 53 interactions;
for (i in 3000:6000){ # 2000 iterations?
    it1000[,(i-3000)] <- fh.sim$b[i,]
}
uni<-unique(newya2[,c("studyid","intid")])
uni$mismatch<-rowMeans(it1000, na.rm=TRUE)

t.test(abs(rowMeans(it1000, na.rm=TRUE)))
wilcox.test(abs(rowMeans(it1000, na.rm=TRUE)))
median(abs((rowMeans(it1000, na.rm=TRUE)*10)))

a<-ggplot(uni, aes(x=abs(mismatch*10)))+geom_histogram(binwidth=2, colour="black", fill="grey")+theme_bw()+ylim(0,13)+geom_vline(xintercept=0,linetype=2,size=0.75)+theme(legend.position="none", axis.title.x = element_text(size=17), axis.text.x=element_text(size=17), axis.text.y=element_text(size=17), axis.title.y=element_text(size=17, angle=90))+ylab("Number of interactions")+xlab("relative timing change (days/decade)")+geom_vline(xintercept=median(abs((rowMeans(it1000, na.rm=TRUE)*10))),size=0.75, colour="red")
b<-ggplot(uni, aes(x=mismatch*10))+geom_histogram(binwidth=2, colour="black", fill="grey")+ylim(0,13)+theme_bw()+geom_vline(xintercept=0,linetype=2,size=0.75)+theme(legend.position="none", axis.title.x = element_text(size=17), axis.text.x=element_text(size=17), axis.text.y=element_text(size=17), axis.title.y=element_text(size=17, angle=90))+ylab("Number of interactions")+xlab("relative timing change (days/decade)")+geom_vline(xintercept=-0.08830949,size=0.75, colour="red")
multiplot(a,b, cols=2)
