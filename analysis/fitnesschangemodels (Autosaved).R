#########################
#Q3- Has fitness changed over recent decades? - just for consumers

slim2<-rawlong[,c("studyid","year","fitnessvalue","genus","species","genus2","species2","fitnesstype")]
slim2$int<-with(slim2, paste(studyid,genus,species,genus2,species2))

total<-merge(rawtaxa[,c("studyid","intid","int")], slim2, by=c("studyid","int"))

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

go<-subset(total, intid!="int38" & studyid!="HMK039" & studyid!="HMK052" & studyid!="HMK050" & studyid!="HMK018" & studyid!="HMK024" & studyid!="HMK026" & intid!="int17") # get rid of HMK018 because residuals, HMK024 because no raw data; HMK050 because same as HMK054; HMK026 because same as HMK030
# exclude int17 because mismatch index => not measured in days!
go<-na.omit(go)
go$year2<-with(go, round(year, digits=0))


#Hinge
#1980- 3 groups of species: those with enough data <1980, those with some data but not enough <1980, and those with no data <1980
pre<-subset(go, year2<=1980)
pre$count<-1
sss<- aggregate(pre["count"], pre[c("studyid", "intid")], FUN=sum)
sss2<-subset(sss, count>=5) #datasets with enough data pre climate change
sss2$speciesid<-1:nrow(sss2) #number datas

hinge<-merge(go, sss2, by=c("studyid", "intid"))
hinge_pre<-subset(hinge, year<=1980); hinge_pre$newyear<-1980
hinge_post<-subset(hinge, year>1980); hinge_post$newyear<-hinge_post$year
hinges<-rbind(hinge_pre, hinge_post)

hinge_non<-subset(sss, count<5) #datasets with NOT enough data pre climate change (n=21)
hinge_non$speciesid<-1:nrow(hinge_non) #number datas
hinge_non2<-merge(go, hinge_non, by=c("studyid", "intid"))
hinge_non2$newyear<-hinge_non2$year
sums<-rbind(hinges, hinge_non2)

letstry<-anti_join(go, sums) #groups of intxns without any data below 1980
letstry$newyear<-letstry$year
letstry2<-rbind(letstry, sums[,c("studyid","int", "intid","year","fitnessvalue","genus","species","genus2","species2","fitnesstype","year2","newyear")])
go<-letstry2

sups<-aggregate(go["fitnessvalue"], go[c("studyid", "intid")], FUN=mean, na.action=na.omit); names(sups)[3]<-"fitnessmean"
go2<-merge(go, sups, by=c("studyid","intid"))

sds<-aggregate(go["fitnessvalue"], go[c("studyid", "intid")], FUN=sd); names(sds)[3]<-"fitness_sd"
go3<-merge(go2, sds, by=c("studyid","intid"))

go3$fitness_z<-with(go3, (fitnessvalue-fitnessmean)/fitness_sd)
go<-go3


N<-nrow(go)
y<-go$fitness_z
year<-go$newyear
Nint<-length(unique(go$intid)); Nint
intxn<-as.numeric(as.factor(go$intid))

ggplot(go, aes(y=fitness_z, x=year, colour=as.factor(intid)))+geom_point(size=1)+geom_smooth(method="lm", se=FALSE)+theme_bw()+theme(legend.position="none", axis.title.x = element_text(size=20), axis.text.x=element_text(size=20), axis.text.y=element_text(size=20), axis.title.y=element_text(size=20, angle=90))+ylab("Fitness (z-score)")+xlab("Year")
#+geom_hline(yintercept=0, line="dashed")
#+geom_abline(slope=0.0063, intercept=-12.64, size=0.75)


m1<-lme(fitness_z~newyear, random=~1|intid, data=go); summary(m1)
m2<-lme(fitness_z~year, random=~1|intid, data=go); summary(m2)
m3<-lme(fitnessvalue~newyear, random=~1|intid, data=go); summary(m3)
m1<-lmer(fitness_z~newyear + (newyear|intid), data=go); summary(m1)

fit.model<-stan("analysis/stanmodels/twolevelrandomeffects.stan", data=c("N","Nint","y","intxn","year"), iter=16000, chains=4)
print(fit.model, pars = c("mu_a", "mu_b", "sigma_y", "a", "b"))

fit.model<-stan("analysis/stanmodels/twolevelrandomslope.stan", data=c("N","Nint","y","intxn","year"), iter=8000, chains=4)
print(fit.model, pars = c("mu_b", "sigma_y", "a", "b"))
summary(fit.model, pars="mu_b")[[1]]

fit.model.int<-stan("analysis/stanmodels/twolevelrandomintercept.stan", data=c("N","Nint","y","intxn","year"), iter=8000, chains=4)
print(fit.model.int, pars = c("mu_a", "sigma_y", "a", "b"))

fh.sim <- extract(fit.model)# 
it1000 <- matrix(0, ncol=3000, nrow=Nint) #2000 iterations for 53 interactions;
for (i in 3000:6000){ # 2000 iterations?
    it1000[,(i-3000)] <- fh.sim$b[i,]
}
uni<-unique(go[,c("studyid","intid")])
uni$fitness<-rowMeans(it1000, na.rm=TRUE)

ggplot(uni, aes(x=fitness))+geom_histogram(binwidth=0.2, colour="black", fill="grey")+theme_bw()+geom_vline(xintercept=0,linetype=2,size=0.75)+theme(legend.position="none", axis.title.x = element_text(size=15), axis.text.x=element_text(size=15), axis.text.y=element_text(size=15), axis.title.y=element_text(size=15, angle=90))+ylab("Number of interactions")+xlab("Change in number of days/decade")

change<-summary(fit.model, pars="b")[[1]][1:25]
low<-summary(fit.model, pars="b")[[1]][76:100]
upper<-summary(fit.model, pars="b")[[1]][176:200]
dr<-data.frame(cbind(asdf, low, upper))
dr$id<-1:25
ggplot(dr, aes(x=factor(id), y=change))+geom_errorbar(aes(ymin=low, ymax=upper), width=.0025, colour="black")+geom_hline(yintercept=0, linetype="dashed")+geom_point(size=4, aes(order=abs(change)))+theme_bw()+coord_flip()+xlab("Interaction")+ylab("Change in fitness (z/year)")


