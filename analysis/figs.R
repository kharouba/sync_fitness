## to make normal distribution curves

plot(function(x) dnorm(x, mean=0), -3, 3, lwd=3, xaxt="n", yaxt="n", xlab="day of year", ylab="fitness", cex.lab=3)
curve(dnorm(x), add=T)
curve(dnorm(x), add=T, col=1, lwd=4)
curve(dnorm(x, sd=1.5), add=T, col=2, lwd=4) #directly overlap
curve(dnorm(x, mean=-2, sd=1.5), add=T, col=2, lwd=4)
curve(dnorm(x, mean=2, sd=1.5), add=T, col=2, lwd=4, yaxis="")

#baseline
plot(function(x) dnorm(x, mean=0), -3, 3, lwd=3, xaxt="n", yaxt="n", xlab="relative timing (days)", ylab="fitness", cex.lab=3)
curve(dnorm(x), add=T)
curve(dnorm(x), add=T, col=1, lwd=4)
curve(dnorm(x, sd=4), add=T, col=2, lwd=4) #directly overlap

OR
d1<-density(rnorm(1000000, mean=0, sd=4))
d2<-density(rnorm(1000000, mean=0, sd=5.5))
plot(range(d1$x, d1$x), range(d1$y, d2$y), type="n", xaxt="n", yaxt="n", xlab="day of year", ylab="fitness", cex.lab=3)
lines(d2, col="red", lwd=4)
lines(d1, col="black", lwd=4)


d1<-density(rnorm(1000000, mean=0, sd=4))
d2<-density(rnorm(1000000, mean=-5, sd=5.5))
d3<-density(rnorm(1000000, mean=5, sd=3))
plot(range(d1$x, d2$x), range(d1$y, d1$y), type="n", xaxt="n", yaxt="n", xlab="day of year", ylab="fitness", cex.lab=3)
lines(d1, col="black", lwd=4)
lines(d2, col="red", lwd=4)
lines(d3, col="black")

#baseline
d1<-density(rnorm(1000000, mean=0, sd=6))
d2<-density(rnorm(500, mean=0, sd=10))
plot(range(d1$x, d1$x), range(d1$y, d2$y), type="n")
lines(d1, col="red")
lines(d2, col="black")

plot(function(x) dnorm(x, mean=0), -4,4, lwd=4)
#curve(dnorm(x), add=T)
#curve(dnorm(x), add=T, col=1)
curve(dnorm(x, sd=4.5), add=T, col=2, lwd=4) #directly overlap
#curve(dnorm(x, mean=-2, sd=2), add=T, col=2, lwd=3)
#curve(dnorm(x, mean=2, sd=2), add=T, col=2, lwd=3, yaxis="")


#EXPLORATORY
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

ggplot(subset(go, phenodiff>0 & intid!="int17"), aes(y=fitness_z, x=phenodiff, colour=as.factor(intid)))+geom_point()+geom_smooth(method="lm")

# make a f(x), which I adapted from one I found online
# and use lapply
doPlot <- function(sel_name) {
   subby <- super[super$studyid == sel_name,]
   ggobj <- ggplot(subby, aes(x=phenodiff_center, y=fitness_z)) +geom_point(size=3) +geom_smooth(method="lm", formula=y~x+I(x^2))+facet_wrap(~intid)+theme_bw()+theme(legend.position="none",axis.title.x =element_text(size=17), axis.text.x=element_text(size=17), axis.text.y=element_text(size=17), axis.title.y=element_text(size=17, angle=90))+ylab("fitness")+xlab("mismatch")
   print(ggobj)
   ggsave(sprintf("graphs/quad/int%s_center.pdf", sel_name))
}
#formula=y~x+I(x^2),
lapply(unique(super$studyid), doPlot)


#check fitness
doPlot <- function(sel_name) {
   subby <- go[go$studyid == sel_name,]
   ggobj <- ggplot(subby, aes(x=fitnessvalue))+geom_histogram()+facet_wrap(~intid)
   print(ggobj)
   ggsave(sprintf("graphs/fitness/hist_%s.pdf", sel_name))
}
lapply(unique(go$studyid), doPlot)


doPlot <- function(sel_name) {
   subby <- go[go$studyid == sel_name,]
   ggobj <- ggplot(subby, aes(x=fitness_z))+geom_histogram()+facet_wrap(~intid)
   print(ggobj)
   ggsave(sprintf("graphs/fitness/hist_%s_z.pdf", sel_name))
}
lapply(unique(go$studyid), doPlot)


## RELATED TO Q2: MISMATCH CHANGES OVER TIME
doPlot <- function(sel_name) {
   subby <- newya[newya$studyid == sel_name,]
   ggobj <- ggplot(subby, aes(x=year2, y=phenodiff_base))+geom_point(size=3) + geom_line()+facet_wrap(~intid)+geom_smooth(method="lm")+theme_bw()+ theme(legend.position="none",axis.title.x =element_text(size=17), axis.text.x=element_text(size=17), axis.text.y=element_text(size=17), axis.title.y=element_text(size=17, angle=90))+ylab("mismatch")+xlab("year")
   print(ggobj)
   ggsave(sprintf("graphs/overtime/int%s_base.pdf", sel_name))
}
#formula=y~x+I(x^2),
lapply(unique(newya$studyid), doPlot)


doPlot <- function(sel_name) {
   subby <- newya[best$intid == sel_name,]
   ggobj <- ggplot(subby, aes(x=year2, y=phenodiff))+geom_point(size=3) + geom_line()+facet_wrap(~intid)+geom_smooth(method="lm")+theme_bw()+ theme(legend.position="none",axis.title.x =element_text(size=17), axis.text.x=element_text(size=17), axis.text.y=element_text(size=17), axis.title.y=element_text(size=17, angle=90))+ylab("mismatch")+xlab("year")
   print(ggobj)
   ggsave(sprintf("graphs/overtime/int%s_base.pdf", sel_name))
}
#formula=y~x+I(x^2),
lapply(unique(newya$studyid), doPlot)


#related to Q3- changes in fitness over time
doPlot <- function(sel_name) {
   subby <- go[go$studyid == sel_name,]
   ggobj <- ggplot(subby, aes(x=year2, y=fitness_z)) +
       geom_point(size=3) + facet_wrap(~intid)+geom_smooth(method="lm")+theme_bw()+ theme(legend.position="none",axis.title.x =element_text(size=17), axis.text.x=element_text(size=17), axis.text.y=element_text(size=17), axis.title.y=element_text(size=17, angle=90))+ylab("fitness")+xlab("mismatch")
   print(ggobj)
   ggsave(sprintf("graphs/fitness/overtime/int%s_z.pdf", sel_name))
}
#formula=y~x+I(x^2),
lapply(unique(go$studyid), doPlot)

# MAP  #
library(rworldmap)
newmap<-getMap(resolution="coarse")
dad<-read.csv("analysis/input/studies_geog.csv", header=TRUE, na.strings="NA", as.is=TRUE)
goes<-merge(go[,c("studyid","intid")], dad, by=("studyid"))

coordinates(goes)<-c("x","y")
plot(newmap)
plot(goes, add=T, pch=21,  bg=2, col="red", cex=1.5)
plot(goes, add=T, pch=21,  bg=2, col=1, cex=1.5)