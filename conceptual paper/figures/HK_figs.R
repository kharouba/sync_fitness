setwd("/users/kharouba/google drive/uOttawa/research/synchrony_fitness/figures_conceptual")
source("/users/kharouba/google drive/UBC/multiplot.R")

## Figure 3
raw <- read.csv("HMK038_data.csv", header=TRUE, na.strings="NA")
sub<-subset(raw, tree!="NA")
yes<-aggregate(sub["mortality"], sub[c("day")], FUN=median)
yes2<-aggregate(sub["time"], sub[c("day")], FUN=median); names(yes2)[2]<-"corr_time"
yes3<-merge(yes,yes2)
yes3$expt<-"first"

sub2<-raw[1:16,c("mortality","corr_time")]
sub2$expt<-"second"
raw2<-rbind(sub2,yes3[,2:4])

raw2$surv<-with(raw2, 100-mortality)
a<-ggplot(raw2, aes(corr_time, y=surv))+geom_vline(xintercept=0, linetype="dashed", size=0.5)+geom_point(aes(colour=factor(expt)), size=3)+ylim(0,100)+xlim(-90, 90)+theme_bw()+theme(legend.position="none", axis.title.x = element_text(size=15), axis.text.x=element_text(size=15), axis.text.y=element_text(size=15), axis.title.y=element_text(size=15, angle=90))+ylab("Survival of O. brumata (%)")+xlab("")+annotation_custom(grob = textGrob(label = "a)", hjust = 0, gp = gpar(cex = 1.5)), ymin = 100, ymax = 100, xmin = -90, xmax = -90)
#+geom_smooth(method="lm", formula=y~x+I(x^2), se=FALSE)


obs <- read.csv("vanAsch_data.csv", header=TRUE, na.strings="NA")
obs$order<-10:1 

b<-ggplot(obs, aes(y=mismatch_med, x=factor(reorder(year,order))))+coord_flip()+geom_hline(yintercept=0, linetype="dashed", size=0.5)+geom_point(size=3)+geom_errorbar(aes(ymin=mismatch_low, ymax=mismatch_high), width=.0025, colour="black")+theme_bw()+theme(legend.position="none", axis.title.x = element_text(size=15), axis.text.x=element_text(size=15), axis.text.y=element_text(size=15), axis.title.y=element_text(size=15, angle=90))+ylab("relative timing(days)")+xlab("")+ylim(-90, 90)+ annotation_custom(grob = textGrob(label = "b)", hjust = 0, gp = gpar(cex = 1.5)), ymin = -90, ymax = -90, xmin = 10, xmax = 10)
png('Fig3_v1.png', width=800,height=950, res=100)
multiplot(a,b,cols=1)
dev.off()

## to make normal distribution curves-- Figure 1

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