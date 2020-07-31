setwd("/users/kharouba/google drive/uOttawa/research/complete/conceptual paper/figures")
source("/users/kharouba/google drive/UBC/multiplot.R")

library(ggplot2)
library(grid)
library(gridExtra)

## to make normal distribution curves-- Figure 1

plot(function(x) dnorm(x, mean=0), -3, 3, lwd=3, xaxt="n", yaxt="n", xlab="time (days)", ylab="fitness", col=2, cex.lab=3) #was col=1
#curve(dnorm(x), add=T)
curve(dnorm(x), add=T, col=2, lwd=4)
curve(dnorm(x, sd=1.5), add=T, col=1, lwd=4) #directly overlap
curve(dnorm(x, mean=-2, sd=1.5), add=T, col=1, lwd=4)
curve(dnorm(x, mean=2, sd=1.5), add=T, col=1, lwd=4, ylab="")

#updated (Nov 2019)
#USE:


#First panel
x  = seq(-3, 3, length = 200)
y1 = dnorm(x, mean = 0,sd = 1)
y2 = 0.5*dnorm(x, mean = -2,sd = 1)

mydf = data.frame(x, y1, y2)

a<-ggplot(mydf, aes(x = x))+geom_line(aes(y = y1), size=1, linetype="dashed")+geom_line(aes(y = y2), size=1, colour = 'black')+theme_bw()+theme(plot.title = element_text(face = "bold", colour="red", size=18, hjust=0.5), axis.title.x = element_text(size=18), axis.title.y=element_text(size=18, angle=90), panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+annotation_custom(grob = textGrob(label = "consumer", hjust = 0, gp = gpar(cex = 1.5)), ymin =0.21, ymax = 0.23, xmin = -3, xmax = -3)+annotation_custom(grob = textGrob(label = "resource", hjust = 0, gp = gpar(cex = 1.5)), ymin =0.35, ymax = 0.35, xmin = -2.3, xmax = -2.3)+annotation_custom(grob = textGrob(label = "a)", hjust = 0, gp = gpar(cex = 1.5)), ymin =0.4, ymax = 0.4, xmin = -3, xmax = -3)+ylab("Consumer energetic demand/ \n Resource abundance")+xlab("")+labs(title="mismatch", colour="red")
#+geom_area(aes(y = pmin(y1, y2)), fill = 'gray60', alpha=0.5)

#Second panel
y1 = dnorm(x, mean = 0,sd = 1)
y2 = 0.5*dnorm(x, mean = 0,sd = 1)

mydf = data.frame(x, y1, y2)

b<-ggplot(mydf, aes(x = x))+geom_line(aes(y = y1), size=1, linetype="dashed")+geom_line(aes(y = y2), size=1, colour = 'black')+theme_bw()+theme(plot.title = element_text(face = "bold", size=18, hjust=0.5), axis.title.x = element_text(size=18), panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+ylab("")+xlab("Time (days)")+annotation_custom(grob = textGrob(label = "b)", hjust = 0, gp = gpar(cex = 1.5)), ymin =0.4, ymax = 0.4, xmin = -3, xmax = -3)+labs(title="match")
#+geom_area(aes(y = pmin(y1, y2)), fill = 'gray60', alpha=0.5)

#Third panel
y1 = dnorm(x, mean = 0,sd = 1)
y2 = 0.5*dnorm(x, mean = 2,sd = 1)

mydf = data.frame(x, y1, y2)

c<-ggplot(mydf, aes(x = x))+geom_line(aes(y = y1), size=1, linetype="dashed")+geom_line(aes(y = y2), size=1, colour = 'black')+theme_bw()+theme(plot.title = element_text(face = "bold", colou="red", size=18, hjust=0.5), panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+ylab("")+xlab("")+annotation_custom(grob = textGrob(label = "c)", hjust = 0, gp = gpar(cex = 1.5)), ymin =0.4, ymax = 0.4, xmin = -3, xmax = -3)+labs(title="mismatch", col="red")
#+geom_area(aes(y = pmin(y1, y2)), fill = 'gray60', alpha=0.5)

#d<-ggplot(mydf, aes(x = x))+geom_line(aes(y = y1), size=1)+theme_bw()+geom_vline(xintercept=0, linetype="dashed")+theme(axis.title.x = element_text(size=18), axis.title.y = element_text(size=18), panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+ylab("Fitness")+xlab("Relative timing (days)")+annotation_custom(grob = textGrob(label = "d)", hjust = 0, gp = gpar(cex = 1.5)), ymin =0.4, ymax = 0.4, xmin = -3, xmax = -3)+annotation_custom(grob = textGrob(label = "mismatch (a)", hjust = 0, gp = gpar(cex = 1.5,col="red", face="bold"), rot=73), ymin =0.2, ymax = 0.2, xmin = -1.5, xmax = -1.5)+annotation_custom(grob = textGrob(label = "mismatch (c)", hjust = 0, gp = gpar(cex = 1.5,col="red", fontface=2), rot=285), ymin =0.33, ymax = 0.33, xmin = 0.9, xmax = 0.9)+annotation_custom(grob = textGrob(label = "match (b)", hjust = 0, gp = gpar(cex = 1.5, face="bold")), ymin =0.4075, ymax = 0.4075, xmin = -1.16, xmax = 0)

d<-ggplot(mydf, aes(x = x))+geom_line(aes(y = y1), size=1)+theme_bw()+geom_vline(xintercept=0, linetype="dashed")+theme(axis.title.x = element_text(size=18), axis.title.y = element_text(size=18), panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+ylab("Total fitness")+xlab("Relative timing (days)")+annotation_custom(grob = textGrob(label = "d)", hjust = 0, gp = gpar(cex = 1.5)), ymin =0.4, ymax = 0.4, xmin = -3, xmax = -3)+annotation_custom(grob = textGrob(label = "mismatch", hjust = 0, gp = gpar(cex = 1.5,col="red", fontface=2)), ymin =0, ymax = 0, xmin = -2.6, xmax = -2.6)+annotation_custom(grob = textGrob(label = "mismatch", hjust = 0, gp = gpar(cex = 1.5,col="red", fontface=2)), ymin =0, ymax = 0, xmin = 0.9, xmax = 0.9)+annotation_custom(grob = textGrob(label = "match", hjust = 0, gp = gpar(cex = 1.5, fontface=2)), ymin =0.415, ymax = 0.415, xmin = -1.13, xmax = 0)+ylim(0,0.41)


t<-textGrob("")
w<-textGrob("")
grid.arrange(a,b,c,t,d,w, ncol=3)

#multiplot(a,b,c, cols=3)

*************************************
## FIgure 3- updated version Nov 19 2019
set.seed(177)
yrs <- c(1930:1980)
sp1 <- rnorm(length(yrs), 150, 5)
sp2 <- sp1-7+rnorm(length(yrs), 0, 4)

xlim <- c(1930,2017)
ylim <- c(110, 190)

mydf2<-data.frame(yrs, sp1, sp2)

#plot(sp1~yrs, type="l", col="darkslategray", xlim=xlim, ylim=ylim)
#lines(sp2~yrs, col="darkred", xlim=xlim, ylim=ylim)

a<-ggplot(mydf2, aes(x = yrs))+geom_line(aes(y = sp1), size=1, colour="darkslategray")+geom_line(aes(y = sp2), size=1, colour = 'darkred')+xlim(1930,2017)+ylim(120,180)+theme_bw()+theme(axis.title.x = element_text(size=18), axis.title.y=element_text(size=18, angle=90), panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(), axis.text.x = element_text(size=15), axis.text.y = element_text(size=15))+ylab("Phenology (doy)")+xlab("Year")+annotation_custom(grob = textGrob(label = "a)", hjust = 0, gp = gpar(cex = 1.2)), ymin =180, ymax = 180, xmin = 1925, xmax = 1930)+annotation_custom(grob = textGrob(label = "Consumer", hjust = 0, gp = gpar(cex = 0.9, fontface=2, col="darkslategray")), ymin =160, ymax = 160, xmin = 1930, xmax = 1930)+annotation_custom(grob = textGrob(label = "Resource", hjust = 0, gp = gpar(cex = 0.9, fontface=2, col="darkred")), ymin =130, ymax = 130, xmin = 1930, xmax = 1930)

#NEW first panel-
x  = seq(-15, 15, length = 800)
y1 = dnorm(x, mean = 0,sd = 5)
y2 = 0.5*dnorm(x, mean = 0,sd = 5)

#y1<-density(rnorm(100000000, 0, 10))

mydf = data.frame(x, y1, y2)
b<-ggplot(mydf, aes(x = x))+geom_line(aes(y = y1), size=1)+geom_line(aes(y = y2), size=1, colour = 'black', linetype="dashed")+theme_bw()+theme(plot.title = element_text(face = "bold", colour="red", size=18), axis.title.x = element_text(size=18), axis.title.y=element_text(size=18, angle=90), panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.ticks.x=element_blank())+geom_vline(xintercept=0, size=0.25)+annotate("point", x = 0, y = 0.08, colour = "purple", size=5)+annotate("point", x = 0, y = 0.04, colour = "blue", size=5)+xlab("Relative timing (days)")+ylab("Total fitness")+annotation_custom(grob = textGrob(label = "Synchrony baseline", hjust = 0, gp = gpar(cex = 0.9, fontface=2, col="purple")), ymin =0.08, ymax = 0.08, xmin = 2, xmax = 2)+annotation_custom(grob = textGrob(label = "Effect of \nclimate change", hjust = 0, gp = gpar(cex = 0.9, fontface=2, col="red")), ymin =0.07, ymax = 0.07, xmin = -11, xmax = -11)+xlim(-15,15)+annotation_custom(grob = textGrob(label = "Adaptive mismatch \nhypothesis with \nsynchrony baseline", hjust = 0, gp = gpar(cex = 0.9, fontface=2, col="blue")), ymin =0.04, ymax = 0.04, xmin = 6.8, xmax = 8)+xlim(-15,15)+annotation_custom(grob = textGrob(label = "b)", hjust = 0, gp = gpar(cex = 1.2)), ymin =0.08, ymax = 0.08, xmin = -17, xmax = -15)


c<-ggplot(mydf, aes(x = x))+geom_line(aes(y = y1), size=1)+theme_bw()+theme(plot.title = element_text(face = "bold", colour="red", size=18), axis.title.x = element_text(size=18), axis.title.y=element_text(size=18, angle=90), panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.ticks.x=element_blank())+annotate("point", x = 3.9, y = 0.06, colour = "dark green", size=5)+annotate("point", x = 10, y = 0.011, colour = "dark green", size=5)+xlab("Relative timing (days)")+ylab("Total fitness")+xlim(-15,15)+annotation_custom(grob = textGrob(label = "Asynchrony baseline", hjust = 0, gp = gpar(cex = 0.9, fontface=2, col="dark green")), ymin =0.08, ymax = 0.04, xmin = 2.5, xmax = 8)+xlim(-15,15)+annotation_custom(grob = textGrob(label = "c)", hjust = 0, gp = gpar(cex = 1.2)), ymin =0.08, ymax = 0.08, xmin = -17, xmax = -15)+annotation_custom(grob = textGrob(label = "Effect of \nclimate change", hjust = 0, gp = gpar(cex = 0.9, fontface=2, col="red")), ymin =0.03, ymax = 0.03, xmin = 9, xmax = 9)
#+geom_line(aes(y = y2), size=1, colour = 'black', linetype="dashed")+annotate("point", x = 3.9, y = 0.03, colour = "dark green", size=5)+geom_vline(xintercept=0, size=0.25)

grid.arrange(a,b,c, ncol=1)

## Figure 3- Option 2 (NOT USED)

#NEW Second panel-
x  = seq(-15, 15, length = 400)
y1 = dnorm(x, mean = 0,sd = 3)
y2 = dnorm(x, mean = -5,sd = 3)

mydf = data.frame(x, y1, y2)
b<-ggplot(mydf, aes(x = x))+geom_line(aes(y = y1), size=1)+geom_line(aes(y = y2), size=1, colour = 'black', linetype="dashed")+theme_bw()+theme(plot.title = element_text(face = "bold", colour="red", size=18), axis.title.x = element_text(size=18), axis.title.y=element_text(size=18, angle=90), panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(), axis.text.x = element_text(size=15), axis.text.y = element_blank())+annotate("point", x = 0, y = 0.133, colour = "purple", size=4)+annotate("point", x = -5, y = 0.133, colour = "blue", size=4)+annotate("point", x = -8.4, y = 0.073, colour = "red", size=4)+annotate("point", x = -1.7, y = 0.073, colour = "red", size=4)+annotate("point", x = -3.3, y = 0.073, colour = "red", size=4)+annotate("point", x = 3.3, y = 0.073, colour = "red", size=4)+xlab("Relative timing (days)")+geom_vline(xintercept=0, size=0.25)+geom_vline(xintercept=-5, size=0.25)+ylab("Fitness")+annotation_custom(grob = textGrob(label = "Synchrony baseline", hjust = 0, gp = gpar(cex = 1.3, face="bold", col="purple")), ymin =0.13, ymax = 0.13, xmin = 2, xmax = 2)+xlim(-15,15)+annotation_custom(grob = textGrob(label = "Adaptive mismatch \nhypothesis with \nsynchrony baseline", hjust = 0, gp = gpar(cex = 1.3, face="bold", col="blue")), ymin =0.12, ymax = 0.12, xmin = -16, xmax = -16)+annotation_custom(grob = textGrob(label = "Asynchrony \nbaseline", hjust = 0, gp = gpar(cex = 1.3, face="bold", col="red")), ymin =0.07, ymax = 0.07, xmin = -14, xmax = -14)+xlim(-15,15)

#Third panel
c<-ggplot(mydf, aes(x = x))+geom_line(aes(y = y1), size=1)+geom_line(aes(y = y2), size=1, colour = 'black', linetype="dashed")+theme_bw()+theme(plot.title = element_text(face = "bold", colour="red", size=18), axis.title.x = element_text(size=18), axis.title.y=element_text(size=18, angle=90), panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(), axis.text.x = element_text(size=15), axis.text.y = element_blank())+annotate("point", x = 0, y = 0.133, colour = "purple", size=4)+annotate("point", x = -5, y = 0.133, colour = "blue", size=4)+annotate("point", x = -8.4, y = 0.073, colour = "red", size=4)+annotate("point", x = -1.7, y = 0.073, colour = "red", size=4)+annotate("point", x = -3.3, y = 0.073, colour = "red", size=4)+annotate("point", x = 3.3, y = 0.073, colour = "red", size=4)+xlab("Relative timing (days)")+geom_vline(xintercept=0, size=0.25)+geom_vline(xintercept=-5, size=0.25)+ylab("Fitness")

grid.arrange(b,c, ncol=1)


#First panel- OLD- DONT USE
x  = seq(-3, 3, length = 200)
y1 = dnorm(x, mean = 0,sd = 1)
y2 = 5*dnorm(x, mean = 0,sd = 5)

mydf = data.frame(x, y1, y2)
ggplot(mydf, aes(x = x))+geom_line(aes(y = y1), size=1)+geom_line(aes(y = y2), size=1, colour = 'black', linetype="dashed")+theme_bw()+theme(plot.title = element_text(face = "bold", colour="red", size=18), axis.title.x = element_text(size=18), axis.title.y=element_text(size=18, angle=90), panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+annotate("point", x = 0, y = 0.4, colour = "purple", size=4)+annotate("point", x = -0.5, y = 0.397, colour = "red", size=4)+annotate("point", x = 0.5, y = 0.397, colour = "red", size=4)



******************************
## Figure 4
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
a<-ggplot(raw2, aes(corr_time, y=surv))+geom_vline(xintercept=0, linetype="dashed", size=0.5)+geom_point(aes(colour=factor(expt), shape=factor(expt)), size=3)+ylim(0,100)+xlim(-90, 90)+theme_bw()+theme(legend.position="none", axis.title.x = element_text(size=15), axis.text.x=element_text(size=15), axis.text.y=element_text(size=15), axis.title.y=element_text(size=15, angle=90))+ylab("Survival of O. brumata (%)")+xlab("Relative timing (degree-days)")+annotation_custom(grob = textGrob(label = "a)", hjust = 0, gp = gpar(cex = 1.5)), ymin = 100, ymax = 100, xmin = -90, xmax = -90)+annotate("label", x = -71, y = 25, label = "egg hatching occurs \nbefore bud opening \n(i.e. time without food)", colour="#00BFC4", fontface="bold")+annotate("label", x = 73, y = 40, label = "egg hatching occurs \nafter bud opening \n(i.e. time with food)", colour="#F8766D", fontface="bold") 
#+annotation_custom(text_high, xmin=-75, xmax=-75, ymin=-0.8, ymax=-0.8)
#annotation_custom(text_low, xmin=65, xmax=65, ymin=-0.8, ymax=-0.8)

#Equations for regression lines are: Y=12.37 e 0.055 X for (A) and Y=50.88+0.492X for (B)
#+geom_segment(aes(x=0, xend=80, y=-0.492+49.12, yend=-0.492+49.12*80), size=0.75)
#+geom_smooth(method="lm", formula=y~x+I(x^2), se=FALSE)

+geom_abline(slope=0.08, intercept=1.26, size=0.75)

obs <- read.csv("vanAsch_data.csv", header=TRUE, na.strings="NA")
obs$order<-10:1 

text_high <- textGrob("Time without food", gp=gpar(fontsize=12, fontface="bold"))
text_low <- textGrob("Time with food", gp=gpar(fontsize=12, fontface="bold"))

b<-ggplot(obs, aes(y=mismatch_med, x=factor(reorder(year,order))))+coord_flip()+geom_hline(yintercept=0, linetype="dashed", size=0.5)+geom_point(size=3, colour="#00BFC4", shape=17)+geom_errorbar(aes(ymin=mismatch_low, ymax=mismatch_high), width=.0025, colour="#00BFC4")+theme_bw()+theme(legend.position="none", axis.title.x = element_text(size=15), axis.text.x=element_text(size=15), axis.text.y=element_text(size=15), axis.title.y=element_text(size=15, angle=90))+ylab("Relative timing (days)")+xlab("")+ylim(-90, 90)+ annotation_custom(grob = textGrob(label = "b)", hjust = 0, gp = gpar(cex = 1.5)), ymin = -90, ymax = -90, xmin = 10, xmax = 10)+annotate("label", label="Time with food", x=0.75, y=78, colour="#F8766D", fontface="bold")+annotate("label", label="Time without food", x=0.75, y=-74, colour="#00BFC4", fontface="bold")
#png('Fig3_v2.png', width=800,height=950, res=100)
multiplot(a,b,cols=1)
#dev.off()

> library(scales)
show_col(hue_pal()(4))
show_col(hue_pal()(2))

----
x <- seq(-3,3,0.01)
y1 <- dnorm(x,0,1)
y2 <- 0.5*dnorm(x,0,1)
plot(x,y1,type="l",bty="L",xlab="X",ylab="Abundance", lwd=3, col=2)
points(x,y2,type="l",lwd=3, col=1)
polygon(x,y2,col="gray")
points(x,y2,type="l",col=1, lwd=3)

y2 <- 0.5*dnorm(x,-2,1)
plot(x,y1,type="l",bty="L",xlab="X",ylab="Abundance", lwd=3, col=2)
points(x,y2,type="l",col=1, lwd=3)
x.shade<-seq()
polygon(x,y2,col="gray")
points(x,y2,type="l",col=1, lwd=3)


OR
plot(function(x) dnorm(x, mean=0), -3, 3, lwd=3, xaxt="n", yaxt="n", xlab="time (days)", ylab="fitness", col=2, cex.lab=3) #was col=1
#curve(dnorm(x), add=T)
curve(dnorm(x), add=T, col=2, lwd=4)
 
#cord.x <- c(-3)
#cord.y <- c(0)
#cord.x <- c(cord.x,-3)
#cord.y <- c(cord.y,dnorm(-3))
cord.x <- c(-3,seq(-3,-2,0.01),-2) 
cord.y <- c(0,dnorm(seq(-3,-2,0.01)),0)
polygon(cord.x,cord.y,col='skyblue')
curve(0.5*dnorm(x, -2,1), add=T, col=1, lwd=4)
polygon(cord.x,cord.y,col='gray')

curve(dnorm(x,0,1),xlim=c(-3,3),main='Standard Normal') 
polygon(cord.x,cord.y,col='skyblue')

---

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