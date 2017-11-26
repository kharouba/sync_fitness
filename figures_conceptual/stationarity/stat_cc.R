## Simple code to make figure 1 for Heather's conceptual paper

set.seed(177)
yrs <- c(1930:1980)
sp1 <- rnorm(length(yrs), 150, 5)
sp2 <- sp1-7+rnorm(length(yrs), 0, 4)

xlim <- c(1930,2017)
ylim <- c(110, 190)

plot(sp1~yrs, type="l", col="darkslategray", xlim=xlim, ylim=ylim)
lines(sp2~yrs, col="darkred", xlim=xlim, ylim=ylim)
