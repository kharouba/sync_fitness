#started June 30 2017
#clean up for HMK041
moms <- read.csv("/users/kharouba/google drive/UOttawa/research/synchrony_fitness/papers/HMK041_rawdata.csv", header=TRUE)



me <- aggregate(moms["no.recruits"], moms[c("year")], FUN=mean)