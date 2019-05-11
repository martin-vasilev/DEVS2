
rm(list= ls())

library(itsadug)
library(mgcv)
library(readr)
traj <- read_csv("https://raw.githubusercontent.com/soskuthy/gamm_intro/master/traj.csv")

plot(traj$measurement.no, traj$f2)

summary(demo.lm <- lm(f2 ~ measurement.no, data = traj))
abline(demo.lm)

demo.gam <- bam(f2 ~ s(measurement.no, bs = "cr"), data = traj)
plot(traj$measurement.no, traj$f2)
plot(demo.gam)
summary(demo.gam)


##
traj.50 <- read_csv("https://raw.githubusercontent.com/soskuthy/gamm_intro/master/traj_50.csv")

plot(traj.50$measurement.no, traj.50$f2)

demo.gam.k.10 <- bam(f2 ~ s(measurement.no, bs = "cr", k = 10), data = traj.50)
demo.gam.k.20 <- bam(f2 ~ s(measurement.no, bs = "cr", k = 20), data = traj.50)
demo.gam.k.50 <- bam(f2 ~ s(measurement.no, bs = "cr", k = 50), data = traj.50)
