# TITLE: STAT 425 GROUP PROJECT CODE
# CREATED BY: Cam Hobbs, Justin Marquez, and Jana Osea
# DATE CREATED: March 27, 2020
# DESCRIPTION: This is an R file that contains the code we will use
# for the analysis of our project experiment.

# setting up libraries
library(ggplot2)
library(mosaic)
library(dplyr)
library(EnvStats)
library(lawstat)
library(DescTools)
library(gridExtra)
library(MASS)
library(car)
library(ggpubr)

# set working directy (note: for Justin and Cam, this may change according
# to your personal computer)
setwd("C:/Users/surfacepro/Google Drive/STAT425_GROUP/project_report")

# importing data
proj <- read.csv('gp.csv')
head(proj)
tail(proj)

# residual plot
windows()
proj.aov = aov(Time ~ Brand + Brand:Connection + Brand:Connection:Message, data=proj)
summary(proj.aov)
proj$e.terms <- residuals(proj.aov)
proj$fit.terms <- residuals(proj.aov)
ggplot(data=proj, aes(sample = e.terms)) + stat_qq(size=2, col="blue") + stat_qqline(col="red") + ggtitle("Normal Probability Plot of Residuals")
shapiro.test(proj$e.terms) # p-value = 1.203e-05


# variance stabilization
a <- favstats(Time ~ Brand + Brand:Connection + Brand:Connection:Message, data=proj)
logsds = log(a$sd)
logmeans = log(a$mean)
logstats.df = data.frame(a$Brand, logsds, logmeans)
summary(lm(logsds ~ logmeans, data=logstats.df))
1 - 0.8406 # lambda = 0.125
lambda <- 0.125


proj$TimeT <- as.numeric(proj$Time)^lambda
proj.aovT = aov(TimeT ~ Brand + Brand:Connection + Brand:Connection:Message, data=proj)
proj$e.termsT <- residuals(proj.aovT)
shapiro.test(proj$e.termsT) # p-value = 0.02603
ggplot(data=proj, aes(sample = e.termsT)) + stat_qq(size=2, col="blue") + stat_qqline(col="red") + ggtitle("Normal Probability Plot of Residuals")
# we decided to send it 


# analysis
proj.aovT = aov(TimeT ~ Brand + Brand:Connection + Brand:Connection:Message, data=proj)
summary(proj.aovT)
proj.TukeyT = TukeyHSD(proj.aovT, conf.level=0.95, ordered=T)
proj.TukeyT$Brand

proj.Tukey = TukeyHSD(proj.aov, conf.level=0.95, ordered=T)
proj.Tukey$Brand
# note: we are waiting on the solution for 3 factor nested design for the multiple comparison of the messages

bon.t <- qt(1 - (0.05/(2*1)), df=48)
mse <- 62
b <- 2
c <- 3
nijl <- 5
a <- favstats(Time ~ Brand,  data=proj)
apple.mean <- a$mean[1] 
samsung.mean <- a$mean[2]
(apple.mean - samsung.mean) + c(-1,1) * bon.t * sqrt(mse/(b*c*nijl))
