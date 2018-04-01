###knowing distribution of predictors####

#### Install required package to plot the fitte distribution #####
install.packages("fitdistrplus")
library(fitdistrplus)
###### First lets test for non transformed data for the column Veh(fleetsize) wich is a response variable  #########
df<-read.csv("F:/Project/Programme/ML/DAR Machine Learning TR Part A/train_darp.csv")

fit_gaussian_normal<-fitdist(df$veh,distr = "norm");plot(fit_gaussian_normal)#Normal distribution fit
summary(fit_gaussian_normal)

fit_weibull<-fitdist(df$veh,distr = "weibull");plot(fit_weibull)#Weibull distribution fit
summary(fit_weibull)

fit_gamma<-fitdist(df$veh,distr = "gamma");plot(fit_gamma)#Gamma distribution fit
summary(fit_gamma)


##### Now lets do the test for natural log tranfromed columns of prediction/response variables###

df_ln<-read.csv("F:/Project/Programme/ML/DAR Machine Learning TR Part A/train_darp_ln.csv")

fit_normal_ln<-fitdist(df_ln$veh,distr = "norm");plot(fit_normal_ln)#Normal distribution fit
summary(fit_normal_ln)
coef(fit_normal_ln)

fit_weibull_ln<-fitdist(df_ln$veh,distr = "weibull");plot(fit_weibull_ln)#Weibull distribution fit
summary(fit_weibull_ln)
coef(fit_weibull_ln)

fit_gamma_ln<-fitdist(df_ln$veh,distr = "gamma");plot(fit_gamma_ln)#Gamma distribution fit
summary(fit_gamma_ln)
coef(fit_gamma_ln)


### gamma distribution is best fit According to lowest AIC Value from summary####









