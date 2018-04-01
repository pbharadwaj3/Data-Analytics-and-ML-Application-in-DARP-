###### correlation coefficient among the variables and correlation plot   #####
install.packages("corrplot")
library(corrplot)
df_ln<-read.csv("F:/Project/Programme/ML/DAR Machine Learning TR Part A/train_darp_ln.csv")
M<-cor(df_ln)
corrplot(M,method="number")

##### U can see from the plot that area having highest positive correlation(0.55) with veh and speed having highest
#negetive correlation (-0.49) with Veh


##### Linear Regression for showing the relation between one predictor area and prediction veh###
df_ln<-read.csv("F:/Project/Programme/ML/DAR Machine Learning TR Part A/train_darp_ln.csv")
plot(df_ln$area,df_ln$veh,xlab = "area",ylab = "Fleet size",col="blue",lwd=3)
Linearmodel_DARP<-lm(veh~area,data = df_ln)
summary(Linearmodel_DARP)
{
  plot(df_ln$area,df_ln$veh,xlab = "area",ylab = "Fleet size",col="blue",lwd=2)
  abline(Linearmodel_DARP,col="red")
}

#### Let's test the model wheather it can predict the ln(veh) on the graph correctly with confidenceinterval of 95% ###
summary(df_ln$area)
points_to_predict<-data.frame(area=c(2000,4000))
predictionWithInterval <- predict(Linearmodel_DARP, points_to_predict, interval = 'confidence')
predictionWithInterval
{
  plot(df_ln$area,df_ln$veh,xlab = "area",ylab = "Fleet size",col="blue",lwd=1)
  points(points_to_predict$area, predictionWithInterval[,"fit"], col = "red", lwd = 4)
  points(points_to_predict$area, predictionWithInterval[,"lwr"], col = "firebrick4", lwd = 4)
  points(points_to_predict$area, predictionWithInterval[,"upr"], col = "firebrick4", lwd = 4)
  legend("topleft",legend = c("Dataset Points", "Prediction Points"), fill = c("blue","red"), bty = "n")
  abline(Linearmodel_DARP,col="red")
}


####Multiple regression####
fit2=lm(veh~.,data = df_ln) ## multiple regression model veh with all other variables
summary(fit2)
par(mfrow=c(2,2))
plot(fit2)
fit3=update(fit2,~.-cap-s-crit..CV)
summary(fit3)
#####nonlinear and interactions###
fit4=lm(veh~area+I(area^2),df_ln);summary(fit4)
attach(df_ln)
par(mfrow=c(1,1))
plot(veh~area)
points(area,fitted(fit4),col="red",pch=20)

## lets check for cross validation i.e leave one out cross validation(loocv) and calculate the cross validation error
install.packages("boot")
library(boot)
?cv.glm
glm.fit=glm(veh~.-veh.hrs-veh.km,data=df_ln,family = "gaussian")# generalized linear model fit 
summary(glm.fit)
cv.glm(df_ln,glm.fit)$delta
# as the direct calculation through inbuilt function takes a lot time for execution lets build the user defined function

loocv=function(fit){
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
  
}
loocv(glm.fit)
cv.error=rep(0,5)
degree=1:5
for(d in degree){
  glm.fit=glm(veh~poly(area,d),data=df_ln,family = "gaussian")
  cv.error[d]=loocv(glm.fit)
}
plot(degree,cv.error,type="b")# as you can see from the plot that the error decreases with degree of area

cv.error5=rep(0,5)## 5 fold cross validation
for(d in degree){
  glm.fit=glm(veh~poly(area,d),data=df_ln,family = "gaussian")
  cv.error5[d]=cv.glm(df_ln,glm.fit,K=5)$delta[1]
}
lines(degree,cv.error5,type = "b",col="green")


cv.error10=rep(0,5)## 10 fold cross validation
for(d in degree){
  glm.fit=glm(veh~poly(area,d),data=df_ln,family = "gaussian")
  cv.error10[d]=cv.glm(df_ln,glm.fit,K=10)$delta[2]
}
lines(degree,cv.error10,type = "b",col="red")
 ## you can experiment on cross validation to this by using this code ###
## N:B: Change delta[] everytime you use different  cross vaidation fold

####Reference James, G., Witten, D., Hastie, T. and Tibshirani, R., 2013. An introduction to statistical learning (Vol. 112). New York: springer.