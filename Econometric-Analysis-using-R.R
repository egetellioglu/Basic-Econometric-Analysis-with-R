#  1) An economic data set with one dependent variable (regressant) Y 
# and two independent variables (regressors) X1 and X2, and with at least
# 50 observations, which is suitable to be used for illustrating the application 
# of the multiple linear regression model.
  

setwd("C:\\Users\\user\\Documents\\Econometric-Analysis-using-R")
getwd()

data_1 <- read.table("C:\\Users\\user\\Documents\\Econometric-Analysis-using-R\\Question_1.txt")


Y=as.numeric(data_1[,1])
X1=as.numeric(data_1[,2])
X2=as.numeric(data_1[,3]) 


# Creating Simple Linear Models

Model1=lm(Y~X1)
summary(Model1)

plot(X1,Y,pch=20,col="blue",xlab="Birth Rate for Females 15-17 Years Old",
     ylab="Poverty Rate of 51 States of USA")
title("The Data and Model 1")
abline(Model1,col="red")

Model2= lm(Y~X2)
summary(Model2)

plot(X2,Y,pch=20, col="blue", 
     xlab="Birth Rate for Females 18-19 Years Old",
     ylab="Poverty Rate of 51 States of USA")
title("The Data and Model 2")
abline(Model2,col="red")

# Creating a Multiple Linear Model


Model3=lm(Y~X1+X2)
summary(Model3)

# 2) A data set with two positive variables and at least 40 observations
# to illustrate the application of the simple linear regression 
# model after some suitable non-linear transformation of the data.

# Order the data according to the X variable. Display the data
# in a suitable figure. Indicate in particular the reason why a simple 
# linear regression model is not suitable for analyzing this data set.

data_2 <- read.table("C:\\Users\\user\\Documents\\Econometric-Analysis-using-R\\Question_2.txt")

X=as.numeric(data_2[,2])  
Y=as.numeric(data_2[,1])  
n = 61


ord=order(X)
X=X[ord] 
Y=Y[ord]

plot(X,Y,pch=18,col="black",ylab="Effective Federal Funds Rate",xlab="Unemployment Level")
title("Regression Plot of Model 1")

# Use the ordered data and display the figures

Xs=log(X)
Xss=1/X
Ys=log(Y)

par(mfrow=c(2,2))
plot(Xs,Y,col="black",pch=18, ylab = "Federal Funds Rate", xlab = "Unemployment Level (Log)" )
title("Log-Lin")
plot(Xss,Y,col="black",pch=18,ylab = "Federal Funds Rate", xlab = "Unemployment Level (Reciprocal)")
title("Reciprocal")
plot(X,Ys,col="black",pch=18, ylab = "Federal Funds Rate (Log)", xlab = "Unemployment Level (Log)")
title("Log-Lin")
plot(Xs,Ys,col="black",pch=18, ylab = "Federal Funds Rate (Log)", xlab = "Unemployment Level (Log)")
title("Log-Log")

# Fit a simple linear regression model to each of the ve data sets

M1=lm(Y~X) 
M2=lm(Y~Xs)
M3=lm(Y~Xss)
M4=lm(Ys~X)
M5=lm(Ys~Xs)

summary(M1)  
summary(M2)
summary(M3)
summary(M4)
summary(M5)


# State the re-transformed regression models in the last two cases.

Ye1=M1$fitted.values
Ye2=M2$fitted.values
Ye3=M3$fitted.values
Ye4=exp(M4$fitted.values)
Ye5=exp(M5$fitted.values)

(Y-Ye1) 

RSS1=sum((Y-Ye1)**2)
RSS2=sum((Y-Ye2)**2)
RSS3=sum((Y-Ye3)**2)
RSS4=sum((Y-Ye4)**2)
RSS5=sum((Y-Ye5)**2)

RSS1
RSS2
RSS3
RSS4
RSS5

## Select the best model with the smallest RSS.

par(mfrow=c(1,1))
plot(X,Y,xlab="Unemployment Level",ylab="Federal Funds Rate",pch=20)
title("Linear model against the Reciprocal model")
lines(X,Ye1,col="blue")
lines(X,Ye3,col="red")
text(10300,0.21,"Linear model",col="blue",cex=1.0)
text(6800,0.83,"Reciprocal",col="red",cex=1.0)

# Using the fitted linear regression format of the model selected in previous example, calculate the 95% prediction intervals for individual observations at all observation points.

model3=lm(Y~Xss)
Ye3=model3$fitted.values
Sig21=sum((Y-Ye3)**2)/(n-2)

Sx1=sum(Xs) 
Sx12=sum(Xs**2)
TSSxs=Sx12-Sx1**2/n

seZ=sqrt(Sig21*(1+1/n+(Xs-mean(Xs))**2/TSSxs)) 

Ind.L1=Ye3-2*seZ 
Ind.U1=Ye3+2*seZ 

par(mfrow=c(1,1))
matplot(Xss, cbind(Y,Ye3,Ind.L1,Ind.U1), type="plll", pch=18,lwd=3,
        lty=c(1,6,1,1,6,6),col=c("black","red","blue","blue"),
        cex=1.2,ylab="Federal Funds Rate",xlab="Unemployment Level")
title("95% confidence intervals for best regression model (transformed)")

# Display the estimated results according to the selected model in the original

seZ=sqrt(Sig21*(1+1/n+(Xs-mean(Xs))++2/TSSxs)) 

Ind.L1=Ye3-2*seZ
Ind.U1=Ye3+2*seZ

matplot(X,cbind(Y,Ye3,Ind.L1,Ind.U1),type="plll", pch=18,lwd=3,
        lty=c(1,6,1,1,6,6),col=c("black","red","blue","blue"),
        cex=1.2,ylab="Federal Funds Rate",xlab="Unemployment Level")
title("95% confidence intervals for retransformed regression") 
