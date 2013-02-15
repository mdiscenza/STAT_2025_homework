plot(p, dbinom(40,100,p))
glm(y~x1 +x2, family=binomial, data=toyadata) #logistic regression here
subset(ToyExample,)


library(BMA)
library(ggplot2)
library(popbio)

FORMULAS[1]
FORMULAS[2]
FORMULAS[3]

#x1
ggplot(ToyExample, aes(x1, fill = as.factor(y))) +
    geom_density(alpha = 0.2) + 
    opts(title = expression("Comparing x1 density for Y==0 and Y==1"))
testmodel1 <- glm(FORMULAS[1], family=binomial, data=ToyExample)
pred1 <- predict(testmodel1, type = "response") 
plot(ToyExample$y~ToyExample$x1, main="Fitted Logistic Regression with x1 as Predictor") + 
    lines(ToyExample$x1, pred1, col="red") 
logi.hist.plot(x1,y,boxp=FALSE,type="hist",col="gray",
               main="Fitted Logistic Regression with x1 as Predictor")
plot(testmodel1$residuals~x1, "Residuals vs. x1"))

#x2
ggplot(ToyExample, aes(x2, fill = as.factor(y))) +
    geom_density(alpha = 0.2) + 
    opts(title = expression("Comparing x2 density for Y==0 and Y==1"))
testmodel2 <- glm(FORMULAS[2], family=binomial, data=ToyExample)
pred2 <- predict(testmodel2, type = "response")
plot(ToyExample$y~ToyExample$x2, main="Fitted Logistic Regression with x2 as Predictor") + 
    lines(ToyExample$x2, pred2, col="red")
logi.hist.plot(x2,y,boxp=FALSE,type="hist",col="gray", 
               main="Fitted Logistic Regression with x2 as Predictor")
plot(testmodel2$residuals~x2, "Residuals vs. x2"))

#x3
ggplot(ToyExample, aes(x3, fill = as.factor(y))) +
    geom_density(alpha = 0.2) + 
    opts(title = expression("Comparing x3 density for Y==0 and Y==1"))
testmodel3a <- glm(y~x3, family=binomial, data=ToyExample)
pred3 <- predict(testmodel3a, type = "response")
plot(ToyExample$y~ToyExample$x3, main="Fitted Logistic Regression with x3 as Predictor") + 
    lines(ToyExample$x3,pred3,col="red")
plot(testmodel3a$residuals~x3))
logi.hist.plot(log(x3),y,boxp=FALSE,type="hist",col="gray")
plot(testmodel3a$residuals~x3, main="Residuals vs. x3")

#log(x3)
ggplot(ToyExample, aes(log(x3), fill = as.factor(y))) +
    geom_density(alpha = 0.2) + 
    opts(title = expression("Comparing log(x3) density for Y==0 and Y==1"))
testmodel3b <- glm(y~log(x3), family=binomial, data=ToyExample)
logi.hist.plot(log(x3),y,boxp=FALSE,type="hist",col="gray")
plot(testmodel3b$residuals~log(x3), main="Residuals vs. log(x3)")



testmodel1$aic

Pred <-  
Pred
plot
curve()

bic.glm(FORMULAS[4],data=ToyExample, glm.family=binomial(link = "logit"))

ggplot(data=ToyExample, aes(x1, fill = y)) +
    geom_density(alpha = 0.2)






<<modelfitting, message=FALSE, echo=TRUE>>=
    #x1
    testmodel1 <- glm(FORMULAS[1], family=binomial, data=ToyExample)
pred1 <- predict(testmodel1, type = "response") 
logi.hist.plot(x1,y,boxp=FALSE,type="hist",col="gray",
               main="Fitted Logistic Regression with x1 as Predictor")
plot(testmodel1$residuals~x1, "Residuals vs. x1")
#x2
testmodel2 <- glm(FORMULAS[2], family=binomial, data=ToyExample)
pred2 <- predict(testmodel2, type = "response")

logi.hist.plot(x2,y,boxp=FALSE,type="hist",col="gray", 
               main="Fitted Logistic Regression with x2 as Predictor")
plot(testmodel2$residuals~x2, "Residuals vs. x2"))
#3
testmodel3a <- glm(y~x3, family=binomial, data=ToyExample)
pred3 <- predict(testmodel3a, type = "response")
logi.hist.plot(log(x3),y,boxp=FALSE,type="hist",col="gray")
plot(testmodel3a$residuals~x3, main="Residuals vs. x3")
@



#x1
testmodel1 <- glm(y ~ x1, family=binomial, data=ToyExample)
pred1 <- predict(testmodel1, type = "response") 
logi.hist.plot(x1,y,boxp=FALSE,type="hist",col="gray",
               main="Fitted Logistic Regression with x1 as Predictor")
plot(testmodel1$residuals~x1, "Residuals vs. x1")
#x2
testmodel2 <- glm(y ~ x2, family=binomial, data=ToyExample)
pred2 <- predict(testmodel2, type = "response")

logi.hist.plot(x2,y,boxp=FALSE,type="hist",col="gray", 
               main="Fitted Logistic Regression with x2 as Predictor")
plot(testmodel2$residuals~x2, "Residuals vs. x2"))
#3


library(glmnet)
glmnetModel <- glmnet(x=as.matrix(ToyExample[,2:4]),y=ToyExample[,1], family="binomial")
plot(glmnetModel)
my.cv <- cv.glmnet(x=as.matrix(ToyExample[,2:4]),y=ToyExample[,1],family="binomial")
predict(glmnetModel,type="coef",s=my.cv$lambda.min) # returns the coefficents for the best model

glm()
str(glmnetModel)
glmnetModel$beta

glm(y~x1+x2+logx3,data=ToyExample, family="binomial")
