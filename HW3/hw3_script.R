library(BMA)
library(ggplot2)
library(popbio)

FORMULAS[1]
FORMULAS[2]
FORMULAS[3]

#x1
ggplot(houseprices, aes(x1, fill = as.factor(y))) +
    geom_density(alpha = 0.2) + 
    opts(title = expression("Comparing x1 density for Y==0 and Y==1"))
testmodel1 <- glm(FORMULAS[1], family=binomial, data=houseprices)
pred1 <- predict(testmodel1, type = "response") 
plot(houseprices$y~houseprices$x1, main="Fitted Logistic Regression with x1 as Predictor") + 
    lines(houseprices$x1, pred1, col="red") 
logi.hist.plot(x1,y,boxp=FALSE,type="hist",col="gray",
               main="Fitted Logistic Regression with x1 as Predictor")
plot(testmodel1$residuals~x1, "Residuals vs. x1"))

#x2
ggplot(houseprices, aes(x2, fill = as.factor(y))) +
    geom_density(alpha = 0.2) + 
    opts(title = expression("Comparing x2 density for Y==0 and Y==1"))
testmodel2 <- glm(FORMULAS[2], family=binomial, data=houseprices)
pred2 <- predict(testmodel2, type = "response")
plot(houseprices$y~houseprices$x2, main="Fitted Logistic Regression with x2 as Predictor") + 
    lines(houseprices$x2, pred2, col="red")
logi.hist.plot(x2,y,boxp=FALSE,type="hist",col="gray", 
               main="Fitted Logistic Regression with x2 as Predictor")
plot(testmodel2$residuals~x2, "Residuals vs. x2"))

#x3
ggplot(houseprices, aes(x3, fill = as.factor(y))) +
    geom_density(alpha = 0.2) + 
    opts(title = expression("Comparing x3 density for Y==0 and Y==1"))
testmodel3a <- glm(y~x3, family=binomial, data=houseprices)
pred3 <- predict(testmodel3a, type = "response")
plot(houseprices$y~houseprices$x3, main="Fitted Logistic Regression with x3 as Predictor") + 
    lines(houseprices$x3,pred3,col="red")
plot(testmodel3a$residuals~x3))
logi.hist.plot(log(x3),y,boxp=FALSE,type="hist",col="gray")
plot(testmodel3a$residuals~x3, main="Residuals vs. x3")

#log(x3)
ggplot(houseprices, aes(log(x3), fill = as.factor(y))) +
    geom_density(alpha = 0.2) + 
    opts(title = expression("Comparing log(x3) density for Y==0 and Y==1"))
testmodel3b <- glm(y~log(x3), family=binomial, data=houseprices)
logi.hist.plot(log(x3),y,boxp=FALSE,type="hist",col="gray")
plot(testmodel3b$residuals~log(x3), main="Residuals vs. log(x3)")



testmodel1$aic

Pred <-  
Pred
plot
curve()

bic.glm(FORMULAS[4],data=houseprices, glm.family=binomial(link = "logit"))

ggplot(data=houseprices, aes(x1, fill = y)) +
    geom_density(alpha = 0.2)


