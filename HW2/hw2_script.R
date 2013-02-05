#Import dataset
library(DAAG)
houseprices <- read.delim("http://www.stat.columbia.edu/~madigan/W2025/data/houseprices.txt")



#Manually Fitting models and to get a good understanding of the data
attach(houseprices)
#fit single predictor models:
#Area
plot(sale.price~area)
model1 <- lm(sale.price~area)
summary(model1)
anova(model1)
#Bedrooms
plot(sale.price~bedrooms)
model2 <- lm(sale.price~bedrooms)
summary(model2) # explains a good chunck of the variance
anova(model2) # though here, this is mainly because we have the house with 6 br - an influential point

#Bedrooms
plot(sale.price~bedrooms)
model2 <- lm(sale.price~bedrooms)
summary(model2) # explains a good chunck of the variance
anova(model2) # though here, this is mainly because we have the house with 6 br - an influential point

#Floors
plot(sale.price~floors)
model3 <- lm(sale.price~floors)
summary(model3) # explains a good chunck of the variance
anova(model3) # though here, this is mainly because we have the house with 6 br - 
# it is an influential point


# Now let's make a model with two predictors:
model4 <-  lm(sale.price~area + bedrooms)
summary(model4)

library(scatterplot3d)
scatterplot3d(sale.price,area,bedrooms)
s3d$plane3d(model4)

s3d <-scatterplot3d(sale.price,area,bedrooms, pch=16, highlight.3d=TRUE,
                    type="h", main="3D Scatterplot")
s3d$plane3d(model4)






###### QUESTION 1 ######
# 5-fold cross-validation for best subset model selection

library(DAAG)
#************************************************************************************
#The following chunk of code is based on the source of Hadley's Meify Package:
#https://github.com/hadley/meifly/blob/master/R/generate.r)
#it generates a list of all of the 7 different simple linear models that could be fit
all_possible_models <- function(x.column.names, y.col.name){
    Cols <- x.column.names
    n <- length(Cols)
    id <- unlist(
        lapply(1:n,
               function(i)combn(1:n,i,simplify=F)
        )
        ,recursive=F)
    Formulas <- sapply(id,function(i)
        paste(y.col.name, "~",paste(Cols[i],collapse="+"))
    )
}
#************************************************************************************
#Function for crossvalidating 
apply_formula <- function(x, cv=5){ #make the default value for this function 5
    cv.model <- CVlm(houseprices,lm(x, data=houseprices),m=cv)
    cvmse <-mean(abs(cv.model$sale.price - cv.model$cvpred)) 
}
FORMULAS <- all_possible_models(x.column.names=colnames(houseprices[1:3]),
                                y.col.name=colnames(houseprices[4]))
CV.MSE <- lapply(FORMULAS, apply_formula)
final.results <- cbind(FORMULAS, CV.MSE)
final.results



###### QUESTION 2 ######
#Check the residuals to see if there any gross departures from the model assumptions. 
#Provide plots and comments.  We proceed with the following model:
#sale.price ~ area+bedrooms
lm.optimatal <- lm(sale.price ~ area+bedrooms, data=houseprices)
plot(lm.optimatal$residuals~sale.price, main="Residuals against Sale Price",xlab="Sale Price (in thousands of $)", ylab="Residual")++abline(h=0)
#indicates that there might be issues with non-linearity
#we should expect to see    
plot(lm.optimatal$residuals~area, main="Residuals against Area",xlab="Area", ylab="Residual")+abline(h=0)#seems to violate the homoscedasticity assumption - not constant variance
plot(lm.optimatal, which=2)
#this shows that there seems to be significant deviation from a normal pattern when we plot and order the residuals

#Confidence interval:
nd <- data.frame(floors=2,area=1000, bedrooms=4)
confidence_interval <- predict(lm.optimatal,interval="confidence",newdata=nd, level=.95)
confidence_interval

#prediction interval
nd <- data.frame(floors=2,area=1000, bedrooms=4)
prediction_interval <- predict(lm.optimatal,interval="prediction",newdata=nd, level=.95)
prediction_interval
    
