




######################################################
###### UNCOVERTED DATA - MODEL FITTING
######################################################
str(study_data)
colnames(study_data)
#do "predictors" have predictive power?
summary(study_data)
#there is no significant variation in the "know_about" variables (HOW TO QUANTIFY). So we will not include those four predictors in the regression.


#Running Logistic Regression 1 (Analysis 1) - the full model
########################################
full_model<-sex_wo_contraception~age+ethnicity+religion+highest_yr_school_completed+know_preg_peera+fathers_education+mothers_education+educational_aspiration+dont_participate_in_school_activites_1+smoking_freq+drinking_freq+drug_freq

lr1<-glm(full_model,data=study_data,family=binomial(link=logit))
summary(lr1)
AIC(lr1)
plot(lr1)



#Lasso (Analysis 2)
########################################
library(glmnet)
# to use glmnet, we need to recode the factor variables as binary
model.matrix(full_model,data=study_data,contrasts.arg=list(a=ethnicity,b=religion))
y<-as.matrix(study_data[,1])
x_mat <- model.matrix(full_model, data=study_data )
#running 
glmnetModel <- glmnet(x_mat,y,family="binomial",alpha=1)
plot(glmnetModel)
my.cv <- cv.glmnet(x_mat,y,family="binomial")
predict(glmnetModel,type="coef",s=my.cv$lambda.min)

#Leaps (Analysis 3)
########################################
library(leaps)
model_spec <- regsubsets(x_mat,y,method="backward")
model_spec

#this didn't work, need to be in matrix form
x=as.matrix(study_data[,2:15])
y=as.matrix(study_data[,1])
model_spec2 <- regsubsets(x,y,method="backward")




glmnetModel <- glmnet(x=as.matrix(ToyExample[,2:4]),y=ToyExample[,1], family="binomial")
#plot(glmnetModel)
my.cv <- cv.glmnet(x=as.matrix(ToyExample[,2:4]),y=ToyExample[,1],family="binomial")
predict(glmnetModel,type="coef",s=my.cv$lambda.min) # returns the coefficents for the best model


######################################################
###### COVERTED DATA - MODEL FITTING
######################################################
str(study_data_converted)
colnames(study_data_converted)
#do "predictors" have predictive power?
summary(study_data_converted)
#there is no significant variation in the "know_about" variables (HOW TO QUANTIFY). So we will not include those four predictors in the regression.


#Running Logistic Regression 1 (Analysis 1) - the full model
########################################
full_model<-sex_wo_contraception~age+ethnicity+religion+highest_yr_school_completed+know_preg_peera+fathers_education+mothers_education+educational_aspiration+dont_participate_in_school_activites_1+smoking_freq+drinking_freq+drug_freq+age_frist_drinking

lr1c<-glm(full_model,data=study_data_converted,family=binomial(link=logit))
summary(lr1c)
#explained deviance
(lr1c$null.deviance - lr1c$deviance)/lr1c$null.deviance

AIC(lr1c)
plot(lr1c)

partial_model<-sex_wo_contraception~age+ethnicity+highest_yr_school_completed+know_preg_peera+fathers_education+mothers_education+educational_aspiration+dont_participate_in_school_activites_1+smoking_freq+drinking_freq+drug_freq
lr2c<-glm(partial_model,data=study_data_converted,family=binomial(link=logit))
summary(lr2c)
AIC(lr1c)
plot(lr1c)

#Lasso (Analysis 2)
########################################
library(glmnet)
# to use glmnet, we need to recode the factor variables as binary
yc<-as.matrix(study_data_converted[,1])
x_matc <- model.matrix(partial_model, data=study_data_converted[,1:13])
#running 
glmnetModelc_lasso <- glmnet(x_matc,yc,family="binomial",alpha=1)
plot(glmnetModelc_lasso)
my.cvc <- cv.glmnet(x_matc,yc,family="binomial")
predict(glmnetModelc_lasso,type="coef",s=my.cvc$lambda.min)
glmnetModelc_lasso$nulldev
#explained.deviance:
(glmnetModelc_lasso$nulldev - glmnetModelc_lasso$nulldev)/glmnetModelc_lasso$nulldev
my.cvc$

glmnetModelc_ridge <- glmnet(x_matc,yc,family="binomial",alpha=0)
plot(glmnetModelc_ridge)
my.cvc <- cv.glmnet(x_matc,yc,family="binomial")
predict(glmnetModelc_ridge,type="coef",s=my.cvc$lambda.min)


#Leaps (Analysis 3)
########################################
full_model<-sex_wo_contraception~age+ethnicity+religion+highest_yr_school_completed+know_preg_peera+fathers_education+mothers_education+educational_aspiration+dont_participate_in_school_activites_1+smoking_freq+drinking_freq+drug_freq

library(leaps)
subsets <- regsubsets(sex_wo_contraception~age+ethnicity+religion+highest_yr_school_completed+know_preg_peera+fathers_education+mothers_education+educational_aspiration+dont_participate_in_school_activites_1+smoking_freq+drinking_freq+drug_freq, data=study_data_converted, method="forward", nbest=2)
plot(subsets)
summary(subsets)
#this didn't work, need to be in matrix form
x=as.matrix(study_data_converted[,2:13])
y=as.matrix(study_data_converted[,1])
model_spec2 <- regsubsets(x,y,method="backward")




glmnetModel <- glmnet(x=as.matrix(ToyExample[,2:4]),y=ToyExample[,1], family="binomial")
#plot(glmnetModel)
my.cv <- cv.glmnet(x=as.matrix(ToyExample[,2:4]),y=ToyExample[,1],family="binomial")
predict(glmnetModel,type="coef",s=my.cv$lambda.min) # returns the coefficents for the best model






###### All models on converted data ########
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
    Formulas <- c(Formulas, paste( y.col.name, "~1"))
}

apply_formula <- function(model, cv=5){ #make the default value for this function 5
    fitmodel <- glm(model, family=binomial, data=trimed_data)
    explained.deviance <- (fitmodel$null.deviance - fitmodel$deviance)/fitmodel$null.deviance
    return(cbind(fitmodel$aic,explained.deviance))
}

# apply_formula <- function(model, cv=5){ #make the default value for this function 5
#     library(DAAG)
#     fitmodel <- glm(model, family=binomial, data=trimed_data)
#     AIC <- fitmodel$aic
#     cvmse <- cv.glm(data=trimed_data,glmfit=fitmodel,K=cv)$delta[2]
#     result <- c(AIC, cvmse)
#     return (result)
# }


trimed_data <-study_data_converted
trimed_data$age_frist_drinking_a <- NULL
trimed_data$age_frist_drinking_b <- NULL
trimed_data$religion <- NULL
FORMULAS <- all_possible_models(
    x.column.names=colnames(trimed_data[2:12]),
    y.col.name=colnames(trimed_data[1])
)
#costf=function(r, pi = 0) mean((r-round(pi))^2)
FORMULAS[length(FORMULAS)+1] <- "y ~ 1" # add the null model
crit <- sapply(X=FORMULAS, FUN = apply_formula)
# crit <- sapply(X=FORMULAS, FUN = apply_formula)
crit <- t(crit)
names(crit) = NULL
model.selection <- as.data.frame(cbind(FORMULAS, crit))
colnames(model.selection)[2] <- "AIC"
colnames(model.selection)[3] <- "Explained.Deviance"
model.selection <- model.selection[order(model.selection$AIC),]
model.selection <- model.selection[order(model.selection$Explained.Deviance,decreasing=TRUE),]

model.selection$FORMULAS = NULL
head(model.selection)




small.model<-glm(sex_wo_contraception ~ mothers_education+smoking_freq,data=study_data_converted,family=binomial(link=logit))
summary(small.model)



library(MASS)
tbl = table(study_data$ethnicity, study_data$religion) #religion is top
chisq.test(tbl)
tbl