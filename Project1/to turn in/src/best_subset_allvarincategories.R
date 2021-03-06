

###### All models on converted data ########
all_possible_models_all <- function(x.column.names, y.col.name){
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
    fitmodel <- glm(model, family=binomial, data=study_data_converted)
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


FORMULAS <- all_possible_models_all(
    x.column.names=colnames(study_data_converted[2:12]),
    y.col.name=colnames(study_data_converted[1])
)
#costf=function(r, pi = 0) mean((r-round(pi))^2)
FORMULAS[length(FORMULAS)+1] <- "sex_wo_contraception ~ 1" # add the null model
crit <- sapply(X=FORMULAS, FUN = apply_formula)
crit <- t(crit)
names(crit) = NULL
model.selection.all <- as.data.frame(cbind(FORMULAS, crit))
colnames(model.selection.all)[2] <- "AIC"
colnames(model.selection.all)[3] <- "Explained.Deviance"
model.selection.all <- model.selection.all[order(model.selection.all$AIC),]

model.selection.all$FORMULAS = NULL
rownames(model.selection.all)
test <- cbind(rownames(model.selection.all),model.selection.all)
rownames(test) <-NULL
head(test)
test$AIC <- round(as.numeric(as.character(test$AIC)),digits=2)
test$Explained.Deviance <- round(as.numeric(as.character(test$Explained.Deviance)),digits=4)
colnames(test)[1] <- "Model"
head(test)                  
library(xtable)                  
xtable(head(test))