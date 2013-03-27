study_data_converted <- study_data
#study_data_converted <-study_data_converted_copy
#re_combining variables:


####################
#education
####################
code_education <- function(response){
    if (response == 1 | response == 2 | response == 3){
        return("didnt.grad.hs")
    }
    if (response == 4 | response == 5){
        return("grad.hs.some.college")
    }
    if (response == 6 | response == 7 | response == 8){
        return("grad.college.beyond")
    }
    if (response == 9){
        return("other.schooling")
    }
    if (response == -9){
        return("no.response")
    }
}

#wrote test function and the above passed

####################
#drugs, smoking, drinking freq
####################
code_drinkdrugsmoke <- function(response){
    if (response == 1){
        return("abstain")
    }
    if (response == 2 | response == 3 | response == 4 | response == 5 | response == 6){
        return("partake")
    }
    if (response == -9){
        return("no_response")
    }
}

########Applying functions:
for(i in 7:9){
    vector_to_code <- as.character(study_data_converted[,i])
    study_data_converted[,i] <- sapply(vector_to_code,FUN=code_education)    
}

for(i in 11:13){
    vector_to_code <- as.character(study_data_converted[,i])
    study_data_converted[,i] <- sapply(vector_to_code,FUN=code_drinkdrugsmoke)    
}

# str(study_data_converted$smoking_freq)
# str(study_data_converted$drinking_freq)
# 
# colnames(study_data_converted)
# table(study_data_converted$smoking_freq)
# table(study_data_converted$drug_freq)
# table(study_data_converted$drinking_freq)
# 
# study_data_converted$drinking_freq
# study_data_converted$smoking_freq
# study_data_converted$fathers_education
# 
# colnames(study_data_converted)
# 
# study_data_converted$fathers_education
# study_data_converted$mothers_education
# study_data_converted$educational_aspiration