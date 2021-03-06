# Setting Mike's working directory
setwd("~/Dropbox/_Spring_2013/STAT_2025/Homework/Project1")
preg <- read.csv("./dapm5m6.csv")

#ever been or gotten somone pregnant
table(fem_preg$GRSM5003)
#subsetting for only female who have sex
fem_preg <- subset(preg, preg$GRSM5003==1)
preg$GRSM5003==1
fem_preg_sex <- subset(fem_preg, fem_preg$SXBM5124==1)
fem_preg_sex$SXBM5124


#adding variables that we want to a new data frame
study_data <-  as.data.frame(cbind(fem_preg_sex$CNBM5130,fem_preg_sex$CNCM5152, fem_preg_sex$CNCM5153, fem_preg_sex$CNCM5154, fem_preg_sex$CNCM5155, fem_preg_sex$CNCM5597, fem_preg_sex$CNCM5598,fem_preg_sex$CNCM5599,fem_preg_sex$CNCM5600,fem_preg_sex$AGHM5393, fem_preg_sex$RASM5004, fem_preg_sex$RLSM5005,fem_preg_sex$EDSM5017, fem_preg_sex$EDSM5410,fem_preg_sex$CBHM5875, fem_preg_sex$CBSM5105, fem_preg_sex$CBSM5535, fem_preg_sex$CNAM5171, fem_preg_sex$CNBM5429, fem_preg_sex$CNBM5571,fem_preg_sex$EDSM5008, fem_preg_sex$EDSM5010, fem_preg_sex$EDIM5013,fem_preg_sex$RCBM5018, fem_preg_sex$RCBM5412, fem_preg_sex$SABM5421, fem_preg_sex$SABM5423, fem_preg_sex$SABM5424, fem_preg_sex$SAIM5046))

colnames(study_data) <- c("sex_wo_contraception","know_about_pill", "know_about_condoms", "know_about_diaphram","know_about_withdrawl", "know_about_pill_b", "know_about_condoms_b", "know_about_diaphram_b","know_about_withdrawl_b", "age", "ethnicity", "religion", "highest_yr_school_completed", "school_enrollment", "scare_atleast1_prega","know_preg_peera", "know_preg_peerb", "birth_control_used_by_peers", "friends_use_contraception_a", "friends_use_contraception_b", "fathers_education", "mothers_education", "educational_aspiration", "dont_participate_in_school_activites_1", "dont_participate_in_school_activites_2", "smoking_freq", "drinking_freq", "drug_freq", "age_frist_drinking")



#exploring the distribution of some of the variables
# cbind(study_data$know_about_pill, study_data$know_about_pill_b)
# table(study_data$know_about_pill)
# table(study_data$know_about_pill_b)
# cbind(study_data$know_about_condoms, study_data$know_about_condoms_b)
# table(study_data$know_about_condoms)
# table(study_data$know_about_condoms_b)
# cbind(study_data$know_about_diaphram, study_data$know_about_diaphram_b)
# table(study_data$know_about_diaphram)
# table(study_data$know_about_diaphram_b)
# 
# cbind(study_data$know_preg_peera, study_data$know_preg_peerb)
# table(study_data$know_preg_peera)
# table(study_data$know_preg_peerb)



# removing the features with lots of NAs because they were not all collected by each survey instrument.
study_data$know_about_pill_b <- NULL
study_data$know_about_condoms_b <- NULL
study_data$know_about_diaphram_b <- NULL
study_data$know_about_withdrawl_b <- NULL
study_data$school_enrollment <- NULL
study_data$scare_atleast1_prega <- NULL
study_data$know_preg_peerb <- NULL
study_data$friends_use_contraception_a <- NULL
study_data$friends_use_contraception_b <- NULL
study_data$dont_participate_in_school_activites_2 <- NULL

#coding variables as either numeric or a factor:
study_data$sex_wo_contraception <- as.factor(study_data$sex_wo_contraception)
is.factor(study_data$sex_wo_contraception)
study_data$ethnicity <- as.factor(study_data$ethnicity)
is.factor(study_data$ethnicity)
study_data$religion<- as.factor(study_data$religion)
is.factor(study_data$religion)
study_data$know_preg_peera <- as.factor(study_data$know_preg_peera)
is.factor(study_data$know_preg_peera)
#next one isn't working
study_data$birth_control_used_by_peers <- study_data[,11]
study_data$birth_control_used_by_peers <- as.factor(study_data$birth_control_used_by_peers)
is.factor(study_data$birth_control_used_by_peers)
study_data[11]<-NULL
study_data$fathers_education <- as.factor(study_data$fathers_education)
is.factor(study_data$fathers_education)
study_data$mothers_education <- as.factor(study_data$mothers_education)
is.factor(study_data$mothers_education)
study_data$educational_aspiration <- as.factor(study_data$educational_aspiration)
is.factor(study_data$educational_aspiration)
study_data$dont_participate_in_school_activites_1 <- as.factor(study_data$dont_participate_in_school_activites_1)
is.factor(study_data$dont_participate_in_school_activites_1)
study_data$educational_aspiration <- as.factor(study_data$educational_aspiration)
is.factor(study_data$educational_aspiration)
study_data$drug_freq <- as.factor(study_data$drug_freq)
is.factor(study_data$drug_freq)
study_data$drinking_freq <- as.factor(study_data$drinking_freq)
is.factor(study_data$drinking_freq)
study_data$smoking_freq <- as.factor(study_data$smoking_freq)
is.factor(study_data$smoking_freq)


#also we decided to take out the the "know about" variables because tg
study_data$know_about_pill <- NULL
study_data$know_about_condoms <- NULL
study_data$know_about_diaphram <- NULL
study_data$know_about_withdrawl <- NULL


#change the response variable to 0, 1
# we want sex without contraception to be 1, with contraception to be 0
# right now sex wihtout contraception is 1
convert_response <- function(response){
  if(response==2){
    return(0)
  }
  if(response==1){
    return(1)
  }
}
study_data$sex_wo_contraception <- as.numeric(as.character(study_data$sex_wo_contraception))
study_data$sex_wo_contraception <- sapply(study_data$sex_wo_contraception, FUN=convert_response)
#study_data$sex_wo_contraception


#we also have to code age in the right way:
study_data$age <- 84-study_data$age
typeof(study_data$age)