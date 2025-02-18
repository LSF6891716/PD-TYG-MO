# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Section 0. Packages and Functions used <<<<< ####
{#* section 0 Packages ####
  library(caret)
  library(car)
  library(cmprsk)
  library(dplyr)
  library(foreign)
  library(ggplot2)
  library(ggsci)
  library(ggrepel)
  library("ggthemes")
  library(lava)
  library(Matching)
  library(mediation)
  library(mice)
  library(pec)
  #install.packages("poLCA", dependencies = TRUE)
  library(poLCA)
  library(plyr)
  library(prodlim)
  library(reshape2)
  library(rms)
  library(riskRegression)
  library(survey)
  library(scales)
  library(survminer)
  library(survival)
  library(splines)
  library(timeROC)
  library(tableone)
  library(rms)
  library(withr)
  library(dplyr)
  library(doParallel)
}
# +++++++++++================================+++++++++++ ####
# +++++++++++============Manuscript==============+++++++++++ ####
# +++++++++++================================+++++++++++ ####  
# +++++++++++============Tables==========+++++++++++ ####  
# >>>>> section 18. Multiple interpolation data (Table 1, Table S3)  ####
load(file="I:/NHANES study/PD&TYG&MO/Data/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)

table(Interpolation_weighted$TYG_quantile,Interpolation_weighted$MORT_stat)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
var<-c("MORT_stat","Age_status","Sex","Marital_status","Race_ethnicity","Education_levels","PIR",
       "Health_insurance","SEI","SES","Smoking_status","Drinking_status","Physical_status",
       "HPL_status","HTN_status","CVD_status",
       "BMI_status","T2D_status","Cancer_status","Cohort","TyG_WC_quantile","TyG_WHtR_quantile","TyG_BMI_quantile")
VAR<-c("MORT_stat","Age","Marital_status","Age_status","Sex","Race_ethnicity","Education_levels","PIR",
           "Health_insurance","SEI","SES","Smoking_status","Drinking_status","Physical_status", "HTN_status","HPL_status",
           "BMI","BMI_status","T2D_status","CVD_status","Cancer_status","Cohort","TYG","TyG_WC","TyG_WHtR","TyG_BMI","TyG_WC_quantile","TyG_WHtR_quantile","TyG_BMI_quantile")

options(survey.lonely.psu="adjust")
options(survey.adjust.domain.lonely=TRUE)
{ #* section 18.1 Over all ####
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,1),
                          'SE' = round(svymean$SE*100,1) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
                          'Mean' =round(svymean$mean,1),
                          'SE' = round(svymean$SE,1))
      return(model)
    }
  }  
  Over<- ldply(lapply(VAR, model))
}
options(survey.lonely.psu="adjust")
options(survey.adjust.domain.lonely=TRUE)

{ #* section 18.2 No/Mild periodontitis ####
  rhcSvy_HEI2PD<-subset(rhcSvy,TYG_quantile=="Quantile 1")
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy_HEI2PD,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_HEI2PD, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,1),
                          'SE' = round(svymean$SE*100,1) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_HEI2PD, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
                          'Mean' =round(svymean$mean,1),
                          'SE' = round(svymean$SE,1))
      return(model)
    }
  }  
  Quantile1<- ldply(lapply(VAR, model))
}  
{ #* section 18.3 Moderate/Severe periodontitis ####
  rhcSvy_PD<-subset(rhcSvy,TYG_quantile=="Quantile 2")
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy_PD,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,1),
                          'SE' = round(svymean$SE*100,1) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
                          'Mean' =round(svymean$mean,1),
                          'SE' = round(svymean$SE,1))
      return(model)
    }
  }  
  Quantile2<- ldply(lapply(VAR, model))
}
{ #* section 18.3 Moderate/Severe periodontitis ####
  rhcSvy_PD<-subset(rhcSvy,TYG_quantile=="Quantile 3")
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy_PD,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,1),
                          'SE' = round(svymean$SE*100,1) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
                          'Mean' =round(svymean$mean,1),
                          'SE' = round(svymean$SE,1))
      return(model)
    }
  }  
  Quantile3<- ldply(lapply(VAR, model))
}
{ #* section 18.3 Moderate/Severe periodontitis ####
  rhcSvy_PD<-subset(rhcSvy,TYG_quantile=="Quantile 4")
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy_PD,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,1),
                          'SE' = round(svymean$SE*100,1) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
                          'Mean' =round(svymean$mean,1),
                          'SE' = round(svymean$SE,1))
      return(model)
    }
  }  
  Quantile4<- ldply(lapply(VAR, model))
}
Table1<-cbind(Over,Quantile1[,c("counts","Mean","SE")],
              Quantile2[,c("counts","Mean","SE")],
              Quantile3[,c("counts","Mean","SE")],
              Quantile4[,c("counts","Mean","SE")])
table(Interpolation_weighted$TYG_quantile)
save(Table1,file = "I:/NHANES study/PD&TYG&MO/Data/Table1_Rdata")  
{ #* section 18.4 t-test and chi-test ####
  model<- function(x){
    
    if( x %in% var ) {
      formula<-as.formula(paste0("~",x,"+TYG_quantile"))
      chi_test<-svychisq(formula,rhcSvy)
      model <- data.frame('Covariates'=x,
                          'P value' =chi_test[["p.value"]])
      return(model)
    } else {
      formula<-as.formula(paste0(x,"~TYG_quantile"))
      fit<-svyglm(formula, design=rhcSvy)
      result<-regTermTest(fit, "TYG_quantile")
      model <- data.frame('Covariates'=x,
                          'P value' =result$p)
      
      return(model)
    }
  }  
  test_data<- ldply(lapply(VAR, model))
  test_data$P.value<-round(test_data$P.value,3)
  test_data$P.value[test_data$P.value==0]<-"<0.001"
  new.function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  test_data$P.value<-lapply(test_data$P.value,new.function)
  test_data$P.value<-as.character(test_data$P.value)
}
load(file = "I:/NHANES study/PD&TYG&MO/Data/Table1_Rdata")
Table1<-merge(Table1,test_data,by="Covariates",all.x = T)
Table1$Covariates
Table1$Row<-paste0(Table1$Covariates," ",Table1$grade)
rownames(Table1)<-Table1$Row
colnames(Table1)<-c("Covariates","grade",
                    "counts_all","Mean_all","SE_all",
                    "counts_TYG_quantile1","Mean_TYG_quantile1","SE_TYG_quantile1",
                    "counts_TYG_quantile2","Mean_TYG_quantile2","SE_TYG_quantile2",
                    "counts_TYG_quantile3","Mean_TYG_quantile3","SE_TYG_quantile3",
                    "counts_TYG_quantile4","Mean_TYG_quantile4","SE_TYG_quantile4",
                    "P.value","Row")
rownames(Table1)
{ #* section 18.5 Combine  Table 1#####
#Events
#all.cause
PD.counts<-table(Interpolation_weighted$TYG_quantile,useNA = "ifany")

PD_quantile1<-Interpolation_weighted[which(Interpolation_weighted$TYG_quantile=="Quantile 1"),]
PD_quantile2<-Interpolation_weighted[which(Interpolation_weighted$TYG_quantile=="Quantile 2"),]
PD_quantile3<-Interpolation_weighted[which(Interpolation_weighted$TYG_quantile=="Quantile 3"),]
PD_quantile4<-Interpolation_weighted[which(Interpolation_weighted$TYG_quantile=="Quantile 4"),]

PD_quantile1.counts_ad<-format(round(sum(PD_quantile1$weight)), big.mark = ",", scientific = FALSE)
PD_quantile2.counts_ad<-format(round(sum(PD_quantile2$weight)), big.mark = ",", scientific = FALSE)
PD_quantile3.counts_ad<-format(round(sum(PD_quantile3$weight)), big.mark = ",", scientific = FALSE)
PD_quantile4.counts_ad<-format(round(sum(PD_quantile4$weight)), big.mark = ",", scientific = FALSE)


Counts_ad<-format(round(sum(PD_quantile1$weight))+
                  round(sum(PD_quantile2$weight))+
                  round(sum(PD_quantile3$weight))+
                  round(sum(PD_quantile4$weight)), big.mark = ",", scientific = FALSE)


Table<-c("","","","TYG quantile","","","","","","","","")
Table<-rbind(Table,c("","Over all","",
                     "TYG Quantile 1","",
                     "TYG Quantile 2","",
                     "TYG Quantile 3","",
                     "TYG Quantile 4","",
                     ""))
Table<-rbind(Table,c("Characteristics",
                     "Mean/ %*","SE*",
                     "Mean/ %","SE",
                     "Mean/ %","SE*",
                     "Mean/ %","SE",
                     "Mean/ %","SE","P†"))
Table<-rbind(Table,c("No.(Unweighted)",PD.counts[1]+PD.counts[2]+PD.counts[3]+PD.counts[4],"",
                     PD.counts[1],"",
                     PD.counts[2],"",
                     PD.counts[3],"",
                     PD.counts[4],"",
                     ""))
Table<-rbind(Table,c("No.(Weighted)",Counts_ad,"",
                     PD_quantile1.counts_ad,"",
                     PD_quantile2.counts_ad,"",
                     PD_quantile3.counts_ad,"",
                     PD_quantile4.counts_ad,"",""))
#Age
Table<-rbind(Table,c("Age (years), mean",
                     Table1["Age Mean ± SE","Mean_all"],Table1["Age Mean ± SE","SE_all"],
                     Table1["Age Mean ± SE","Mean_TYG_quantile1"],Table1["Age Mean ± SE","SE_TYG_quantile1"],
                     Table1["Age Mean ± SE","Mean_TYG_quantile2"],Table1["Age Mean ± SE","SE_TYG_quantile2"],
                     Table1["Age Mean ± SE","Mean_TYG_quantile3"],Table1["Age Mean ± SE","SE_TYG_quantile3"],
                     Table1["Age Mean ± SE","Mean_TYG_quantile4"],Table1["Age Mean ± SE","SE_TYG_quantile4"],
                     Table1["Age Mean ± SE","P.value"] ))
Table<-rbind(Table,c("Age status, %","","","","","","","","","","",Table1["Age_status <45","P.value"]))
Table<-rbind(Table,c("<45",
                     Table1["Age_status <45","Mean_all"],Table1["Age_status <45","SE_all"],
                     Table1["Age_status <45","Mean_TYG_quantile1"],Table1["Age_status <45","SE_TYG_quantile1"],
                     Table1["Age_status <45","Mean_TYG_quantile2"],Table1["Age_status <45","SE_TYG_quantile2"],
                     Table1["Age_status <45","Mean_TYG_quantile3"],Table1["Age_status <45","SE_TYG_quantile3"],
                     Table1["Age_status <45","Mean_TYG_quantile4"],Table1["Age_status <45","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("[45, 65)",
                     Table1["Age_status [45,65)","Mean_all"],Table1["Age_status <45","SE_all"],
                     Table1["Age_status [45,65)","Mean_TYG_quantile1"],Table1["Age_status [45,65)","SE_TYG_quantile1"],
                     Table1["Age_status [45,65)","Mean_TYG_quantile2"],Table1["Age_status [45,65)","SE_TYG_quantile2"],
                     Table1["Age_status [45,65)","Mean_TYG_quantile3"],Table1["Age_status [45,65)","SE_TYG_quantile3"],
                     Table1["Age_status [45,65)","Mean_TYG_quantile4"],Table1["Age_status [45,65)","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("≥65",
                     Table1["Age_status >=65","Mean_all"],Table1["Age_status >=65","SE_all"],
                     Table1["Age_status >=65","Mean_TYG_quantile1"],Table1["Age_status >=65","SE_TYG_quantile1"],
                     Table1["Age_status >=65","Mean_TYG_quantile2"],Table1["Age_status >=65","SE_TYG_quantile2"],
                     Table1["Age_status >=65","Mean_TYG_quantile3"],Table1["Age_status >=65","SE_TYG_quantile3"],
                     Table1["Age_status >=65","Mean_TYG_quantile4"],Table1["Age_status >=65","SE_TYG_quantile4"],
                     "" ))

#Sex
Table<-rbind(Table,c("Sex, Female",
                     Table1["Sex Female","Mean_all"],Table1["Sex Female","SE_all"],
                     Table1["Sex Female","Mean_TYG_quantile1"],Table1["Sex Female","SE_TYG_quantile1"],
                     Table1["Sex Female","Mean_TYG_quantile2"],Table1["Sex Female","SE_TYG_quantile2"],
                     Table1["Sex Female","Mean_TYG_quantile3"],Table1["Sex Female","SE_TYG_quantile3"],
                     Table1["Sex Female","Mean_TYG_quantile4"],Table1["Sex Female","SE_TYG_quantile4"],
                     Table1["Sex Female","P.value"]))

#Race/ ethnicity
Table<-rbind(Table,c("Race/ ethnicity, %","","","","","","","","","","",Table1["Race_ethnicity Non-Hispanic White","P.value"]))
Table<-rbind(Table,c("Non-Hispanic white",
                     Table1["Race_ethnicity Non-Hispanic White","Mean_all"],Table1["Race_ethnicity Non-Hispanic White","SE_all"],
                     Table1["Race_ethnicity Non-Hispanic White","Mean_TYG_quantile1"],Table1["Race_ethnicity Non-Hispanic White","SE_TYG_quantile1"],
                     Table1["Race_ethnicity Non-Hispanic White","Mean_TYG_quantile2"],Table1["Race_ethnicity Non-Hispanic White","SE_TYG_quantile2"],
                     Table1["Race_ethnicity Non-Hispanic White","Mean_TYG_quantile3"],Table1["Race_ethnicity Non-Hispanic White","SE_TYG_quantile3"],
                     Table1["Race_ethnicity Non-Hispanic White","Mean_TYG_quantile4"],Table1["Race_ethnicity Non-Hispanic White","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("Non-Hispanic black",
                     Table1["Race_ethnicity Non-Hispanic Black","Mean_all"],Table1["Race_ethnicity Non-Hispanic Black","SE_all"],
                     Table1["Race_ethnicity Non-Hispanic Black","Mean_TYG_quantile1"],Table1["Race_ethnicity Non-Hispanic Black","SE_TYG_quantile1"],
                     Table1["Race_ethnicity Non-Hispanic Black","Mean_TYG_quantile2"],Table1["Race_ethnicity Non-Hispanic Black","SE_TYG_quantile2"],
                     Table1["Race_ethnicity Non-Hispanic Black","Mean_TYG_quantile3"],Table1["Race_ethnicity Non-Hispanic Black","SE_TYG_quantile3"],
                     Table1["Race_ethnicity Non-Hispanic Black","Mean_TYG_quantile4"],Table1["Race_ethnicity Non-Hispanic Black","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("Hispanic",
                     Table1["Race_ethnicity Hispanic","Mean_all"],Table1["Race_ethnicity Hispanic","SE_all"],
                     Table1["Race_ethnicity Hispanic","Mean_TYG_quantile1"],Table1["Race_ethnicity Hispanic","SE_TYG_quantile1"],
                     Table1["Race_ethnicity Hispanic","Mean_TYG_quantile2"],Table1["Race_ethnicity Hispanic","SE_TYG_quantile2"],
                     Table1["Race_ethnicity Hispanic","Mean_TYG_quantile3"],Table1["Race_ethnicity Hispanic","SE_TYG_quantile3"],
                     Table1["Race_ethnicity Hispanic","Mean_TYG_quantile4"],Table1["Race_ethnicity Hispanic","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("Other race/ ethnicity",
                     Table1["Race_ethnicity Other_Race","Mean_all"],Table1["Race_ethnicity Other_Race","SE_all"],
                     Table1["Race_ethnicity Other_Race","Mean_TYG_quantile1"],Table1["Race_ethnicity Other_Race","SE_TYG_quantile1"],
                     Table1["Race_ethnicity Other_Race","Mean_TYG_quantile2"],Table1["Race_ethnicity Other_Race","SE_TYG_quantile2"],
                     Table1["Race_ethnicity Other_Race","Mean_TYG_quantile3"],Table1["Race_ethnicity Other_Race","SE_TYG_quantile3"],
                     Table1["Race_ethnicity Other_Race","Mean_TYG_quantile4"],Table1["Race_ethnicity Other_Race","SE_TYG_quantile4"],
                     "" ))
#Marital status
Table<-rbind(Table,c("Marital status, %","","","","","","","","","","",Table1["Marital_status Married","P.value"]))
Table<-rbind(Table,c("Married",
                     Table1["Marital_status Married","Mean_all"],Table1["Marital_status Married","SE_all"],
                     Table1["Marital_status Married","Mean_TYG_quantile1"],Table1["Marital_status Married","SE_TYG_quantile1"],
                     Table1["Marital_status Married","Mean_TYG_quantile2"],Table1["Marital_status Married","SE_TYG_quantile2"],
                     Table1["Marital_status Married","Mean_TYG_quantile3"],Table1["Marital_status Married","SE_TYG_quantile3"],
                     Table1["Marital_status Married","Mean_TYG_quantile4"],Table1["Marital_status Married","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("Never married",
                     Table1["Marital_status Never_married","Mean_all"],Table1["Marital_status Never_married","SE_all"],
                     Table1["Marital_status Never_married","Mean_TYG_quantile1"],Table1["Marital_status Never_married","SE_TYG_quantile1"],
                     Table1["Marital_status Never_married","Mean_TYG_quantile2"],Table1["Marital_status Never_married","SE_TYG_quantile2"],
                     Table1["Marital_status Never_married","Mean_TYG_quantile3"],Table1["Marital_status Never_married","SE_TYG_quantile3"],
                     Table1["Marital_status Never_married","Mean_TYG_quantile4"],Table1["Marital_status Never_married","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("Widowed/ Divorced/ Separated",
                     Table1["Marital_status Separated","Mean_all"],Table1["Marital_status Separated","SE_all"],
                     Table1["Marital_status Separated","Mean_TYG_quantile1"],Table1["Marital_status Separated","SE_TYG_quantile1"],
                     Table1["Marital_status Separated","Mean_TYG_quantile2"],Table1["Marital_status Separated","SE_TYG_quantile2"],
                     Table1["Marital_status Separated","Mean_TYG_quantile3"],Table1["Marital_status Separated","SE_TYG_quantile3"],
                     Table1["Marital_status Separated","Mean_TYG_quantile4"],Table1["Marital_status Separated","SE_TYG_quantile4"],
                     "" ))

#Socioeconomic Status
Table<-rbind(Table,c("Socioeconomic Status, %","","","","","","","","","","",Table1["SES low","P.value"]))
Table<-rbind(Table,c("Low",
                     Table1["SES low","Mean_all"],Table1["SES low","SE_all"],
                     Table1["SES low","Mean_TYG_quantile1"],Table1["SES low","SE_TYG_quantile1"],
                     Table1["SES low","Mean_TYG_quantile2"],Table1["SES low","SE_TYG_quantile2"],
                     Table1["SES low","Mean_TYG_quantile3"],Table1["SES low","SE_TYG_quantile3"],
                     Table1["SES low","Mean_TYG_quantile4"],Table1["SES low","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("low_Medium",
                     Table1["SES low_medium","Mean_all"],Table1["SES low_medium","SE_all"],
                     Table1["SES low_medium","Mean_TYG_quantile1"],Table1["SES low_medium","SE_TYG_quantile1"],
                     Table1["SES low_medium","Mean_TYG_quantile2"],Table1["SES low_medium","SE_TYG_quantile2"],
                     Table1["SES low_medium","Mean_TYG_quantile3"],Table1["SES low_medium","SE_TYG_quantile3"],
                     Table1["SES low_medium","Mean_TYG_quantile4"],Table1["SES low_medium","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("medium_high",
                     Table1["SES medium_high","Mean_all"],Table1["SES medium_high","SE_all"],
                     Table1["SES medium_high","Mean_TYG_quantile1"],Table1["SES medium_high","SE_TYG_quantile1"],
                     Table1["SES medium_high","Mean_TYG_quantile2"],Table1["SES medium_high","SE_TYG_quantile2"],
                     Table1["SES medium_high","Mean_TYG_quantile3"],Table1["SES medium_high","SE_TYG_quantile3"],
                     Table1["SES medium_high","Mean_TYG_quantile4"],Table1["SES medium_high","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("High",
                     Table1["SES high","Mean_all"],Table1["SES high","SE_all"],
                     Table1["SES high","Mean_TYG_quantile1"],Table1["SES high","SE_TYG_quantile1"],
                     Table1["SES high","Mean_TYG_quantile2"],Table1["SES high","SE_TYG_quantile2"],
                     Table1["SES high","Mean_TYG_quantile3"],Table1["SES high","SE_TYG_quantile3"],
                     Table1["SES high","Mean_TYG_quantile4"],Table1["SES high","SE_TYG_quantile4"],
                     "" ))

#Smoking status
Table<-rbind(Table,c("Smoking status, %","","","","","","","","","","",Table1["Smoking_status Never_smoker","P.value"]))
Table<-rbind(Table,c("Never smoker",
                     Table1["Smoking_status Never_smoker","Mean_all"],Table1["Smoking_status Never_smoker","SE_all"],
                     Table1["Smoking_status Never_smoker","Mean_TYG_quantile1"],Table1["Smoking_status Never_smoker","SE_TYG_quantile1"],
                     Table1["Smoking_status Never_smoker","Mean_TYG_quantile2"],Table1["Smoking_status Never_smoker","SE_TYG_quantile2"],
                     Table1["Smoking_status Never_smoker","Mean_TYG_quantile3"],Table1["Smoking_status Never_smoker","SE_TYG_quantile3"],
                     Table1["Smoking_status Never_smoker","Mean_TYG_quantile4"],Table1["Smoking_status Never_smoker","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("Former smoker",
                     Table1["Smoking_status Former_smoker","Mean_all"],Table1["Smoking_status Former_smoker","SE_all"],
                     Table1["Smoking_status Former_smoker","Mean_TYG_quantile1"],Table1["Smoking_status Former_smoker","SE_TYG_quantile1"],
                     Table1["Smoking_status Former_smoker","Mean_TYG_quantile2"],Table1["Smoking_status Former_smoker","SE_TYG_quantile2"],
                     Table1["Smoking_status Former_smoker","Mean_TYG_quantile3"],Table1["Smoking_status Former_smoker","SE_TYG_quantile3"],
                     Table1["Smoking_status Former_smoker","Mean_TYG_quantile4"],Table1["Smoking_status Former_smoker","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("Current smoker",
                     Table1["Smoking_status Current_smoker","Mean_all"],Table1["Smoking_status Current_smoker","SE_all"],
                     Table1["Smoking_status Current_smoker","Mean_TYG_quantile1"],Table1["Smoking_status Current_smoker","SE_TYG_quantile1"],
                     Table1["Smoking_status Current_smoker","Mean_TYG_quantile2"],Table1["Smoking_status Current_smoker","SE_TYG_quantile2"],
                     Table1["Smoking_status Current_smoker","Mean_TYG_quantile3"],Table1["Smoking_status Current_smoker","SE_TYG_quantile3"],
                     Table1["Smoking_status Current_smoker","Mean_TYG_quantile4"],Table1["Smoking_status Current_smoker","SE_TYG_quantile4"],
                     "" ))

#Drinking status
Table<-rbind(Table,c("Drinking status, %","","","","","","","","","","",Table1["Drinking_status Nondrinker","P.value"]))
Table<-rbind(Table,c("Nondrinker",
                     Table1["Drinking_status Nondrinker","Mean_all"],Table1["Drinking_status Nondrinker","SE_all"],
                     Table1["Drinking_status Nondrinker","Mean_TYG_quantile1"],Table1["Drinking_status Nondrinker","SE_TYG_quantile1"],
                     Table1["Drinking_status Nondrinker","Mean_TYG_quantile2"],Table1["Drinking_status Nondrinker","SE_TYG_quantile2"],
                     Table1["Drinking_status Nondrinker","Mean_TYG_quantile3"],Table1["Drinking_status Nondrinker","SE_TYG_quantile3"],
                     Table1["Drinking_status Nondrinker","Mean_TYG_quantile4"],Table1["Drinking_status Nondrinker","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("Light/ moderate drinker",
                     Table1["Drinking_status Light/moderate_drinker","Mean_all"],Table1["Drinking_status Light/moderate_drinker","SE_all"],
                     Table1["Drinking_status Light/moderate_drinker","Mean_TYG_quantile1"],Table1["Drinking_status Light/moderate_drinker","SE_TYG_quantile1"],
                     Table1["Drinking_status Light/moderate_drinker","Mean_TYG_quantile2"],Table1["Drinking_status Light/moderate_drinker","SE_TYG_quantile2"],
                     Table1["Drinking_status Light/moderate_drinker","Mean_TYG_quantile3"],Table1["Drinking_status Light/moderate_drinker","SE_TYG_quantile3"],
                     Table1["Drinking_status Light/moderate_drinker","Mean_TYG_quantile4"],Table1["Drinking_status Light/moderate_drinker","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("Heavier drinker",
                     Table1["Drinking_status Heavier_drinker","Mean_all"],Table1["Drinking_status Heavier_drinker","SE_all"],
                     Table1["Drinking_status Heavier_drinker","Mean_TYG_quantile1"],Table1["Drinking_status Heavier_drinker","SE_TYG_quantile1"],
                     Table1["Drinking_status Heavier_drinker","Mean_TYG_quantile2"],Table1["Drinking_status Heavier_drinker","SE_TYG_quantile2"],
                     Table1["Drinking_status Heavier_drinker","Mean_TYG_quantile3"],Table1["Drinking_status Heavier_drinker","SE_TYG_quantile3"],
                     Table1["Drinking_status Heavier_drinker","Mean_TYG_quantile4"],Table1["Drinking_status Heavier_drinker","SE_TYG_quantile4"],
                     "" ))
#Physical status
Table<-rbind(Table,c("Physical status, %","","","","","","","","","","",Table1["Physical_status Inactive","P.value"]))
Table<-rbind(Table,c("Inactive",
                     Table1["Physical_status Inactive","Mean_all"],Table1["Physical_status Inactive","SE_all"],
                     Table1["Physical_status Inactive","Mean_TYG_quantile1"],Table1["Physical_status Inactive","SE_TYG_quantile1"],
                     Table1["Physical_status Inactive","Mean_TYG_quantile2"],Table1["Physical_status Inactive","SE_TYG_quantile2"],
                     Table1["Physical_status Inactive","Mean_TYG_quantile3"],Table1["Physical_status Inactive","SE_TYG_quantile3"],
                     Table1["Physical_status Inactive","Mean_TYG_quantile4"],Table1["Physical_status Inactive","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("Insufficient",
                     Table1["Physical_status Insufficient","Mean_all"],Table1["Physical_status Insufficient","SE_all"],
                     Table1["Physical_status Insufficient","Mean_TYG_quantile1"],Table1["Physical_status Insufficient","SE_TYG_quantile1"],
                     Table1["Physical_status Insufficient","Mean_TYG_quantile2"],Table1["Physical_status Insufficient","SE_TYG_quantile2"],
                     Table1["Physical_status Insufficient","Mean_TYG_quantile3"],Table1["Physical_status Insufficient","SE_TYG_quantile3"],
                     Table1["Physical_status Insufficient","Mean_TYG_quantile4"],Table1["Physical_status Insufficient","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("Recommended",
                     Table1["Physical_status Recommended","Mean_all"],Table1["Physical_status Recommended","SE_all"],
                     Table1["Physical_status Recommended","Mean_TYG_quantile1"],Table1["Physical_status Recommended","SE_TYG_quantile1"],
                     Table1["Physical_status Recommended","Mean_TYG_quantile2"],Table1["Physical_status Recommended","SE_TYG_quantile2"],
                     Table1["Physical_status Recommended","Mean_TYG_quantile3"],Table1["Physical_status Recommended","SE_TYG_quantile3"],
                     Table1["Physical_status Recommended","Mean_TYG_quantile4"],Table1["Physical_status Recommended","SE_TYG_quantile4"],
                     "" ))

#Healthy Eating Index
#Table<-rbind(Table,c("Healthy eating index, %","","","","","","",Table1["HEI Quintile 1","P.value"]))

# Table<-rbind(Table,c("Quintile 1",
#                      Table1["HEI Quintile 1","Mean_all"],Table1["HEI Quintile 1","SE_all"],
#                      Table1["HEI Quintile 1","Mean_noPD"],Table1["HEI Quintile 1","SE_noPD"],
#                      Table1["HEI Quintile 1","Mean_PD"],Table1["HEI Quintile 1","SE_PD"],
#                      "" ))
# Table<-rbind(Table,c("Quintile 2",
#                      Table1["HEI Quintile 2","Mean_all"],Table1["HEI Quintile 2","SE_all"],
#                      Table1["HEI Quintile 2","Mean_noPD"],Table1["HEI Quintile 2","SE_noPD"],
#                      Table1["HEI Quintile 2","Mean_PD"],Table1["HEI Quintile 2","SE_PD"],
#                      "" ))
#
# Table<-rbind(Table,c("Quintile 3",
#                      Table1["HEI Quintile 3","Mean_all"],Table1["HEI Quintile 3","SE_all"],
#                      Table1["HEI Quintile 3","Mean_noPD"],Table1["HEI Quintile 3","SE_noPD"],
#                      Table1["HEI Quintile 3","Mean_PD"],Table1["HEI Quintile 3","SE_PD"],
#                      "" ))
# Table<-rbind(Table,c("Quintile 4",
#                      Table1["HEI Quintile 4","Mean_all"],Table1["HEI Quintile 4","SE_all"],
#                      Table1["HEI Quintile 4","Mean_noPD"],Table1["HEI Quintile 4","SE_noPD"],
#                      Table1["HEI Quintile 4","Mean_PD"],Table1["HEI Quintile 4","SE_PD"],
#                      "" ))
# Table<-rbind(Table,c("Quintile 5",
#                      Table1["HEI Quintile 5","Mean_all"],Table1["HEI Quintile 5","SE_all"],
#                      Table1["HEI Quintile 5","Mean_noPD"],Table1["HEI Quintile 5","SE_noPD"],
#                      Table1["HEI Quintile 5","Mean_PD"],Table1["HEI Quintile 5","SE_PD"],
#                      "" ))

#BMI
Table<-rbind(Table,c("BMI, Mean",
                     Table1["BMI Mean ± SE","Mean_all"],Table1["BMI Mean ± SE","SE_all"],
                     Table1["BMI Mean ± SE","Mean_TYG_quantile1"],Table1["BMI Mean ± SE","SE_TYG_quantile1"],
                     Table1["BMI Mean ± SE","Mean_TYG_quantile2"],Table1["BMI Mean ± SE","SE_TYG_quantile2"],
                     Table1["BMI Mean ± SE","Mean_TYG_quantile3"],Table1["BMI Mean ± SE","SE_TYG_quantile3"],
                     Table1["BMI Mean ± SE","Mean_TYG_quantile4"],Table1["BMI Mean ± SE","SE_TYG_quantile4"],
                     Table1["BMI Mean ± SE","P.value"] ))

Table<-rbind(Table,c("BMI status (kg/m2), %","","","","","","","","","","",Table1["BMI_status (0,25)","P.value"]))
Table<-rbind(Table,c("<25",
                     Table1["BMI_status (0,25)","Mean_all"],Table1["BMI_status (0,25)","SE_all"],
                     Table1["BMI_status (0,25)","Mean_TYG_quantile1"],Table1["BMI_status (0,25)","SE_TYG_quantile1"],
                     Table1["BMI_status (0,25)","Mean_TYG_quantile2"],Table1["BMI_status (0,25)","SE_TYG_quantile2"],
                     Table1["BMI_status (0,25)","Mean_TYG_quantile3"],Table1["BMI_status (0,25)","SE_TYG_quantile3"],
                     Table1["BMI_status (0,25)","Mean_TYG_quantile4"],Table1["BMI_status (0,25)","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("[25.0 -30)",
                     Table1["BMI_status [25.0-30)","Mean_all"],Table1["BMI_status [25.0-30)","SE_all"],
                     Table1["BMI_status [25.0-30)","Mean_TYG_quantile1"],Table1["BMI_status [25.0-30)","SE_TYG_quantile1"],
                     Table1["BMI_status [25.0-30)","Mean_TYG_quantile2"],Table1["BMI_status [25.0-30)","SE_TYG_quantile2"],
                     Table1["BMI_status [25.0-30)","Mean_TYG_quantile3"],Table1["BMI_status [25.0-30)","SE_TYG_quantile3"],
                     Table1["BMI_status [25.0-30)","Mean_TYG_quantile4"],Table1["BMI_status [25.0-30)","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("≥30",
                     Table1["BMI_status [30,inf)","Mean_all"],Table1["BMI_status [30,inf)","SE_all"],
                     Table1["BMI_status [30,inf)","Mean_TYG_quantile1"],Table1["BMI_status [30,inf)","SE_TYG_quantile1"],
                     Table1["BMI_status [30,inf)","Mean_TYG_quantile2"],Table1["BMI_status [30,inf)","SE_TYG_quantile2"],
                     Table1["BMI_status [30,inf)","Mean_TYG_quantile3"],Table1["BMI_status [30,inf)","SE_TYG_quantile3"],
                     Table1["BMI_status [30,inf)","Mean_TYG_quantile4"],Table1["BMI_status [30,inf)","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("Comorbidities, %","","","","","","","","","","",""))
#Hypertension
Table<-rbind(Table,c("Hypertension",
                     Table1["HTN_status YES","Mean_all"],Table1["HTN_status YES","SE_all"],
                     Table1["HTN_status YES","Mean_TYG_quantile1"],Table1["HTN_status YES","SE_TYG_quantile1"],
                     Table1["HTN_status YES","Mean_TYG_quantile2"],Table1["HTN_status YES","SE_TYG_quantile2"],
                     Table1["HTN_status YES","Mean_TYG_quantile3"],Table1["HTN_status YES","SE_TYG_quantile3"],
                     Table1["HTN_status YES","Mean_TYG_quantile4"],Table1["HTN_status YES","SE_TYG_quantile4"],
                     Table1["HTN_status YES","P.value"]))
#Hyperlipidemia
Table<-rbind(Table,c("Hyperlipidemia",
                     Table1["HPL_status YES","Mean_all"],Table1["HPL_status YES","SE_all"],
                     Table1["HPL_status YES","Mean_TYG_quantile1"],Table1["HPL_status YES","SE_TYG_quantile1"],
                     Table1["HPL_status YES","Mean_TYG_quantile2"],Table1["HPL_status YES","SE_TYG_quantile2"],
                     Table1["HPL_status YES","Mean_TYG_quantile3"],Table1["HPL_status YES","SE_TYG_quantile3"],
                     Table1["HPL_status YES","Mean_TYG_quantile4"],Table1["HPL_status YES","SE_TYG_quantile4"],
                     Table1["HPL_status YES","P.value"]))
#Diabetes mellitus
Table<-rbind(Table,c("Diabetes mellitus",
                     Table1["T2D_status YES","Mean_all"],Table1["T2D_status YES","SE_all"],
                     Table1["T2D_status YES","Mean_TYG_quantile1"],Table1["T2D_status YES","SE_TYG_quantile1"],
                     Table1["T2D_status YES","Mean_TYG_quantile2"],Table1["T2D_status YES","SE_TYG_quantile2"],
                     Table1["T2D_status YES","Mean_TYG_quantile3"],Table1["T2D_status YES","SE_TYG_quantile3"],
                     Table1["T2D_status YES","Mean_TYG_quantile4"],Table1["T2D_status YES","SE_TYG_quantile4"],
                     Table1["T2D_status YES","P.value"]))
#Cohort
Table<-rbind(Table,c("Cohort period, %","","","","","","","","","","",Table1["Cohort NHANES_CON1","P.value"]))

Table<-rbind(Table,c("NHANES III",
                     Table1["Cohort NHANES_III","Mean_all"],Table1["Cohort NHANES_III","SE_all"],
                     Table1["Cohort NHANES_III","Mean_TYG_quantile1"],Table1["Cohort NHANES_III","SE_TYG_quantile1"],
                     Table1["Cohort NHANES_III","Mean_TYG_quantile2"],Table1["Cohort NHANES_III","SE_TYG_quantile2"],
                     Table1["Cohort NHANES_III","Mean_TYG_quantile3"],Table1["Cohort NHANES_III","SE_TYG_quantile3"],
                     Table1["Cohort NHANES_III","Mean_TYG_quantile4"],Table1["Cohort NHANES_III","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("NHANES 1999-2004",
                     Table1["Cohort NHANES_CON1","Mean_all"],Table1["Cohort NHANES_CON1","SE_all"],
                     Table1["Cohort NHANES_CON1","Mean_TYG_quantile1"],Table1["Cohort NHANES_CON1","SE_TYG_quantile1"],
                     Table1["Cohort NHANES_CON1","Mean_TYG_quantile2"],Table1["Cohort NHANES_CON1","SE_TYG_quantile2"],
                     Table1["Cohort NHANES_CON1","Mean_TYG_quantile3"],Table1["Cohort NHANES_CON1","SE_TYG_quantile3"],
                     Table1["Cohort NHANES_CON1","Mean_TYG_quantile4"],Table1["Cohort NHANES_CON1","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("NHANES 2009-2014",
                     Table1["Cohort NHANES_CON2","Mean_all"],Table1["Cohort NHANES_CON2","SE_all"],
                     Table1["Cohort NHANES_CON2","Mean_TYG_quantile1"],Table1["Cohort NHANES_CON2","SE_TYG_quantile1"],
                     Table1["Cohort NHANES_CON2","Mean_TYG_quantile2"],Table1["Cohort NHANES_CON2","SE_TYG_quantile2"],
                     Table1["Cohort NHANES_CON2","Mean_TYG_quantile3"],Table1["Cohort NHANES_CON2","SE_TYG_quantile3"],
                     Table1["Cohort NHANES_CON2","Mean_TYG_quantile4"],Table1["Cohort NHANES_CON2","SE_TYG_quantile4"],
                     "" ))

Table_1<-Table
write.table(Table_1,sep = ",",file ="I:/NHANES study/PD&TYG&MO/Result/Table 1.csv" ,row.names =F,col.names =F )
}




{ #* section 18.6 Combine  Table S3#####
  Table<-c("","","","TYG quantile","","","","","","","","")
  Table<-rbind(Table,c("","Over all","",
                       "TYG Quantile 1","",
                       "TYG Quantile 2","",
                       "TYG Quantile 3","",
                       "TYG Quantile 4","",
                       ""))
  Table<-rbind(Table,c("Characteristics",
                       "Mean/ %*","SE*",
                       "Mean/ %","SE",
                       "Mean/ %","SE*",
                       "Mean/ %","SE",
                       "Mean/ %","SE","P†"))
  Table<-rbind(Table,c("No.(Unweighted)",PD.counts[1]+PD.counts[2]+PD.counts[3]+PD.counts[4],"",
                       PD.counts[1],"",
                       PD.counts[2],"",
                       PD.counts[3],"",
                       PD.counts[4],"",
                       ""))
  Table<-rbind(Table,c("No.(Weighted)",Counts_ad,"",
                       PD_quantile1.counts_ad,"",
                       PD_quantile2.counts_ad,"",
                       PD_quantile3.counts_ad,"",
                       PD_quantile4.counts_ad,"",""))
#Socioeconomic index
Table<-rbind(Table,c("Socioeconomic index, %","","","","","","","","","","",Table1["SEI Unemployment","P.value"]))
Table<-rbind(Table,c("Unemployment",
                     Table1["SEI Unemployment","Mean_all"],Table1["SEI Unemployment","SE_all"],
                     Table1["SEI Unemployment","Mean_TYG_quantile1"],Table1["SEI Unemployment","SE_TYG_quantile1"],
                     Table1["SEI Unemployment","Mean_TYG_quantile2"],Table1["SEI Unemployment","SE_TYG_quantile2"],
                     Table1["SEI Unemployment","Mean_TYG_quantile3"],Table1["SEI Unemployment","SE_TYG_quantile3"],
                     Table1["SEI Unemployment","Mean_TYG_quantile4"],Table1["SEI Unemployment","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("Lower",
                     Table1["SEI Lower","Mean_all"],Table1["SEI Lower","SE_all"],
                     Table1["SEI Lower","Mean_TYG_quantile1"],Table1["SEI Lower","SE_TYG_quantile1"],
                     Table1["SEI Lower","Mean_TYG_quantile2"],Table1["SEI Lower","SE_TYG_quantile2"],
                     Table1["SEI Lower","Mean_TYG_quantile3"],Table1["SEI Lower","SE_TYG_quantile3"],
                     Table1["SEI Lower","Mean_TYG_quantile4"],Table1["SEI Lower","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("Upper",
                     Table1["SEI Upper","Mean_all"],Table1["SEI Upper","SE_all"],
                     Table1["SEI Upper","Mean_TYG_quantile1"],Table1["SEI Upper","SE_TYG_quantile1"],
                     Table1["SEI Upper","Mean_TYG_quantile2"],Table1["SEI Upper","SE_TYG_quantile2"],
                     Table1["SEI Upper","Mean_TYG_quantile3"],Table1["SEI Upper","SE_TYG_quantile3"],
                     Table1["SEI Upper","Mean_TYG_quantile4"],Table1["SEI Upper","SE_TYG_quantile4"],
                     "" ))

#Poverty income ratio
Table<-rbind(Table,c("Poverty income ratio, %","","","","","","","","","","",Table1["PIR (0, 1]","P.value"]))
Table<-rbind(Table,c("<1.00",
                     Table1["PIR (0, 1]","Mean_all"],Table1["PIR (0, 1]","SE_all"],
                     Table1["PIR (0, 1]","Mean_TYG_quantile1"],Table1["PIR (0, 1]","SE_TYG_quantile1"],
                     Table1["PIR (0, 1]","Mean_TYG_quantile2"],Table1["PIR (0, 1]","SE_TYG_quantile2"],
                     Table1["PIR (0, 1]","Mean_TYG_quantile3"],Table1["PIR (0, 1]","SE_TYG_quantile3"],
                     Table1["PIR (0, 1]","Mean_TYG_quantile4"],Table1["PIR (0, 1]","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("1.00-3.99",
                     Table1["PIR (1,4)","Mean_all"],Table1["PIR (1,4)","SE_all"],
                     Table1["PIR (1,4)","Mean_TYG_quantile1"],Table1["PIR (1,4)","SE_TYG_quantile1"],
                     Table1["PIR (1,4)","Mean_TYG_quantile2"],Table1["PIR (1,4)","SE_TYG_quantile2"],
                     Table1["PIR (1,4)","Mean_TYG_quantile3"],Table1["PIR (1,4)","SE_TYG_quantile3"],
                     Table1["PIR (1,4)","Mean_TYG_quantile4"],Table1["PIR (1,4)","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("≥4.00",
                     Table1["PIR [4,inf)","Mean_all"],Table1["PIR [4,inf)","SE_all"],
                     Table1["PIR [4,inf)","Mean_TYG_quantile1"],Table1["PIR [4,inf)","SE_TYG_quantile1"],
                     Table1["PIR [4,inf)","Mean_TYG_quantile2"],Table1["PIR [4,inf)","SE_TYG_quantile2"],
                     Table1["PIR [4,inf)","Mean_TYG_quantile3"],Table1["PIR [4,inf)","SE_TYG_quantile3"],
                     Table1["PIR [4,inf)","Mean_TYG_quantile4"],Table1["PIR [4,inf)","SE_TYG_quantile4"],
                     "" ))
#Health insurance
Table<-rbind(Table,c("Health insurance, %","","","","","","","","","","",Table1["Health_insurance No_insurance","P.value"]))
Table<-rbind(Table,c("No insurance",
                     Table1["Health_insurance No_insurance","Mean_all"],Table1["Health_insurance No_insurance","SE_all"],
                     Table1["Health_insurance No_insurance","Mean_TYG_quantile1"],Table1["Health_insurance No_insurance","SE_TYG_quantile1"],
                     Table1["Health_insurance No_insurance","Mean_TYG_quantile2"],Table1["Health_insurance No_insurance","SE_TYG_quantile2"],
                     Table1["Health_insurance No_insurance","Mean_TYG_quantile3"],Table1["Health_insurance No_insurance","SE_TYG_quantile3"],
                     Table1["Health_insurance No_insurance","Mean_TYG_quantile4"],Table1["Health_insurance No_insurance","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("Public insurance only",
                     Table1["Health_insurance Public_insurance","Mean_all"],Table1["Health_insurance Public_insurance","SE_all"],
                     Table1["Health_insurance Public_insurance","Mean_TYG_quantile1"],Table1["Health_insurance Public_insurance","SE_TYG_quantile1"],
                     Table1["Health_insurance Public_insurance","Mean_TYG_quantile2"],Table1["Health_insurance Public_insurance","SE_TYG_quantile2"],
                     Table1["Health_insurance Public_insurance","Mean_TYG_quantile3"],Table1["Health_insurance Public_insurance","SE_TYG_quantile3"],
                     Table1["Health_insurance Public_insurance","Mean_TYG_quantile4"],Table1["Health_insurance Public_insurance","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("Private insurance",
                     Table1["Health_insurance Private_insurance","Mean_all"],Table1["Health_insurance Private_insurance","SE_all"],
                     Table1["Health_insurance Private_insurance","Mean_TYG_quantile1"],Table1["Health_insurance Private_insurance","SE_TYG_quantile1"],
                     Table1["Health_insurance Private_insurance","Mean_TYG_quantile2"],Table1["Health_insurance Private_insurance","SE_TYG_quantile2"],
                     Table1["Health_insurance Private_insurance","Mean_TYG_quantile3"],Table1["Health_insurance Private_insurance","SE_TYG_quantile3"],
                     Table1["Health_insurance Private_insurance","Mean_TYG_quantile4"],Table1["Health_insurance Private_insurance","SE_TYG_quantile4"],
                     "" ))
#Education levels
Table<-rbind(Table,c("Education levels, %","","","","","","","","","","",Table1["Education_levels Less_than_high_school","P.value"]))
Table<-rbind(Table,c("Less than high school",
                     Table1["Education_levels Less_than_high_school","Mean_all"],Table1["Education_levels Less_than_high_school","SE_all"],
                     Table1["Education_levels Less_than_high_school","Mean_TYG_quantile1"],Table1["Education_levels Less_than_high_school","SE_TYG_quantile1"],
                     Table1["Education_levels Less_than_high_school","Mean_TYG_quantile2"],Table1["Education_levels Less_than_high_school","SE_TYG_quantile2"],
                     Table1["Education_levels Less_than_high_school","Mean_TYG_quantile3"],Table1["Education_levels Less_than_high_school","SE_TYG_quantile3"],
                     Table1["Education_levels Less_than_high_school","Mean_TYG_quantile4"],Table1["Education_levels Less_than_high_school","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("High school or equivalent",
                     Table1["Education_levels High_school_or_Equivalent","Mean_all"],Table1["Education_levels High_school_or_Equivalent","SE_all"],
                     Table1["Education_levels High_school_or_Equivalent","Mean_TYG_quantile1"],Table1["Education_levels High_school_or_Equivalent","SE_TYG_quantile1"],
                     Table1["Education_levels High_school_or_Equivalent","Mean_TYG_quantile2"],Table1["Education_levels High_school_or_Equivalent","SE_TYG_quantile2"],
                     Table1["Education_levels High_school_or_Equivalent","Mean_TYG_quantile3"],Table1["Education_levels High_school_or_Equivalent","SE_TYG_quantile3"],
                     Table1["Education_levels High_school_or_Equivalent","Mean_TYG_quantile4"],Table1["Education_levels High_school_or_Equivalent","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("Hispanic",
                     Table1["Race_ethnicity Hispanic","Mean_all"],Table1["Race_ethnicity Hispanic","SE_all"],
                     Table1["Race_ethnicity Hispanic","Mean_TYG_quantile1"],Table1["Race_ethnicity Hispanic","SE_TYG_quantile1"],
                     Table1["Race_ethnicity Hispanic","Mean_TYG_quantile2"],Table1["Race_ethnicity Hispanic","SE_TYG_quantile2"],
                     Table1["Race_ethnicity Hispanic","Mean_TYG_quantile3"],Table1["Race_ethnicity Hispanic","SE_TYG_quantile3"],
                     Table1["Race_ethnicity Hispanic","Mean_TYG_quantile4"],Table1["Race_ethnicity Hispanic","SE_TYG_quantile4"],
                     "" ))
#Socioeconomic Status
Table<-rbind(Table,c("Socioeconomic Status, %","","","","","","","","","","",Table1["SES low","P.value"]))
Table<-rbind(Table,c("Low",
                     Table1["SES low","Mean_all"],Table1["SES low","SE_all"],
                     Table1["SES low","Mean_TYG_quantile1"],Table1["SES low","SE_TYG_quantile1"],
                     Table1["SES low","Mean_TYG_quantile2"],Table1["SES low","SE_TYG_quantile2"],
                     Table1["SES low","Mean_TYG_quantile3"],Table1["SES low","SE_TYG_quantile3"],
                     Table1["SES low","Mean_TYG_quantile4"],Table1["SES low","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("low_medium",
                     Table1["SES low_medium","Mean_all"],Table1["SES low_medium","SE_all"],
                     Table1["SES low_medium","Mean_TYG_quantile1"],Table1["SES low_medium","SE_TYG_quantile1"],
                     Table1["SES low_medium","Mean_TYG_quantile2"],Table1["SES low_medium","SE_TYG_quantile2"],
                     Table1["SES low_medium","Mean_TYG_quantile3"],Table1["SES low_medium","SE_TYG_quantile3"],
                     Table1["SES low_medium","Mean_TYG_quantile4"],Table1["SES low_medium","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("medium_high",
                     Table1["SES medium_high","Mean_all"],Table1["SES medium_high","SE_all"],
                     Table1["SES medium_high","Mean_TYG_quantile1"],Table1["SES medium_high","SE_TYG_quantile1"],
                     Table1["SES medium_high","Mean_TYG_quantile2"],Table1["SES medium_high","SE_TYG_quantile2"],
                     Table1["SES medium_high","Mean_TYG_quantile3"],Table1["SES medium_high","SE_TYG_quantile3"],
                     Table1["SES medium_high","Mean_TYG_quantile4"],Table1["SES medium_high","SE_TYG_quantile4"],
                     "" ))
Table<-rbind(Table,c("High",
                     Table1["SES high","Mean_all"],Table1["SES high","SE_all"],
                     Table1["SES high","Mean_TYG_quantile1"],Table1["SES high","SE_TYG_quantile1"],
                     Table1["SES high","Mean_TYG_quantile2"],Table1["SES high","SE_TYG_quantile2"],
                     Table1["SES high","Mean_TYG_quantile3"],Table1["SES high","SE_TYG_quantile3"],
                     Table1["SES high","Mean_TYG_quantile4"],Table1["SES high","SE_TYG_quantile4"],
                     "" ))
Table_S3<-Table
write.table(Table_S3,sep = ",",file ="I:/NHANES study/PD&TYG&MO/Result/Supplementary Table 3.csv" ,row.names =F,col.names =F )
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 22 Relative mortality rates (Table 2) ####  
load(file="I:/NHANES study/PD&TYG&MO/Data/Interpolation_weighted.Rdata")
#Q1
{#* TYg ####
  {#** Q1 ####
    table(Interpolation_weighted$TYG_quantile)
    Q1<-Interpolation_weighted[which(Interpolation_weighted$TYG_quantile=="Quantile 1"),]
    Q1$year<-Q1$peryear
    Q1$Pyear<-Q1$year*Q1$weight
    Q1_death_All<-Q1[which(Q1$MORT_stat==1),]
    Q1_death_CVD<-Q1[which(Q1$CVD_MORT_stat==1),]
    Q1_death_Cancer<-Q1[which(Q1$Cancer_MORT_stat==1),]
    Q1_Perseon_year_ad<-sum(Q1$Pyear)
    Q1_Perseon<-sum(Q1$weight)
    Q1_Perseon_ad_All<-as.numeric(round(sum(Q1_death_All$weight)))
    Q1_Perseon_ad_CVD<-as.numeric(round(sum(Q1_death_CVD$weight)))
    Q1_Perseon_ad_Cancer<-as.numeric(round(sum(Q1_death_Cancer$weight)))
    #Q1_ALL
    Q1_All<-Q1_Perseon_ad_All*(1000/Q1_Perseon_year_ad)
    Q1_All_UCL<-(Q1_Perseon_ad_All+(1.96*sqrt(Q1_Perseon_ad_All)))*(1000/Q1_Perseon_year_ad)
    Q1_All_LCL<-(Q1_Perseon_ad_All-(1.96*sqrt(Q1_Perseon_ad_All)))*(1000/Q1_Perseon_year_ad)
    Q1_All_Incidence<-paste0(round(Q1_All,2)," (",round(Q1_All_LCL,2),"-",round(Q1_All_UCL,2),")")
    Q1_All_Incidence
    #Q1_CVD
    Q1_CVD<-Q1_Perseon_ad_CVD*(1000/Q1_Perseon_year_ad)
    Q1_CVD_UCL<-(Q1_Perseon_ad_CVD+(1.96*sqrt(Q1_Perseon_ad_CVD)))*(1000/Q1_Perseon_year_ad)
    Q1_CVD_LCL<-(Q1_Perseon_ad_CVD-(1.96*sqrt(Q1_Perseon_ad_CVD)))*(1000/Q1_Perseon_year_ad)
    Q1_CVD_Incidence<-paste0(round(Q1_CVD,2)," (",round(Q1_CVD_LCL,2),"-",round(Q1_CVD_UCL,2),")")
    Q1_CVD_Incidence
    #Q1_Cancer
    Q1_Cancer<-Q1_Perseon_ad_Cancer*(1000/Q1_Perseon_year_ad)
    Q1_Cancer_UCL<-(Q1_Perseon_ad_Cancer+(1.96*sqrt(Q1_Perseon_ad_Cancer)))*(1000/Q1_Perseon_year_ad)
    Q1_Cancer_LCL<-(Q1_Perseon_ad_Cancer-(1.96*sqrt(Q1_Perseon_ad_Cancer)))*(1000/Q1_Perseon_year_ad)
    Q1_Cancer_Incidence<-paste0(round(Q1_Cancer,2)," (",round(Q1_Cancer_LCL,2),"-",round(Q1_Cancer_UCL,2),")")
    Q1_Cancer_Incidence
  }
  {#** Q2 ####
    Q2<-Interpolation_weighted[which(Interpolation_weighted$TYG_quantile=="Quantile 2"),]
    Q2$year<-Q2$peryear
    Q2$Pyear<-Q2$year*Q2$weight
    Q2_death_All<-Q2[which(Q2$MORT_stat==1),]
    Q2_death_CVD<-Q2[which(Q2$CVD_MORT_stat==1),]
    Q2_death_Cancer<-Q2[which(Q2$Cancer_MORT_stat==1),]
    Q2_Perseon_year_ad<-sum(Q2$Pyear)
    Q2_Perseon<-sum(Q2$weight)
    Q2_Perseon_ad_All<-as.numeric(round(sum(Q2_death_All$weight)))
    Q2_Perseon_ad_CVD<-as.numeric(round(sum(Q2_death_CVD$weight)))
    Q2_Perseon_ad_Cancer<-as.numeric(round(sum(Q2_death_Cancer$weight)))
    #Q2_ALL
    Q2_All<-Q2_Perseon_ad_All*(1000/Q2_Perseon_year_ad)
    Q2_All_UCL<-(Q2_Perseon_ad_All+(1.96*sqrt(Q2_Perseon_ad_All)))*(1000/Q2_Perseon_year_ad)
    Q2_All_LCL<-(Q2_Perseon_ad_All-(1.96*sqrt(Q2_Perseon_ad_All)))*(1000/Q2_Perseon_year_ad)
    Q2_All_Incidence<-paste0(round(Q2_All,2)," (",round(Q2_All_LCL,2),"-",round(Q2_All_UCL,2),")")
    Q2_All_Incidence
    #Q2_CVD
    Q2_CVD<-Q2_Perseon_ad_CVD*(1000/Q2_Perseon_year_ad)
    Q2_CVD_UCL<-(Q2_Perseon_ad_CVD+(1.96*sqrt(Q2_Perseon_ad_CVD)))*(1000/Q2_Perseon_year_ad)
    Q2_CVD_LCL<-(Q2_Perseon_ad_CVD-(1.96*sqrt(Q2_Perseon_ad_CVD)))*(1000/Q2_Perseon_year_ad)
    Q2_CVD_Incidence<-paste0(round(Q2_CVD,2)," (",round(Q2_CVD_LCL,2),"-",round(Q2_CVD_UCL,2),")")
    Q2_CVD_Incidence
    #Q2_Cancer
    Q2_Cancer<-Q2_Perseon_ad_Cancer*(1000/Q2_Perseon_year_ad)
    Q2_Cancer_UCL<-(Q2_Perseon_ad_Cancer+(1.96*sqrt(Q2_Perseon_ad_Cancer)))*(1000/Q2_Perseon_year_ad)
    Q2_Cancer_LCL<-(Q2_Perseon_ad_Cancer-(1.96*sqrt(Q2_Perseon_ad_Cancer)))*(1000/Q2_Perseon_year_ad)
    Q2_Cancer_Incidence<-paste0(round(Q2_Cancer,2)," (",round(Q2_Cancer_LCL,2),"-",round(Q2_Cancer_UCL,2),")")
    Q2_Cancer_Incidence
  }
  {#** Q3 ####
    Q3<-Interpolation_weighted[which(Interpolation_weighted$TYG_quantile=="Quantile 3"),]
    Q3$year<-Q3$peryear
    Q3$Pyear<-Q3$year*Q3$weight
    Q3_death_All<-Q3[which(Q3$MORT_stat==1),]
    Q3_death_CVD<-Q3[which(Q3$CVD_MORT_stat==1),]
    Q3_death_Cancer<-Q3[which(Q3$Cancer_MORT_stat==1),]
    Q3_Perseon_year_ad<-sum(Q3$Pyear)
    Q3_Perseon<-sum(Q3$weight)
    Q3_Perseon_ad_All<-as.numeric(round(sum(Q3_death_All$weight)))
    Q3_Perseon_ad_CVD<-as.numeric(round(sum(Q3_death_CVD$weight)))
    Q3_Perseon_ad_Cancer<-as.numeric(round(sum(Q3_death_Cancer$weight)))
    #Q3_ALL
    Q3_All<-Q3_Perseon_ad_All*(1000/Q3_Perseon_year_ad)
    Q3_All_UCL<-(Q3_Perseon_ad_All+(1.96*sqrt(Q3_Perseon_ad_All)))*(1000/Q3_Perseon_year_ad)
    Q3_All_LCL<-(Q3_Perseon_ad_All-(1.96*sqrt(Q3_Perseon_ad_All)))*(1000/Q3_Perseon_year_ad)
    Q3_All_Incidence<-paste0(round(Q3_All,2)," (",round(Q3_All_LCL,2),"-",round(Q3_All_UCL,2),")")
    Q3_All_Incidence
    #Q3_CVD
    Q3_CVD<-Q3_Perseon_ad_CVD*(1000/Q3_Perseon_year_ad)
    Q3_CVD_UCL<-(Q3_Perseon_ad_CVD+(1.96*sqrt(Q3_Perseon_ad_CVD)))*(1000/Q3_Perseon_year_ad)
    Q3_CVD_LCL<-(Q3_Perseon_ad_CVD-(1.96*sqrt(Q3_Perseon_ad_CVD)))*(1000/Q3_Perseon_year_ad)
    Q3_CVD_Incidence<-paste0(round(Q3_CVD,2)," (",round(Q3_CVD_LCL,2),"-",round(Q3_CVD_UCL,2),")")
    Q3_CVD_Incidence
    #Q3_Cancer
    Q3_Cancer<-Q3_Perseon_ad_Cancer*(1000/Q3_Perseon_year_ad)
    Q3_Cancer_UCL<-(Q3_Perseon_ad_Cancer+(1.96*sqrt(Q3_Perseon_ad_Cancer)))*(1000/Q3_Perseon_year_ad)
    Q3_Cancer_LCL<-(Q3_Perseon_ad_Cancer-(1.96*sqrt(Q3_Perseon_ad_Cancer)))*(1000/Q3_Perseon_year_ad)
    Q3_Cancer_Incidence<-paste0(round(Q3_Cancer,2)," (",round(Q3_Cancer_LCL,2),"-",round(Q3_Cancer_UCL,2),")")
    Q3_Cancer_Incidence
  }
  {#** Q4 ####
    Q4<-Interpolation_weighted[which(Interpolation_weighted$TYG_quantile=="Quantile 4"),]
    Q4$year<-Q4$peryear
    Q4$Pyear<-Q4$year*Q4$weight
    Q4_death_All<-Q4[which(Q4$MORT_stat==1),]
    Q4_death_CVD<-Q4[which(Q4$CVD_MORT_stat==1),]
    Q4_death_Cancer<-Q4[which(Q4$Cancer_MORT_stat==1),]
    Q4_Perseon_year_ad<-sum(Q4$Pyear)
    Q4_Perseon<-sum(Q4$weight)
    Q4_Perseon_ad_All<-as.numeric(round(sum(Q4_death_All$weight)))
    Q4_Perseon_ad_CVD<-as.numeric(round(sum(Q4_death_CVD$weight)))
    Q4_Perseon_ad_Cancer<-as.numeric(round(sum(Q4_death_Cancer$weight)))
    #Q4_ALL
    Q4_All<-Q4_Perseon_ad_All*(1000/Q4_Perseon_year_ad)
    Q4_All_UCL<-(Q4_Perseon_ad_All+(1.96*sqrt(Q4_Perseon_ad_All)))*(1000/Q4_Perseon_year_ad)
    Q4_All_LCL<-(Q4_Perseon_ad_All-(1.96*sqrt(Q4_Perseon_ad_All)))*(1000/Q4_Perseon_year_ad)
    Q4_All_Incidence<-paste0(round(Q4_All,2)," (",round(Q4_All_LCL,2),"-",round(Q4_All_UCL,2),")")
    Q4_All_Incidence
    #Q4_CVD
    Q4_CVD<-Q4_Perseon_ad_CVD*(1000/Q4_Perseon_year_ad)
    Q4_CVD_UCL<-(Q4_Perseon_ad_CVD+(1.96*sqrt(Q4_Perseon_ad_CVD)))*(1000/Q4_Perseon_year_ad)
    Q4_CVD_LCL<-(Q4_Perseon_ad_CVD-(1.96*sqrt(Q4_Perseon_ad_CVD)))*(1000/Q4_Perseon_year_ad)
    Q4_CVD_Incidence<-paste0(round(Q4_CVD,2)," (",round(Q4_CVD_LCL,2),"-",round(Q4_CVD_UCL,2),")")
    Q4_CVD_Incidence
    #Q4_Cancer
    Q4_Cancer<-Q4_Perseon_ad_Cancer*(1000/Q4_Perseon_year_ad)
    Q4_Cancer_UCL<-(Q4_Perseon_ad_Cancer+(1.96*sqrt(Q4_Perseon_ad_Cancer)))*(1000/Q4_Perseon_year_ad)
    Q4_Cancer_LCL<-(Q4_Perseon_ad_Cancer-(1.96*sqrt(Q4_Perseon_ad_Cancer)))*(1000/Q4_Perseon_year_ad)
    Q4_Cancer_Incidence<-paste0(round(Q4_Cancer,2)," (",round(Q4_Cancer_LCL,2),"-",round(Q4_Cancer_UCL,2),")")
    Q4_Cancer_Incidence
  }
  TYG_Incidence<-rbind(cbind(Q1_All_Incidence,Q2_All_Incidence,Q3_All_Incidence,Q4_All_Incidence),
                       cbind(Q1_CVD_Incidence,Q2_CVD_Incidence,Q3_CVD_Incidence,Q4_CVD_Incidence),
                       cbind(Q1_Cancer_Incidence,Q2_Cancer_Incidence,Q3_Cancer_Incidence,Q4_Cancer_Incidence))
  {#** EVENTS ####
    #all.cause
    Q1.counts<-round(sum(Q1$weight))
    Q1.cause.counts<-round(sum(Q1_death_All$weight))
    Q2.counts<-round(sum(Q2$weight))
    Q2.cause.counts<-round(sum(Q2_death_All$weight))
    Q3.counts<-round(sum(Q3$weight))
    Q3.cause.counts<-round(sum(Q3_death_All$weight))
    Q4.counts<-round(sum(Q4$weight))
    Q4.cause.counts<-round(sum(Q4_death_All$weight))
    
    all.cause<-c(paste0(Q1.cause.counts,"/",Q1.counts),
                 paste0(Q2.cause.counts,"/",Q2.counts),
                 paste0(Q3.cause.counts,"/",Q3.counts),
                 paste0(Q4.cause.counts,"/",Q4.counts))
    all.cause
    
    #CVD.cause
    Q1.counts<-round(sum(Q1$weight))
    Q1.cause.counts<-round(sum(Q1_death_CVD$weight))
    Q2.counts<-round(sum(Q2$weight))
    Q2.cause.counts<-round(sum(Q2_death_CVD$weight))
    Q3.counts<-round(sum(Q3$weight))
    Q3.cause.counts<-round(sum(Q3_death_CVD$weight))
    Q4.counts<-round(sum(Q4$weight))
    Q4.cause.counts<-round(sum(Q4_death_CVD$weight))
    
    CVD.cause<-c(paste0(Q1.cause.counts,"/",Q1.counts),
                 paste0(Q2.cause.counts,"/",Q2.counts),
                 paste0(Q3.cause.counts,"/",Q3.counts),
                 paste0(Q4.cause.counts,"/",Q4.counts))
    CVD.cause
    #Cancer.cause
    Q1.counts<-round(sum(Q1$weight))
    Q1.cause.counts<-round(sum(Q1_death_Cancer$weight))
    Q2.counts<-round(sum(Q2$weight))
    Q2.cause.counts<-round(sum(Q2_death_Cancer$weight))
    Q3.counts<-round(sum(Q3$weight))
    Q3.cause.counts<-round(sum(Q3_death_Cancer$weight))
    Q4.counts<-round(sum(Q4$weight))
    Q4.cause.counts<-round(sum(Q4_death_Cancer$weight))
    
    Cancer.cause<-c(paste0(Q1.cause.counts,"/",Q1.counts),
                    paste0(Q2.cause.counts,"/",Q2.counts),
                    paste0(Q3.cause.counts,"/",Q3.counts),
                    paste0(Q4.cause.counts,"/",Q4.counts))
    Cancer.cause
  }
  TYG_counts<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause))
}
{#* TYg_WC ####
  {#** Q1 ####
    table(Interpolation_weighted$TyG_WC_quantile)
    Q1<-Interpolation_weighted[which(Interpolation_weighted$TyG_WC_quantile=="Quantile 1"),]
    Q1$year<-Q1$peryear
    Q1$Pyear<-Q1$year*Q1$weight
    Q1_death_All<-Q1[which(Q1$MORT_stat==1),]
    Q1_death_CVD<-Q1[which(Q1$CVD_MORT_stat==1),]
    Q1_death_Cancer<-Q1[which(Q1$Cancer_MORT_stat==1),]
    Q1_Perseon_year_ad<-sum(Q1$Pyear)
    Q1_Perseon<-sum(Q1$weight)
    Q1_Perseon_ad_All<-as.numeric(round(sum(Q1_death_All$weight)))
    Q1_Perseon_ad_CVD<-as.numeric(round(sum(Q1_death_CVD$weight)))
    Q1_Perseon_ad_Cancer<-as.numeric(round(sum(Q1_death_Cancer$weight)))
    #Q1_ALL
    Q1_All<-Q1_Perseon_ad_All*(1000/Q1_Perseon_year_ad)
    Q1_All_UCL<-(Q1_Perseon_ad_All+(1.96*sqrt(Q1_Perseon_ad_All)))*(1000/Q1_Perseon_year_ad)
    Q1_All_LCL<-(Q1_Perseon_ad_All-(1.96*sqrt(Q1_Perseon_ad_All)))*(1000/Q1_Perseon_year_ad)
    Q1_All_Incidence<-paste0(round(Q1_All,2)," (",round(Q1_All_LCL,2),"-",round(Q1_All_UCL,2),")")
    Q1_All_Incidence
    #Q1_CVD
    Q1_CVD<-Q1_Perseon_ad_CVD*(1000/Q1_Perseon_year_ad)
    Q1_CVD_UCL<-(Q1_Perseon_ad_CVD+(1.96*sqrt(Q1_Perseon_ad_CVD)))*(1000/Q1_Perseon_year_ad)
    Q1_CVD_LCL<-(Q1_Perseon_ad_CVD-(1.96*sqrt(Q1_Perseon_ad_CVD)))*(1000/Q1_Perseon_year_ad)
    Q1_CVD_Incidence<-paste0(round(Q1_CVD,2)," (",round(Q1_CVD_LCL,2),"-",round(Q1_CVD_UCL,2),")")
    Q1_CVD_Incidence
    #Q1_Cancer
    Q1_Cancer<-Q1_Perseon_ad_Cancer*(1000/Q1_Perseon_year_ad)
    Q1_Cancer_UCL<-(Q1_Perseon_ad_Cancer+(1.96*sqrt(Q1_Perseon_ad_Cancer)))*(1000/Q1_Perseon_year_ad)
    Q1_Cancer_LCL<-(Q1_Perseon_ad_Cancer-(1.96*sqrt(Q1_Perseon_ad_Cancer)))*(1000/Q1_Perseon_year_ad)
    Q1_Cancer_Incidence<-paste0(round(Q1_Cancer,2)," (",round(Q1_Cancer_LCL,2),"-",round(Q1_Cancer_UCL,2),")")
    Q1_Cancer_Incidence
  }
  {#** Q2 ####
    Q2<-Interpolation_weighted[which(Interpolation_weighted$TyG_WC_quantile=="Quantile 2"),]
    Q2$year<-Q2$peryear
    Q2$Pyear<-Q2$year*Q2$weight
    Q2_death_All<-Q2[which(Q2$MORT_stat==1),]
    Q2_death_CVD<-Q2[which(Q2$CVD_MORT_stat==1),]
    Q2_death_Cancer<-Q2[which(Q2$Cancer_MORT_stat==1),]
    Q2_Perseon_year_ad<-sum(Q2$Pyear)
    Q2_Perseon<-sum(Q2$weight)
    Q2_Perseon_ad_All<-as.numeric(round(sum(Q2_death_All$weight)))
    Q2_Perseon_ad_CVD<-as.numeric(round(sum(Q2_death_CVD$weight)))
    Q2_Perseon_ad_Cancer<-as.numeric(round(sum(Q2_death_Cancer$weight)))
    #Q2_ALL
    Q2_All<-Q2_Perseon_ad_All*(1000/Q2_Perseon_year_ad)
    Q2_All_UCL<-(Q2_Perseon_ad_All+(1.96*sqrt(Q2_Perseon_ad_All)))*(1000/Q2_Perseon_year_ad)
    Q2_All_LCL<-(Q2_Perseon_ad_All-(1.96*sqrt(Q2_Perseon_ad_All)))*(1000/Q2_Perseon_year_ad)
    Q2_All_Incidence<-paste0(round(Q2_All,2)," (",round(Q2_All_LCL,2),"-",round(Q2_All_UCL,2),")")
    Q2_All_Incidence
    #Q2_CVD
    Q2_CVD<-Q2_Perseon_ad_CVD*(1000/Q2_Perseon_year_ad)
    Q2_CVD_UCL<-(Q2_Perseon_ad_CVD+(1.96*sqrt(Q2_Perseon_ad_CVD)))*(1000/Q2_Perseon_year_ad)
    Q2_CVD_LCL<-(Q2_Perseon_ad_CVD-(1.96*sqrt(Q2_Perseon_ad_CVD)))*(1000/Q2_Perseon_year_ad)
    Q2_CVD_Incidence<-paste0(round(Q2_CVD,2)," (",round(Q2_CVD_LCL,2),"-",round(Q2_CVD_UCL,2),")")
    Q2_CVD_Incidence
    #Q2_Cancer
    Q2_Cancer<-Q2_Perseon_ad_Cancer*(1000/Q2_Perseon_year_ad)
    Q2_Cancer_UCL<-(Q2_Perseon_ad_Cancer+(1.96*sqrt(Q2_Perseon_ad_Cancer)))*(1000/Q2_Perseon_year_ad)
    Q2_Cancer_LCL<-(Q2_Perseon_ad_Cancer-(1.96*sqrt(Q2_Perseon_ad_Cancer)))*(1000/Q2_Perseon_year_ad)
    Q2_Cancer_Incidence<-paste0(round(Q2_Cancer,2)," (",round(Q2_Cancer_LCL,2),"-",round(Q2_Cancer_UCL,2),")")
    Q2_Cancer_Incidence
  }
  {#** Q3 ####
    Q3<-Interpolation_weighted[which(Interpolation_weighted$TyG_WC_quantile=="Quantile 3"),]
    Q3$year<-Q3$peryear
    Q3$Pyear<-Q3$year*Q3$weight
    Q3_death_All<-Q3[which(Q3$MORT_stat==1),]
    Q3_death_CVD<-Q3[which(Q3$CVD_MORT_stat==1),]
    Q3_death_Cancer<-Q3[which(Q3$Cancer_MORT_stat==1),]
    Q3_Perseon_year_ad<-sum(Q3$Pyear)
    Q3_Perseon<-sum(Q3$weight)
    Q3_Perseon_ad_All<-as.numeric(round(sum(Q3_death_All$weight)))
    Q3_Perseon_ad_CVD<-as.numeric(round(sum(Q3_death_CVD$weight)))
    Q3_Perseon_ad_Cancer<-as.numeric(round(sum(Q3_death_Cancer$weight)))
    #Q3_ALL
    Q3_All<-Q3_Perseon_ad_All*(1000/Q3_Perseon_year_ad)
    Q3_All_UCL<-(Q3_Perseon_ad_All+(1.96*sqrt(Q3_Perseon_ad_All)))*(1000/Q3_Perseon_year_ad)
    Q3_All_LCL<-(Q3_Perseon_ad_All-(1.96*sqrt(Q3_Perseon_ad_All)))*(1000/Q3_Perseon_year_ad)
    Q3_All_Incidence<-paste0(round(Q3_All,2)," (",round(Q3_All_LCL,2),"-",round(Q3_All_UCL,2),")")
    Q3_All_Incidence
    #Q3_CVD
    Q3_CVD<-Q3_Perseon_ad_CVD*(1000/Q3_Perseon_year_ad)
    Q3_CVD_UCL<-(Q3_Perseon_ad_CVD+(1.96*sqrt(Q3_Perseon_ad_CVD)))*(1000/Q3_Perseon_year_ad)
    Q3_CVD_LCL<-(Q3_Perseon_ad_CVD-(1.96*sqrt(Q3_Perseon_ad_CVD)))*(1000/Q3_Perseon_year_ad)
    Q3_CVD_Incidence<-paste0(round(Q3_CVD,2)," (",round(Q3_CVD_LCL,2),"-",round(Q3_CVD_UCL,2),")")
    Q3_CVD_Incidence
    #Q3_Cancer
    Q3_Cancer<-Q3_Perseon_ad_Cancer*(1000/Q3_Perseon_year_ad)
    Q3_Cancer_UCL<-(Q3_Perseon_ad_Cancer+(1.96*sqrt(Q3_Perseon_ad_Cancer)))*(1000/Q3_Perseon_year_ad)
    Q3_Cancer_LCL<-(Q3_Perseon_ad_Cancer-(1.96*sqrt(Q3_Perseon_ad_Cancer)))*(1000/Q3_Perseon_year_ad)
    Q3_Cancer_Incidence<-paste0(round(Q3_Cancer,2)," (",round(Q3_Cancer_LCL,2),"-",round(Q3_Cancer_UCL,2),")")
    Q3_Cancer_Incidence
  }
  {#** Q4 ####
    Q4<-Interpolation_weighted[which(Interpolation_weighted$TyG_WC_quantile=="Quantile 4"),]
    Q4$year<-Q4$peryear
    Q4$Pyear<-Q4$year*Q4$weight
    Q4_death_All<-Q4[which(Q4$MORT_stat==1),]
    Q4_death_CVD<-Q4[which(Q4$CVD_MORT_stat==1),]
    Q4_death_Cancer<-Q4[which(Q4$Cancer_MORT_stat==1),]
    Q4_Perseon_year_ad<-sum(Q4$Pyear)
    Q4_Perseon<-sum(Q4$weight)
    Q4_Perseon_ad_All<-as.numeric(round(sum(Q4_death_All$weight)))
    Q4_Perseon_ad_CVD<-as.numeric(round(sum(Q4_death_CVD$weight)))
    Q4_Perseon_ad_Cancer<-as.numeric(round(sum(Q4_death_Cancer$weight)))
    #Q4_ALL
    Q4_All<-Q4_Perseon_ad_All*(1000/Q4_Perseon_year_ad)
    Q4_All_UCL<-(Q4_Perseon_ad_All+(1.96*sqrt(Q4_Perseon_ad_All)))*(1000/Q4_Perseon_year_ad)
    Q4_All_LCL<-(Q4_Perseon_ad_All-(1.96*sqrt(Q4_Perseon_ad_All)))*(1000/Q4_Perseon_year_ad)
    Q4_All_Incidence<-paste0(round(Q4_All,2)," (",round(Q4_All_LCL,2),"-",round(Q4_All_UCL,2),")")
    Q4_All_Incidence
    #Q4_CVD
    Q4_CVD<-Q4_Perseon_ad_CVD*(1000/Q4_Perseon_year_ad)
    Q4_CVD_UCL<-(Q4_Perseon_ad_CVD+(1.96*sqrt(Q4_Perseon_ad_CVD)))*(1000/Q4_Perseon_year_ad)
    Q4_CVD_LCL<-(Q4_Perseon_ad_CVD-(1.96*sqrt(Q4_Perseon_ad_CVD)))*(1000/Q4_Perseon_year_ad)
    Q4_CVD_Incidence<-paste0(round(Q4_CVD,2)," (",round(Q4_CVD_LCL,2),"-",round(Q4_CVD_UCL,2),")")
    Q4_CVD_Incidence
    #Q4_Cancer
    Q4_Cancer<-Q4_Perseon_ad_Cancer*(1000/Q4_Perseon_year_ad)
    Q4_Cancer_UCL<-(Q4_Perseon_ad_Cancer+(1.96*sqrt(Q4_Perseon_ad_Cancer)))*(1000/Q4_Perseon_year_ad)
    Q4_Cancer_LCL<-(Q4_Perseon_ad_Cancer-(1.96*sqrt(Q4_Perseon_ad_Cancer)))*(1000/Q4_Perseon_year_ad)
    Q4_Cancer_Incidence<-paste0(round(Q4_Cancer,2)," (",round(Q4_Cancer_LCL,2),"-",round(Q4_Cancer_UCL,2),")")
    Q4_Cancer_Incidence
  }
  TyG_WC_Incidence<-rbind(cbind(Q1_All_Incidence,Q2_All_Incidence,Q3_All_Incidence,Q4_All_Incidence),
                          cbind(Q1_CVD_Incidence,Q2_CVD_Incidence,Q3_CVD_Incidence,Q4_CVD_Incidence),
                          cbind(Q1_Cancer_Incidence,Q2_Cancer_Incidence,Q3_Cancer_Incidence,Q4_Cancer_Incidence))
  {#** EVENTS ####
    #all.cause
    Q1.counts<-round(sum(Q1$weight))
    Q1.cause.counts<-round(sum(Q1_death_All$weight))
    Q2.counts<-round(sum(Q2$weight))
    Q2.cause.counts<-round(sum(Q2_death_All$weight))
    Q3.counts<-round(sum(Q3$weight))
    Q3.cause.counts<-round(sum(Q3_death_All$weight))
    Q4.counts<-round(sum(Q4$weight))
    Q4.cause.counts<-round(sum(Q4_death_All$weight))
    
    all.cause<-c(paste0(Q1.cause.counts,"/",Q1.counts),
                 paste0(Q2.cause.counts,"/",Q2.counts),
                 paste0(Q3.cause.counts,"/",Q3.counts),
                 paste0(Q4.cause.counts,"/",Q4.counts))
    all.cause
    
    #CVD.cause
    Q1.counts<-round(sum(Q1$weight))
    Q1.cause.counts<-round(sum(Q1_death_CVD$weight))
    Q2.counts<-round(sum(Q2$weight))
    Q2.cause.counts<-round(sum(Q2_death_CVD$weight))
    Q3.counts<-round(sum(Q3$weight))
    Q3.cause.counts<-round(sum(Q3_death_CVD$weight))
    Q4.counts<-round(sum(Q4$weight))
    Q4.cause.counts<-round(sum(Q4_death_CVD$weight))
    
    CVD.cause<-c(paste0(Q1.cause.counts,"/",Q1.counts),
                 paste0(Q2.cause.counts,"/",Q2.counts),
                 paste0(Q3.cause.counts,"/",Q3.counts),
                 paste0(Q4.cause.counts,"/",Q4.counts))
    CVD.cause
    #Cancer.cause
    Q1.counts<-round(sum(Q1$weight))
    Q1.cause.counts<-round(sum(Q1_death_Cancer$weight))
    Q2.counts<-round(sum(Q2$weight))
    Q2.cause.counts<-round(sum(Q2_death_Cancer$weight))
    Q3.counts<-round(sum(Q3$weight))
    Q3.cause.counts<-round(sum(Q3_death_Cancer$weight))
    Q4.counts<-round(sum(Q4$weight))
    Q4.cause.counts<-round(sum(Q4_death_Cancer$weight))
    
    Cancer.cause<-c(paste0(Q1.cause.counts,"/",Q1.counts),
                    paste0(Q2.cause.counts,"/",Q2.counts),
                    paste0(Q3.cause.counts,"/",Q3.counts),
                    paste0(Q4.cause.counts,"/",Q4.counts))
    Cancer.cause
  }
  TyG_WC_counts<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause))
}
{#* TYg_WHtR ####
  {#** Q1 ####
    table(Interpolation_weighted$TyG_WHtR_quantile)
    Q1<-Interpolation_weighted[which(Interpolation_weighted$TyG_WHtR_quantile=="Quantile 1"),]
    Q1$year<-Q1$peryear
    Q1$Pyear<-Q1$year*Q1$weight
    Q1_death_All<-Q1[which(Q1$MORT_stat==1),]
    Q1_death_CVD<-Q1[which(Q1$CVD_MORT_stat==1),]
    Q1_death_Cancer<-Q1[which(Q1$Cancer_MORT_stat==1),]
    Q1_Perseon_year_ad<-sum(Q1$Pyear)
    Q1_Perseon<-sum(Q1$weight)
    Q1_Perseon_ad_All<-as.numeric(round(sum(Q1_death_All$weight)))
    Q1_Perseon_ad_CVD<-as.numeric(round(sum(Q1_death_CVD$weight)))
    Q1_Perseon_ad_Cancer<-as.numeric(round(sum(Q1_death_Cancer$weight)))
    #Q1_ALL
    Q1_All<-Q1_Perseon_ad_All*(1000/Q1_Perseon_year_ad)
    Q1_All_UCL<-(Q1_Perseon_ad_All+(1.96*sqrt(Q1_Perseon_ad_All)))*(1000/Q1_Perseon_year_ad)
    Q1_All_LCL<-(Q1_Perseon_ad_All-(1.96*sqrt(Q1_Perseon_ad_All)))*(1000/Q1_Perseon_year_ad)
    Q1_All_Incidence<-paste0(round(Q1_All,2)," (",round(Q1_All_LCL,2),"-",round(Q1_All_UCL,2),")")
    Q1_All_Incidence
    #Q1_CVD
    Q1_CVD<-Q1_Perseon_ad_CVD*(1000/Q1_Perseon_year_ad)
    Q1_CVD_UCL<-(Q1_Perseon_ad_CVD+(1.96*sqrt(Q1_Perseon_ad_CVD)))*(1000/Q1_Perseon_year_ad)
    Q1_CVD_LCL<-(Q1_Perseon_ad_CVD-(1.96*sqrt(Q1_Perseon_ad_CVD)))*(1000/Q1_Perseon_year_ad)
    Q1_CVD_Incidence<-paste0(round(Q1_CVD,2)," (",round(Q1_CVD_LCL,2),"-",round(Q1_CVD_UCL,2),")")
    Q1_CVD_Incidence
    #Q1_Cancer
    Q1_Cancer<-Q1_Perseon_ad_Cancer*(1000/Q1_Perseon_year_ad)
    Q1_Cancer_UCL<-(Q1_Perseon_ad_Cancer+(1.96*sqrt(Q1_Perseon_ad_Cancer)))*(1000/Q1_Perseon_year_ad)
    Q1_Cancer_LCL<-(Q1_Perseon_ad_Cancer-(1.96*sqrt(Q1_Perseon_ad_Cancer)))*(1000/Q1_Perseon_year_ad)
    Q1_Cancer_Incidence<-paste0(round(Q1_Cancer,2)," (",round(Q1_Cancer_LCL,2),"-",round(Q1_Cancer_UCL,2),")")
    Q1_Cancer_Incidence
  }
  {#** Q2 ####
    Q2<-Interpolation_weighted[which(Interpolation_weighted$TyG_WHtR_quantile=="Quantile 2"),]
    Q2$year<-Q2$peryear
    Q2$Pyear<-Q2$year*Q2$weight
    Q2_death_All<-Q2[which(Q2$MORT_stat==1),]
    Q2_death_CVD<-Q2[which(Q2$CVD_MORT_stat==1),]
    Q2_death_Cancer<-Q2[which(Q2$Cancer_MORT_stat==1),]
    Q2_Perseon_year_ad<-sum(Q2$Pyear)
    Q2_Perseon<-sum(Q2$weight)
    Q2_Perseon_ad_All<-as.numeric(round(sum(Q2_death_All$weight)))
    Q2_Perseon_ad_CVD<-as.numeric(round(sum(Q2_death_CVD$weight)))
    Q2_Perseon_ad_Cancer<-as.numeric(round(sum(Q2_death_Cancer$weight)))
    #Q2_ALL
    Q2_All<-Q2_Perseon_ad_All*(1000/Q2_Perseon_year_ad)
    Q2_All_UCL<-(Q2_Perseon_ad_All+(1.96*sqrt(Q2_Perseon_ad_All)))*(1000/Q2_Perseon_year_ad)
    Q2_All_LCL<-(Q2_Perseon_ad_All-(1.96*sqrt(Q2_Perseon_ad_All)))*(1000/Q2_Perseon_year_ad)
    Q2_All_Incidence<-paste0(round(Q2_All,2)," (",round(Q2_All_LCL,2),"-",round(Q2_All_UCL,2),")")
    Q2_All_Incidence
    #Q2_CVD
    Q2_CVD<-Q2_Perseon_ad_CVD*(1000/Q2_Perseon_year_ad)
    Q2_CVD_UCL<-(Q2_Perseon_ad_CVD+(1.96*sqrt(Q2_Perseon_ad_CVD)))*(1000/Q2_Perseon_year_ad)
    Q2_CVD_LCL<-(Q2_Perseon_ad_CVD-(1.96*sqrt(Q2_Perseon_ad_CVD)))*(1000/Q2_Perseon_year_ad)
    Q2_CVD_Incidence<-paste0(round(Q2_CVD,2)," (",round(Q2_CVD_LCL,2),"-",round(Q2_CVD_UCL,2),")")
    Q2_CVD_Incidence
    #Q2_Cancer
    Q2_Cancer<-Q2_Perseon_ad_Cancer*(1000/Q2_Perseon_year_ad)
    Q2_Cancer_UCL<-(Q2_Perseon_ad_Cancer+(1.96*sqrt(Q2_Perseon_ad_Cancer)))*(1000/Q2_Perseon_year_ad)
    Q2_Cancer_LCL<-(Q2_Perseon_ad_Cancer-(1.96*sqrt(Q2_Perseon_ad_Cancer)))*(1000/Q2_Perseon_year_ad)
    Q2_Cancer_Incidence<-paste0(round(Q2_Cancer,2)," (",round(Q2_Cancer_LCL,2),"-",round(Q2_Cancer_UCL,2),")")
    Q2_Cancer_Incidence
  }
  {#** Q3 ####
    Q3<-Interpolation_weighted[which(Interpolation_weighted$TyG_WHtR_quantile=="Quantile 3"),]
    Q3$year<-Q3$peryear
    Q3$Pyear<-Q3$year*Q3$weight
    Q3_death_All<-Q3[which(Q3$MORT_stat==1),]
    Q3_death_CVD<-Q3[which(Q3$CVD_MORT_stat==1),]
    Q3_death_Cancer<-Q3[which(Q3$Cancer_MORT_stat==1),]
    Q3_Perseon_year_ad<-sum(Q3$Pyear)
    Q3_Perseon<-sum(Q3$weight)
    Q3_Perseon_ad_All<-as.numeric(round(sum(Q3_death_All$weight)))
    Q3_Perseon_ad_CVD<-as.numeric(round(sum(Q3_death_CVD$weight)))
    Q3_Perseon_ad_Cancer<-as.numeric(round(sum(Q3_death_Cancer$weight)))
    #Q3_ALL
    Q3_All<-Q3_Perseon_ad_All*(1000/Q3_Perseon_year_ad)
    Q3_All_UCL<-(Q3_Perseon_ad_All+(1.96*sqrt(Q3_Perseon_ad_All)))*(1000/Q3_Perseon_year_ad)
    Q3_All_LCL<-(Q3_Perseon_ad_All-(1.96*sqrt(Q3_Perseon_ad_All)))*(1000/Q3_Perseon_year_ad)
    Q3_All_Incidence<-paste0(round(Q3_All,2)," (",round(Q3_All_LCL,2),"-",round(Q3_All_UCL,2),")")
    Q3_All_Incidence
    #Q3_CVD
    Q3_CVD<-Q3_Perseon_ad_CVD*(1000/Q3_Perseon_year_ad)
    Q3_CVD_UCL<-(Q3_Perseon_ad_CVD+(1.96*sqrt(Q3_Perseon_ad_CVD)))*(1000/Q3_Perseon_year_ad)
    Q3_CVD_LCL<-(Q3_Perseon_ad_CVD-(1.96*sqrt(Q3_Perseon_ad_CVD)))*(1000/Q3_Perseon_year_ad)
    Q3_CVD_Incidence<-paste0(round(Q3_CVD,2)," (",round(Q3_CVD_LCL,2),"-",round(Q3_CVD_UCL,2),")")
    Q3_CVD_Incidence
    #Q3_Cancer
    Q3_Cancer<-Q3_Perseon_ad_Cancer*(1000/Q3_Perseon_year_ad)
    Q3_Cancer_UCL<-(Q3_Perseon_ad_Cancer+(1.96*sqrt(Q3_Perseon_ad_Cancer)))*(1000/Q3_Perseon_year_ad)
    Q3_Cancer_LCL<-(Q3_Perseon_ad_Cancer-(1.96*sqrt(Q3_Perseon_ad_Cancer)))*(1000/Q3_Perseon_year_ad)
    Q3_Cancer_Incidence<-paste0(round(Q3_Cancer,2)," (",round(Q3_Cancer_LCL,2),"-",round(Q3_Cancer_UCL,2),")")
    Q3_Cancer_Incidence
  }
  {#** Q4 ####
    Q4<-Interpolation_weighted[which(Interpolation_weighted$TyG_WHtR_quantile=="Quantile 4"),]
    Q4$year<-Q4$peryear
    Q4$Pyear<-Q4$year*Q4$weight
    Q4_death_All<-Q4[which(Q4$MORT_stat==1),]
    Q4_death_CVD<-Q4[which(Q4$CVD_MORT_stat==1),]
    Q4_death_Cancer<-Q4[which(Q4$Cancer_MORT_stat==1),]
    Q4_Perseon_year_ad<-sum(Q4$Pyear)
    Q4_Perseon<-sum(Q4$weight)
    Q4_Perseon_ad_All<-as.numeric(round(sum(Q4_death_All$weight)))
    Q4_Perseon_ad_CVD<-as.numeric(round(sum(Q4_death_CVD$weight)))
    Q4_Perseon_ad_Cancer<-as.numeric(round(sum(Q4_death_Cancer$weight)))
    #Q4_ALL
    Q4_All<-Q4_Perseon_ad_All*(1000/Q4_Perseon_year_ad)
    Q4_All_UCL<-(Q4_Perseon_ad_All+(1.96*sqrt(Q4_Perseon_ad_All)))*(1000/Q4_Perseon_year_ad)
    Q4_All_LCL<-(Q4_Perseon_ad_All-(1.96*sqrt(Q4_Perseon_ad_All)))*(1000/Q4_Perseon_year_ad)
    Q4_All_Incidence<-paste0(round(Q4_All,2)," (",round(Q4_All_LCL,2),"-",round(Q4_All_UCL,2),")")
    Q4_All_Incidence
    #Q4_CVD
    Q4_CVD<-Q4_Perseon_ad_CVD*(1000/Q4_Perseon_year_ad)
    Q4_CVD_UCL<-(Q4_Perseon_ad_CVD+(1.96*sqrt(Q4_Perseon_ad_CVD)))*(1000/Q4_Perseon_year_ad)
    Q4_CVD_LCL<-(Q4_Perseon_ad_CVD-(1.96*sqrt(Q4_Perseon_ad_CVD)))*(1000/Q4_Perseon_year_ad)
    Q4_CVD_Incidence<-paste0(round(Q4_CVD,2)," (",round(Q4_CVD_LCL,2),"-",round(Q4_CVD_UCL,2),")")
    Q4_CVD_Incidence
    #Q4_Cancer
    Q4_Cancer<-Q4_Perseon_ad_Cancer*(1000/Q4_Perseon_year_ad)
    Q4_Cancer_UCL<-(Q4_Perseon_ad_Cancer+(1.96*sqrt(Q4_Perseon_ad_Cancer)))*(1000/Q4_Perseon_year_ad)
    Q4_Cancer_LCL<-(Q4_Perseon_ad_Cancer-(1.96*sqrt(Q4_Perseon_ad_Cancer)))*(1000/Q4_Perseon_year_ad)
    Q4_Cancer_Incidence<-paste0(round(Q4_Cancer,2)," (",round(Q4_Cancer_LCL,2),"-",round(Q4_Cancer_UCL,2),")")
    Q4_Cancer_Incidence
  }
  TyG_WHtR_Incidence<-rbind(cbind(Q1_All_Incidence,Q2_All_Incidence,Q3_All_Incidence,Q4_All_Incidence),
                            cbind(Q1_CVD_Incidence,Q2_CVD_Incidence,Q3_CVD_Incidence,Q4_CVD_Incidence),
                            cbind(Q1_Cancer_Incidence,Q2_Cancer_Incidence,Q3_Cancer_Incidence,Q4_Cancer_Incidence))
  {#** EVENTS ####
    #all.cause
    Q1.counts<-round(sum(Q1$weight))
    Q1.cause.counts<-round(sum(Q1_death_All$weight))
    Q2.counts<-round(sum(Q2$weight))
    Q2.cause.counts<-round(sum(Q2_death_All$weight))
    Q3.counts<-round(sum(Q3$weight))
    Q3.cause.counts<-round(sum(Q3_death_All$weight))
    Q4.counts<-round(sum(Q4$weight))
    Q4.cause.counts<-round(sum(Q4_death_All$weight))
    
    all.cause<-c(paste0(Q1.cause.counts,"/",Q1.counts),
                 paste0(Q2.cause.counts,"/",Q2.counts),
                 paste0(Q3.cause.counts,"/",Q3.counts),
                 paste0(Q4.cause.counts,"/",Q4.counts))
    all.cause
    
    #CVD.cause
    Q1.counts<-round(sum(Q1$weight))
    Q1.cause.counts<-round(sum(Q1_death_CVD$weight))
    Q2.counts<-round(sum(Q2$weight))
    Q2.cause.counts<-round(sum(Q2_death_CVD$weight))
    Q3.counts<-round(sum(Q3$weight))
    Q3.cause.counts<-round(sum(Q3_death_CVD$weight))
    Q4.counts<-round(sum(Q4$weight))
    Q4.cause.counts<-round(sum(Q4_death_CVD$weight))
    
    CVD.cause<-c(paste0(Q1.cause.counts,"/",Q1.counts),
                 paste0(Q2.cause.counts,"/",Q2.counts),
                 paste0(Q3.cause.counts,"/",Q3.counts),
                 paste0(Q4.cause.counts,"/",Q4.counts))
    CVD.cause
    #Cancer.cause
    Q1.counts<-round(sum(Q1$weight))
    Q1.cause.counts<-round(sum(Q1_death_Cancer$weight))
    Q2.counts<-round(sum(Q2$weight))
    Q2.cause.counts<-round(sum(Q2_death_Cancer$weight))
    Q3.counts<-round(sum(Q3$weight))
    Q3.cause.counts<-round(sum(Q3_death_Cancer$weight))
    Q4.counts<-round(sum(Q4$weight))
    Q4.cause.counts<-round(sum(Q4_death_Cancer$weight))
    
    Cancer.cause<-c(paste0(Q1.cause.counts,"/",Q1.counts),
                    paste0(Q2.cause.counts,"/",Q2.counts),
                    paste0(Q3.cause.counts,"/",Q3.counts),
                    paste0(Q4.cause.counts,"/",Q4.counts))
    Cancer.cause
  }
  TyG_WHtR_counts<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause))
}
{#* TYg_BMI ####
  {#** Q1 ####
    table(Interpolation_weighted$TyG_BMI_quantile)
    Q1<-Interpolation_weighted[which(Interpolation_weighted$TyG_BMI_quantile=="Quantile 1"),]
    Q1$year<-Q1$peryear
    Q1$Pyear<-Q1$year*Q1$weight
    Q1_death_All<-Q1[which(Q1$MORT_stat==1),]
    Q1_death_CVD<-Q1[which(Q1$CVD_MORT_stat==1),]
    Q1_death_Cancer<-Q1[which(Q1$Cancer_MORT_stat==1),]
    Q1_Perseon_year_ad<-sum(Q1$Pyear)
    Q1_Perseon<-sum(Q1$weight)
    Q1_Perseon_ad_All<-as.numeric(round(sum(Q1_death_All$weight)))
    Q1_Perseon_ad_CVD<-as.numeric(round(sum(Q1_death_CVD$weight)))
    Q1_Perseon_ad_Cancer<-as.numeric(round(sum(Q1_death_Cancer$weight)))
    #Q1_ALL
    Q1_All<-Q1_Perseon_ad_All*(1000/Q1_Perseon_year_ad)
    Q1_All_UCL<-(Q1_Perseon_ad_All+(1.96*sqrt(Q1_Perseon_ad_All)))*(1000/Q1_Perseon_year_ad)
    Q1_All_LCL<-(Q1_Perseon_ad_All-(1.96*sqrt(Q1_Perseon_ad_All)))*(1000/Q1_Perseon_year_ad)
    Q1_All_Incidence<-paste0(round(Q1_All,2)," (",round(Q1_All_LCL,2),"-",round(Q1_All_UCL,2),")")
    Q1_All_Incidence
    #Q1_CVD
    Q1_CVD<-Q1_Perseon_ad_CVD*(1000/Q1_Perseon_year_ad)
    Q1_CVD_UCL<-(Q1_Perseon_ad_CVD+(1.96*sqrt(Q1_Perseon_ad_CVD)))*(1000/Q1_Perseon_year_ad)
    Q1_CVD_LCL<-(Q1_Perseon_ad_CVD-(1.96*sqrt(Q1_Perseon_ad_CVD)))*(1000/Q1_Perseon_year_ad)
    Q1_CVD_Incidence<-paste0(round(Q1_CVD,2)," (",round(Q1_CVD_LCL,2),"-",round(Q1_CVD_UCL,2),")")
    Q1_CVD_Incidence
    #Q1_Cancer
    Q1_Cancer<-Q1_Perseon_ad_Cancer*(1000/Q1_Perseon_year_ad)
    Q1_Cancer_UCL<-(Q1_Perseon_ad_Cancer+(1.96*sqrt(Q1_Perseon_ad_Cancer)))*(1000/Q1_Perseon_year_ad)
    Q1_Cancer_LCL<-(Q1_Perseon_ad_Cancer-(1.96*sqrt(Q1_Perseon_ad_Cancer)))*(1000/Q1_Perseon_year_ad)
    Q1_Cancer_Incidence<-paste0(round(Q1_Cancer,2)," (",round(Q1_Cancer_LCL,2),"-",round(Q1_Cancer_UCL,2),")")
    Q1_Cancer_Incidence
  }
  {#** Q2 ####
    Q2<-Interpolation_weighted[which(Interpolation_weighted$TyG_BMI_quantile=="Quantile 2"),]
    Q2$year<-Q2$peryear
    Q2$Pyear<-Q2$year*Q2$weight
    Q2_death_All<-Q2[which(Q2$MORT_stat==1),]
    Q2_death_CVD<-Q2[which(Q2$CVD_MORT_stat==1),]
    Q2_death_Cancer<-Q2[which(Q2$Cancer_MORT_stat==1),]
    Q2_Perseon_year_ad<-sum(Q2$Pyear)
    Q2_Perseon<-sum(Q2$weight)
    Q2_Perseon_ad_All<-as.numeric(round(sum(Q2_death_All$weight)))
    Q2_Perseon_ad_CVD<-as.numeric(round(sum(Q2_death_CVD$weight)))
    Q2_Perseon_ad_Cancer<-as.numeric(round(sum(Q2_death_Cancer$weight)))
    #Q2_ALL
    Q2_All<-Q2_Perseon_ad_All*(1000/Q2_Perseon_year_ad)
    Q2_All_UCL<-(Q2_Perseon_ad_All+(1.96*sqrt(Q2_Perseon_ad_All)))*(1000/Q2_Perseon_year_ad)
    Q2_All_LCL<-(Q2_Perseon_ad_All-(1.96*sqrt(Q2_Perseon_ad_All)))*(1000/Q2_Perseon_year_ad)
    Q2_All_Incidence<-paste0(round(Q2_All,2)," (",round(Q2_All_LCL,2),"-",round(Q2_All_UCL,2),")")
    Q2_All_Incidence
    #Q2_CVD
    Q2_CVD<-Q2_Perseon_ad_CVD*(1000/Q2_Perseon_year_ad)
    Q2_CVD_UCL<-(Q2_Perseon_ad_CVD+(1.96*sqrt(Q2_Perseon_ad_CVD)))*(1000/Q2_Perseon_year_ad)
    Q2_CVD_LCL<-(Q2_Perseon_ad_CVD-(1.96*sqrt(Q2_Perseon_ad_CVD)))*(1000/Q2_Perseon_year_ad)
    Q2_CVD_Incidence<-paste0(round(Q2_CVD,2)," (",round(Q2_CVD_LCL,2),"-",round(Q2_CVD_UCL,2),")")
    Q2_CVD_Incidence
    #Q2_Cancer
    Q2_Cancer<-Q2_Perseon_ad_Cancer*(1000/Q2_Perseon_year_ad)
    Q2_Cancer_UCL<-(Q2_Perseon_ad_Cancer+(1.96*sqrt(Q2_Perseon_ad_Cancer)))*(1000/Q2_Perseon_year_ad)
    Q2_Cancer_LCL<-(Q2_Perseon_ad_Cancer-(1.96*sqrt(Q2_Perseon_ad_Cancer)))*(1000/Q2_Perseon_year_ad)
    Q2_Cancer_Incidence<-paste0(round(Q2_Cancer,2)," (",round(Q2_Cancer_LCL,2),"-",round(Q2_Cancer_UCL,2),")")
    Q2_Cancer_Incidence
  }
  {#** Q3 ####
    Q3<-Interpolation_weighted[which(Interpolation_weighted$TyG_BMI_quantile=="Quantile 3"),]
    Q3$year<-Q3$peryear
    Q3$Pyear<-Q3$year*Q3$weight
    Q3_death_All<-Q3[which(Q3$MORT_stat==1),]
    Q3_death_CVD<-Q3[which(Q3$CVD_MORT_stat==1),]
    Q3_death_Cancer<-Q3[which(Q3$Cancer_MORT_stat==1),]
    Q3_Perseon_year_ad<-sum(Q3$Pyear)
    Q3_Perseon<-sum(Q3$weight)
    Q3_Perseon_ad_All<-as.numeric(round(sum(Q3_death_All$weight)))
    Q3_Perseon_ad_CVD<-as.numeric(round(sum(Q3_death_CVD$weight)))
    Q3_Perseon_ad_Cancer<-as.numeric(round(sum(Q3_death_Cancer$weight)))
    #Q3_ALL
    Q3_All<-Q3_Perseon_ad_All*(1000/Q3_Perseon_year_ad)
    Q3_All_UCL<-(Q3_Perseon_ad_All+(1.96*sqrt(Q3_Perseon_ad_All)))*(1000/Q3_Perseon_year_ad)
    Q3_All_LCL<-(Q3_Perseon_ad_All-(1.96*sqrt(Q3_Perseon_ad_All)))*(1000/Q3_Perseon_year_ad)
    Q3_All_Incidence<-paste0(round(Q3_All,2)," (",round(Q3_All_LCL,2),"-",round(Q3_All_UCL,2),")")
    Q3_All_Incidence
    #Q3_CVD
    Q3_CVD<-Q3_Perseon_ad_CVD*(1000/Q3_Perseon_year_ad)
    Q3_CVD_UCL<-(Q3_Perseon_ad_CVD+(1.96*sqrt(Q3_Perseon_ad_CVD)))*(1000/Q3_Perseon_year_ad)
    Q3_CVD_LCL<-(Q3_Perseon_ad_CVD-(1.96*sqrt(Q3_Perseon_ad_CVD)))*(1000/Q3_Perseon_year_ad)
    Q3_CVD_Incidence<-paste0(round(Q3_CVD,2)," (",round(Q3_CVD_LCL,2),"-",round(Q3_CVD_UCL,2),")")
    Q3_CVD_Incidence
    #Q3_Cancer
    Q3_Cancer<-Q3_Perseon_ad_Cancer*(1000/Q3_Perseon_year_ad)
    Q3_Cancer_UCL<-(Q3_Perseon_ad_Cancer+(1.96*sqrt(Q3_Perseon_ad_Cancer)))*(1000/Q3_Perseon_year_ad)
    Q3_Cancer_LCL<-(Q3_Perseon_ad_Cancer-(1.96*sqrt(Q3_Perseon_ad_Cancer)))*(1000/Q3_Perseon_year_ad)
    Q3_Cancer_Incidence<-paste0(round(Q3_Cancer,2)," (",round(Q3_Cancer_LCL,2),"-",round(Q3_Cancer_UCL,2),")")
    Q3_Cancer_Incidence
  }
  {#** Q4 ####
    Q4<-Interpolation_weighted[which(Interpolation_weighted$TyG_BMI_quantile=="Quantile 4"),]
    Q4$year<-Q4$peryear
    Q4$Pyear<-Q4$year*Q4$weight
    Q4_death_All<-Q4[which(Q4$MORT_stat==1),]
    Q4_death_CVD<-Q4[which(Q4$CVD_MORT_stat==1),]
    Q4_death_Cancer<-Q4[which(Q4$Cancer_MORT_stat==1),]
    Q4_Perseon_year_ad<-sum(Q4$Pyear)
    Q4_Perseon<-sum(Q4$weight)
    Q4_Perseon_ad_All<-as.numeric(round(sum(Q4_death_All$weight)))
    Q4_Perseon_ad_CVD<-as.numeric(round(sum(Q4_death_CVD$weight)))
    Q4_Perseon_ad_Cancer<-as.numeric(round(sum(Q4_death_Cancer$weight)))
    #Q4_ALL
    Q4_All<-Q4_Perseon_ad_All*(1000/Q4_Perseon_year_ad)
    Q4_All_UCL<-(Q4_Perseon_ad_All+(1.96*sqrt(Q4_Perseon_ad_All)))*(1000/Q4_Perseon_year_ad)
    Q4_All_LCL<-(Q4_Perseon_ad_All-(1.96*sqrt(Q4_Perseon_ad_All)))*(1000/Q4_Perseon_year_ad)
    Q4_All_Incidence<-paste0(round(Q4_All,2)," (",round(Q4_All_LCL,2),"-",round(Q4_All_UCL,2),")")
    Q4_All_Incidence
    #Q4_CVD
    Q4_CVD<-Q4_Perseon_ad_CVD*(1000/Q4_Perseon_year_ad)
    Q4_CVD_UCL<-(Q4_Perseon_ad_CVD+(1.96*sqrt(Q4_Perseon_ad_CVD)))*(1000/Q4_Perseon_year_ad)
    Q4_CVD_LCL<-(Q4_Perseon_ad_CVD-(1.96*sqrt(Q4_Perseon_ad_CVD)))*(1000/Q4_Perseon_year_ad)
    Q4_CVD_Incidence<-paste0(round(Q4_CVD,2)," (",round(Q4_CVD_LCL,2),"-",round(Q4_CVD_UCL,2),")")
    Q4_CVD_Incidence
    #Q4_Cancer
    Q4_Cancer<-Q4_Perseon_ad_Cancer*(1000/Q4_Perseon_year_ad)
    Q4_Cancer_UCL<-(Q4_Perseon_ad_Cancer+(1.96*sqrt(Q4_Perseon_ad_Cancer)))*(1000/Q4_Perseon_year_ad)
    Q4_Cancer_LCL<-(Q4_Perseon_ad_Cancer-(1.96*sqrt(Q4_Perseon_ad_Cancer)))*(1000/Q4_Perseon_year_ad)
    Q4_Cancer_Incidence<-paste0(round(Q4_Cancer,2)," (",round(Q4_Cancer_LCL,2),"-",round(Q4_Cancer_UCL,2),")")
    Q4_Cancer_Incidence
  }
  TyG_BMI_Incidence<-rbind(cbind(Q1_All_Incidence,Q2_All_Incidence,Q3_All_Incidence,Q4_All_Incidence),
                           cbind(Q1_CVD_Incidence,Q2_CVD_Incidence,Q3_CVD_Incidence,Q4_CVD_Incidence),
                           cbind(Q1_Cancer_Incidence,Q2_Cancer_Incidence,Q3_Cancer_Incidence,Q4_Cancer_Incidence))
  {#** EVENTS ####
    #all.cause
    Q1.counts<-round(sum(Q1$weight))
    Q1.cause.counts<-round(sum(Q1_death_All$weight))
    Q2.counts<-round(sum(Q2$weight))
    Q2.cause.counts<-round(sum(Q2_death_All$weight))
    Q3.counts<-round(sum(Q3$weight))
    Q3.cause.counts<-round(sum(Q3_death_All$weight))
    Q4.counts<-round(sum(Q4$weight))
    Q4.cause.counts<-round(sum(Q4_death_All$weight))
    
    all.cause<-c(paste0(Q1.cause.counts,"/",Q1.counts),
                 paste0(Q2.cause.counts,"/",Q2.counts),
                 paste0(Q3.cause.counts,"/",Q3.counts),
                 paste0(Q4.cause.counts,"/",Q4.counts))
    all.cause
    
    #CVD.cause
    Q1.counts<-round(sum(Q1$weight))
    Q1.cause.counts<-round(sum(Q1_death_CVD$weight))
    Q2.counts<-round(sum(Q2$weight))
    Q2.cause.counts<-round(sum(Q2_death_CVD$weight))
    Q3.counts<-round(sum(Q3$weight))
    Q3.cause.counts<-round(sum(Q3_death_CVD$weight))
    Q4.counts<-round(sum(Q4$weight))
    Q4.cause.counts<-round(sum(Q4_death_CVD$weight))
    
    CVD.cause<-c(paste0(Q1.cause.counts,"/",Q1.counts),
                 paste0(Q2.cause.counts,"/",Q2.counts),
                 paste0(Q3.cause.counts,"/",Q3.counts),
                 paste0(Q4.cause.counts,"/",Q4.counts))
    CVD.cause
    #Cancer.cause
    Q1.counts<-round(sum(Q1$weight))
    Q1.cause.counts<-round(sum(Q1_death_Cancer$weight))
    Q2.counts<-round(sum(Q2$weight))
    Q2.cause.counts<-round(sum(Q2_death_Cancer$weight))
    Q3.counts<-round(sum(Q3$weight))
    Q3.cause.counts<-round(sum(Q3_death_Cancer$weight))
    Q4.counts<-round(sum(Q4$weight))
    Q4.cause.counts<-round(sum(Q4_death_Cancer$weight))
    
    Cancer.cause<-c(paste0(Q1.cause.counts,"/",Q1.counts),
                    paste0(Q2.cause.counts,"/",Q2.counts),
                    paste0(Q3.cause.counts,"/",Q3.counts),
                    paste0(Q4.cause.counts,"/",Q4.counts))
    Cancer.cause
  }
  TyG_BMI_counts<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause))
}
{#* Table 2 ####
Table<-c("Outcome","Quantile 1","Quantile 2",
         "Quantile 3","Quantile 4")
#All-cause ####
Table<-rbind(Table,c("All-cause mortality","","","",""))
Table<-rbind(Table,c("TyG index","","","",""))
Table<-rbind(Table,c("Events, n/N (Weighted)",
                     TYG_counts[1,1],TYG_counts[1,2],
                     TYG_counts[1,3],TYG_counts[1,4])) 
Table<-rbind(Table,c("Incidence Rate (95% CI)",
                     TYG_Incidence[1,1],TYG_Incidence[1,2],
                     TYG_Incidence[1,3],TYG_Incidence[1,4])) 

Table<-rbind(Table,c("TyG-WC index","","","",""))
Table<-rbind(Table,c("Events, n/N (Weighted)",
                     TyG_WC_counts[1,1],TyG_WC_counts[1,2],
                     TyG_WC_counts[1,3],TyG_WC_counts[1,4])) 
Table<-rbind(Table,c("Incidence Rate (95% CI)",
                     TyG_WC_Incidence[1,1],TyG_WC_Incidence[1,2],
                     TyG_WC_Incidence[1,3],TyG_WC_Incidence[1,4])) 

Table<-rbind(Table,c("TyG-WHtR index","","","",""))
Table<-rbind(Table,c("Events, n/N (Weighted)",
                     TyG_WHtR_counts[1,1],TyG_WHtR_counts[1,2],
                     TyG_WHtR_counts[1,3],TyG_WHtR_counts[1,4])) 
Table<-rbind(Table,c("Incidence Rate (95% CI)",
                     TyG_WHtR_Incidence[1,1],TyG_WHtR_Incidence[1,2],
                     TyG_WHtR_Incidence[1,3],TyG_WHtR_Incidence[1,4])) 

Table<-rbind(Table,c("TyG-BMI index","","","",""))
Table<-rbind(Table,c("Events, n/N (Weighted)",
                     TyG_BMI_counts[1,1],TyG_BMI_counts[1,2],
                     TyG_BMI_counts[1,3],TyG_BMI_counts[1,4])) 
Table<-rbind(Table,c("Incidence Rate (95% CI)",
                     TyG_BMI_Incidence[1,1],TyG_BMI_Incidence[1,2],
                     TyG_BMI_Incidence[1,3],TyG_BMI_Incidence[1,4])) 
#CVD cause ####
Table<-rbind(Table,c("CVD-related mortality","","","",""))
Table<-rbind(Table,c("TyG index","","","",""))
Table<-rbind(Table,c("Events, n/N (Weighted)",
                     TYG_counts[2,1],TYG_counts[2,2],
                     TYG_counts[2,3],TYG_counts[2,4])) 
Table<-rbind(Table,c("Incidence Rate (95% CI)",
                     TYG_Incidence[2,1],TYG_Incidence[2,2],
                     TYG_Incidence[2,3],TYG_Incidence[2,4])) 

Table<-rbind(Table,c("TyG-WC index","","","",""))
Table<-rbind(Table,c("Events, n/N (Weighted)",
                     TyG_WC_counts[2,1],TyG_WC_counts[2,2],
                     TyG_WC_counts[2,3],TyG_WC_counts[2,4])) 
Table<-rbind(Table,c("Incidence Rate (95% CI)",
                     TyG_WC_Incidence[2,1],TyG_WC_Incidence[2,2],
                     TyG_WC_Incidence[2,3],TyG_WC_Incidence[2,4])) 

Table<-rbind(Table,c("TyG-WHtR index","","","",""))
Table<-rbind(Table,c("Events, n/N (Weighted)",
                     TyG_WHtR_counts[2,1],TyG_WHtR_counts[2,2],
                     TyG_WHtR_counts[2,3],TyG_WHtR_counts[2,4])) 
Table<-rbind(Table,c("Incidence Rate (95% CI)",
                     TyG_WHtR_Incidence[2,1],TyG_WHtR_Incidence[2,2],
                     TyG_WHtR_Incidence[2,3],TyG_WHtR_Incidence[2,4])) 

Table<-rbind(Table,c("TyG-BMI index","","","",""))
Table<-rbind(Table,c("Events, n/N (Weighted)",
                     TyG_BMI_counts[2,1],TyG_BMI_counts[2,2],
                     TyG_BMI_counts[2,3],TyG_BMI_counts[2,4])) 
Table<-rbind(Table,c("Incidence Rate (95% CI)",
                     TyG_BMI_Incidence[2,1],TyG_BMI_Incidence[2,2],
                     TyG_BMI_Incidence[2,3],TyG_BMI_Incidence[2,4])) 
#CVD cause ####
Table<-rbind(Table,c("Cancer-related mortality","","","",""))
Table<-rbind(Table,c("TyG index","","","",""))
Table<-rbind(Table,c("Events, n/N (Weighted)",
                     TYG_counts[3,1],TYG_counts[3,2],
                     TYG_counts[3,3],TYG_counts[3,4])) 
Table<-rbind(Table,c("Incidence Rate (95% CI)",
                     TYG_Incidence[3,1],TYG_Incidence[3,2],
                     TYG_Incidence[3,3],TYG_Incidence[3,4])) 

Table<-rbind(Table,c("TyG-WC index","","","",""))
Table<-rbind(Table,c("Events, n/N (Weighted)",
                     TyG_WC_counts[3,1],TyG_WC_counts[3,2],
                     TyG_WC_counts[3,3],TyG_WC_counts[3,4])) 
Table<-rbind(Table,c("Incidence Rate (95% CI)",
                     TyG_WC_Incidence[3,1],TyG_WC_Incidence[3,2],
                     TyG_WC_Incidence[3,3],TyG_WC_Incidence[3,4])) 

Table<-rbind(Table,c("TyG-WHtR index","","","",""))
Table<-rbind(Table,c("Events, n/N (Weighted)",
                     TyG_WHtR_counts[3,1],TyG_WHtR_counts[3,2],
                     TyG_WHtR_counts[3,3],TyG_WHtR_counts[3,4])) 
Table<-rbind(Table,c("Incidence Rate (95% CI)",
                     TyG_WHtR_Incidence[3,1],TyG_WHtR_Incidence[3,2],
                     TyG_WHtR_Incidence[3,3],TyG_WHtR_Incidence[3,4])) 

Table<-rbind(Table,c("TyG-BMI index","","","",""))
Table<-rbind(Table,c("Events, n/N (Weighted)",
                     TyG_BMI_counts[3,1],TyG_BMI_counts[3,2],
                     TyG_BMI_counts[3,3],TyG_BMI_counts[3,4])) 
Table<-rbind(Table,c("Incidence Rate (95% CI)",
                     TyG_BMI_Incidence[3,1],TyG_BMI_Incidence[3,2],
                     TyG_BMI_Incidence[3,3],TyG_BMI_Incidence[3,4])) 


Table
write.table(Table,sep = ",",file ="I:/NHANES study/PD&TYG&MO/Result/Table 2.csv",row.names =F,col.names =F )
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 19 TYG (Table S4) ####  
load(file="I:/NHANES study/PD&TYG&MO/Data/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)

{ #* all model #####
  #all model1
  model1<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     TYG, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model2
  model2<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     TYG+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result<-rbind(result,result2)
  #all model3
  model3<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TYG+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_result<-summary(model3)
  
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result<-rbind(result,result3)
  result
  #all model4
  model4<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TYG+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort
                       , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result_All<-rbind(result,result4)
  result_All
}

{ #* CVD model #####
  
  #CVD Crude
  model1<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TYG, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                           'P value' =P,'model'="model1",'status'="CVD cause")
  result
  #CVD model1
  model2<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TYG+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model2",'status'="CVD cause")
  result<-rbind(result,result2)
  result
  #CVD model2
  model3<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TYG+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_result<-summary(model3)
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model3",'status'="CVD cause")
  result<-rbind(result,result3)
  result
  #CVD model3
  model4<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TYG+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort
                       , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result_CVD<-rbind(result,result4)
  result_CVD
  
}

{ #* Cancer model #####
  
  #Cancer model1
  model1<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            TYG, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                              'P value' =P,'model'="model1",'status'="Cancer cause")
  result
  #Cancer model2
  model2<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            TYG+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model2",'status'="Cancer cause")
  result<-rbind(result,result2)
  result
  #Cancer model3
  model3<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            TYG+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                            Smoking_status+Drinking_status+BMI_status+Physical_status,design =rhcSvy)
  model3_result<-summary(model3)
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                               'P value' =P,'model'="model3",'status'="Cancer cause")
  result<-rbind(result,result3)
  result
  #Cancer model4
  model4<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     TYG+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result_Cancer<-rbind(result,result4)
  result_Cancer
}



{ #* Combine #####
  #Results
  result.cause<-rbind(result_All,result_CVD,result_Cancer)
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  round_3_function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
  
  result.cause[result.cause=="1000"]<-"1.00"
  result.cause[result.cause=="2000"]<-"2.00"
  result.cause[result.cause=="00000"]<-"<0.001"
  Table_S4<-as.data.frame(result.cause)
  save(Table_S4,file = "I:/NHANES study/PD&TYG&MO/Result/Table S4.Rdata")
  Table_S4 = data.frame(lapply(Table_S4, as.character), stringsAsFactors=FALSE)
  #Cbind
  Table<-c("","TYG","")
  Table<-rbind(Table,c("", "Per 1-index","P"))
  Table<-rbind(Table,c("All cause*","",""))
  Table<-rbind(Table,c("Model 1†",paste0(result.cause[1,1]," (",result.cause[1,2],", ",result.cause[1,3],")"),result.cause[1,4]))
  Table<-rbind(Table,c("Model 2‡",paste0(result.cause[2,1]," (",result.cause[2,2],", ",result.cause[2,3],")"),result.cause[2,4]))
  Table<-rbind(Table,c("Model 3§",paste0(result.cause[3,1]," (",result.cause[3,2],", ",result.cause[3,3],")"),result.cause[3,4]))
  Table<-rbind(Table,c("Model 4¶",paste0(result.cause[4,1]," (",result.cause[4,2],", ",result.cause[4,3],")"),result.cause[4,4]))
  Table<-rbind(Table,c("CVD cause","",""))
  Table<-rbind(Table,c("Model 1",paste0(result.cause[5,1]," (",result.cause[5,2],", ",result.cause[5,3],")"),result.cause[5,4]))
  Table<-rbind(Table,c("Model 2",paste0(result.cause[6,1]," (",result.cause[6,2],", ",result.cause[6,3],")"),result.cause[6,4]))
  Table<-rbind(Table,c("Model 3",paste0(result.cause[7,1]," (",result.cause[7,2],", ",result.cause[7,3],")"),result.cause[7,4]))
  Table<-rbind(Table,c("Model 4",paste0(result.cause[8,1]," (",result.cause[8,2],", ",result.cause[8,3],")"),result.cause[8,4]))
  Table<-rbind(Table,c("Cancer cause","",""))
  Table<-rbind(Table,c("Model 1",paste0(result.cause[9,1]," (",result.cause[9,2],", ",result.cause[9,3],")"),result.cause[9,4]))
  Table<-rbind(Table,c("Model 2",paste0(result.cause[10,1]," (",result.cause[10,2],", ",result.cause[10,3],")"),result.cause[10,4]))
  Table<-rbind(Table,c("Model 3",paste0(result.cause[11,1]," (",result.cause[11,2],", ",result.cause[11,3],")"),result.cause[11,4]))
  Table<-rbind(Table,c("Model 4",paste0(result.cause[12,1]," (",result.cause[12,2],", ",result.cause[12,3],")"),result.cause[12,4]))
  Table<-as.data.frame(Table)
  Table = data.frame(lapply(Table, as.character), stringsAsFactors=FALSE)
  Table
  write.table(Table,sep = ",",file ="I:/NHANES study/PD&TYG&MO/Result/Supplementary Table 4.csv",row.names =F,col.names =F )
}
Table
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 20 TYG WC (Table S5) ####  
load(file="I:/NHANES study/PD&TYG&MO/Data/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
Interpolation_weighted$TyG_WC<-Interpolation_weighted$TyG_WC/100
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)

{ #* all model #####
  #all model1
  model1<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     TyG_WC, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model2
  model2<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     TyG_WC+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result<-rbind(result,result2)
  #all model3
  model3<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     TyG_WC+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_result<-summary(model3)
  
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result<-rbind(result,result3)
  result
  #all model4
  model4<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     TyG_WC+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result_All<-rbind(result,result4)
  result_All
}

{ #* CVD model #####
  
  #CVD Crude
  model1<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     TyG_WC, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="CVD cause")
  result
  #CVD model1
  model2<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     TyG_WC+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="CVD cause")
  result<-rbind(result,result2)
  result
  #CVD model2
  model3<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     TyG_WC+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_result<-summary(model3)
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="CVD cause")
  result<-rbind(result,result3)
  result
  #CVD model3
  model4<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     TyG_WC+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result_CVD<-rbind(result,result4)
  result_CVD
  
}

{ #* Cancer model #####
  
  #Cancer model1
  model1<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     TyG_WC, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="Cancer cause")
  result
  #Cancer model2
  model2<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     TyG_WC+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="Cancer cause")
  result<-rbind(result,result2)
  result
  #Cancer model3
  model3<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     TyG_WC+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status,design =rhcSvy)
  model3_result<-summary(model3)
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="Cancer cause")
  result<-rbind(result,result3)
  result
  #Cancer model4
  model4<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     TyG_WC+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result_Cancer<-rbind(result,result4)
  result_Cancer
}



{ #* Combine #####
  #Results
  result.cause<-rbind(result_All,result_CVD,result_Cancer)
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  round_3_function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
  
  result.cause[result.cause=="1000"]<-"1.00"
  result.cause[result.cause=="2000"]<-"2.00"
  result.cause[result.cause=="00000"]<-"<0.001"
  Table_S4<-as.data.frame(result.cause)
  save(Table_S4,file = "I:/NHANES study/PD&TYG&MO/Result/Table S4.Rdata")
  Table_S4 = data.frame(lapply(Table_S4, as.character), stringsAsFactors=FALSE)
  #Cbind
  Table<-c("","TyG-WC","")
  Table<-rbind(Table,c("", "Per 1-index","P"))
  Table<-rbind(Table,c("All cause*","",""))
  Table<-rbind(Table,c("Model 1†",paste0(result.cause[1,1]," (",result.cause[1,2],", ",result.cause[1,3],")"),result.cause[1,4]))
  Table<-rbind(Table,c("Model 2‡",paste0(result.cause[2,1]," (",result.cause[2,2],", ",result.cause[2,3],")"),result.cause[2,4]))
  Table<-rbind(Table,c("Model 3§",paste0(result.cause[3,1]," (",result.cause[3,2],", ",result.cause[3,3],")"),result.cause[3,4]))
  Table<-rbind(Table,c("Model 4¶",paste0(result.cause[4,1]," (",result.cause[4,2],", ",result.cause[4,3],")"),result.cause[4,4]))
  Table<-rbind(Table,c("CVD cause","",""))
  Table<-rbind(Table,c("Model 1",paste0(result.cause[5,1]," (",result.cause[5,2],", ",result.cause[5,3],")"),result.cause[5,4]))
  Table<-rbind(Table,c("Model 2",paste0(result.cause[6,1]," (",result.cause[6,2],", ",result.cause[6,3],")"),result.cause[6,4]))
  Table<-rbind(Table,c("Model 3",paste0(result.cause[7,1]," (",result.cause[7,2],", ",result.cause[7,3],")"),result.cause[7,4]))
  Table<-rbind(Table,c("Model 4",paste0(result.cause[8,1]," (",result.cause[8,2],", ",result.cause[8,3],")"),result.cause[8,4]))
  Table<-rbind(Table,c("Cancer cause","",""))
  Table<-rbind(Table,c("Model 1",paste0(result.cause[9,1]," (",result.cause[9,2],", ",result.cause[9,3],")"),result.cause[9,4]))
  Table<-rbind(Table,c("Model 2",paste0(result.cause[10,1]," (",result.cause[10,2],", ",result.cause[10,3],")"),result.cause[10,4]))
  Table<-rbind(Table,c("Model 3",paste0(result.cause[11,1]," (",result.cause[11,2],", ",result.cause[11,3],")"),result.cause[11,4]))
  Table<-rbind(Table,c("Model 4",paste0(result.cause[12,1]," (",result.cause[12,2],", ",result.cause[12,3],")"),result.cause[12,4]))
  Table<-as.data.frame(Table)
  Table = data.frame(lapply(Table, as.character), stringsAsFactors=FALSE)
  Table
  write.table(Table,sep = ",",file ="I:/NHANES study/PD&TYG&MO/Result/Supplementary Table 5.csv",row.names =F,col.names =F )
}


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 21 TYG WHtR (Table S6) ####  
load(file="I:/NHANES study/PD&TYG&MO/Data/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)

{ #* all model #####
  #all model1
  model1<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     TyG_WHtR, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model2
  model2<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     TyG_WHtR+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result<-rbind(result,result2)
  #all model3
  model3<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     TyG_WHtR+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_result<-summary(model3)
  
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result<-rbind(result,result3)
  result
  #all model4
  model4<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     TyG_WHtR+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result_All<-rbind(result,result4)
  result_All
}

{ #* CVD model #####
  
  #CVD Crude
  model1<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     TyG_WHtR, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="CVD cause")
  result
  #CVD model1
  model2<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     TyG_WHtR+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="CVD cause")
  result<-rbind(result,result2)
  result
  #CVD model2
  model3<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     TyG_WHtR+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_result<-summary(model3)
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="CVD cause")
  result<-rbind(result,result3)
  result
  #CVD model3
  model4<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     TyG_WHtR+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result_CVD<-rbind(result,result4)
  result_CVD
  
}

{ #* Cancer model #####
  
  #Cancer model1
  model1<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     TyG_WHtR, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="Cancer cause")
  result
  #Cancer model2
  model2<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     TyG_WHtR+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="Cancer cause")
  result<-rbind(result,result2)
  result
  #Cancer model3
  model3<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     TyG_WHtR+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status,design =rhcSvy)
  model3_result<-summary(model3)
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="Cancer cause")
  result<-rbind(result,result3)
  result
  #Cancer model4
  model4<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     TyG_WHtR+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result_Cancer<-rbind(result,result4)
  result_Cancer
}



{ #* Combine #####
  #Results
  result.cause<-rbind(result_All,result_CVD,result_Cancer)
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  round_3_function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
  
  result.cause[result.cause=="1000"]<-"1.00"
  result.cause[result.cause=="2000"]<-"2.00"
  result.cause[result.cause=="00000"]<-"<0.001"
  Table_S4<-as.data.frame(result.cause)
  save(Table_S4,file = "I:/NHANES study/PD&TYG&MO/Result/Table S4.Rdata")
  Table_S4 = data.frame(lapply(Table_S4, as.character), stringsAsFactors=FALSE)
  #Cbind
  Table<-c("","TyG-WHtR","")
  Table<-rbind(Table,c("", "Per 1-SD increment","P"))
  Table<-rbind(Table,c("All cause*","",""))
  Table<-rbind(Table,c("Model 1†",paste0(result.cause[1,1]," (",result.cause[1,2],", ",result.cause[1,3],")"),result.cause[1,4]))
  Table<-rbind(Table,c("Model 2‡",paste0(result.cause[2,1]," (",result.cause[2,2],", ",result.cause[2,3],")"),result.cause[2,4]))
  Table<-rbind(Table,c("Model 3§",paste0(result.cause[3,1]," (",result.cause[3,2],", ",result.cause[3,3],")"),result.cause[3,4]))
  Table<-rbind(Table,c("Model 4¶",paste0(result.cause[4,1]," (",result.cause[4,2],", ",result.cause[4,3],")"),result.cause[4,4]))
  Table<-rbind(Table,c("CVD cause","",""))
  Table<-rbind(Table,c("Model 1",paste0(result.cause[5,1]," (",result.cause[5,2],", ",result.cause[5,3],")"),result.cause[5,4]))
  Table<-rbind(Table,c("Model 2",paste0(result.cause[6,1]," (",result.cause[6,2],", ",result.cause[6,3],")"),result.cause[6,4]))
  Table<-rbind(Table,c("Model 3",paste0(result.cause[7,1]," (",result.cause[7,2],", ",result.cause[7,3],")"),result.cause[7,4]))
  Table<-rbind(Table,c("Model 4",paste0(result.cause[8,1]," (",result.cause[8,2],", ",result.cause[8,3],")"),result.cause[8,4]))
  Table<-rbind(Table,c("Cancer cause","",""))
  Table<-rbind(Table,c("Model 1",paste0(result.cause[9,1]," (",result.cause[9,2],", ",result.cause[9,3],")"),result.cause[9,4]))
  Table<-rbind(Table,c("Model 2",paste0(result.cause[10,1]," (",result.cause[10,2],", ",result.cause[10,3],")"),result.cause[10,4]))
  Table<-rbind(Table,c("Model 3",paste0(result.cause[11,1]," (",result.cause[11,2],", ",result.cause[11,3],")"),result.cause[11,4]))
  Table<-rbind(Table,c("Model 4",paste0(result.cause[12,1]," (",result.cause[12,2],", ",result.cause[12,3],")"),result.cause[12,4]))
  Table<-as.data.frame(Table)
  Table = data.frame(lapply(Table, as.character), stringsAsFactors=FALSE)
  Table
  write.table(Table,sep = ",",file ="I:/NHANES study/PD&TYG&MO/Result/Supplementary Table 6.csv",row.names =F,col.names =F )
}



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 22 TYG BMI (Table S7) ####  
load(file="I:/NHANES study/PD&TYG&MO/Data/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
Interpolation_weighted$TyG_BMI<-Interpolation_weighted$TyG_BMI/100
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)

{ #* all model #####
  #all model1
  model1<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     TyG_BMI, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model2
  model2<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     TyG_BMI+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result<-rbind(result,result2)
  #all model3
  model3<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     TyG_BMI+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_result<-summary(model3)
  
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result<-rbind(result,result3)
  result
  #all model4
  model4<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     TyG_BMI+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result_All<-rbind(result,result4)
  result_All
}

{ #* CVD model #####
  
  #CVD Crude
  model1<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     TyG_BMI, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="CVD cause")
  result
  #CVD model1
  model2<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     TyG_BMI+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="CVD cause")
  result<-rbind(result,result2)
  result
  #CVD model2
  model3<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     TyG_BMI+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_result<-summary(model3)
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="CVD cause")
  result<-rbind(result,result3)
  result
  #CVD model3
  model4<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     TyG_BMI+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result_CVD<-rbind(result,result4)
  result_CVD
  
}

{ #* Cancer model #####
  
  #Cancer model1
  model1<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     TyG_BMI, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="Cancer cause")
  result
  #Cancer model2
  model2<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     TyG_BMI+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="Cancer cause")
  result<-rbind(result,result2)
  result
  #Cancer model3
  model3<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     TyG_BMI+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status,design =rhcSvy)
  model3_result<-summary(model3)
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="Cancer cause")
  result<-rbind(result,result3)
  result
  #Cancer model4
  model4<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     TyG_BMI+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result_Cancer<-rbind(result,result4)
  result_Cancer
}



{ #* Combine #####
  #Results
  result.cause<-rbind(result_All,result_CVD,result_Cancer)
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  round_3_function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
  
  result.cause[result.cause=="1000"]<-"1.00"
  result.cause[result.cause=="2000"]<-"2.00"
  result.cause[result.cause=="00000"]<-"<0.001"
  Table_S4<-as.data.frame(result.cause)
  save(Table_S4,file = "I:/NHANES study/PD&TYG&MO/Result/Table S4.Rdata")
  Table_S4 = data.frame(lapply(Table_S4, as.character), stringsAsFactors=FALSE)
  #Cbind
  Table<-c("","TyG-BMI","")
  Table<-rbind(Table,c("", "Per 100-index","P"))
  Table<-rbind(Table,c("All cause*","",""))
  Table<-rbind(Table,c("Model 1†",paste0(result.cause[1,1]," (",result.cause[1,2],", ",result.cause[1,3],")"),result.cause[1,4]))
  Table<-rbind(Table,c("Model 2‡",paste0(result.cause[2,1]," (",result.cause[2,2],", ",result.cause[2,3],")"),result.cause[2,4]))
  Table<-rbind(Table,c("Model 3§",paste0(result.cause[3,1]," (",result.cause[3,2],", ",result.cause[3,3],")"),result.cause[3,4]))
  Table<-rbind(Table,c("Model 4¶",paste0(result.cause[4,1]," (",result.cause[4,2],", ",result.cause[4,3],")"),result.cause[4,4]))
  Table<-rbind(Table,c("CVD cause","",""))
  Table<-rbind(Table,c("Model 1",paste0(result.cause[5,1]," (",result.cause[5,2],", ",result.cause[5,3],")"),result.cause[5,4]))
  Table<-rbind(Table,c("Model 2",paste0(result.cause[6,1]," (",result.cause[6,2],", ",result.cause[6,3],")"),result.cause[6,4]))
  Table<-rbind(Table,c("Model 3",paste0(result.cause[7,1]," (",result.cause[7,2],", ",result.cause[7,3],")"),result.cause[7,4]))
  Table<-rbind(Table,c("Model 4",paste0(result.cause[8,1]," (",result.cause[8,2],", ",result.cause[8,3],")"),result.cause[8,4]))
  Table<-rbind(Table,c("Cancer cause","",""))
  Table<-rbind(Table,c("Model 1",paste0(result.cause[9,1]," (",result.cause[9,2],", ",result.cause[9,3],")"),result.cause[9,4]))
  Table<-rbind(Table,c("Model 2",paste0(result.cause[10,1]," (",result.cause[10,2],", ",result.cause[10,3],")"),result.cause[10,4]))
  Table<-rbind(Table,c("Model 3",paste0(result.cause[11,1]," (",result.cause[11,2],", ",result.cause[11,3],")"),result.cause[11,4]))
  Table<-rbind(Table,c("Model 4",paste0(result.cause[12,1]," (",result.cause[12,2],", ",result.cause[12,3],")"),result.cause[12,4]))
  Table<-as.data.frame(Table)
  Table = data.frame(lapply(Table, as.character), stringsAsFactors=FALSE)
  Table
  write.table(Table,sep = ",",file ="I:/NHANES study/PD&TYG&MO/Result/Supplementary Table 7.csv",row.names =F,col.names =F )
}
Table
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 23 TYG quantile (Table S8) ####    

load(file="I:/NHANES study/PD&TYG&MO/Data/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
table(Interpolation_weighted$TYG_quantile)
Interpolation_weighted$TYG_quantile<-as.character(Interpolation_weighted$TYG_quantile)
Interpolation_weighted$TYG_qua[Interpolation_weighted$TYG_quantile=="Quantile 1"]<-0
Interpolation_weighted$TYG_qua[Interpolation_weighted$TYG_quantile=="Quantile 2"]<-1
Interpolation_weighted$TYG_qua[Interpolation_weighted$TYG_quantile=="Quantile 3"]<-2
Interpolation_weighted$TYG_qua[Interpolation_weighted$TYG_quantile=="Quantile 4"]<-3

table(Interpolation_weighted$TYG_qua)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
{ #* all model #####
  #all model1
  model1_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TYG_quantile, design =rhcSvy)
  model1_all_result<-summary(model1_all)
  model1_all_result
  P<-model1_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model1_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model2
  model2_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TYG_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_all_result<-summary(model2_all)
  P<-model2_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model2_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result.all<-rbind(result,result2)
  #all model3
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TYG_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model3_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  result.all
  #all model4
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TYG_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-rbind(result.all,result4)
  result.all
}

{ #* CVD model #####
  #CVD Crude
  model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TYG_quantile, design =rhcSvy)
  model1_CVD_result<-summary(model1_CVD)
  model1_CVD_result
  P<-model1_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model1_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                       'P value' =P,'model'="model1",'status'="CVD cause")
  result
  #CVD model1
  model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TYG_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_CVD_result<-summary(model2_CVD)
  P<-model2_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model2_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model2",'status'="CVD cause")
  result2
  result.CVD<-rbind(result,result2)
  #CVD model3
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TYG_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_CVD_result<-summary(model3_CVD)
  
  P<-model3_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model3_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3)
  result.CVD
  #CVD model4
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TYG_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result4)
  result.CVD
}
{ #* Cancer model #####
  #Cancer Crude
  model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            TYG_quantile, design =rhcSvy)
  model1_Cancer_result<-summary(model1_Cancer)
  model1_Cancer_result
  P<-model1_Cancer_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model1_Cancer_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                       'P value' =P,'model'="model1",'status'="Cancer cause")
  result
  #Cancer model1
  model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            TYG_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_Cancer_result<-summary(model2_Cancer)
  P<-model2_Cancer_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model2_Cancer_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model2",'status'="Cancer cause")
  result2
  result.Cancer<-rbind(result,result2)
  #Cancer model3
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            TYG_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                            Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_Cancer_result<-summary(model3_Cancer)
  model3_Cancer_result
  P<-model3_Cancer_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model3_Cancer_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3)
  result.Cancer
  
  #Cancer model4
  model4_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            TYG_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                            Smoking_status+Drinking_status+BMI_status+Physical_status+
                            HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_Cancer_result<-summary(model4_Cancer)
  model4_Cancer_result
  P<-model4_Cancer_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_Cancer_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result4)
  result.Cancer
}


{ #* Combine #####
  result.cause<-rbind(result.all,result.CVD,result.Cancer)
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  round_3_function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause[result.cause=="1000"]<-"1.00"
  result.cause[result.cause=="2000"]<-"2.00"
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
 
  result.cause[result.cause=="00000"]<-"<0.001"
  TYG<-result.cause
  TYG
}

{ #* all model #####
  #all model1
  model1_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TYG_qua, design =rhcSvy)
  model1_all_result<-summary(model1_all)
  model1_all_result
  P<-model1_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model1_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model2
  model2_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TYG_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_all_result<-summary(model2_all)
  P<-model2_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model2_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result.all<-rbind(result,result2)
  #all model3
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TYG_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  result.all
  #all model4
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TYG_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-rbind(result.all,result4)
  result.all
}

{ #* CVD model #####
  #CVD Crude
  model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TYG_qua, design =rhcSvy)
  model1_CVD_result<-summary(model1_CVD)
  model1_CVD_result
  P<-model1_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model1_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="CVD cause")
  result
  #CVD model1
  model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TYG_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_CVD_result<-summary(model2_CVD)
  P<-model2_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model2_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="CVD cause")
  result2
  result.CVD<-rbind(result,result2)
  #CVD model3
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TYG_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_CVD_result<-summary(model3_CVD)
  
  P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3)
  result.CVD
  #CVD model4
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TYG_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result4)
  result.CVD
}
{ #* Cancer model #####
  #Cancer Crude
  model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            TYG_qua, design =rhcSvy)
  model1_Cancer_result<-summary(model1_Cancer)
  model1_Cancer_result
  P<-model1_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model1_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="Cancer cause")
  result
  #Cancer model1
  model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            TYG_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_Cancer_result<-summary(model2_Cancer)
  P<-model2_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model2_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="Cancer cause")
  result2
  result.Cancer<-rbind(result,result2)
  #Cancer model3
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            TYG_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                            Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_Cancer_result<-summary(model3_Cancer)
  model3_Cancer_result
  P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3)
  result.Cancer
  
  #Cancer model4
  model4_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            TYG_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                            Smoking_status+Drinking_status+BMI_status+Physical_status+
                            HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_Cancer_result<-summary(model4_Cancer)
  model4_Cancer_result
  P<-model4_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result4)
  result.Cancer
}


{ #* Combine #####
  result.cause<-rbind(result.all,result.CVD,result.Cancer)
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  round_3_function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
  result.cause[result.cause=="00000"]<-"<0.001"
  TYG_qua<-result.cause
  TYG_qua
}
Table<-c("","TYG Quantile","","","","")
Table<-rbind(Table,c("","Quantile 1*", "Quantile 2","Quantile 3","Quantile 4","P for trend"))

Table<-rbind(Table,c("All cause","", "","","",""))

Table<-rbind(Table,c("Model 1†","1.00 [Reference]",TYG[1,"HR_CI"],TYG[2,"HR_CI"],TYG[3,"HR_CI"],TYG_qua[1,"P.value"]))
Table<-rbind(Table,c("Model 2‡","1.00 [Reference]",TYG[4,"HR_CI"],TYG[5,"HR_CI"],TYG[6,"HR_CI"],TYG_qua[2,"P.value"]))
Table<-rbind(Table,c("Model 3§","1.00 [Reference]",TYG[7,"HR_CI"],TYG[8,"HR_CI"],TYG[9,"HR_CI"],TYG_qua[3,"P.value"]))
Table<-rbind(Table,c("Model 4¶","1.00 [Reference]",TYG[10,"HR_CI"],TYG[11,"HR_CI"],TYG[12,"HR_CI"],TYG_qua[4,"P.value"]))

Table<-rbind(Table,c("CVD cause","", "","","",""))
Table<-rbind(Table,c("Model 1","1.00 [Reference]",TYG[13,"HR_CI"],TYG[14,"HR_CI"],TYG[15,"HR_CI"],TYG_qua[5,"P.value"]))
Table<-rbind(Table,c("Model 2","1.00 [Reference]",TYG[16,"HR_CI"],TYG[17,"HR_CI"],TYG[18,"HR_CI"],TYG_qua[6,"P.value"]))
Table<-rbind(Table,c("Model 3","1.00 [Reference]",TYG[19,"HR_CI"],TYG[20,"HR_CI"],TYG[21,"HR_CI"],TYG_qua[7,"P.value"]))
Table<-rbind(Table,c("Model 4","1.00 [Reference]",TYG[22,"HR_CI"],TYG[23,"HR_CI"],TYG[24,"HR_CI"],TYG_qua[8,"P.value"]))

Table<-rbind(Table,c("Cancer cause","","","","",""))
Table<-rbind(Table,c("Model 1","1.00 [Reference]",TYG[25,"HR_CI"],TYG[26,"HR_CI"],TYG[27,"HR_CI"],TYG_qua[9,"P.value"]))
Table<-rbind(Table,c("Model 2","1.00 [Reference]",TYG[28,"HR_CI"],TYG[29,"HR_CI"],TYG[30,"HR_CI"],TYG_qua[10,"P.value"]))
Table<-rbind(Table,c("Model 3","1.00 [Reference]",TYG[31,"HR_CI"],TYG[32,"HR_CI"],TYG[33,"HR_CI"],TYG_qua[11,"P.value"]))
Table<-rbind(Table,c("Model 4","1.00 [Reference]",TYG[34,"HR_CI"],TYG[35,"HR_CI"],TYG[36,"HR_CI"],TYG_qua[12,"P.value"]))
Table
Table<-as.data.frame(Table)
Table= data.frame(lapply(Table, as.character), stringsAsFactors=FALSE)
write.table(Table,sep = ",",file ="I:/NHANES study/PD&TYG&MO/Result/Supplementary Table 8.csv",row.names =F,col.names =F )


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 24 TYG WC quantile (Table S9) ####    
load(file="I:/NHANES study/PD&TYG&MO/Data/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
table(Interpolation_weighted$TyG_WC_quantile)
Interpolation_weighted$TyG_WC_quantile<-as.character(Interpolation_weighted$TyG_WC_quantile)
Interpolation_weighted$TyG_WC_qua[Interpolation_weighted$TyG_WC_quantile=="Quantile 1"]<-0
Interpolation_weighted$TyG_WC_qua[Interpolation_weighted$TyG_WC_quantile=="Quantile 2"]<-1
Interpolation_weighted$TyG_WC_qua[Interpolation_weighted$TyG_WC_quantile=="Quantile 3"]<-2
Interpolation_weighted$TyG_WC_qua[Interpolation_weighted$TyG_WC_quantile=="Quantile 4"]<-3

table(Interpolation_weighted$TyG_WC_qua)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
{ #* all model #####
  #all model1
  model1_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_WC_quantile, design =rhcSvy)
  model1_all_result<-summary(model1_all)
  model1_all_result
  P<-model1_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model1_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model2
  model2_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_WC_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_all_result<-summary(model2_all)
  P<-model2_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model2_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result.all<-rbind(result,result2)
  #all model3
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_WC_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model3_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  result.all
  #all model4
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_WC_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-rbind(result.all,result4)
  result.all
}

{ #* CVD model #####
  #CVD Crude
  model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_WC_quantile, design =rhcSvy)
  model1_CVD_result<-summary(model1_CVD)
  model1_CVD_result
  P<-model1_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model1_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                       'P value' =P,'model'="model1",'status'="CVD cause")
  result
  #CVD model1
  model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_WC_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_CVD_result<-summary(model2_CVD)
  P<-model2_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model2_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model2",'status'="CVD cause")
  result2
  result.CVD<-rbind(result,result2)
  #CVD model3
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_WC_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_CVD_result<-summary(model3_CVD)
  
  P<-model3_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model3_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3)
  result.CVD
  #CVD model4
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_WC_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result4)
  result.CVD
}
{ #* Cancer model #####
  #Cancer Crude
  model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            TyG_WC_quantile, design =rhcSvy)
  model1_Cancer_result<-summary(model1_Cancer)
  model1_Cancer_result
  P<-model1_Cancer_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model1_Cancer_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                       'P value' =P,'model'="model1",'status'="Cancer cause")
  result
  #Cancer model1
  model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            TyG_WC_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_Cancer_result<-summary(model2_Cancer)
  P<-model2_Cancer_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model2_Cancer_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model2",'status'="Cancer cause")
  result2
  result.Cancer<-rbind(result,result2)
  #Cancer model3
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            TyG_WC_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                            Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_Cancer_result<-summary(model3_Cancer)
  model3_Cancer_result
  P<-model3_Cancer_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model3_Cancer_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3)
  result.Cancer
  
  #Cancer model4
  model4_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            TyG_WC_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                            Smoking_status+Drinking_status+BMI_status+Physical_status+
                            HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_Cancer_result<-summary(model4_Cancer)
  model4_Cancer_result
  P<-model4_Cancer_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_Cancer_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result4)
  result.Cancer
}


{ #* Combine #####
  result.cause<-rbind(result.all,result.CVD,result.Cancer)
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  round_3_function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause[result.cause=="1000"]<-"1.00"
  result.cause[result.cause=="2000"]<-"2.00"
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
  
  result.cause[result.cause=="00000"]<-"<0.001"
  TYG<-result.cause
  TYG
}

{ #* all model #####
  #all model1
  model1_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_WC_qua, design =rhcSvy)
  model1_all_result<-summary(model1_all)
  model1_all_result
  P<-model1_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model1_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model2
  model2_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_WC_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_all_result<-summary(model2_all)
  P<-model2_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model2_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result.all<-rbind(result,result2)
  #all model3
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_WC_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  result.all
  #all model4
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_WC_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-rbind(result.all,result4)
  result.all
}

{ #* CVD model #####
  #CVD Crude
  model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_WC_qua, design =rhcSvy)
  model1_CVD_result<-summary(model1_CVD)
  model1_CVD_result
  P<-model1_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model1_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="CVD cause")
  result
  #CVD model1
  model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_WC_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_CVD_result<-summary(model2_CVD)
  P<-model2_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model2_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="CVD cause")
  result2
  result.CVD<-rbind(result,result2)
  #CVD model3
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_WC_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_CVD_result<-summary(model3_CVD)
  
  P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3)
  result.CVD
  #CVD model4
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_WC_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result4)
  result.CVD
}
{ #* Cancer model #####
  #Cancer Crude
  model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            TyG_WC_qua, design =rhcSvy)
  model1_Cancer_result<-summary(model1_Cancer)
  model1_Cancer_result
  P<-model1_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model1_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="Cancer cause")
  result
  #Cancer model1
  model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            TyG_WC_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_Cancer_result<-summary(model2_Cancer)
  P<-model2_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model2_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="Cancer cause")
  result2
  result.Cancer<-rbind(result,result2)
  #Cancer model3
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            TyG_WC_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                            Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_Cancer_result<-summary(model3_Cancer)
  model3_Cancer_result
  P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3)
  result.Cancer
  
  #Cancer model4
  model4_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            TyG_WC_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                            Smoking_status+Drinking_status+BMI_status+Physical_status+
                            HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_Cancer_result<-summary(model4_Cancer)
  model4_Cancer_result
  P<-model4_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result4)
  result.Cancer
}


{ #* Combine #####
  result.cause<-rbind(result.all,result.CVD,result.Cancer)
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  round_3_function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
  result.cause[result.cause=="00000"]<-"<0.001"
  TyG_WC_qua<-result.cause
  TyG_WC_qua
}
Table<-c("","TYG-WC Quantile","","","","")
Table<-rbind(Table,c("","Quantile 1*", "Quantile 2","Quantile 3","Quantile 4","P for trend"))

Table<-rbind(Table,c("All cause","", "","","",""))

Table<-rbind(Table,c("Model 1†","1.00 [Reference]",TYG[1,"HR_CI"],TYG[2,"HR_CI"],TYG[3,"HR_CI"],TyG_WC_qua[1,"P.value"]))
Table<-rbind(Table,c("Model 2‡","1.00 [Reference]",TYG[4,"HR_CI"],TYG[5,"HR_CI"],TYG[6,"HR_CI"],TyG_WC_qua[2,"P.value"]))
Table<-rbind(Table,c("Model 3§","1.00 [Reference]",TYG[7,"HR_CI"],TYG[8,"HR_CI"],TYG[9,"HR_CI"],TyG_WC_qua[3,"P.value"]))
Table<-rbind(Table,c("Model 4¶","1.00 [Reference]",TYG[10,"HR_CI"],TYG[11,"HR_CI"],TYG[12,"HR_CI"],TyG_WC_qua[4,"P.value"]))

Table<-rbind(Table,c("CVD cause","", "","","",""))
Table<-rbind(Table,c("Model 1","1.00 [Reference]",TYG[13,"HR_CI"],TYG[14,"HR_CI"],TYG[15,"HR_CI"],TyG_WC_qua[5,"P.value"]))
Table<-rbind(Table,c("Model 2","1.00 [Reference]",TYG[16,"HR_CI"],TYG[17,"HR_CI"],TYG[18,"HR_CI"],TyG_WC_qua[6,"P.value"]))
Table<-rbind(Table,c("Model 3","1.00 [Reference]",TYG[19,"HR_CI"],TYG[20,"HR_CI"],TYG[21,"HR_CI"],TyG_WC_qua[7,"P.value"]))
Table<-rbind(Table,c("Model 4","1.00 [Reference]",TYG[22,"HR_CI"],TYG[23,"HR_CI"],TYG[24,"HR_CI"],TyG_WC_qua[8,"P.value"]))

Table<-rbind(Table,c("Cancer cause","","","","",""))
Table<-rbind(Table,c("Model 1","1.00 [Reference]",TYG[25,"HR_CI"],TYG[26,"HR_CI"],TYG[27,"HR_CI"],TyG_WC_qua[9,"P.value"]))
Table<-rbind(Table,c("Model 2","1.00 [Reference]",TYG[28,"HR_CI"],TYG[29,"HR_CI"],TYG[30,"HR_CI"],TyG_WC_qua[10,"P.value"]))
Table<-rbind(Table,c("Model 3","1.00 [Reference]",TYG[31,"HR_CI"],TYG[32,"HR_CI"],TYG[33,"HR_CI"],TyG_WC_qua[11,"P.value"]))
Table<-rbind(Table,c("Model 4","1.00 [Reference]",TYG[34,"HR_CI"],TYG[35,"HR_CI"],TYG[36,"HR_CI"],TyG_WC_qua[12,"P.value"]))
Table
Table<-as.data.frame(Table)
Table= data.frame(lapply(Table, as.character), stringsAsFactors=FALSE)
write.table(Table,sep = ",",file ="I:/NHANES study/PD&TYG&MO/Result/Supplementary Table 9.csv",row.names =F,col.names =F )

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 25 TYG WHtR quantile (Table S10) ####    
load(file="I:/NHANES study/PD&TYG&MO/Data/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
table(Interpolation_weighted$TyG_WHtR_quantile)
Interpolation_weighted$TyG_WHtR_quantile<-as.character(Interpolation_weighted$TyG_WHtR_quantile)
Interpolation_weighted$TyG_WHtR_qua[Interpolation_weighted$TyG_WHtR_quantile=="Quantile 1"]<-0
Interpolation_weighted$TyG_WHtR_qua[Interpolation_weighted$TyG_WHtR_quantile=="Quantile 2"]<-1
Interpolation_weighted$TyG_WHtR_qua[Interpolation_weighted$TyG_WHtR_quantile=="Quantile 3"]<-2
Interpolation_weighted$TyG_WHtR_qua[Interpolation_weighted$TyG_WHtR_quantile=="Quantile 4"]<-3

table(Interpolation_weighted$TyG_WHtR_qua)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
{ #* all model #####
  #all model1
  model1_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_WHtR_quantile, design =rhcSvy)
  model1_all_result<-summary(model1_all)
  model1_all_result
  P<-model1_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model1_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model2
  model2_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_WHtR_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_all_result<-summary(model2_all)
  P<-model2_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model2_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result.all<-rbind(result,result2)
  #all model3
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_WHtR_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model3_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  result.all
  #all model4
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_WHtR_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-rbind(result.all,result4)
  result.all
}

{ #* CVD model #####
  #CVD Crude
  model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_WHtR_quantile, design =rhcSvy)
  model1_CVD_result<-summary(model1_CVD)
  model1_CVD_result
  P<-model1_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model1_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                       'P value' =P,'model'="model1",'status'="CVD cause")
  result
  #CVD model1
  model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_WHtR_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_CVD_result<-summary(model2_CVD)
  P<-model2_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model2_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model2",'status'="CVD cause")
  result2
  result.CVD<-rbind(result,result2)
  #CVD model3
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_WHtR_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_CVD_result<-summary(model3_CVD)
  
  P<-model3_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model3_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3)
  result.CVD
  #CVD model4
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_WHtR_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result4)
  result.CVD
}
{ #* Cancer model #####
  #Cancer Crude
  model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            TyG_WHtR_quantile, design =rhcSvy)
  model1_Cancer_result<-summary(model1_Cancer)
  model1_Cancer_result
  P<-model1_Cancer_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model1_Cancer_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                       'P value' =P,'model'="model1",'status'="Cancer cause")
  result
  #Cancer model1
  model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            TyG_WHtR_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_Cancer_result<-summary(model2_Cancer)
  P<-model2_Cancer_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model2_Cancer_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model2",'status'="Cancer cause")
  result2
  result.Cancer<-rbind(result,result2)
  #Cancer model3
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            TyG_WHtR_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                            Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_Cancer_result<-summary(model3_Cancer)
  model3_Cancer_result
  P<-model3_Cancer_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model3_Cancer_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3)
  result.Cancer
  
  #Cancer model4
  model4_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            TyG_WHtR_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                            Smoking_status+Drinking_status+BMI_status+Physical_status+
                            HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_Cancer_result<-summary(model4_Cancer)
  model4_Cancer_result
  P<-model4_Cancer_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_Cancer_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result4)
  result.Cancer
}


{ #* Combine #####
  result.cause<-rbind(result.all,result.CVD,result.Cancer)
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  round_3_function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause[result.cause=="1000"]<-"1.00"
  result.cause[result.cause=="2000"]<-"2.00"
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
  
  result.cause[result.cause=="00000"]<-"<0.001"
  TYG<-result.cause
  TYG
}

{ #* all model #####
  #all model1
  model1_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_WHtR_qua, design =rhcSvy)
  model1_all_result<-summary(model1_all)
  model1_all_result
  P<-model1_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model1_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model2
  model2_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_WHtR_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_all_result<-summary(model2_all)
  P<-model2_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model2_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result.all<-rbind(result,result2)
  #all model3
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_WHtR_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  result.all
  #all model4
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_WHtR_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-rbind(result.all,result4)
  result.all
}

{ #* CVD model #####
  #CVD Crude
  model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_WHtR_qua, design =rhcSvy)
  model1_CVD_result<-summary(model1_CVD)
  model1_CVD_result
  P<-model1_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model1_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="CVD cause")
  result
  #CVD model1
  model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_WHtR_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_CVD_result<-summary(model2_CVD)
  P<-model2_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model2_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="CVD cause")
  result2
  result.CVD<-rbind(result,result2)
  #CVD model3
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_WHtR_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_CVD_result<-summary(model3_CVD)
  
  P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3)
  result.CVD
  #CVD model4
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_WHtR_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result4)
  result.CVD
}
{ #* Cancer model #####
  #Cancer Crude
  model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            TyG_WHtR_qua, design =rhcSvy)
  model1_Cancer_result<-summary(model1_Cancer)
  model1_Cancer_result
  P<-model1_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model1_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="Cancer cause")
  result
  #Cancer model1
  model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            TyG_WHtR_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_Cancer_result<-summary(model2_Cancer)
  P<-model2_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model2_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="Cancer cause")
  result2
  result.Cancer<-rbind(result,result2)
  #Cancer model3
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            TyG_WHtR_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                            Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_Cancer_result<-summary(model3_Cancer)
  model3_Cancer_result
  P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3)
  result.Cancer
  
  #Cancer model4
  model4_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            TyG_WHtR_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                            Smoking_status+Drinking_status+BMI_status+Physical_status+
                            HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_Cancer_result<-summary(model4_Cancer)
  model4_Cancer_result
  P<-model4_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result4)
  result.Cancer
}


{ #* Combine #####
  result.cause<-rbind(result.all,result.CVD,result.Cancer)
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  round_3_function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
  result.cause[result.cause=="00000"]<-"<0.001"
  TyG_WHtR_qua<-result.cause
  TyG_WHtR_qua
}
Table<-c("","TYG-WHtR Quantile","","","","")
Table<-rbind(Table,c("","Quantile 1*", "Quantile 2","Quantile 3","Quantile 4","P for trend"))

Table<-rbind(Table,c("All cause","", "","","",""))

Table<-rbind(Table,c("Model 1†","1.00 [Reference]",TYG[1,"HR_CI"],TYG[2,"HR_CI"],TYG[3,"HR_CI"],TyG_WHtR_qua[1,"P.value"]))
Table<-rbind(Table,c("Model 2‡","1.00 [Reference]",TYG[4,"HR_CI"],TYG[5,"HR_CI"],TYG[6,"HR_CI"],TyG_WHtR_qua[2,"P.value"]))
Table<-rbind(Table,c("Model 3§","1.00 [Reference]",TYG[7,"HR_CI"],TYG[8,"HR_CI"],TYG[9,"HR_CI"],TyG_WHtR_qua[3,"P.value"]))
Table<-rbind(Table,c("Model 4¶","1.00 [Reference]",TYG[10,"HR_CI"],TYG[11,"HR_CI"],TYG[12,"HR_CI"],TyG_WHtR_qua[4,"P.value"]))

Table<-rbind(Table,c("CVD cause","", "","","",""))
Table<-rbind(Table,c("Model 1","1.00 [Reference]",TYG[13,"HR_CI"],TYG[14,"HR_CI"],TYG[15,"HR_CI"],TyG_WHtR_qua[5,"P.value"]))
Table<-rbind(Table,c("Model 2","1.00 [Reference]",TYG[16,"HR_CI"],TYG[17,"HR_CI"],TYG[18,"HR_CI"],TyG_WHtR_qua[6,"P.value"]))
Table<-rbind(Table,c("Model 3","1.00 [Reference]",TYG[19,"HR_CI"],TYG[20,"HR_CI"],TYG[21,"HR_CI"],TyG_WHtR_qua[7,"P.value"]))
Table<-rbind(Table,c("Model 4","1.00 [Reference]",TYG[22,"HR_CI"],TYG[23,"HR_CI"],TYG[24,"HR_CI"],TyG_WHtR_qua[8,"P.value"]))

Table<-rbind(Table,c("Cancer cause","","","","",""))
Table<-rbind(Table,c("Model 1","1.00 [Reference]",TYG[25,"HR_CI"],TYG[26,"HR_CI"],TYG[27,"HR_CI"],TyG_WHtR_qua[9,"P.value"]))
Table<-rbind(Table,c("Model 2","1.00 [Reference]",TYG[28,"HR_CI"],TYG[29,"HR_CI"],TYG[30,"HR_CI"],TyG_WHtR_qua[10,"P.value"]))
Table<-rbind(Table,c("Model 3","1.00 [Reference]",TYG[31,"HR_CI"],TYG[32,"HR_CI"],TYG[33,"HR_CI"],TyG_WHtR_qua[11,"P.value"]))
Table<-rbind(Table,c("Model 4","1.00 [Reference]",TYG[34,"HR_CI"],TYG[35,"HR_CI"],TYG[36,"HR_CI"],TyG_WHtR_qua[12,"P.value"]))
Table
Table<-as.data.frame(Table)
Table= data.frame(lapply(Table, as.character), stringsAsFactors=FALSE)
write.table(Table,sep = ",",file ="I:/NHANES study/PD&TYG&MO/Result/Supplementary Table 10.csv",row.names =F,col.names =F )

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 26 TYG BMI quantile (Table S11) #### 
load(file="I:/NHANES study/PD&TYG&MO/Data/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
table(Interpolation_weighted$TyG_BMI_quantile)
Interpolation_weighted$TyG_BMI_quantile<-as.character(Interpolation_weighted$TyG_BMI_quantile)
Interpolation_weighted$TyG_BMI_qua[Interpolation_weighted$TyG_BMI_quantile=="Quantile 1"]<-0
Interpolation_weighted$TyG_BMI_qua[Interpolation_weighted$TyG_BMI_quantile=="Quantile 2"]<-1
Interpolation_weighted$TyG_BMI_qua[Interpolation_weighted$TyG_BMI_quantile=="Quantile 3"]<-2
Interpolation_weighted$TyG_BMI_qua[Interpolation_weighted$TyG_BMI_quantile=="Quantile 4"]<-3

table(Interpolation_weighted$TyG_BMI_qua)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
{ #* all model #####
  #all model1
  model1_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_BMI_quantile, design =rhcSvy)
  model1_all_result<-summary(model1_all)
  model1_all_result
  P<-model1_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model1_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model2
  model2_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_BMI_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_all_result<-summary(model2_all)
  P<-model2_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model2_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result.all<-rbind(result,result2)
  #all model3
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_BMI_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model3_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  result.all
  #all model4
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_BMI_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-rbind(result.all,result4)
  result.all
}

{ #* CVD model #####
  #CVD Crude
  model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_BMI_quantile, design =rhcSvy)
  model1_CVD_result<-summary(model1_CVD)
  model1_CVD_result
  P<-model1_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model1_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                       'P value' =P,'model'="model1",'status'="CVD cause")
  result
  #CVD model1
  model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_BMI_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_CVD_result<-summary(model2_CVD)
  P<-model2_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model2_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model2",'status'="CVD cause")
  result2
  result.CVD<-rbind(result,result2)
  #CVD model3
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_BMI_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_CVD_result<-summary(model3_CVD)
  
  P<-model3_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model3_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3)
  result.CVD
  #CVD model4
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_BMI_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result4)
  result.CVD
}
{ #* Cancer model #####
  #Cancer Crude
  model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            TyG_BMI_quantile, design =rhcSvy)
  model1_Cancer_result<-summary(model1_Cancer)
  model1_Cancer_result
  P<-model1_Cancer_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model1_Cancer_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                       'P value' =P,'model'="model1",'status'="Cancer cause")
  result
  #Cancer model1
  model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            TyG_BMI_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_Cancer_result<-summary(model2_Cancer)
  P<-model2_Cancer_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model2_Cancer_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model2",'status'="Cancer cause")
  result2
  result.Cancer<-rbind(result,result2)
  #Cancer model3
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            TyG_BMI_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                            Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_Cancer_result<-summary(model3_Cancer)
  model3_Cancer_result
  P<-model3_Cancer_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model3_Cancer_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3)
  result.Cancer
  
  #Cancer model4
  model4_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            TyG_BMI_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                            Smoking_status+Drinking_status+BMI_status+Physical_status+
                            HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_Cancer_result<-summary(model4_Cancer)
  model4_Cancer_result
  P<-model4_Cancer_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_Cancer_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result4)
  result.Cancer
}


{ #* Combine #####
  result.cause<-rbind(result.all,result.CVD,result.Cancer)
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  round_3_function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause[result.cause=="1000"]<-"1.00"
  result.cause[result.cause=="2000"]<-"2.00"
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
  
  result.cause[result.cause=="00000"]<-"<0.001"
  TYG<-result.cause
  TYG
}

{ #* all model #####
  #all model1
  model1_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_BMI_qua, design =rhcSvy)
  model1_all_result<-summary(model1_all)
  model1_all_result
  P<-model1_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model1_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model2
  model2_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_BMI_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_all_result<-summary(model2_all)
  P<-model2_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model2_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result.all<-rbind(result,result2)
  #all model3
  model3_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_BMI_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_all_result<-summary(model3_all)
  
  P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result.all<-rbind(result.all,result3)
  result.all
  #all model4
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_BMI_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-rbind(result.all,result4)
  result.all
}

{ #* CVD model #####
  #CVD Crude
  model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_BMI_qua, design =rhcSvy)
  model1_CVD_result<-summary(model1_CVD)
  model1_CVD_result
  P<-model1_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model1_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="CVD cause")
  result
  #CVD model1
  model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_BMI_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_CVD_result<-summary(model2_CVD)
  P<-model2_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model2_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="CVD cause")
  result2
  result.CVD<-rbind(result,result2)
  #CVD model3
  model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_BMI_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_CVD_result<-summary(model3_CVD)
  
  P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result3)
  result.CVD
  #CVD model4
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_BMI_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-rbind(result.CVD,result4)
  result.CVD
}
{ #* Cancer model #####
  #Cancer Crude
  model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            TyG_BMI_qua, design =rhcSvy)
  model1_Cancer_result<-summary(model1_Cancer)
  model1_Cancer_result
  P<-model1_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model1_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="Cancer cause")
  result
  #Cancer model1
  model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            TyG_BMI_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_Cancer_result<-summary(model2_Cancer)
  P<-model2_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model2_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="Cancer cause")
  result2
  result.Cancer<-rbind(result,result2)
  #Cancer model3
  model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            TyG_BMI_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                            Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_Cancer_result<-summary(model3_Cancer)
  model3_Cancer_result
  P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result3)
  result.Cancer
  
  #Cancer model4
  model4_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                            TyG_BMI_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                            Smoking_status+Drinking_status+BMI_status+Physical_status+
                            HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_Cancer_result<-summary(model4_Cancer)
  model4_Cancer_result
  P<-model4_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result.Cancer<-rbind(result.Cancer,result4)
  result.Cancer
}


{ #* Combine #####
  result.cause<-rbind(result.all,result.CVD,result.Cancer)
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  round_3_function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
  result.cause[result.cause=="00000"]<-"<0.001"
  TyG_BMI_qua<-result.cause
  TyG_BMI_qua
}
Table<-c("","TYG-BMI Quantile","","","","")
Table<-rbind(Table,c("","Quantile 1*", "Quantile 2","Quantile 3","Quantile 4","P for trend"))

Table<-rbind(Table,c("All cause","", "","","",""))

Table<-rbind(Table,c("Model 1†","1.00 [Reference]",TYG[1,"HR_CI"],TYG[2,"HR_CI"],TYG[3,"HR_CI"],TyG_BMI_qua[1,"P.value"]))
Table<-rbind(Table,c("Model 2‡","1.00 [Reference]",TYG[4,"HR_CI"],TYG[5,"HR_CI"],TYG[6,"HR_CI"],TyG_BMI_qua[2,"P.value"]))
Table<-rbind(Table,c("Model 3§","1.00 [Reference]",TYG[7,"HR_CI"],TYG[8,"HR_CI"],TYG[9,"HR_CI"],TyG_BMI_qua[3,"P.value"]))
Table<-rbind(Table,c("Model 4¶","1.00 [Reference]",TYG[10,"HR_CI"],TYG[11,"HR_CI"],TYG[12,"HR_CI"],TyG_BMI_qua[4,"P.value"]))

Table<-rbind(Table,c("CVD cause","", "","","",""))
Table<-rbind(Table,c("Model 1","1.00 [Reference]",TYG[13,"HR_CI"],TYG[14,"HR_CI"],TYG[15,"HR_CI"],TyG_BMI_qua[5,"P.value"]))
Table<-rbind(Table,c("Model 2","1.00 [Reference]",TYG[16,"HR_CI"],TYG[17,"HR_CI"],TYG[18,"HR_CI"],TyG_BMI_qua[6,"P.value"]))
Table<-rbind(Table,c("Model 3","1.00 [Reference]",TYG[19,"HR_CI"],TYG[20,"HR_CI"],TYG[21,"HR_CI"],TyG_BMI_qua[7,"P.value"]))
Table<-rbind(Table,c("Model 4","1.00 [Reference]",TYG[22,"HR_CI"],TYG[23,"HR_CI"],TYG[24,"HR_CI"],TyG_BMI_qua[8,"P.value"]))

Table<-rbind(Table,c("Cancer cause","","","","",""))
Table<-rbind(Table,c("Model 1","1.00 [Reference]",TYG[25,"HR_CI"],TYG[26,"HR_CI"],TYG[27,"HR_CI"],TyG_BMI_qua[9,"P.value"]))
Table<-rbind(Table,c("Model 2","1.00 [Reference]",TYG[28,"HR_CI"],TYG[29,"HR_CI"],TYG[30,"HR_CI"],TyG_BMI_qua[10,"P.value"]))
Table<-rbind(Table,c("Model 3","1.00 [Reference]",TYG[31,"HR_CI"],TYG[32,"HR_CI"],TYG[33,"HR_CI"],TyG_BMI_qua[11,"P.value"]))
Table<-rbind(Table,c("Model 4","1.00 [Reference]",TYG[34,"HR_CI"],TYG[35,"HR_CI"],TYG[36,"HR_CI"],TyG_BMI_qua[12,"P.value"]))
Table<-as.data.frame(Table)
Table= data.frame(lapply(Table, as.character), stringsAsFactors=FALSE)

write.table(Table,sep = ",",file ="I:/NHANES study/PD&TYG&MO/Result/Supplementary Table 11.csv",row.names =F,col.names =F )
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 26 Total (Table 3) #### 
TableS4<-read.table(file ="I:/NHANES study/PD&TYG&MO/Result/Supplementary Table 4.csv",sep = ",")
TableS5<-read.table(file ="I:/NHANES study/PD&TYG&MO/Result/Supplementary Table 5.csv",sep = ",")
TableS6<-read.table(file ="I:/NHANES study/PD&TYG&MO/Result/Supplementary Table 6.csv",sep = ",")
TableS7<-read.table(file ="I:/NHANES study/PD&TYG&MO/Result/Supplementary Table 7.csv",sep = ",")
TableS8<-read.table(file ="I:/NHANES study/PD&TYG&MO/Result/Supplementary Table 8.csv",sep = ",")
TableS9<-read.table(file ="I:/NHANES study/PD&TYG&MO/Result/Supplementary Table 9.csv",sep = ",")
TableS10<-read.table(file ="I:/NHANES study/PD&TYG&MO/Result/Supplementary Table 10.csv",sep = ",")
TableS11<-read.table(file ="I:/NHANES study/PD&TYG&MO/Result/Supplementary Table 11.csv",sep = ",")

Table<-c("Exposure","All cause","","CVD-related","","Cancer-related","")
Table<-rbind(Table,c("","HR (95% CI)*","P†", "HR (95% CI)","P","HR (95% CI)","P"))
Table<-rbind(Table,c("TyG index","", "", "", "", "",""))
Table<-rbind(Table,c("Per 1-index‡",TableS4[7,2],TableS4[7,3], 
                     TableS4[12,2],TableS4[12,3],
                     TableS4[17,2],TableS4[17,3]))
Table<-rbind(Table,c("Quantile 1", 
                     "1.00 [Reference]","",
                     "1.00 [Reference]","",
                     "1.00 [Reference]",""))
Table<-rbind(Table,c("Quantile 2", 
                     TableS8[7,3],"",
                     TableS8[12,3],"",
                     TableS8[17,3],""))
Table<-rbind(Table,c("Quantile 3", 
                     TableS8[7,4],"",
                     TableS8[12,4],"",
                     TableS8[17,4],""))
Table<-rbind(Table,c("Quantile 4", 
                     TableS8[7,5],TableS8[7,6],
                     TableS8[12,5],TableS8[12,6],
                     TableS8[17,5],TableS8[17,6]))

Table<-rbind(Table,c("TyG-WC index","", "", "", "", "",""))
Table<-rbind(Table,c("Per 100-index",TableS5[7,2],TableS5[7,3], 
                     TableS5[12,2],TableS5[12,3],
                     TableS5[17,2],TableS5[17,3]))
Table<-rbind(Table,c("Quantile 1", 
                     "1.00 [Reference]","",
                     "1.00 [Reference]","",
                     "1.00 [Reference]",""))
Table<-rbind(Table,c("Quantile 2", 
                     TableS9[7,3],"",
                     TableS9[12,3],"",
                     TableS9[17,3],""))
Table<-rbind(Table,c("Quantile 3", 
                     TableS9[7,4],"",
                     TableS9[12,4],"",
                     TableS9[17,4],""))
Table<-rbind(Table,c("Quantile 4", 
                     TableS9[7,5],TableS9[7,6],
                     TableS9[12,5],TableS9[12,6],
                     TableS9[17,5],TableS9[17,6]))

Table<-rbind(Table,c("TyG-WHtR index","", "", "", "", "",""))
Table<-rbind(Table,c("Per 1-index",TableS6[7,2],TableS6[7,3], 
                     TableS6[12,2],TableS6[12,3],
                     TableS6[17,2],TableS6[17,3]))
Table<-rbind(Table,c("Quantile 1", 
                     "1.00 [Reference]","",
                     "1.00 [Reference]","",
                     "1.00 [Reference]",""))
Table<-rbind(Table,c("Quantile 2", 
                     TableS10[7,3],"",
                     TableS10[12,3],"",
                     TableS10[17,3],""))
Table<-rbind(Table,c("Quantile 3", 
                     TableS10[7,4],"",
                     TableS10[12,4],"",
                     TableS10[17,4],""))
Table<-rbind(Table,c("Quantile 4", 
                     TableS10[7,5],TableS10[7,6],
                     TableS10[12,5],TableS10[12,6],
                     TableS10[17,5],TableS10[17,6]))

Table<-rbind(Table,c("TyG-BMI index","", "", "", "", "",""))
Table<-rbind(Table,c("Per 100-index",TableS7[7,2],TableS7[7,3], 
                     TableS7[12,2],TableS7[12,3],
                     TableS7[17,2],TableS7[17,3]))
Table<-rbind(Table,c("Quantile 1", 
                     "1.00 [Reference]","",
                     "1.00 [Reference]","",
                     "1.00 [Reference]",""))
Table<-rbind(Table,c("Quantile 2", 
                     TableS11[7,3],"",
                     TableS11[12,3],"",
                     TableS11[17,3],""))
Table<-rbind(Table,c("Quantile 3", 
                     TableS11[7,4],"",
                     TableS11[12,4],"",
                     TableS11[17,4],""))
Table<-rbind(Table,c("Quantile 4", 
                     TableS11[7,5],TableS11[7,6],
                     TableS11[12,5],TableS11[12,6],
                     TableS11[17,5],TableS11[17,6]))

Table<-as.data.frame(Table)
Table= data.frame(lapply(Table, as.character), stringsAsFactors=FALSE)
write.table(Table,sep = ",",file ="I:/NHANES study/PD&TYG&MO/Result/Table 3.csv",row.names =F,col.names =F )
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 26 Threshold effect analyses (Table 4) #### 
load(file="I:/NHANES study/PD&TYG&MO/Data/Interpolation_weighted.Rdata")

Interpolation_weighted$TyG_threshold <- ifelse(Interpolation_weighted$TYG<= 8.74, 0, 1)
Interpolation_weighted$TyG_WC_threshold <- ifelse(Interpolation_weighted$TyG_WC<= 855.70, 0, 1)
Interpolation_weighted$TyG_WHtR_threshold <- ifelse(Interpolation_weighted$TyG_WHtR<= 5.16, 0, 1)
Interpolation_weighted$TyG_BMI_threshold <- ifelse(Interpolation_weighted$TyG_BMI<= 241.74, 0, 1)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)

{ #* TyG ALL Mortality ####
model<- svycoxph(Surv(peryear, MORT_stat==1) ~ TyG_threshold + Age_status + Sex + Race_ethnicity + SES + 
                           Marital_status + Smoking_status + Drinking_status + BMI_status + Physical_status + 
                           HTN_status + T2D_status + HPL_status + Cohort, design = rhcSvy)
model_result<-summary(model)
P<-model_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'model'="TyG",'status'="All cause")
result_All<-result
}
{ #* TyG-WC ALL Mortality ####
model<- svycoxph(Surv(peryear, MORT_stat==1) ~ TyG_WC_threshold + Age_status + Sex + Race_ethnicity + SES + 
                   Marital_status + Smoking_status + Drinking_status + BMI_status + Physical_status + 
                   HTN_status + T2D_status + HPL_status + Cohort, design = rhcSvy)
model_result<-summary(model)
P<-model_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'model'="TyG-WC",'status'="All cause")
result_All<-rbind(result_All,result)
}
result_All
{ #* TyG-WHtR ALL Mortality ####
  model<- svycoxph(Surv(peryear, MORT_stat==1) ~ TyG_WHtR_threshold + Age_status + Sex + Race_ethnicity + SES + 
                     Marital_status + Smoking_status + Drinking_status + BMI_status + Physical_status + 
                     HTN_status + T2D_status + HPL_status + Cohort, design = rhcSvy)
  model_result<-summary(model)
  P<-model_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="TyG-WHtR",'status'="All cause")
  result_All<-rbind(result_All,result)
}
result_All
{ #* TyG-BMI ALL Mortality ####
  model<- svycoxph(Surv(peryear, MORT_stat==1) ~ TyG_BMI_threshold + Age_status + Sex + Race_ethnicity + SES + 
                     Marital_status + Smoking_status + Drinking_status + BMI_status + Physical_status + 
                     HTN_status + T2D_status + HPL_status + Cohort, design = rhcSvy)
  model_result<-summary(model)
  P<-model_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="TyG-BMI",'status'="All cause")
  result_All<-rbind(result_All,result)
}
result_All

load(file="I:/NHANES study/PD&TYG&MO/Data/Interpolation_weighted.Rdata")

Interpolation_weighted$TyG_threshold <- ifelse(Interpolation_weighted$TYG<= 8.69, 0, 1)
Interpolation_weighted$TyG_WC_threshold <- ifelse(Interpolation_weighted$TyG_WC<= 849.56, 0, 1)
Interpolation_weighted$TyG_WHtR_threshold <- ifelse(Interpolation_weighted$TyG_WHtR<= 5.09, 0, 1)
Interpolation_weighted$TyG_BMI_threshold <- ifelse(Interpolation_weighted$TyG_BMI<= 241.63, 0, 1)
{ #* TyG ALL Mortality ####
  model<- svycoxph(Surv(peryear, CVD_MORT_stat==1) ~ TyG_threshold + Age_status + Sex + Race_ethnicity + SES + 
                     Marital_status + Smoking_status + Drinking_status + BMI_status + Physical_status + 
                     HTN_status + T2D_status + HPL_status + Cohort, design = rhcSvy)
  model_result<-summary(model)
  P<-model_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="TyG",'status'="CVD cause")
  result_All<-rbind(result_All,result)
}
{ #* TyG-WC ALL Mortality ####
  model<- svycoxph(Surv(peryear, CVD_MORT_stat==1) ~ TyG_WC_threshold + Age_status + Sex + Race_ethnicity + SES + 
                     Marital_status + Smoking_status + Drinking_status + BMI_status + Physical_status + 
                     HTN_status + T2D_status + HPL_status + Cohort, design = rhcSvy)
  model_result<-summary(model)
  P<-model_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="TyG-WC",'status'="CVD cause")
  result_All<-rbind(result_All,result)
}
result_All
{ #* TyG-WHtR ALL Mortality ####
  model<- svycoxph(Surv(peryear, CVD_MORT_stat==1) ~ TyG_WHtR_threshold + Age_status + Sex + Race_ethnicity + SES + 
                     Marital_status + Smoking_status + Drinking_status + BMI_status + Physical_status + 
                     HTN_status + T2D_status + HPL_status + Cohort, design = rhcSvy)
  model_result<-summary(model)
  P<-model_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="TyG-WHtR",'status'="CVD cause")
  result_All<-rbind(result_All,result)
}
result_All
{ #* TyG-BMI ALL Mortality ####
  model<- svycoxph(Surv(peryear, CVD_MORT_stat==1) ~ TyG_BMI_threshold + Age_status + Sex + Race_ethnicity + SES + 
                     Marital_status + Smoking_status + Drinking_status + BMI_status + Physical_status + 
                     HTN_status + T2D_status + HPL_status + Cohort, design = rhcSvy)
  model_result<-summary(model)
  P<-model_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="TyG-BMI",'status'="CVD cause")
  result_All<-rbind(result_All,result)
}
result_All

Interpolation_weighted$TyG_threshold <- ifelse(Interpolation_weighted$TYG<= 8.69, 0, 1)
Interpolation_weighted$TyG_WC_threshold <- ifelse(Interpolation_weighted$TyG_WC<= 848.12, 0, 1)
Interpolation_weighted$TyG_WHtR_threshold <- ifelse(Interpolation_weighted$TyG_WHtR<= 5.05, 0, 1)
Interpolation_weighted$TyG_BMI_threshold <- ifelse(Interpolation_weighted$TyG_BMI<= 241.36, 0, 1)
{ #* TyG ALL Mortality ####
  model<- svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~ TyG_threshold + Age_status + Sex + Race_ethnicity + SES + 
                     Marital_status + Smoking_status + Drinking_status + BMI_status + Physical_status + 
                     HTN_status + T2D_status + HPL_status + Cohort, design = rhcSvy)
  model_result<-summary(model)
  P<-model_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="TyG",'status'="Cancer cause")
  result_All<-rbind(result_All,result)
}
{ #* TyG-WC ALL Mortality ####
  model<- svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~ TyG_WC_threshold + Age_status + Sex + Race_ethnicity + SES + 
                     Marital_status + Smoking_status + Drinking_status + BMI_status + Physical_status + 
                     HTN_status + T2D_status + HPL_status + Cohort, design = rhcSvy)
  model_result<-summary(model)
  P<-model_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="TyG-WC",'status'="Cancer cause")
  result_All<-rbind(result_All,result)
}
result_All
{ #* TyG-WHtR ALL Mortality ####
  model<- svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~ TyG_WHtR_threshold + Age_status + Sex + Race_ethnicity + SES + 
                     Marital_status + Smoking_status + Drinking_status + BMI_status + Physical_status + 
                     HTN_status + T2D_status + HPL_status + Cohort, design = rhcSvy)
  model_result<-summary(model)
  P<-model_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="TyG-WHtR",'status'="Cancer cause")
  result_All<-rbind(result_All,result)
}
result_All
{ #* TyG-BMI ALL Mortality ####
  model<- svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~ TyG_BMI_threshold + Age_status + Sex + Race_ethnicity + SES + 
                     Marital_status + Smoking_status + Drinking_status + BMI_status + Physical_status + 
                     HTN_status + T2D_status + HPL_status + Cohort, design = rhcSvy)
  model_result<-summary(model)
  P<-model_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="TyG-BMI",'status'="Cancer cause")
  result_All<-rbind(result_All,result)
}
result_All
{ #* Combine #####
  result.cause<-result_All
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  round_3_function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
  result.cause[result.cause=="00000"]<-"<0.001"
}

{#* Cbind #####
Table<-c("","TyG index ","","")
Table<-rbind(Table,c("", "< Inflection point",">= Inflection point","P"))
Table<-rbind(Table,c("All cause*","","",""))
Table<-rbind(Table,c("TyG (8.74)†","1.00 [Reference]",paste0(result.cause[1,1]," (",result.cause[1,2],", ",result.cause[1,3],")"),result.cause[1,4]))
Table<-rbind(Table,c("TyG-WC (855.70)‡","1.00 [Reference]",paste0(result.cause[2,1]," (",result.cause[2,2],", ",result.cause[2,3],")"),result.cause[2,4]))
Table<-rbind(Table,c("TyG-WHtR (5.16)§","1.00 [Reference]",paste0(result.cause[3,1]," (",result.cause[3,2],", ",result.cause[3,3],")"),result.cause[3,4]))
Table<-rbind(Table,c("TyG-BMI (241.74)¶","1.00 [Reference]",paste0(result.cause[4,1]," (",result.cause[4,2],", ",result.cause[4,3],")"),result.cause[4,4]))
Table<-rbind(Table,c("CVD cause","","",""))
Table<-rbind(Table,c("TyG (8.69)","1.00 [Reference]",paste0(result.cause[5,1]," (",result.cause[5,2],", ",result.cause[5,3],")"),result.cause[5,4]))
Table<-rbind(Table,c("TyG-WC (849.56)","1.00 [Reference]",paste0(result.cause[6,1]," (",result.cause[6,2],", ",result.cause[6,3],")"),result.cause[6,4]))
Table<-rbind(Table,c("TyG-WHtR (5.09)","1.00 [Reference]",paste0(result.cause[7,1]," (",result.cause[7,2],", ",result.cause[7,3],")"),result.cause[7,4]))
Table<-rbind(Table,c("TyG-BMI (241.63)","1.00 [Reference]",paste0(result.cause[8,1]," (",result.cause[8,2],", ",result.cause[8,3],")"),result.cause[8,4]))
Table<-rbind(Table,c("Cancer cause","","",""))
Table<-rbind(Table,c("TyG (8.69)","1.00 [Reference]",paste0(result.cause[9,1]," (",result.cause[9,2],", ",result.cause[9,3],")"),result.cause[9,4]))
Table<-rbind(Table,c("TyG-WC (848.12)","1.00 [Reference]",paste0(result.cause[10,1]," (",result.cause[10,2],", ",result.cause[10,3],")"),result.cause[10,4]))
Table<-rbind(Table,c("TyG-WHtR (5.05)","1.00 [Reference]",paste0(result.cause[11,1]," (",result.cause[11,2],", ",result.cause[11,3],")"),result.cause[11,4]))
Table<-rbind(Table,c("TyG-BMI (241.36)","1.00 [Reference]",paste0(result.cause[12,1]," (",result.cause[12,2],", ",result.cause[12,3],")"),result.cause[12,4]))
Table<-as.data.frame(Table)
Table= data.frame(lapply(Table, as.character), stringsAsFactors=FALSE)
Table
write.table(Table,sep = ",",file ="I:/NHANES study/PD&TYG&MO/Result/Table 4.csv",row.names =F,col.names =F )
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# +++++++++++============Figures==========+++++++++++ ####  
# >>>>> section 22 Figure 1 * Figure S4 ####  
load(file="I:/NHANES study/PD&TYG&MO/Data/Interpolation_weighted.Rdata")
{ #* Figure 1 (ALL Mortality) ####
  colnames(Interpolation_weighted)
  FIT_ALL_TYG<-survfit(Surv(peryear, MORT_stat==1) ~ TYG_quantile, Interpolation_weighted)
  #Table 
  FigureA <- ggsurvplot(
    FIT_ALL_TYG,
    conf.int = TRUE, fun = "cumhaz",  tables.theme = theme_cleantable(), 
    palette = c("#8686864C", "#EB7E60", "#F5A216", "#B46DA9") )
  Figure_S <- FigureA$plot
  Figure_S 
  ggsave("I:/NHANES study/PD&TYG&MO/Result/Figure s.pdf", plot = Figure_S , width = 8, height = 7)
  
  FigureA <- ggsurvplot(
    FIT_ALL_TYG,
    conf.int = TRUE, fun = "cumhaz",  tables.theme = theme_cleantable(), 
    palette = c("#8686864C", "#EB7E60", "#F5A216", "#B46DA9"), 
    ncensor.plot = FALSE, risk.table = FALSE, break.x.by = 5,
    font.x = c(25, "bold", "black"),legend = "none", 
    font.y = c(25, "bold", "black"),
    font.tickslab = c(25, "plain", "black"),
    xlab = "Time in Year",  pval = F,  legend.title = "", ylim = c(0, 1.5),ggtheme = theme_calc() +
      theme( panel.border = element_rect(color = "black", size = 2)))
  Figure_A <- FigureA$plot
  Figure_A 
  #P<0.0001
  ggsave("I:/NHANES study/PD&TYG&MO/Result/Figure 1a.pdf", plot = Figure_A , width = 8, height = 7)
  #TyG_WC_quantile
  FIT_ALL_TYG_WC<-survfit(Surv(peryear, MORT_stat==1) ~ TyG_WC_quantile, Interpolation_weighted)
  FigureB <- ggsurvplot(
    FIT_ALL_TYG_WC,
    conf.int = TRUE, fun = "cumhaz",  tables.theme = theme_cleantable(), 
    palette = c("#8686864C", "#EB7E60", "#F5A216", "#B46DA9"), 
    ncensor.plot = FALSE, risk.table = FALSE, break.x.by = 5,
    font.x = c(25, "bold", "black"),legend = "none", 
    font.y = c(25, "bold", "black"),
    font.tickslab = c(25, "plain", "black"),
    xlab = "Time in Year",  pval = F,  legend.title = "", ylim = c(0, 1.5),ggtheme = theme_calc() +
      theme( panel.border = element_rect(color = "black", size = 2)))
  Figure_B <- FigureB$plot
  #P<0.0001
  Figure_B 
  ggsave("I:/NHANES study/PD&TYG&MO/Result/Figure 1b.pdf", plot = Figure_B , width = 8, height = 7)
  #TyG_WHtR_quantile
  FIT_ALL_TYG_WHtR<-survfit(Surv(peryear, MORT_stat==1) ~ TyG_WHtR_quantile, Interpolation_weighted)
  FigureC <- ggsurvplot(
    FIT_ALL_TYG_WHtR,
    conf.int = TRUE, fun = "cumhaz",  tables.theme = theme_cleantable(), 
    palette = c("#8686864C", "#EB7E60", "#F5A216", "#B46DA9"), 
    ncensor.plot = FALSE, risk.table = FALSE, break.x.by = 5,
    font.x = c(25, "bold", "black"),legend = "none", 
    font.y = c(25, "bold", "black"),
    font.tickslab = c(25, "plain", "black"),
    xlab = "Time in Year",  pval = F,  legend.title = "", ylim = c(0, 1.5),ggtheme = theme_calc() +
      theme( panel.border = element_rect(color = "black", size = 2)))
  Figure_C <- FigureC$plot
  Figure_C 
  #P<0.0001
  ggsave("I:/NHANES study/PD&TYG&MO/Result/Figure 1c.pdf", plot = Figure_C , width = 8, height = 7)
  #TyG_BMI_quantile
  FIT_ALL_TYG_BMI<-survfit(Surv(peryear, MORT_stat==1) ~ TyG_BMI_quantile, Interpolation_weighted)
  FigureD <- ggsurvplot(
    FIT_ALL_TYG_BMI,
    conf.int = TRUE, fun = "cumhaz",  tables.theme = theme_cleantable(), 
    palette = c("#8686864C", "#EB7E60", "#F5A216", "#B46DA9"), 
    ncensor.plot = FALSE, risk.table = FALSE, break.x.by = 5,
    font.x = c(25, "bold", "black"),legend = "none", 
    font.y = c(25, "bold", "black"),
    font.tickslab = c(25, "plain", "black"),
    xlab = "Time in Year",  pval = F,  legend.title = "", ylim = c(0, 1.5),ggtheme = theme_calc() +
      theme( panel.border = element_rect(color = "black", size = 2)))
  Figure_D <- FigureD$plot
  Figure_D 
  #P=0.27
  ggsave("I:/NHANES study/PD&TYG&MO/Result/Figure 1d.pdf", plot = Figure_D , width = 8, height = 7)
  
} 
{ #* Figure S4 (CVD Mortality) ####
  colnames(Interpolation_weighted)
  FIT_CVD_TYG<-survfit(Surv(peryear, CVD_MORT_stat==1) ~ TYG_quantile, Interpolation_weighted)
  
  FigureA <- ggsurvplot(
    FIT_CVD_TYG,
    conf.int = TRUE, fun = "cumhaz",  tables.theme = theme_cleantable(), 
    palette = c("#8686864C", "#EB7E60", "#F5A216", "#B46DA9"), 
    ncensor.plot = FALSE, risk.table = FALSE, break.x.by = 5,
    font.x = c(25, "bold", "black"),legend = "none", 
    font.y = c(25, "bold", "black"),
    font.tickslab = c(25, "plain", "black"),
    xlab = "Time in Year",  pval = F,  legend.title = "", ylim = c(0, 0.5),ggtheme = theme_calc() +
      theme( panel.border = element_rect(color = "black", size = 2)))
  Figure_A <- FigureA$plot
  Figure_A 
  #P<0.0001
  ggsave("I:/NHANES study/PD&TYG&MO/Result/Supplementary Fig. 4a.pdf", plot = Figure_A , width = 8, height = 7)
  #TyG_WC_quantile
  FIT_CVD_TYG_WC<-survfit(Surv(peryear, CVD_MORT_stat==1) ~ TyG_WC_quantile, Interpolation_weighted)
  FigureB <- ggsurvplot(
    FIT_CVD_TYG_WC,
    conf.int = TRUE, fun = "cumhaz",  tables.theme = theme_cleantable(), 
    palette = c("#8686864C", "#EB7E60", "#F5A216", "#B46DA9"), 
    ncensor.plot = FALSE, risk.table = FALSE, break.x.by = 5,
    font.x = c(25, "bold", "black"),legend = "none", 
    font.y = c(25, "bold", "black"),
    font.tickslab = c(25, "plain", "black"),
    xlab = "Time in Year",  pval = F,  legend.title = "", ylim = c(0, 0.5),ggtheme = theme_calc() +
      theme( panel.border = element_rect(color = "black", size = 2)))
  Figure_B <- FigureB$plot
  #P<0.0001
  Figure_B 
  ggsave("I:/NHANES study/PD&TYG&MO/Result/Supplementary Fig. 4b.pdf", plot = Figure_B , width = 8, height = 7)
  #TyG_WHtR_quantile
  FIT_CVD_TYG_WHtR<-survfit(Surv(peryear, CVD_MORT_stat==1) ~ TyG_WHtR_quantile, Interpolation_weighted)
  FigureC <- ggsurvplot(
    FIT_CVD_TYG_WHtR,
    conf.int = TRUE, fun = "cumhaz",  tables.theme = theme_cleantable(), 
    palette = c("#8686864C", "#EB7E60", "#F5A216", "#B46DA9"), 
    ncensor.plot = FALSE, risk.table = FALSE, break.x.by = 5,
    font.x = c(25, "bold", "black"),legend = "none", 
    font.y = c(25, "bold", "black"),
    font.tickslab = c(25, "plain", "black"),
    xlab = "Time in Year",  pval =F,  legend.title = "", ylim = c(0, 0.5),ggtheme = theme_calc() +
      theme( panel.border = element_rect(color = "black", size = 2)))
  Figure_C <- FigureC$plot
  Figure_C 
  #P<0.0001
  ggsave("I:/NHANES study/PD&TYG&MO/Result/Supplementary Fig. 4c.pdf", plot = Figure_C , width = 8, height = 7)
  
  #TyG_BMI_quantile
  FIT_CVD_TYG_BMI<-survfit(Surv(peryear, CVD_MORT_stat==1) ~ TyG_BMI_quantile, Interpolation_weighted)
  FigureD <- ggsurvplot(
    FIT_CVD_TYG_BMI,
    conf.int = TRUE, fun = "cumhaz",  tables.theme = theme_cleantable(), 
    palette = c("#8686864C", "#EB7E60", "#F5A216", "#B46DA9"), 
    ncensor.plot = FALSE, risk.table = FALSE, break.x.by = 5,
    font.x = c(25, "bold", "black"),legend = "none", 
    font.y = c(25, "bold", "black"),
    font.tickslab = c(25, "plain", "black"),
    xlab = "Time in Year",  pval = F,  legend.title = "", ylim = c(0, 0.5),ggtheme = theme_calc() +
      theme( panel.border = element_rect(color = "black", size = 2)))
  Figure_D <- FigureD$plot
  Figure_D 
  #P=0.072
  ggsave("I:/NHANES study/PD&TYG&MO/Result/Supplementary Fig. 4d.pdf", plot = Figure_D , width = 8, height = 7)
  
} 
{ #* Figure S5 (Cancer Mortality) ####
  colnames(Interpolation_weighted)
  FIT_Cancer_TYG<-survfit(Surv(peryear, Cancer_MORT_stat==1) ~ TYG_quantile, Interpolation_weighted)
  
  FigureA <- ggsurvplot(
    FIT_Cancer_TYG,
    conf.int = TRUE, fun = "cumhaz",  tables.theme = theme_cleantable(), 
    palette = c("#8686864C", "#EB7E60", "#F5A216", "#B46DA9"), 
    ncensor.plot = FALSE, risk.table = FALSE, break.x.by = 5,
    font.x = c(25, "bold", "black"),legend = "none", 
    font.y = c(25, "bold", "black"),
    font.tickslab = c(25, "plain", "black"),
    xlab = "Time in Year",  pval = F,  legend.title = "", ylim = c(0, 0.5),ggtheme = theme_calc() +
      theme( panel.border = element_rect(color = "black", size = 2)))
  Figure_A <- FigureA$plot
  Figure_A 
  #P<0.0001
  ggsave("I:/NHANES study/PD&TYG&MO/Result/Supplementary Fig. 4e.pdf", plot = Figure_A , width = 8, height = 7)
  #TyG_WC_quantile
  FIT_Cancer_TYG_WC<-survfit(Surv(peryear, Cancer_MORT_stat==1) ~ TyG_WC_quantile, Interpolation_weighted)
  FigureB <- ggsurvplot(
    FIT_Cancer_TYG_WC,
    conf.int = TRUE, fun = "cumhaz",  tables.theme = theme_cleantable(), 
    palette = c("#8686864C", "#EB7E60", "#F5A216", "#B46DA9"), 
    ncensor.plot = FALSE, risk.table = FALSE, break.x.by = 5,
    font.x = c(25, "bold", "black"),legend = "none", 
    font.y = c(25, "bold", "black"),
    font.tickslab = c(25, "plain", "black"),
    xlab = "Time in Year",  pval = F,  legend.title = "", ylim = c(0, 0.5),ggtheme = theme_calc() +
      theme( panel.border = element_rect(color = "black", size = 2)))
  Figure_B <- FigureB$plot
  #P<0.0001
  Figure_B 
  ggsave("I:/NHANES study/PD&TYG&MO/Result/Supplementary Fig. 4f.pdf", plot = Figure_B , width = 8, height = 7)
  #TyG_WHtR_quantile
  FIT_Cancer_TYG_WHtR<-survfit(Surv(peryear, Cancer_MORT_stat==1) ~ TyG_WHtR_quantile, Interpolation_weighted)
  FigureC <- ggsurvplot(
    FIT_Cancer_TYG_WHtR,
    conf.int = TRUE, fun = "cumhaz",  tables.theme = theme_cleantable(), 
    palette = c("#8686864C", "#EB7E60", "#F5A216", "#B46DA9"), 
    ncensor.plot = FALSE, risk.table = FALSE, break.x.by = 5,
    font.x = c(25, "bold", "black"),legend = "none", 
    font.y = c(25, "bold", "black"),
    font.tickslab = c(25, "plain", "black"),
    xlab = "Time in Year",  pval =F,  legend.title = "", ylim = c(0, 0.5),ggtheme = theme_calc() +
      theme( panel.border = element_rect(color = "black", size = 2)))
  Figure_C <- FigureC$plot
  Figure_C 
  #P<0.0001
  ggsave("I:/NHANES study/PD&TYG&MO/Result/Supplementary Fig. 4g.pdf", plot = Figure_C , width = 8, height = 7)
  
  #TyG_BMI_quantile
  FIT_Cancer_TYG_BMI<-survfit(Surv(peryear, Cancer_MORT_stat==1) ~ TyG_BMI_quantile, Interpolation_weighted)
  FigureD <- ggsurvplot(
    FIT_Cancer_TYG_BMI,
    conf.int = TRUE, fun = "cumhaz",  tables.theme = theme_cleantable(), 
    palette = c("#8686864C", "#EB7E60", "#F5A216", "#B46DA9"), 
    ncensor.plot = FALSE, risk.table = FALSE, break.x.by = 5,
    font.x = c(25, "bold", "black"),legend = "none", 
    font.y = c(25, "bold", "black"),
    font.tickslab = c(25, "plain", "black"),
    xlab = "Time in Year",  pval = F,  legend.title = "", ylim = c(0, 0.5),ggtheme = theme_calc() +
      theme( panel.border = element_rect(color = "black", size = 2)))
  Figure_D <- FigureD$plot
  Figure_D 
  #P=0.27
  ggsave("I:/NHANES study/PD&TYG&MO/Result/Supplementary Fig. 4h.pdf", plot = Figure_D , width = 8, height = 7)
  
} 
# >>>>> section 23 RCS for TyGs (Figure 2, Figure S6, S7) ####
setwd("I:/NHANES study/PD&TYG&MO/Result")

{#* All cause mortarity ####
  
  {#** TYG ####
    load(file="I:/NHANES study/PD&TYG&MO/Data/Interpolation_weighted.Rdata")
    RCS <- Interpolation_weighted
    pdf("Figure 2a.pdf",width=8,height=7)
    options(datadist = "ddist")
    ddist <- datadist(RCS)
    limUp <- 3 * IQR(RCS[,"TYG"], na.rm = TRUE) + quantile(RCS[,"TYG"], 3/4, na.rm = TRUE, names = FALSE)
    limDown <- quantile(RCS[,"TYG"], 1/4, na.rm = TRUE, names = FALSE) - 3 * IQR(RCS[,"TYG"], na.rm = TRUE)
    RCS <- RCS[!RCS[,"TYG"] >= limUp & !RCS[,"TYG"] <= limDown,]
    RCS$MORT_stat <- as.numeric(RCS$MORT_stat)
    fit <- cph(Surv(peryear, MORT_stat) ~ rcs(TYG, 3) + Age_status + Sex + Race_ethnicity + SES + Marital_status +
                 Smoking_status + Drinking_status + BMI_status + Physical_status +
                 HTN_status + T2D_status + HPL_status + Cohort, data = RCS, x = TRUE, y = TRUE)
    anova(fit)
    cox.zph(fit, "rank")
    p <- round(anova(fit)[, 3], 3)
    options(datadist = "ddist")
    pred_OR <- Predict(fit, TYG, ref.zero = TRUE, fun = exp)
    pred_OR$distance_to_1 <- abs(pred_OR$yhat - 1)
    pred_OR$distance_to_2 <- abs(pred_OR$lower - 1)
    pred_OR$distance_to_3 <- abs(pred_OR$upper - 1)
    pred_OR$distance<-pred_OR$distance_to_1+pred_OR$distance_to_2+pred_OR$distance_to_3
    # 找到最接近1的点
    closest_point <- pred_OR[which.min(pred_OR$distance), ]
    # 提取最接近1的TYG值
    closest_TYG_value <- closest_point$TYG
    closest_TYG_value
    refvalue <- round(closest_TYG_value, 2)
    
    violet <- "#96C37D"
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd = NA)
    ylim.bot <- min(pred_OR[,"lower"])
    ylim.top <- max(pred_OR[,"upper"])
    
    dens <- density(RCS$TYG)
    
    # 绘制密度图，调整颜色和透明度
    plot(dens$x, dens$y, col = ggplot2::alpha(violet, 0.5), type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
         lwd = 1.7) # 加粗线条
    polygon(dens$x, dens$y, col = ggplot2::alpha(violet, 0.5), border = ggplot2::alpha(violet, 0.5)) 
    
    axis(side = 4, cex.axis =1.7) # 增加右侧坐标轴字体大小
    par(new = TRUE)
    
    # 绘制HR vs TyG指数
    plot(pred_OR[, 1], pred_OR[,"yhat"], xlab = " ", ylab = " ", type = "l", ylim = c(ylim.bot, ylim.top),
         col = "#CC5B45", lwd = 3, cex.axis =1.7,cex.lab=1.7) # 红色粗线条
    polygon(c(pred_OR[,1], rev(pred_OR[,1])), 
            c(pred_OR[,"upper"], rev(pred_OR[,"lower"])), 
            col = ggplot2::alpha("#EB7E60", 0.3), border = NA)  # 在置信区间之间填充阴影
    
    lines(pred_OR[, 1], pred_OR[,"lower"], lty = 2, lwd = 2.5,col ="#6A8EC9")
    lines(pred_OR[, 1], pred_OR[,"upper"], lty = 2, lwd = 2.5,col ="#6A8EC9")
    
    lines(x = range(pred_OR[, 1]), y = c(1, 1), lty = 3, col = "#652884", lwd = 3.5) # 添加水平参考线
    
    points(refvalue, 1, pch = 16, cex = 1.5) # 加大标记点尺寸
    box(lwd = 3)
    
    dev.off()
  }
  p
  #0.967
  refvalue 
  #8.729
  {#** TyG_WC  ####
    load(file="I:/NHANES study/PD&TYG&MO/Data/Interpolation_weighted.Rdata")
    RCS <- Interpolation_weighted
    pdf("Figure 2b.pdf",width=8,height=7)
    options(datadist = "ddist")
    ddist <- datadist(RCS)
    limUp <- 3 * IQR(RCS[,"TyG_WC"], na.rm = TRUE) + quantile(RCS[,"TyG_WC"], 3/4, na.rm = TRUE, names = FALSE)
    limDown <- quantile(RCS[,"TyG_WC"], 1/4, na.rm = TRUE, names = FALSE) - 3 * IQR(RCS[,"TyG_WC"], na.rm = TRUE)
    RCS <- RCS[!RCS[,"TyG_WC"] >= limUp & !RCS[,"TyG_WC"] <= limDown,]
    RCS$MORT_stat <- as.numeric(RCS$MORT_stat)
    fit <- cph(Surv(peryear, MORT_stat) ~ rcs(TyG_WC, 3) + Age_status + Sex + Race_ethnicity + SES + Marital_status +
                 Smoking_status + Drinking_status + BMI_status + Physical_status +
                 HTN_status + T2D_status + HPL_status + Cohort, data = RCS, x = TRUE, y = TRUE)
    anova(fit)
    cox.zph(fit, "rank")
    p <- round(anova(fit)[, 3], 3)
    options(datadist = "ddist")
    pred_OR <- Predict(fit, TyG_WC, ref.zero = TRUE, fun = exp)
    pred_OR$distance_to_1 <- abs(pred_OR$yhat - 1)
    pred_OR$distance_to_2 <- abs(pred_OR$lower - 1)
    pred_OR$distance_to_3 <- abs(pred_OR$upper - 1)
    pred_OR$distance<-pred_OR$distance_to_1+pred_OR$distance_to_2+pred_OR$distance_to_3
    # 找到最接近1的点
    closest_point <- pred_OR[which.min(pred_OR$distance), ]
    # 提取最接近1的TyG_WC值
    closest_TyG_WC_value <- closest_point$TyG_WC
    closest_TyG_WC_value
    refvalue <- round(closest_TyG_WC_value, 2)
    
    violet <- "#5CB0C3"
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd = NA)
    ylim.bot <- min(pred_OR[,"lower"])
    ylim.top <- max(pred_OR[,"upper"])
    
    dens <- density(RCS$TyG_WC)
    
    # 绘制密度图，调整颜色和透明度
    plot(dens$x, dens$y, col = ggplot2::alpha(violet, 0.5), type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
         lwd = 1.7) # 加粗线条
    polygon(dens$x, dens$y, col = ggplot2::alpha(violet, 0.5), border = ggplot2::alpha(violet, 0.5)) 
    
    axis(side = 4, cex.axis =1.7) # 增加右侧坐标轴字体大小
    par(new = TRUE)
    
    # 绘制HR vs TyG_WC指数
    plot(pred_OR[, 1], pred_OR[,"yhat"], xlab = " ", ylab = " ", type = "l", ylim = c(ylim.bot, ylim.top),
         col = "#CC5B45", lwd = 3, cex.axis =1.7,cex.lab=1.7) # 红色粗线条
    polygon(c(pred_OR[,1], rev(pred_OR[,1])), 
            c(pred_OR[,"upper"], rev(pred_OR[,"lower"])), 
            col = ggplot2::alpha("#EB7E60", 0.3), border = NA)  # 在置信区间之间填充阴影
    
    lines(pred_OR[, 1], pred_OR[,"lower"], lty = 2, lwd = 2.5,col ="#6A8EC9")
    lines(pred_OR[, 1], pred_OR[,"upper"], lty = 2, lwd = 2.5,col ="#6A8EC9")
    
    lines(x = range(pred_OR[, 1]), y = c(1, 1), lty = 3, col = "#652884", lwd = 3.5) # 添加水平参考线
    
    points(refvalue, 1, pch = 16, cex = 1.5) # 加大标记点尺寸
    box(lwd = 3)
    
    dev.off()
  }
  p
  #0.967
  refvalue 
  #8.729
  
  {#** TyG_WHtR  ####
    load(file="I:/NHANES study/PD&TYG&MO/Data/Interpolation_weighted.Rdata")
    RCS <- Interpolation_weighted
    pdf("Figure 2c.pdf",width=8,height=7)
    options(datadist = "ddist")
    ddist <- datadist(RCS)
    limUp <- 3 * IQR(RCS[,"TyG_WHtR"], na.rm = TRUE) + quantile(RCS[,"TyG_WHtR"], 3/4, na.rm = TRUE, names = FALSE)
    limDown <- quantile(RCS[,"TyG_WHtR"], 1/4, na.rm = TRUE, names = FALSE) - 3 * IQR(RCS[,"TyG_WHtR"], na.rm = TRUE)
    RCS <- RCS[!RCS[,"TyG_WHtR"] >= limUp & !RCS[,"TyG_WHtR"] <= limDown,]
    RCS$MORT_stat <- as.numeric(RCS$MORT_stat)
    fit <- cph(Surv(peryear, MORT_stat) ~ rcs(TyG_WHtR, 3) + Age_status + Sex + Race_ethnicity + SES + Marital_status +
                 Smoking_status + Drinking_status + BMI_status + Physical_status +
                 HTN_status + T2D_status + HPL_status + Cohort, data = RCS, x = TRUE, y = TRUE)
    anova(fit)
    cox.zph(fit, "rank")
    p <- round(anova(fit)[, 3], 3)
    ddist$limits$BMI[2] <- refvalue
    options(datadist = "ddist")
    pred_OR <- Predict(fit, TyG_WHtR, ref.zero = TRUE, fun = exp)
    pred_OR$distance_to_1 <- abs(pred_OR$yhat - 1)
    pred_OR$distance_to_2 <- abs(pred_OR$lower - 1)
    pred_OR$distance_to_3 <- abs(pred_OR$upper - 1)
    pred_OR$distance<-pred_OR$distance_to_1+pred_OR$distance_to_2+pred_OR$distance_to_3
    # 找到最接近1的点
    closest_point <- pred_OR[which.min(pred_OR$distance), ]
    # 提取最接近1的TyG_WHtR值
    closest_TyG_WHtR_value <- closest_point$TyG_WHtR
    closest_TyG_WHtR_value
    refvalue <- round(closest_TyG_WHtR_value, 2)
    
    violet <- "#F5A216"
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd = NA)
    ylim.bot <- min(pred_OR[,"lower"])
    ylim.top <- max(pred_OR[,"upper"])
    
    dens <- density(RCS$TyG_WHtR)
    
    # 绘制密度图，调整颜色和透明度
    plot(dens$x, dens$y, col = ggplot2::alpha(violet, 0.5), type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
         lwd = 1.7) # 加粗线条
    polygon(dens$x, dens$y, col = ggplot2::alpha(violet, 0.5), border = ggplot2::alpha(violet, 0.5)) 
    
    axis(side = 4, cex.axis =1.7) # 增加右侧坐标轴字体大小
    par(new = TRUE)
    
    # 绘制HR vs TyG_WHtR指数
    plot(pred_OR[, 1], pred_OR[,"yhat"], xlab = " ", ylab = " ", type = "l", ylim = c(ylim.bot, ylim.top),
         col = "#CC5B45", lwd = 3, cex.axis =1.7,cex.lab=1.7) # 红色粗线条
    polygon(c(pred_OR[,1], rev(pred_OR[,1])), 
            c(pred_OR[,"upper"], rev(pred_OR[,"lower"])), 
            col = ggplot2::alpha("#EB7E60", 0.3), border = NA)  # 在置信区间之间填充阴影
    
    lines(pred_OR[, 1], pred_OR[,"lower"], lty = 2, lwd = 2.5,col ="#6A8EC9")
    lines(pred_OR[, 1], pred_OR[,"upper"], lty = 2, lwd = 2.5,col ="#6A8EC9")
    
    lines(x = range(pred_OR[, 1]), y = c(1, 1), lty = 3, col = "#652884", lwd = 3.5) # 添加水平参考线
    
    points(refvalue, 1, pch = 16, cex = 1.5) # 加大标记点尺寸
    box(lwd = 3)
    
    dev.off()
  }
  p
  #0.967
  refvalue 
  #8.729
  
  {#** TyG_BMI  ####
    load(file="I:/NHANES study/PD&TYG&MO/Data/Interpolation_weighted.Rdata")
    RCS <- Interpolation_weighted
    pdf("Figure 2d.pdf",width=8,height=7)
    options(datadist = "ddist")
    ddist <- datadist(RCS)
    limUp <- 3 * IQR(RCS[,"TyG_BMI"], na.rm = TRUE) + quantile(RCS[,"TyG_BMI"], 3/4, na.rm = TRUE, names = FALSE)
    limDown <- quantile(RCS[,"TyG_BMI"], 1/4, na.rm = TRUE, names = FALSE) - 3 * IQR(RCS[,"TyG_BMI"], na.rm = TRUE)
    RCS <- RCS[!RCS[,"TyG_BMI"] >= limUp & !RCS[,"TyG_BMI"] <= limDown,]
    RCS$MORT_stat <- as.numeric(RCS$MORT_stat)
    fit <- cph(Surv(peryear, MORT_stat) ~ rcs(TyG_BMI, 3) + Age_status + Sex + Race_ethnicity + SES + Marital_status +
                 Smoking_status + Drinking_status + BMI_status + Physical_status +
                 HTN_status + T2D_status + HPL_status + Cohort, data = RCS, x = TRUE, y = TRUE)
    anova(fit)
    cox.zph(fit, "rank")
    p <- round(anova(fit)[, 3], 3)
    options(datadist = "ddist")
    pred_OR <- Predict(fit, TyG_BMI, ref.zero = TRUE, fun = exp)
    pred_OR$distance_to_1 <- abs(pred_OR$yhat - 1)
    pred_OR$distance_to_2 <- abs(pred_OR$lower - 1)
    pred_OR$distance_to_3 <- abs(pred_OR$upper - 1)
    pred_OR$distance<-pred_OR$distance_to_1+pred_OR$distance_to_2+pred_OR$distance_to_3
    # 找到最接近1的点
    closest_point <- pred_OR[which.min(pred_OR$distance), ]
    # 提取最接近1的TyG_BMI值
    closest_TyG_BMI_value <- closest_point$TyG_BMI
    closest_TyG_BMI_value
    refvalue <- round(closest_TyG_BMI_value, 2)
    
    violet <- "#B46DA9"
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd = NA)
    ylim.bot <- min(pred_OR[,"lower"])
    ylim.top <- max(pred_OR[,"upper"])
    
    dens <- density(RCS$TyG_BMI)
    
    # 绘制密度图，调整颜色和透明度
    plot(dens$x, dens$y, col = ggplot2::alpha(violet, 0.5), type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
         lwd = 1.7) # 加粗线条
    polygon(dens$x, dens$y, col = ggplot2::alpha(violet, 0.5), border = ggplot2::alpha(violet, 0.5)) 
    
    axis(side = 4, cex.axis =1.7) # 增加右侧坐标轴字体大小
    par(new = TRUE)
    
    # 绘制HR vs TyG_BMI指数
    plot(pred_OR[, 1], pred_OR[,"yhat"], xlab = " ", ylab = " ", type = "l", ylim = c(ylim.bot, ylim.top),
         col = "#CC5B45", lwd = 3, cex.axis =1.7,cex.lab=1.7) # 红色粗线条
    polygon(c(pred_OR[,1], rev(pred_OR[,1])), 
            c(pred_OR[,"upper"], rev(pred_OR[,"lower"])), 
            col = ggplot2::alpha("#EB7E60", 0.3), border = NA)  # 在置信区间之间填充阴影
    
    lines(pred_OR[, 1], pred_OR[,"lower"], lty = 2, lwd = 2.5,col ="#6A8EC9")
    lines(pred_OR[, 1], pred_OR[,"upper"], lty = 2, lwd = 2.5,col ="#6A8EC9")
    
    lines(x = range(pred_OR[, 1]), y = c(1, 1), lty = 3, col = "#652884", lwd = 3.5) # 添加水平参考线
    
    points(refvalue, 1, pch = 16, cex = 1.5) # 加大标记点尺寸
    box(lwd = 3)
    
    dev.off()
  }
  p
  #0.967
  refvalue 
  #8.729
}
{#* CVD cause mortarity ####
  
  {#** TYG ####
    load(file="I:/NHANES study/PD&TYG&MO/Data/Interpolation_weighted.Rdata")
    RCS <- subset(Interpolation_weighted,CVD_MORT_stat!=2)
    pdf("Supplementary Fig. 6a.pdf",width=8,height=7)
    options(datadist = "ddist")
    ddist <- datadist(RCS)
    limUp <- 3 * IQR(RCS[,"TYG"], na.rm = TRUE) + quantile(RCS[,"TYG"], 3/4, na.rm = TRUE, names = FALSE)
    limDown <- quantile(RCS[,"TYG"], 1/4, na.rm = TRUE, names = FALSE) - 3 * IQR(RCS[,"TYG"], na.rm = TRUE)
    RCS <- RCS[!RCS[,"TYG"] >= limUp & !RCS[,"TYG"] <= limDown,]
    RCS$Cancer_MORT_stat <- as.numeric(RCS$CVD_MORT_stat)
    fit <- cph(Surv(peryear, CVD_MORT_stat) ~ rcs(TYG, 4) + Age_status + Sex + Race_ethnicity + SES + Marital_status +
                 Smoking_status + Drinking_status + BMI_status + Physical_status +
                 HTN_status + T2D_status + HPL_status + Cohort, data = RCS, x = TRUE, y = TRUE)
    anova(fit)
    cox.zph(fit, "rank")
    p <- round(anova(fit)[, 3], 3)
    options(datadist = "ddist")
    pred_OR <- Predict(fit, TYG, ref.zero = TRUE, fun = exp)
    pred_OR$distance_to_1 <- abs(pred_OR$yhat - 1)
    pred_OR$distance_to_2 <- abs(pred_OR$lower - 1)
    pred_OR$distance_to_3 <- abs(pred_OR$upper - 1)
    pred_OR$distance<-pred_OR$distance_to_1+pred_OR$distance_to_2+pred_OR$distance_to_3
    # 找到最接近1的点
    closest_point <- pred_OR[which.min(pred_OR$distance), ]
    # 提取最接近1的TYG值
    closest_TYG_value <- closest_point$TYG
    closest_TYG_value
    refvalue <- round(closest_TYG_value, 2)
    
    violet <- "#96C37D"
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd = NA)
    ylim.bot <- min(pred_OR[,"lower"])
    ylim.top <- max(pred_OR[,"upper"])
    
    dens <- density(RCS$TYG)
    
    # 绘制密度图，调整颜色和透明度
    plot(dens$x, dens$y, col = ggplot2::alpha(violet, 0.5), type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
         lwd = 1.7) # 加粗线条
    polygon(dens$x, dens$y, col = ggplot2::alpha(violet, 0.5), border = ggplot2::alpha(violet, 0.5)) 
    
    axis(side = 4, cex.axis =1.7) # 增加右侧坐标轴字体大小
    par(new = TRUE)
    
    # 绘制HR vs TyG指数
    plot(pred_OR[, 1], pred_OR[,"yhat"], xlab = " ", ylab = " ", type = "l", ylim = c(ylim.bot, ylim.top),
         col = "#CC5B45", lwd = 3, cex.axis =1.7,cex.lab=1.7) # 红色粗线条
    polygon(c(pred_OR[,1], rev(pred_OR[,1])), 
            c(pred_OR[,"upper"], rev(pred_OR[,"lower"])), 
            col = ggplot2::alpha("#EB7E60", 0.3), border = NA)  # 在置信区间之间填充阴影
    
    lines(pred_OR[, 1], pred_OR[,"lower"], lty = 2, lwd = 2.5,col ="#6A8EC9")
    lines(pred_OR[, 1], pred_OR[,"upper"], lty = 2, lwd = 2.5,col ="#6A8EC9")
    
    lines(x = range(pred_OR[, 1]), y = c(1, 1), lty = 3, col = "#652884", lwd = 3.5) # 添加水平参考线
    
    points(refvalue, 1, pch = 16, cex = 1.5) # 加大标记点尺寸
    box(lwd = 3)
    
    dev.off()
  }
  p
  #0.967
  refvalue 
  #8.729
  {#** TyG_WC  ####
    load(file="I:/NHANES study/PD&TYG&MO/Data/Interpolation_weighted.Rdata")
    RCS <- subset(Interpolation_weighted,CVD_MORT_stat!=2)
    pdf("Supplementary Fig. 6b.pdf",width=8,height=7)
    options(datadist = "ddist")
    ddist <- datadist(RCS)
    limUp <- 3 * IQR(RCS[,"TyG_WC"], na.rm = TRUE) + quantile(RCS[,"TyG_WC"], 3/4, na.rm = TRUE, names = FALSE)
    limDown <- quantile(RCS[,"TyG_WC"], 1/4, na.rm = TRUE, names = FALSE) - 3 * IQR(RCS[,"TyG_WC"], na.rm = TRUE)
    RCS <- RCS[!RCS[,"TyG_WC"] >= limUp & !RCS[,"TyG_WC"] <= limDown,]
    RCS$CVD_MORT_stat <- as.numeric(RCS$CVD_MORT_stat)
    fit <- cph(Surv(peryear, CVD_MORT_stat) ~ rcs(TyG_WC, 3) + Age_status + Sex + Race_ethnicity + SES + Marital_status +
                 Smoking_status + Drinking_status + BMI_status + Physical_status +
                 HTN_status + T2D_status + HPL_status + Cohort, data = RCS, x = TRUE, y = TRUE)
    anova(fit)
    cox.zph(fit, "rank")
    p <- round(anova(fit)[, 3], 3)
    options(datadist = "ddist")
    pred_OR <- Predict(fit, TyG_WC, ref.zero = TRUE, fun = exp)
    pred_OR$distance_to_1 <- abs(pred_OR$yhat - 1)
    pred_OR$distance_to_2 <- abs(pred_OR$lower - 1)
    pred_OR$distance_to_3 <- abs(pred_OR$upper - 1)
    pred_OR$distance<-pred_OR$distance_to_1+pred_OR$distance_to_2+pred_OR$distance_to_3
    # 找到最接近1的点
    closest_point <- pred_OR[which.min(pred_OR$distance), ]
    # 提取最接近1的TyG_WC值
    closest_TyG_WC_value <- closest_point$TyG_WC
    closest_TyG_WC_value
    refvalue <- round(closest_TyG_WC_value, 2)
    
    violet <- "#5CB0C3"
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd = NA)
    ylim.bot <- min(pred_OR[,"lower"])
    ylim.top <- max(pred_OR[,"upper"])
    
    dens <- density(RCS$TyG_WC)
    
    # 绘制密度图，调整颜色和透明度
    plot(dens$x, dens$y, col = ggplot2::alpha(violet, 0.5), type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
         lwd = 1.7) # 加粗线条
    polygon(dens$x, dens$y, col = ggplot2::alpha(violet, 0.5), border = ggplot2::alpha(violet, 0.5)) 
    
    axis(side = 4, cex.axis =1.7) # 增加右侧坐标轴字体大小
    par(new = TRUE)
    
    # 绘制HR vs TyG_WC指数
    plot(pred_OR[, 1], pred_OR[,"yhat"], xlab = " ", ylab = " ", type = "l", ylim = c(ylim.bot, ylim.top),
         col = "#CC5B45", lwd = 3, cex.axis =1.7,cex.lab=1.7) # 红色粗线条
    polygon(c(pred_OR[,1], rev(pred_OR[,1])), 
            c(pred_OR[,"upper"], rev(pred_OR[,"lower"])), 
            col = ggplot2::alpha("#EB7E60", 0.3), border = NA)  # 在置信区间之间填充阴影
    
    lines(pred_OR[, 1], pred_OR[,"lower"], lty = 2, lwd = 2.5,col ="#6A8EC9")
    lines(pred_OR[, 1], pred_OR[,"upper"], lty = 2, lwd = 2.5,col ="#6A8EC9")
    
    lines(x = range(pred_OR[, 1]), y = c(1, 1), lty = 3, col = "#652884", lwd = 3.5) # 添加水平参考线
    
    points(refvalue, 1, pch = 16, cex = 1.5) # 加大标记点尺寸
    box(lwd = 3)
    
    dev.off()
  }
  p
  #0.967
  refvalue 
  #8.729
  
  {#** TyG_WHtR  ####
    load(file="I:/NHANES study/PD&TYG&MO/Data/Interpolation_weighted.Rdata")
    RCS <- subset(Interpolation_weighted,CVD_MORT_stat!=2)
    pdf("Supplementary Fig. 6c.pdf",width=8,height=7)
    options(datadist = "ddist")
    ddist <- datadist(RCS)
    limUp <- 3 * IQR(RCS[,"TyG_WHtR"], na.rm = TRUE) + quantile(RCS[,"TyG_WHtR"], 3/4, na.rm = TRUE, names = FALSE)
    limDown <- quantile(RCS[,"TyG_WHtR"], 1/4, na.rm = TRUE, names = FALSE) - 3 * IQR(RCS[,"TyG_WHtR"], na.rm = TRUE)
    RCS <- RCS[!RCS[,"TyG_WHtR"] >= limUp & !RCS[,"TyG_WHtR"] <= limDown,]
    RCS$CVD_MORT_stat <- as.numeric(RCS$CVD_MORT_stat)
    fit <- cph(Surv(peryear, CVD_MORT_stat) ~ rcs(TyG_WHtR, 3) + Age_status + Sex + Race_ethnicity + SES + Marital_status +
                 Smoking_status + Drinking_status + BMI_status + Physical_status +
                 HTN_status + T2D_status + HPL_status + Cohort, data = RCS, x = TRUE, y = TRUE)
    anova(fit)
    cox.zph(fit, "rank")
    p <- round(anova(fit)[, 3], 3)
    ddist$limits$BMI[2] <- refvalue
    options(datadist = "ddist")
    pred_OR <- Predict(fit, TyG_WHtR, ref.zero = TRUE, fun = exp)
    pred_OR$distance_to_1 <- abs(pred_OR$yhat - 1)
    pred_OR$distance_to_2 <- abs(pred_OR$lower - 1)
    pred_OR$distance_to_3 <- abs(pred_OR$upper - 1)
    pred_OR$distance<-pred_OR$distance_to_1+pred_OR$distance_to_2+pred_OR$distance_to_3
    # 找到最接近1的点
    closest_point <- pred_OR[which.min(pred_OR$distance), ]
    # 提取最接近1的TyG_WHtR值
    closest_TyG_WHtR_value <- closest_point$TyG_WHtR
    closest_TyG_WHtR_value
    refvalue <- round(closest_TyG_WHtR_value, 2)
    
    violet <- "#F5A216"
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd = NA)
    ylim.bot <- min(pred_OR[,"lower"])
    ylim.top <- max(pred_OR[,"upper"])
    
    dens <- density(RCS$TyG_WHtR)
    
    # 绘制密度图，调整颜色和透明度
    plot(dens$x, dens$y, col = ggplot2::alpha(violet, 0.5), type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
         lwd = 1.7) # 加粗线条
    polygon(dens$x, dens$y, col = ggplot2::alpha(violet, 0.5), border = ggplot2::alpha(violet, 0.5)) 
    
    axis(side = 4, cex.axis =1.7) # 增加右侧坐标轴字体大小
    par(new = TRUE)
    
    # 绘制HR vs TyG_WHtR指数
    plot(pred_OR[, 1], pred_OR[,"yhat"], xlab = " ", ylab = " ", type = "l", ylim = c(ylim.bot, ylim.top),
         col = "#CC5B45", lwd = 3, cex.axis =1.7,cex.lab=1.7) # 红色粗线条
    polygon(c(pred_OR[,1], rev(pred_OR[,1])), 
            c(pred_OR[,"upper"], rev(pred_OR[,"lower"])), 
            col = ggplot2::alpha("#EB7E60", 0.3), border = NA)  # 在置信区间之间填充阴影
    
    lines(pred_OR[, 1], pred_OR[,"lower"], lty = 2, lwd = 2.5,col ="#6A8EC9")
    lines(pred_OR[, 1], pred_OR[,"upper"], lty = 2, lwd = 2.5,col ="#6A8EC9")
    
    lines(x = range(pred_OR[, 1]), y = c(1, 1), lty = 3, col = "#652884", lwd = 3.5) # 添加水平参考线
    
    points(refvalue, 1, pch = 16, cex = 1.5) # 加大标记点尺寸
    box(lwd = 3)
    
    dev.off()
  }
  p
  #0.967
  refvalue 
  #8.729
  
  {#** TyG_BMI  ####
    load(file="I:/NHANES study/PD&TYG&MO/Data/Interpolation_weighted.Rdata")
    RCS <-  subset(Interpolation_weighted,CVD_MORT_stat!=2)
    pdf("Supplementary Fig. 6d.pdf",width=8,height=7)
    options(datadist = "ddist")
    ddist <- datadist(RCS)
    limUp <- 3 * IQR(RCS[,"TyG_BMI"], na.rm = TRUE) + quantile(RCS[,"TyG_BMI"], 3/4, na.rm = TRUE, names = FALSE)
    limDown <- quantile(RCS[,"TyG_BMI"], 1/4, na.rm = TRUE, names = FALSE) - 3 * IQR(RCS[,"TyG_BMI"], na.rm = TRUE)
    RCS <- RCS[!RCS[,"TyG_BMI"] >= limUp & !RCS[,"TyG_BMI"] <= limDown,]
    RCS$CVD_MORT_stat <- as.numeric(RCS$CVD_MORT_stat)
    fit <- cph(Surv(peryear, CVD_MORT_stat) ~ rcs(TyG_BMI, 3) + Age_status + Sex + Race_ethnicity + SES + Marital_status +
                 Smoking_status + Drinking_status + BMI_status + Physical_status +
                 HTN_status + T2D_status + HPL_status + Cohort, data = RCS, x = TRUE, y = TRUE)
    anova(fit)
    cox.zph(fit, "rank")
    p <- round(anova(fit)[, 3], 3)
    options(datadist = "ddist")
    pred_OR <- Predict(fit, TyG_BMI, ref.zero = TRUE, fun = exp)
    pred_OR$distance_to_1 <- abs(pred_OR$yhat - 1)
    pred_OR$distance_to_2 <- abs(pred_OR$lower - 1)
    pred_OR$distance_to_3 <- abs(pred_OR$upper - 1)
    pred_OR$distance<-pred_OR$distance_to_1+pred_OR$distance_to_2+pred_OR$distance_to_3
    # 找到最接近1的点
    closest_point <- pred_OR[which.min(pred_OR$distance), ]
    # 提取最接近1的TyG_BMI值
    closest_TyG_BMI_value <- closest_point$TyG_BMI
    closest_TyG_BMI_value
    refvalue <- round(closest_TyG_BMI_value, 2)
    
    violet <- "#B46DA9"
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd = NA)
    ylim.bot <- min(pred_OR[,"lower"])
    ylim.top <- max(pred_OR[,"upper"])
    
    dens <- density(RCS$TyG_BMI)
    
    # 绘制密度图，调整颜色和透明度
    plot(dens$x, dens$y, col = ggplot2::alpha(violet, 0.5), type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
         lwd = 1.7) # 加粗线条
    polygon(dens$x, dens$y, col = ggplot2::alpha(violet, 0.5), border = ggplot2::alpha(violet, 0.5)) 
    
    axis(side = 4, cex.axis =1.7) # 增加右侧坐标轴字体大小
    par(new = TRUE)
    
    # 绘制HR vs TyG_BMI指数
    plot(pred_OR[, 1], pred_OR[,"yhat"], xlab = " ", ylab = " ", type = "l", ylim = c(ylim.bot, ylim.top),
         col = "#CC5B45", lwd = 3, cex.axis =1.7,cex.lab=1.7) # 红色粗线条
    polygon(c(pred_OR[,1], rev(pred_OR[,1])), 
            c(pred_OR[,"upper"], rev(pred_OR[,"lower"])), 
            col = ggplot2::alpha("#EB7E60", 0.3), border = NA)  # 在置信区间之间填充阴影
    
    lines(pred_OR[, 1], pred_OR[,"lower"], lty = 2, lwd = 2.5,col ="#6A8EC9")
    lines(pred_OR[, 1], pred_OR[,"upper"], lty = 2, lwd = 2.5,col ="#6A8EC9")
    
    lines(x = range(pred_OR[, 1]), y = c(1, 1), lty = 3, col = "#652884", lwd = 3.5) # 添加水平参考线
    
    points(refvalue, 1, pch = 16, cex = 1.5) # 加大标记点尺寸
    box(lwd = 3)
    
    dev.off()
  }
  p
  #0.967
  refvalue 
  #8.729
}
{#* Cancer cause mortarity ####
  
  {#** TYG ####
    load(file="I:/NHANES study/PD&TYG&MO/Data/Interpolation_weighted.Rdata")
    RCS <- subset(Interpolation_weighted,Cancer_MORT_stat!=2)
    pdf("Supplementary Fig. 6e.pdf",width=8,height=7)
    options(datadist = "ddist")
    ddist <- datadist(RCS)
    limUp <- 3 * IQR(RCS[,"TYG"], na.rm = TRUE) + quantile(RCS[,"TYG"], 3/4, na.rm = TRUE, names = FALSE)
    limDown <- quantile(RCS[,"TYG"], 1/4, na.rm = TRUE, names = FALSE) - 3 * IQR(RCS[,"TYG"], na.rm = TRUE)
    RCS <- RCS[!RCS[,"TYG"] >= limUp & !RCS[,"TYG"] <= limDown,]
    RCS$Cancer_MORT_stat <- as.numeric(RCS$Cancer_MORT_stat)
    fit <- cph(Surv(peryear, Cancer_MORT_stat) ~ rcs(TYG, 4) + Age_status + Sex + Race_ethnicity + SES + Marital_status +
                 Smoking_status + Drinking_status + BMI_status + Physical_status +
                 HTN_status + T2D_status + HPL_status + Cohort, data = RCS, x = TRUE, y = TRUE)
    anova(fit)
    cox.zph(fit, "rank")
    p <- round(anova(fit)[, 3], 3)
    options(datadist = "ddist")
    pred_OR <- Predict(fit, TYG, ref.zero = TRUE, fun = exp)
    pred_OR$distance_to_1 <- abs(pred_OR$yhat - 1)
    pred_OR$distance_to_2 <- abs(pred_OR$lower - 1)
    pred_OR$distance_to_3 <- abs(pred_OR$upper - 1)
    pred_OR$distance<-pred_OR$distance_to_1+pred_OR$distance_to_2+pred_OR$distance_to_3
    # 找到最接近1的点
    closest_point <- pred_OR[which.min(pred_OR$distance), ]
    # 提取最接近1的TYG值
    closest_TYG_value <- closest_point$TYG
    closest_TYG_value
    refvalue <- round(closest_TYG_value, 2)
    
    violet <- "#96C37D"
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd = NA)
    ylim.bot <- min(pred_OR[,"lower"])
    ylim.top <- max(pred_OR[,"upper"])
    
    dens <- density(RCS$TYG)
    
    # 绘制密度图，调整颜色和透明度
    plot(dens$x, dens$y, col = ggplot2::alpha(violet, 0.5), type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
         lwd = 1.7) # 加粗线条
    polygon(dens$x, dens$y, col = ggplot2::alpha(violet, 0.5), border = ggplot2::alpha(violet, 0.5)) 
    
    axis(side = 4, cex.axis =1.7) # 增加右侧坐标轴字体大小
    par(new = TRUE)
    
    # 绘制HR vs TyG指数
    plot(pred_OR[, 1], pred_OR[,"yhat"], xlab = " ", ylab = " ", type = "l", ylim = c(ylim.bot, ylim.top),
         col = "#CC5B45", lwd = 3, cex.axis =1.7,cex.lab=1.7) # 红色粗线条
    polygon(c(pred_OR[,1], rev(pred_OR[,1])), 
            c(pred_OR[,"upper"], rev(pred_OR[,"lower"])), 
            col = ggplot2::alpha("#EB7E60", 0.3), border = NA)  # 在置信区间之间填充阴影
    
    lines(pred_OR[, 1], pred_OR[,"lower"], lty = 2, lwd = 2.5,col ="#6A8EC9")
    lines(pred_OR[, 1], pred_OR[,"upper"], lty = 2, lwd = 2.5,col ="#6A8EC9")
    
    lines(x = range(pred_OR[, 1]), y = c(1, 1), lty = 3, col = "#652884", lwd = 3.5) # 添加水平参考线
    
    points(refvalue, 1, pch = 16, cex = 1.5) # 加大标记点尺寸
    box(lwd = 3)
    
    dev.off()
  }
  p
  #0.967
  refvalue 
  #8.729
  {#** TyG_WC  ####
    load(file="I:/NHANES study/PD&TYG&MO/Data/Interpolation_weighted.Rdata")
    RCS <- subset(Interpolation_weighted,Cancer_MORT_stat!=2)
    pdf("Supplementary Fig. 6f.pdf",width=8,height=7)
    options(datadist = "ddist")
    ddist <- datadist(RCS)
    limUp <- 3 * IQR(RCS[,"TyG_WC"], na.rm = TRUE) + quantile(RCS[,"TyG_WC"], 3/4, na.rm = TRUE, names = FALSE)
    limDown <- quantile(RCS[,"TyG_WC"], 1/4, na.rm = TRUE, names = FALSE) - 3 * IQR(RCS[,"TyG_WC"], na.rm = TRUE)
    RCS <- RCS[!RCS[,"TyG_WC"] >= limUp & !RCS[,"TyG_WC"] <= limDown,]
    RCS$Cancer_MORT_stat <- as.numeric(RCS$Cancer_MORT_stat)
    fit <- cph(Surv(peryear, Cancer_MORT_stat) ~ rcs(TyG_WC, 3) + Age_status + Sex + Race_ethnicity + SES + Marital_status +
                 Smoking_status + Drinking_status + BMI_status + Physical_status +
                 HTN_status + T2D_status + HPL_status + Cohort, data = RCS, x = TRUE, y = TRUE)
    anova(fit)
    cox.zph(fit, "rank")
    p <- round(anova(fit)[, 3], 3)
    options(datadist = "ddist")
    pred_OR <- Predict(fit, TyG_WC, ref.zero = TRUE, fun = exp)
    pred_OR$distance_to_1 <- abs(pred_OR$yhat - 1)
    pred_OR$distance_to_2 <- abs(pred_OR$lower - 1)
    pred_OR$distance_to_3 <- abs(pred_OR$upper - 1)
    pred_OR$distance<-pred_OR$distance_to_1+pred_OR$distance_to_2+pred_OR$distance_to_3
    # 找到最接近1的点
    closest_point <- pred_OR[which.min(pred_OR$distance), ]
    # 提取最接近1的TyG_WC值
    closest_TyG_WC_value <- closest_point$TyG_WC
    closest_TyG_WC_value
    refvalue <- round(closest_TyG_WC_value, 2)
    
    violet <- "#5CB0C3"
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd = NA)
    ylim.bot <- min(pred_OR[,"lower"])
    ylim.top <- max(pred_OR[,"upper"])
    
    dens <- density(RCS$TyG_WC)
    
    # 绘制密度图，调整颜色和透明度
    plot(dens$x, dens$y, col = ggplot2::alpha(violet, 0.5), type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
         lwd = 1.7) # 加粗线条
    polygon(dens$x, dens$y, col = ggplot2::alpha(violet, 0.5), border = ggplot2::alpha(violet, 0.5)) 
    
    axis(side = 4, cex.axis =1.7) # 增加右侧坐标轴字体大小
    par(new = TRUE)
    
    # 绘制HR vs TyG_WC指数
    plot(pred_OR[, 1], pred_OR[,"yhat"], xlab = " ", ylab = " ", type = "l", ylim = c(ylim.bot, ylim.top),
         col = "#CC5B45", lwd = 3, cex.axis =1.7,cex.lab=1.7) # 红色粗线条
    polygon(c(pred_OR[,1], rev(pred_OR[,1])), 
            c(pred_OR[,"upper"], rev(pred_OR[,"lower"])), 
            col = ggplot2::alpha("#EB7E60", 0.3), border = NA)  # 在置信区间之间填充阴影
    
    lines(pred_OR[, 1], pred_OR[,"lower"], lty = 2, lwd = 2.5,col ="#6A8EC9")
    lines(pred_OR[, 1], pred_OR[,"upper"], lty = 2, lwd = 2.5,col ="#6A8EC9")
    
    lines(x = range(pred_OR[, 1]), y = c(1, 1), lty = 3, col = "#652884", lwd = 3.5) # 添加水平参考线
    
    points(refvalue, 1, pch = 16, cex = 1.5) # 加大标记点尺寸
    box(lwd = 3)
    
    dev.off()
  }
  p
  #0.967
  refvalue 
  #8.729
  
  {#** TyG_WHtR  ####
    load(file="I:/NHANES study/PD&TYG&MO/Data/Interpolation_weighted.Rdata")
    RCS <- subset(Interpolation_weighted,Cancer_MORT_stat!=2)
    pdf("Supplementary Fig. 6g.pdf",width=8,height=7)
    options(datadist = "ddist")
    ddist <- datadist(RCS)
    limUp <- 3 * IQR(RCS[,"TyG_WHtR"], na.rm = TRUE) + quantile(RCS[,"TyG_WHtR"], 3/4, na.rm = TRUE, names = FALSE)
    limDown <- quantile(RCS[,"TyG_WHtR"], 1/4, na.rm = TRUE, names = FALSE) - 3 * IQR(RCS[,"TyG_WHtR"], na.rm = TRUE)
    RCS <- RCS[!RCS[,"TyG_WHtR"] >= limUp & !RCS[,"TyG_WHtR"] <= limDown,]
    RCS$Cancer_MORT_stat <- as.numeric(RCS$Cancer_MORT_stat)
    fit <- cph(Surv(peryear, Cancer_MORT_stat) ~ rcs(TyG_WHtR, 3) + Age_status + Sex + Race_ethnicity + SES + Marital_status +
                 Smoking_status + Drinking_status + BMI_status + Physical_status +
                 HTN_status + T2D_status + HPL_status + Cohort, data = RCS, x = TRUE, y = TRUE)
    anova(fit)
    cox.zph(fit, "rank")
    p <- round(anova(fit)[, 3], 3)
    ddist$limits$BMI[2] <- refvalue
    options(datadist = "ddist")
    pred_OR <- Predict(fit, TyG_WHtR, ref.zero = TRUE, fun = exp)
    pred_OR$distance_to_1 <- abs(pred_OR$yhat - 1)
    pred_OR$distance_to_2 <- abs(pred_OR$lower - 1)
    pred_OR$distance_to_3 <- abs(pred_OR$upper - 1)
    pred_OR$distance<-pred_OR$distance_to_1+pred_OR$distance_to_2+pred_OR$distance_to_3
    # 找到最接近1的点
    closest_point <- pred_OR[which.min(pred_OR$distance), ]
    # 提取最接近1的TyG_WHtR值
    closest_TyG_WHtR_value <- closest_point$TyG_WHtR
    closest_TyG_WHtR_value
    refvalue <- round(closest_TyG_WHtR_value, 2)
    
    violet <- "#F5A216"
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd = NA)
    ylim.bot <- min(pred_OR[,"lower"])
    ylim.top <- max(pred_OR[,"upper"])
    
    dens <- density(RCS$TyG_WHtR)
    
    # 绘制密度图，调整颜色和透明度
    plot(dens$x, dens$y, col = ggplot2::alpha(violet, 0.5), type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
         lwd = 1.7) # 加粗线条
    polygon(dens$x, dens$y, col = ggplot2::alpha(violet, 0.5), border = ggplot2::alpha(violet, 0.5)) 
    
    axis(side = 4, cex.axis =1.7) # 增加右侧坐标轴字体大小
    par(new = TRUE)
    
    # 绘制HR vs TyG_WHtR指数
    plot(pred_OR[, 1], pred_OR[,"yhat"], xlab = " ", ylab = " ", type = "l", ylim = c(ylim.bot, ylim.top),
         col = "#CC5B45", lwd = 3, cex.axis =1.7,cex.lab=1.7) # 红色粗线条
    polygon(c(pred_OR[,1], rev(pred_OR[,1])), 
            c(pred_OR[,"upper"], rev(pred_OR[,"lower"])), 
            col = ggplot2::alpha("#EB7E60", 0.3), border = NA)  # 在置信区间之间填充阴影
    
    lines(pred_OR[, 1], pred_OR[,"lower"], lty = 2, lwd = 2.5,col ="#6A8EC9")
    lines(pred_OR[, 1], pred_OR[,"upper"], lty = 2, lwd = 2.5,col ="#6A8EC9")
    
    lines(x = range(pred_OR[, 1]), y = c(1, 1), lty = 3, col = "#652884", lwd = 3.5) # 添加水平参考线
    
    points(refvalue, 1, pch = 16, cex = 1.5) # 加大标记点尺寸
    box(lwd = 3)
    
    dev.off()
  }
  p
  #0.967
  refvalue 
  #8.729
  
  {#** TyG_BMI  ####
    load(file="I:/NHANES study/PD&TYG&MO/Data/Interpolation_weighted.Rdata")
    RCS <-  subset(Interpolation_weighted,Cancer_MORT_stat!=2)
    pdf("Supplementary Fig. 6h.pdf",width=8,height=7)
    options(datadist = "ddist")
    ddist <- datadist(RCS)
    limUp <- 3 * IQR(RCS[,"TyG_BMI"], na.rm = TRUE) + quantile(RCS[,"TyG_BMI"], 3/4, na.rm = TRUE, names = FALSE)
    limDown <- quantile(RCS[,"TyG_BMI"], 1/4, na.rm = TRUE, names = FALSE) - 3 * IQR(RCS[,"TyG_BMI"], na.rm = TRUE)
    RCS <- RCS[!RCS[,"TyG_BMI"] >= limUp & !RCS[,"TyG_BMI"] <= limDown,]
    RCS$Cancer_MORT_stat <- as.numeric(RCS$Cancer_MORT_stat)
    fit <- cph(Surv(peryear, Cancer_MORT_stat) ~ rcs(TyG_BMI, 3) + Age_status + Sex + Race_ethnicity + SES + Marital_status +
                 Smoking_status + Drinking_status + BMI_status + Physical_status +
                 HTN_status + T2D_status + HPL_status + Cohort, data = RCS, x = TRUE, y = TRUE)
    anova(fit)
    cox.zph(fit, "rank")
    p <- round(anova(fit)[, 3], 3)
    options(datadist = "ddist")
    pred_OR <- Predict(fit, TyG_BMI, ref.zero = TRUE, fun = exp)
    pred_OR$distance_to_1 <- abs(pred_OR$yhat - 1)
    pred_OR$distance_to_2 <- abs(pred_OR$lower - 1)
    pred_OR$distance_to_3 <- abs(pred_OR$upper - 1)
    pred_OR$distance<-pred_OR$distance_to_1+pred_OR$distance_to_2+pred_OR$distance_to_3
    # 找到最接近1的点
    closest_point <- pred_OR[which.min(pred_OR$distance), ]
    # 提取最接近1的TyG_BMI值
    closest_TyG_BMI_value <- closest_point$TyG_BMI
    closest_TyG_BMI_value
    refvalue <- round(closest_TyG_BMI_value, 2)
    
    violet <- "#B46DA9"
    par(mar = c(5, 4, 4, 4) + 0.3)
    par(xpd = NA)
    ylim.bot <- min(pred_OR[,"lower"])
    ylim.top <- max(pred_OR[,"upper"])
    
    dens <- density(RCS$TyG_BMI)
    
    # 绘制密度图，调整颜色和透明度
    plot(dens$x, dens$y, col = ggplot2::alpha(violet, 0.5), type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
         lwd = 1.7) # 加粗线条
    polygon(dens$x, dens$y, col = ggplot2::alpha(violet, 0.5), border = ggplot2::alpha(violet, 0.5)) 
    
    axis(side = 4, cex.axis =1.7) # 增加右侧坐标轴字体大小
    par(new = TRUE)
    
    # 绘制HR vs TyG_BMI指数
    plot(pred_OR[, 1], pred_OR[,"yhat"], xlab = " ", ylab = " ", type = "l", ylim = c(ylim.bot, ylim.top),
         col = "#CC5B45", lwd = 3, cex.axis =1.7,cex.lab=1.7) # 红色粗线条
    polygon(c(pred_OR[,1], rev(pred_OR[,1])), 
            c(pred_OR[,"upper"], rev(pred_OR[,"lower"])), 
            col = ggplot2::alpha("#EB7E60", 0.3), border = NA)  # 在置信区间之间填充阴影
    
    lines(pred_OR[, 1], pred_OR[,"lower"], lty = 2, lwd = 2.5,col ="#6A8EC9")
    lines(pred_OR[, 1], pred_OR[,"upper"], lty = 2, lwd = 2.5,col ="#6A8EC9")
    
    lines(x = range(pred_OR[, 1]), y = c(1, 1), lty = 3, col = "#652884", lwd = 3.5) # 添加水平参考线
    
    points(refvalue, 1, pch = 16, cex = 1.5) # 加大标记点尺寸
    box(lwd = 3)
    
    dev.off()
  }
  p
  #0.967
  refvalue 
  #8.729
}



# +++++++++++================================+++++++++++ ####
# +++++++++++========Supplementary===========+++++++++++ ####
# +++++++++++================================+++++++++++ #### 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# +++++++++++++++++++Tables++++++++++++++++++++++++++ ####
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 24 Stratified analyses (Table S12) ####  

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 27 Peryear>=2 (Table S10) #### 
load(file="I:/NHANES study/PD&TYG&MO/Data/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
Interpolation_weighted<-subset(Interpolation_weighted,peryear>=2)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)

{ #* all model #####
  #all model1
  model1<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PD_diagnosis, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model2
  model2<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result<-rbind(result,result2)
  #all model3
  model3<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_result<-summary(model3)
  
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result<-rbind(result,result3)
  result
  #all model4
  model4<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result_All<-rbind(result,result4)
  result_All
}

{ #* CVD model #####
  
  #CVD Crude
  model1<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PD_diagnosis, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="CVD cause")
  result
  #CVD model1
  model2<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="CVD cause")
  result<-rbind(result,result2)
  result
  #CVD model2
  model3<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_result<-summary(model3)
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="CVD cause")
  result<-rbind(result,result3)
  result
  #CVD model3
  model4<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result_CVD<-rbind(result,result4)
  result_CVD
  
}

{ #* Cancer model #####
  
  #Cancer model1
  model1<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="Cancer cause")
  result
  #Cancer model2
  model2<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="Cancer cause")
  result<-rbind(result,result2)
  result
  #Cancer model3
  model3<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status,design =rhcSvy)
  model3_result<-summary(model3)
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="Cancer cause")
  result<-rbind(result,result3)
  result
  #Cancer model4
  model4<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result_Cancer<-rbind(result,result4)
  result_Cancer
}



{ #* Combine #####
  #Results
  result.cause<-rbind(result_All,result_CVD,result_Cancer)
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  round_3_function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
  
  result.cause[result.cause=="10000"]<-"1.000"
  result.cause[result.cause=="00000"]<-"<0.001"
  result.cause
  #Events
  #all.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$MORT_stat,useNA = "ifany")
  all.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #CVD.cause
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$CVD_MORT_stat,useNA = "ifany")
  CVD.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #Cancer.cause
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$Cancer_MORT_stat,useNA = "ifany")
  Cancer.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  total.counts<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause))
  total.counts
  
  #PD_MDD
  PD_MDD<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="Moderate/Severe periodontitis"),]
  PD_MDD$year<-PD_MDD$peryear
  PD_MDD$Pyear<-PD_MDD$year*PD_MDD$weight
  PD_MDD_death_All<-PD_MDD[which(PD_MDD$MORT_stat==1),]
  PD_MDD_death_CVD<-PD_MDD[which(PD_MDD$CVD_MORT_stat==1),]
  PD_MDD_death_Cancer<-PD_MDD[which(PD_MDD$Cancer_MORT_stat==1),]
  
  #noPD_MDD
  noPD_MDD<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="No/Mild periodontitis"),]
  noPD_MDD$year<-noPD_MDD$peryear
  noPD_MDD$Pyear<-noPD_MDD$year*noPD_MDD$weight
  noPD_MDD_death_All<-noPD_MDD[which(noPD_MDD$MORT_stat==1),]
  noPD_MDD_death_CVD<-noPD_MDD[which(noPD_MDD$CVD_MORT_stat==1),]
  noPD_MDD_death_Cancer<-noPD_MDD[which(noPD_MDD$Cancer_MORT_stat==1),]
  
  #all.cause
  PD.counts<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  PD.cause.counts<-format(round(sum(PD_MDD_death_All$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.cause.counts<-format(round(sum(noPD_MDD_death_All$weight)), big.mark = ",", scientific = FALSE)
  
  all.cause<-c(paste0(PD.cause.counts,"/",PD.counts),paste0(noPD.cause.counts,"/",noPD.counts))
  #CVD.cause
  PD.counts<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  PD.cause.counts<-format(round(sum(PD_MDD_death_CVD$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.cause.counts<-format(round(sum(noPD_MDD_death_CVD$weight)), big.mark = ",", scientific = FALSE)
  CVD.cause<-c(paste0(PD.cause.counts,"/",PD.counts),paste0(noPD.cause.counts,"/",noPD.counts))
  #Cancer.cause
  PD.counts<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  PD.cause.counts<-format(round(sum(PD_MDD_death_Cancer$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.cause.counts<-format(round(sum(noPD_MDD_death_Cancer$weight)), big.mark = ",", scientific = FALSE)
  Cancer.cause<-c(paste0(PD.cause.counts,"/",PD.counts),paste0(noPD.cause.counts,"/",noPD.counts))
  total.counts_ad<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause))
  total.counts_ad
  
  #Cbind
  Table<-c("","Periodontal status","","")
  Table<-rbind(Table,c("", "No/Mild periodontitis","Moderate/Severe periodontitis","P"))
  Table<-rbind(Table,c("All cause*","","",""))
  Table<-rbind(Table,c("Deaths/total (Unweighted)",total.counts[1,1],total.counts[1,2],""))
  Table<-rbind(Table,c("Deaths/total (Weighted)",total.counts_ad[1,1],total.counts_ad[1,2],""))
  Table<-rbind(Table,c("Model 1†","1.00 [Reference]",paste0(result.cause[1,1]," (",result.cause[1,2],", ",result.cause[1,3],")"),result.cause[1,4]))
  Table<-rbind(Table,c("Model 2‡","1.00 [Reference]",paste0(result.cause[2,1]," (",result.cause[2,2],", ",result.cause[2,3],")"),result.cause[2,4]))
  Table<-rbind(Table,c("Model 3§","1.00 [Reference]",paste0(result.cause[3,1]," (",result.cause[3,2],", ",result.cause[3,3],")"),result.cause[3,4]))
  Table<-rbind(Table,c("Model 4¶","1.00 [Reference]",paste0(result.cause[4,1]," (",result.cause[4,2],", ",result.cause[4,3],")"),result.cause[4,4]))
  Table<-rbind(Table,c("CVD cause","","",""))
  Table<-rbind(Table,c("Deaths/total (Unweighted)",total.counts[2,1],total.counts[2,2],""))
  Table<-rbind(Table,c("Deaths/total (Weighted)",total.counts_ad[2,1],total.counts_ad[2,2],""))
  Table<-rbind(Table,c("Model 1","1.00 [Reference]",paste0(result.cause[5,1]," (",result.cause[5,2],", ",result.cause[5,3],")"),result.cause[5,4]))
  Table<-rbind(Table,c("Model 2","1.00 [Reference]",paste0(result.cause[6,1]," (",result.cause[6,2],", ",result.cause[6,3],")"),result.cause[6,4]))
  Table<-rbind(Table,c("Model 3","1.00 [Reference]",paste0(result.cause[7,1]," (",result.cause[7,2],", ",result.cause[7,3],")"),result.cause[7,4]))
  Table<-rbind(Table,c("Model 4","1.00 [Reference]",paste0(result.cause[8,1]," (",result.cause[8,2],", ",result.cause[8,3],")"),result.cause[8,4]))
  Table<-rbind(Table,c("Cancer cause","","",""))
  Table<-rbind(Table,c("Deaths/total (Unweighted)",total.counts[3,1],total.counts[3,2],""))
  Table<-rbind(Table,c("Deaths/total (Weighted)",total.counts_ad[3,1],total.counts_ad[3,2],""))
  Table<-rbind(Table,c("Model 1","1.00 [Reference]",paste0(result.cause[9,1]," (",result.cause[9,2],", ",result.cause[9,3],")"),result.cause[9,4]))
  Table<-rbind(Table,c("Model 2","1.00 [Reference]",paste0(result.cause[10,1]," (",result.cause[10,2],", ",result.cause[10,3],")"),result.cause[10,4]))
  Table<-rbind(Table,c("Model 3","1.00 [Reference]",paste0(result.cause[11,1]," (",result.cause[11,2],", ",result.cause[11,3],")"),result.cause[11,4]))
  Table<-rbind(Table,c("Model 4","1.00 [Reference]",paste0(result.cause[12,1]," (",result.cause[12,2],", ",result.cause[12,3],")"),result.cause[12,4]))
  Table<-as.data.frame(Table)
  Table_S10 = data.frame(lapply(Table, as.character), stringsAsFactors=FALSE)
  Table_S10
  write.table(Table_S10,sep = ",",file ="I:/NHANES study/PD&TYG&MO/Result/Supplementary Table 10.csv",row.names =F,col.names =F )
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 28 omitted cancer and CVD patients (Table S11) ####  
load(file="I:/NHANES study/PD&TYG&MO/Data/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
Interpolation_weighted<-subset(Interpolation_weighted,Cancer_status!="YES"&CVD_status!="YES")
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)

{ #* all model #####
  #all model1
  model1<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PD_diagnosis, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model2
  model2<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result<-rbind(result,result2)
  #all model3
  model3<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_result<-summary(model3)
  
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result<-rbind(result,result3)
  result
  #all model4
  model4<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result_All<-rbind(result,result4)
  result_All
}

{ #* CVD model #####
  
  #CVD Crude
  model1<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PD_diagnosis, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="CVD cause")
  result
  #CVD model1
  model2<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="CVD cause")
  result<-rbind(result,result2)
  result
  #CVD model2
  model3<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_result<-summary(model3)
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="CVD cause")
  result<-rbind(result,result3)
  result
  #CVD model3
  model4<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result_CVD<-rbind(result,result4)
  result_CVD
  
}

{ #* Cancer model #####
  
  #Cancer model1
  model1<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="Cancer cause")
  result
  #Cancer model2
  model2<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="Cancer cause")
  result<-rbind(result,result2)
  result
  #Cancer model3
  model3<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status,design =rhcSvy)
  model3_result<-summary(model3)
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="Cancer cause")
  result<-rbind(result,result3)
  result
  #Cancer model4
  model4<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result_Cancer<-rbind(result,result4)
  result_Cancer
}



{ #* Combine #####
  #Results
  result.cause<-rbind(result_All,result_CVD,result_Cancer)
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  round_3_function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
  
  result.cause[result.cause=="10000"]<-"1.000"
  result.cause[result.cause=="00000"]<-"<0.001"
  result.cause
  #Events
  #all.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$MORT_stat,useNA = "ifany")
  all.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #CVD.cause
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$CVD_MORT_stat,useNA = "ifany")
  CVD.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #Cancer.cause
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$Cancer_MORT_stat,useNA = "ifany")
  Cancer.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  total.counts<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause))
  total.counts
  
  #PD_MDD
  PD_MDD<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="Moderate/Severe periodontitis"),]
  PD_MDD$year<-PD_MDD$peryear
  PD_MDD$Pyear<-PD_MDD$year*PD_MDD$weight
  PD_MDD_death_All<-PD_MDD[which(PD_MDD$MORT_stat==1),]
  PD_MDD_death_CVD<-PD_MDD[which(PD_MDD$CVD_MORT_stat==1),]
  PD_MDD_death_Cancer<-PD_MDD[which(PD_MDD$Cancer_MORT_stat==1),]
  
  #noPD_MDD
  noPD_MDD<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="No/Mild periodontitis"),]
  noPD_MDD$year<-noPD_MDD$peryear
  noPD_MDD$Pyear<-noPD_MDD$year*noPD_MDD$weight
  noPD_MDD_death_All<-noPD_MDD[which(noPD_MDD$MORT_stat==1),]
  noPD_MDD_death_CVD<-noPD_MDD[which(noPD_MDD$CVD_MORT_stat==1),]
  noPD_MDD_death_Cancer<-noPD_MDD[which(noPD_MDD$Cancer_MORT_stat==1),]
  
  #all.cause
  PD.counts<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  PD.cause.counts<-format(round(sum(PD_MDD_death_All$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.cause.counts<-format(round(sum(noPD_MDD_death_All$weight)), big.mark = ",", scientific = FALSE)
  
  all.cause<-c(paste0(PD.cause.counts,"/",PD.counts),paste0(noPD.cause.counts,"/",noPD.counts))
  #CVD.cause
  PD.counts<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  PD.cause.counts<-format(round(sum(PD_MDD_death_CVD$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.cause.counts<-format(round(sum(noPD_MDD_death_CVD$weight)), big.mark = ",", scientific = FALSE)
  CVD.cause<-c(paste0(PD.cause.counts,"/",PD.counts),paste0(noPD.cause.counts,"/",noPD.counts))
  #Cancer.cause
  PD.counts<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  PD.cause.counts<-format(round(sum(PD_MDD_death_Cancer$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.cause.counts<-format(round(sum(noPD_MDD_death_Cancer$weight)), big.mark = ",", scientific = FALSE)
  Cancer.cause<-c(paste0(PD.cause.counts,"/",PD.counts),paste0(noPD.cause.counts,"/",noPD.counts))
  total.counts_ad<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause))
  total.counts_ad
  
  #Cbind
  Table<-c("","Periodontal status","","")
  Table<-rbind(Table,c("", "No/Mild periodontitis","Moderate/Severe periodontitis","P"))
  Table<-rbind(Table,c("All cause*","","",""))
  Table<-rbind(Table,c("Deaths/total (Unweighted)",total.counts[1,1],total.counts[1,2],""))
  Table<-rbind(Table,c("Deaths/total (Weighted)",total.counts_ad[1,1],total.counts_ad[1,2],""))
  Table<-rbind(Table,c("Model 1†","1.00 [Reference]",paste0(result.cause[1,1]," (",result.cause[1,2],", ",result.cause[1,3],")"),result.cause[1,4]))
  Table<-rbind(Table,c("Model 2‡","1.00 [Reference]",paste0(result.cause[2,1]," (",result.cause[2,2],", ",result.cause[2,3],")"),result.cause[2,4]))
  Table<-rbind(Table,c("Model 3§","1.00 [Reference]",paste0(result.cause[3,1]," (",result.cause[3,2],", ",result.cause[3,3],")"),result.cause[3,4]))
  Table<-rbind(Table,c("Model 4¶","1.00 [Reference]",paste0(result.cause[4,1]," (",result.cause[4,2],", ",result.cause[4,3],")"),result.cause[4,4]))
  Table<-rbind(Table,c("CVD cause","","",""))
  Table<-rbind(Table,c("Deaths/total (Unweighted)",total.counts[2,1],total.counts[2,2],""))
  Table<-rbind(Table,c("Deaths/total (Weighted)",total.counts_ad[2,1],total.counts_ad[2,2],""))
  Table<-rbind(Table,c("Model 1","1.00 [Reference]",paste0(result.cause[5,1]," (",result.cause[5,2],", ",result.cause[5,3],")"),result.cause[5,4]))
  Table<-rbind(Table,c("Model 2","1.00 [Reference]",paste0(result.cause[6,1]," (",result.cause[6,2],", ",result.cause[6,3],")"),result.cause[6,4]))
  Table<-rbind(Table,c("Model 3","1.00 [Reference]",paste0(result.cause[7,1]," (",result.cause[7,2],", ",result.cause[7,3],")"),result.cause[7,4]))
  Table<-rbind(Table,c("Model 4","1.00 [Reference]",paste0(result.cause[8,1]," (",result.cause[8,2],", ",result.cause[8,3],")"),result.cause[8,4]))
  Table<-rbind(Table,c("Cancer cause","","",""))
  Table<-rbind(Table,c("Deaths/total (Unweighted)",total.counts[3,1],total.counts[3,2],""))
  Table<-rbind(Table,c("Deaths/total (Weighted)",total.counts_ad[3,1],total.counts_ad[3,2],""))
  Table<-rbind(Table,c("Model 1","1.00 [Reference]",paste0(result.cause[9,1]," (",result.cause[9,2],", ",result.cause[9,3],")"),result.cause[9,4]))
  Table<-rbind(Table,c("Model 2","1.00 [Reference]",paste0(result.cause[10,1]," (",result.cause[10,2],", ",result.cause[10,3],")"),result.cause[10,4]))
  Table<-rbind(Table,c("Model 3","1.00 [Reference]",paste0(result.cause[11,1]," (",result.cause[11,2],", ",result.cause[11,3],")"),result.cause[11,4]))
  Table<-rbind(Table,c("Model 4","1.00 [Reference]",paste0(result.cause[12,1]," (",result.cause[12,2],", ",result.cause[12,3],")"),result.cause[12,4]))
  Table<-as.data.frame(Table)
  Table_S11 = data.frame(lapply(Table, as.character), stringsAsFactors=FALSE)
  Table_S11
  write.table(Table_S11,sep = ",",file ="I:/NHANES study/PD&TYG&MO/Result/Supplementary Table 11.csv",row.names =F,col.names =F )
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 29 Original data (Table S12) ####  
load(file="I:/NHANES study/PD&TYG&MO/Data/Original_weighted.Rdata")
Interpolation_weighted<-Original_weighted
colnames(Interpolation_weighted)

table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$MORT_stat)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
var<-c("MORT_stat","Age_status","Sex","Marital_status","Race_ethnicity","Education_levels","PIR",
       "Health_insurance","SEI","SES","Smoking_status","Drinking_status","Physical_status","HEI",
       "HPL_status","HTN_status","CVD_status",
       "BMI_status","T2D_status","Cancer_status","Cohort")
VAR<-c("MORT_stat","TYG","TyG_WC_mean","Age","Marital_status","Age_status","Sex","Race_ethnicity","Education_levels","PIR",
       "Health_insurance","SEI","SES","Smoking_status","Drinking_status","Physical_status","HEI", "HTN_status","HPL_status",
       "BMI","BMI_status","T2D_status","CVD_status","Cancer_status","Cohort")

options(survey.lonely.psu="adjust")
options(survey.adjust.domain.lonely=TRUE)
{ #* section 18.1 Over all ####
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,1),
                          'SE' = round(svymean$SE*100,1) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
                          'Mean' =round(svymean$mean,1),
                          'SE' = round(svymean$SE,1))
      return(model)
    }
  }  
  Over<- ldply(lapply(VAR, model))
}
options(survey.lonely.psu="adjust")
options(survey.adjust.domain.lonely=TRUE)

{ #* section 18.2 No/Mild periodontitis ####
  rhcSvy_HEI2PD<-subset(rhcSvy,PD_diagnosis=="No/Mild periodontitis")
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy_HEI2PD,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_HEI2PD, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,1),
                          'SE' = round(svymean$SE*100,1) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_HEI2PD, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
                          'Mean' =round(svymean$mean,1),
                          'SE' = round(svymean$SE,1))
      return(model)
    }
  }  
  noPD<- ldply(lapply(VAR, model))
}  
{ #* section 18.3 Moderate/Severe periodontitis ####
  rhcSvy_PD<-subset(rhcSvy,PD_diagnosis=="Moderate/Severe periodontitis")
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy_PD,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,1),
                          'SE' = round(svymean$SE*100,1) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
                          'Mean' =round(svymean$mean,1),
                          'SE' = round(svymean$SE,1))
      return(model)
    }
  }  
  PD<- ldply(lapply(VAR, model))
}
Table1<-cbind(Over,noPD[,c("counts","Mean","SE")],PD[,c("counts","Mean","SE")])
save(Table1,file = "I:/NHANES study/PD&TYG&MO/Data/Table1_Rdata")  
{ #* section 18.4 t-test and chi-test ####
  model<- function(x){
    
    if( x %in% var ) {
      formula<-as.formula(paste0("~",x,"+PD_diagnosis"))
      chi_test<-svychisq(formula,rhcSvy)
      model <- data.frame('Covariates'=x,
                          'P value' =chi_test[["p.value"]])
      return(model)
    } else {
      formula<-as.formula(paste0(x,"~PD_diagnosis"))
      t_test<-svyttest(formula,rhcSvy)
      model <- data.frame('Covariates'=x,
                          'P value' =t_test[["p.value"]])
      return(model)
    }
  }  
  test_data<- ldply(lapply(VAR, model))
  test_data$P.value<-round(test_data$P.value,3)
  test_data$P.value[test_data$P.value==0]<-"<0.001"
  new.function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  test_data$P.value<-lapply(test_data$P.value,new.function)
  test_data$P.value<-as.character(test_data$P.value)
}
load(file = "I:/NHANES study/PD&TYG&MO/Data/Table1_Rdata")
Table1<-merge(Table1,test_data,by="Covariates",all.x = T)
Table1$Covariates
Table1$Row<-paste0(Table1$Covariates," ",Table1$grade)
rownames(Table1)<-Table1$Row
colnames(Table1)<-c("Covariates","grade",
                    "counts_all","Mean_all","SE_all",
                    "counts_noPD","Mean_noPD","SE_noPD",
                    "counts_PD","Mean_PD","SE_PD",
                    "P.value","Row")
rownames(Table1)
{ #* section 18.5 Combine  Table S12 #####
  #Events
  #all.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  
  PD_MDD<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="Moderate/Severe periodontitis"),]
  noPD_MDD<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="No/Mild periodontitis"),]
  
  PD.counts_ad<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts_ad<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  Counts_ad<-format(round(sum(PD_MDD$weight))+round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  
  
  Table<-c("","","","Periodontal status","","","","")
  Table<-rbind(Table,c("","Over all","","No/Mild periodontitis","","Moderate/Severe periodontitis","",""))
  Table<-rbind(Table,c("Characteristics","Mean/ %*","SE*","Mean/ %","SE","Mean/ %","SE","P†"))
  Table<-rbind(Table,c("No.(Unweighted)",PD.counts[1]+PD.counts[2],"",PD.counts[1],"",PD.counts[2],"",""))
  Table<-rbind(Table,c("No.(Weighted)",Counts_ad,"",PD.counts_ad,"",noPD.counts_ad,"",""))
  #Age
  Table<-rbind(Table,c("Age (years), mean",
                       Table1["Age Mean ± SE","Mean_all"],Table1["Age Mean ± SE","SE_all"],
                       Table1["Age Mean ± SE","Mean_noPD"],Table1["Age Mean ± SE","SE_noPD"],
                       Table1["Age Mean ± SE","Mean_PD"],Table1["Age Mean ± SE","SE_PD"],
                       Table1["Age Mean ± SE","P.value"] ))
  Table<-rbind(Table,c("Age status, %","","","","","","",Table1["Age_status <45","P.value"]))
  Table<-rbind(Table,c("<45",
                       Table1["Age_status <45","Mean_all"],Table1["Age_status <45","SE_all"],
                       Table1["Age_status <45","Mean_noPD"],Table1["Age_status <45","SE_noPD"],
                       Table1["Age_status <45","Mean_PD"],Table1["Age_status <45","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("[45, 65)",
                       Table1["Age_status [45,65)","Mean_all"],Table1["Age_status [45,65)","SE_all"],
                       Table1["Age_status [45,65)","Mean_noPD"],Table1["Age_status [45,65)","SE_noPD"],
                       Table1["Age_status [45,65)","Mean_PD"],Table1["Age_status [45,65)","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("≥65",
                       Table1["Age_status >=65","Mean_all"],Table1["Age_status >=65","SE_all"],
                       Table1["Age_status >=65","Mean_noPD"],Table1["Age_status >=65","SE_noPD"],
                       Table1["Age_status >=65","Mean_PD"],Table1["Age_status >=65","SE_PD"],
                       "" ))
  
  #Sex
  Table<-rbind(Table,c("Sex, Female",
                       Table1["Sex Female","Mean_all"],Table1["Sex Female","SE_all"],
                       Table1["Sex Female","Mean_noPD"],Table1["Sex Female","SE_noPD"],
                       Table1["Sex Female","Mean_PD"],Table1["Sex Female","SE_PD"],
                       Table1["Sex Female","P.value"]))
  
  #Race/ ethnicity
  Table<-rbind(Table,c("Race/ ethnicity, %","","","","","","",Table1["Race_ethnicity Non-Hispanic White","P.value"]))
  Table<-rbind(Table,c("Non-Hispanic white",
                       Table1["Race_ethnicity Non-Hispanic White","Mean_all"],Table1["Race_ethnicity Non-Hispanic White","SE_all"],
                       Table1["Race_ethnicity Non-Hispanic White","Mean_noPD"],Table1["Race_ethnicity Non-Hispanic White","SE_noPD"],
                       Table1["Race_ethnicity Non-Hispanic White","Mean_PD"],Table1["Race_ethnicity Non-Hispanic White","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Non-Hispanic black",
                       Table1["Race_ethnicity Non-Hispanic Black","Mean_all"],Table1["Race_ethnicity Non-Hispanic Black","SE_all"],
                       Table1["Race_ethnicity Non-Hispanic Black","Mean_noPD"],Table1["Race_ethnicity Non-Hispanic Black","SE_noPD"],
                       Table1["Race_ethnicity Non-Hispanic Black","Mean_PD"],Table1["Race_ethnicity Non-Hispanic Black","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Hispanic",
                       Table1["Race_ethnicity Hispanic","Mean_all"],Table1["Race_ethnicity Hispanic","SE_all"],
                       Table1["Race_ethnicity Hispanic","Mean_noPD"],Table1["Race_ethnicity Hispanic","SE_noPD"],
                       Table1["Race_ethnicity Hispanic","Mean_PD"],Table1["Race_ethnicity Hispanic","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Other race/ ethnicity",
                       Table1["Race_ethnicity Other_Race","Mean_all"],Table1["Race_ethnicity Other_Race","SE_all"],
                       Table1["Race_ethnicity Other_Race","Mean_noPD"],Table1["Race_ethnicity Other_Race","SE_noPD"],
                       Table1["Race_ethnicity Other_Race","Mean_PD"],Table1["Race_ethnicity Other_Race","SE_PD"],
                       "" ))
  #Marital status
  Table<-rbind(Table,c("Marital status, %","","","","","","",Table1["Marital_status Married","P.value"]))
  Table<-rbind(Table,c("Married",
                       Table1["Marital_status Married","Mean_all"],Table1["Marital_status Married","SE_all"],
                       Table1["Marital_status Married","Mean_noPD"],Table1["Marital_status Married","SE_noPD"],
                       Table1["Marital_status Married","Mean_PD"],Table1["Marital_status Married","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Never married",
                       Table1["Marital_status Never_married","Mean_all"],Table1["Marital_status Never_married","SE_all"],
                       Table1["Marital_status Never_married","Mean_noPD"],Table1["Marital_status Never_married","SE_noPD"],
                       Table1["Marital_status Never_married","Mean_PD"],Table1["Marital_status Never_married","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Widowed/ Divorced/ Separated",
                       Table1["Marital_status Separated","Mean_all"],Table1["Marital_status Separated","SE_all"],
                       Table1["Marital_status Separated","Mean_noPD"],Table1["Marital_status Separated","SE_noPD"],
                       Table1["Marital_status Separated","Mean_PD"],Table1["Marital_status Separated","SE_PD"],
                       "" ))
  
  #Socioeconomic Status
  Table<-rbind(Table,c("Socioeconomic Status, %","","","","","","",Table1["SES low","P.value"]))
  Table<-rbind(Table,c("Low",
                       Table1["SES low","Mean_all"],Table1["SES low","SE_all"],
                       Table1["SES low","Mean_noPD"],Table1["SES low","SE_noPD"],
                       Table1["SES low","Mean_PD"],Table1["SES low","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Medium",
                       Table1["SES medium","Mean_all"],Table1["SES medium","SE_all"],
                       Table1["SES medium","Mean_noPD"],Table1["SES medium","SE_noPD"],
                       Table1["SES medium","Mean_PD"],Table1["SES medium","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("High",
                       Table1["SES high","Mean_all"],Table1["SES high","SE_all"],
                       Table1["SES high","Mean_noPD"],Table1["SES high","SE_noPD"],
                       Table1["SES high","Mean_PD"],Table1["SES high","SE_PD"],
                       "" ))
  
  #Smoking status
  Table<-rbind(Table,c("Smoking status, %","","","","","","",Table1["Smoking_status Never_smoker","P.value"]))
  Table<-rbind(Table,c("Never smoker",
                       Table1["Smoking_status Never_smoker","Mean_all"],Table1["Smoking_status Never_smoker","SE_all"],
                       Table1["Smoking_status Never_smoker","Mean_noPD"],Table1["Smoking_status Never_smoker","SE_noPD"],
                       Table1["Smoking_status Never_smoker","Mean_PD"],Table1["Smoking_status Never_smoker","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Former smoker",
                       Table1["Smoking_status Former_smoker","Mean_all"],Table1["Smoking_status Former_smoker","SE_all"],
                       Table1["Smoking_status Former_smoker","Mean_noPD"],Table1["Smoking_status Former_smoker","SE_noPD"],
                       Table1["Smoking_status Former_smoker","Mean_PD"],Table1["Smoking_status Former_smoker","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Current smoker",
                       Table1["Smoking_status Current_smoker","Mean_all"],Table1["Smoking_status Current_smoker","SE_all"],
                       Table1["Smoking_status Current_smoker","Mean_noPD"],Table1["Smoking_status Current_smoker","SE_noPD"],
                       Table1["Smoking_status Current_smoker","Mean_PD"],Table1["Smoking_status Current_smoker","SE_PD"],
                       "" ))
  
  #Drinking status
  Table<-rbind(Table,c("Drinking status, %","","","","","","",Table1["Drinking_status Nondrinker","P.value"]))
  Table<-rbind(Table,c("Nondrinker",
                       Table1["Drinking_status Nondrinker","Mean_all"],Table1["Drinking_status Nondrinker","SE_all"],
                       Table1["Drinking_status Nondrinker","Mean_noPD"],Table1["Drinking_status Nondrinker","SE_noPD"],
                       Table1["Drinking_status Nondrinker","Mean_PD"],Table1["Drinking_status Nondrinker","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Light/ moderate drinker",
                       Table1["Drinking_status Light/moderate_drinker","Mean_all"],Table1["Drinking_status Light/moderate_drinker","SE_all"],
                       Table1["Drinking_status Light/moderate_drinker","Mean_noPD"],Table1["Drinking_status Light/moderate_drinker","SE_noPD"],
                       Table1["Drinking_status Light/moderate_drinker","Mean_PD"],Table1["Drinking_status Light/moderate_drinker","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Heavier drinker",
                       Table1["Drinking_status Heavier_drinker","Mean_all"],Table1["Drinking_status Heavier_drinker","SE_all"],
                       Table1["Drinking_status Heavier_drinker","Mean_noPD"],Table1["Drinking_status Heavier_drinker","SE_noPD"],
                       Table1["Drinking_status Heavier_drinker","Mean_PD"],Table1["Drinking_status Heavier_drinker","SE_PD"],
                       "" ))
  #Physical status
  Table<-rbind(Table,c("Physical status, %","","","","","","",Table1["Physical_status Inactive","P.value"]))
  Table<-rbind(Table,c("Inactive",
                       Table1["Physical_status Inactive","Mean_all"],Table1["Physical_status Inactive","SE_all"],
                       Table1["Physical_status Inactive","Mean_noPD"],Table1["Physical_status Inactive","SE_noPD"],
                       Table1["Physical_status Inactive","Mean_PD"],Table1["Physical_status Inactive","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Insufficient",
                       Table1["Physical_status Insufficient","Mean_all"],Table1["Physical_status Insufficient","SE_all"],
                       Table1["Physical_status Insufficient","Mean_noPD"],Table1["Physical_status Insufficient","SE_noPD"],
                       Table1["Physical_status Insufficient","Mean_PD"],Table1["Physical_status Insufficient","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Recommended",
                       Table1["Physical_status Recommended","Mean_all"],Table1["Physical_status Recommended","SE_all"],
                       Table1["Physical_status Recommended","Mean_noPD"],Table1["Physical_status Recommended","SE_noPD"],
                       Table1["Physical_status Recommended","Mean_PD"],Table1["Physical_status Recommended","SE_PD"],
                       "" ))
  #BMI
  Table<-rbind(Table,c("BMI, Mean",
                       Table1["BMI Mean ± SE","Mean_all"],Table1["BMI Mean ± SE","SE_all"],
                       Table1["BMI Mean ± SE","Mean_noPD"],Table1["BMI Mean ± SE","SE_noPD"],
                       Table1["BMI Mean ± SE","Mean_PD"],Table1["BMI Mean ± SE","SE_PD"],
                       Table1["BMI Mean ± SE","P.value"] ))
  
  Table<-rbind(Table,c("BMI status (kg/m2), %","","","","","","",Table1["BMI_status (0,25)","P.value"]))
  Table<-rbind(Table,c("<25",
                       Table1["BMI_status (0,25)","Mean_all"],Table1["BMI_status (0,25)","SE_all"],
                       Table1["BMI_status (0,25)","Mean_noPD"],Table1["BMI_status (0,25)","SE_noPD"],
                       Table1["BMI_status (0,25)","Mean_PD"],Table1["BMI_status (0,25)","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("[25.0 -30)",
                       Table1["BMI_status [25.0-30)","Mean_all"],Table1["BMI_status [25.0-30)","SE_all"],
                       Table1["BMI_status [25.0-30)","Mean_noPD"],Table1["BMI_status [25.0-30)","SE_noPD"],
                       Table1["BMI_status [25.0-30)","Mean_PD"],Table1["BMI_status [25.0-30)","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("≥30",
                       Table1["BMI_status [30,inf)","Mean_all"],Table1["BMI_status [30,inf)","SE_all"],
                       Table1["BMI_status [30,inf)","Mean_noPD"],Table1["BMI_status [30,inf)","SE_noPD"],
                       Table1["BMI_status [30,inf)","Mean_PD"],Table1["BMI_status [30,inf)","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Comorbidities, %","","","","","","",""))
  #Hypertension
  Table<-rbind(Table,c("Hypertension",
                       Table1["HTN_status YES","Mean_all"],Table1["HTN_status YES","SE_all"],
                       Table1["HTN_status YES","Mean_noPD"],Table1["HTN_status YES","SE_noPD"],
                       Table1["HTN_status YES","Mean_PD"],Table1["HTN_status YES","SE_PD"],
                       Table1["HTN_status YES","P.value"]))
  #Hyperlipidemia
  Table<-rbind(Table,c("Hyperlipidemia",
                       Table1["HPL_status YES","Mean_all"],Table1["HPL_status YES","SE_all"],
                       Table1["HPL_status YES","Mean_noPD"],Table1["HPL_status YES","SE_noPD"],
                       Table1["HPL_status YES","Mean_PD"],Table1["HPL_status YES","SE_PD"],
                       Table1["HPL_status YES","P.value"]))
  #Diabetes mellitus
  Table<-rbind(Table,c("Diabetes mellitus",
                       Table1["T2D_status YES","Mean_all"],Table1["T2D_status YES","SE_all"],
                       Table1["T2D_status YES","Mean_noPD"],Table1["T2D_status YES","SE_noPD"],
                       Table1["T2D_status YES","Mean_PD"],Table1["T2D_status YES","SE_PD"],
                       Table1["T2D_status YES","P.value"]))
  #Cohort
  Table<-rbind(Table,c("Cohort period, %","","","","","","",Table1["Cohort NHANES_CON1","P.value"]))
  
  Table<-rbind(Table,c("NHANES III",
                       Table1["Cohort NHANES_III","Mean_all"],Table1["Cohort NHANES_III","SE_all"],
                       Table1["Cohort NHANES_III","Mean_noPD"],Table1["Cohort NHANES_III","SE_noPD"],
                       Table1["Cohort NHANES_III","Mean_PD"],Table1["Cohort NHANES_III","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("NHANES 1999-2004",
                       Table1["Cohort NHANES_CON1","Mean_all"],Table1["Cohort NHANES_CON1","SE_all"],
                       Table1["Cohort NHANES_CON1","Mean_noPD"],Table1["Cohort NHANES_CON1","SE_noPD"],
                       Table1["Cohort NHANES_CON1","Mean_PD"],Table1["Cohort NHANES_CON1","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("NHANES 2009-2014",
                       Table1["Cohort NHANES_CON2","Mean_all"],Table1["Cohort NHANES_CON2","SE_all"],
                       Table1["Cohort NHANES_CON2","Mean_noPD"],Table1["Cohort NHANES_CON2","SE_noPD"],
                       Table1["Cohort NHANES_CON2","Mean_PD"],Table1["Cohort NHANES_CON2","SE_PD"],
                       "" ))
  
  Table_S12<-Table
  write.table(Table_S12,sep = ",",file ="I:/NHANES study/PD&TYG&MO/Result/Supplementary Table 12.csv" ,row.names =F,col.names =F )
}


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 30 Original data COX (Table S13) ####  
load(file="I:/NHANES study/PD&TYG&MO/Data/Original_weighted.Rdata")
Interpolation_weighted<-Original_weighted
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
{ #* all model #####
  #all model1
  model1<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PD_diagnosis, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model2
  model2<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result<-rbind(result,result2)
  #all model3
  model3<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_result<-summary(model3)
  
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result<-rbind(result,result3)
  result
  #all model4
  model4<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result_All<-rbind(result,result4)
  result_All
}

{ #* CVD model #####
  
  #CVD Crude
  model1<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PD_diagnosis, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="CVD cause")
  result
  #CVD model1
  model2<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="CVD cause")
  result<-rbind(result,result2)
  result
  #CVD model2
  model3<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_result<-summary(model3)
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="CVD cause")
  result<-rbind(result,result3)
  result
  #CVD model3
  model4<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result_CVD<-rbind(result,result4)
  result_CVD
  
}

{ #* Cancer model #####
  
  #Cancer model1
  model1<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="Cancer cause")
  result
  #Cancer model2
  model2<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="Cancer cause")
  result<-rbind(result,result2)
  result
  #Cancer model3
  model3<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status,design =rhcSvy)
  model3_result<-summary(model3)
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="Cancer cause")
  result<-rbind(result,result3)
  result
  #Cancer model4
  model4<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result_Cancer<-rbind(result,result4)
  result_Cancer
}



{ #* Combine #####
  #Results
  result.cause<-rbind(result_All,result_CVD,result_Cancer)
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  round_3_function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
  
  result.cause[result.cause=="10000"]<-"1.000"
  result.cause[result.cause=="00000"]<-"<0.001"
  result.cause
  #Events
  #all.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$MORT_stat,useNA = "ifany")
  all.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #CVD.cause
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$CVD_MORT_stat,useNA = "ifany")
  CVD.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #Cancer.cause
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$Cancer_MORT_stat,useNA = "ifany")
  Cancer.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  total.counts<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause))
  total.counts
  
  #PD_MDD
  PD_MDD<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="Moderate/Severe periodontitis"),]
  PD_MDD$year<-PD_MDD$peryear
  PD_MDD$Pyear<-PD_MDD$year*PD_MDD$weight
  PD_MDD_death_All<-PD_MDD[which(PD_MDD$MORT_stat==1),]
  PD_MDD_death_CVD<-PD_MDD[which(PD_MDD$CVD_MORT_stat==1),]
  PD_MDD_death_Cancer<-PD_MDD[which(PD_MDD$Cancer_MORT_stat==1),]
  
  #noPD_MDD
  noPD_MDD<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="No/Mild periodontitis"),]
  noPD_MDD$year<-noPD_MDD$peryear
  noPD_MDD$Pyear<-noPD_MDD$year*noPD_MDD$weight
  noPD_MDD_death_All<-noPD_MDD[which(noPD_MDD$MORT_stat==1),]
  noPD_MDD_death_CVD<-noPD_MDD[which(noPD_MDD$CVD_MORT_stat==1),]
  noPD_MDD_death_Cancer<-noPD_MDD[which(noPD_MDD$Cancer_MORT_stat==1),]
  
  #all.cause
  PD.counts<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  PD.cause.counts<-format(round(sum(PD_MDD_death_All$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.cause.counts<-format(round(sum(noPD_MDD_death_All$weight)), big.mark = ",", scientific = FALSE)
  
  all.cause<-c(paste0(PD.cause.counts,"/",PD.counts),paste0(noPD.cause.counts,"/",noPD.counts))
  #CVD.cause
  PD.counts<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  PD.cause.counts<-format(round(sum(PD_MDD_death_CVD$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.cause.counts<-format(round(sum(noPD_MDD_death_CVD$weight)), big.mark = ",", scientific = FALSE)
  CVD.cause<-c(paste0(PD.cause.counts,"/",PD.counts),paste0(noPD.cause.counts,"/",noPD.counts))
  #Cancer.cause
  PD.counts<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  PD.cause.counts<-format(round(sum(PD_MDD_death_Cancer$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.cause.counts<-format(round(sum(noPD_MDD_death_Cancer$weight)), big.mark = ",", scientific = FALSE)
  Cancer.cause<-c(paste0(PD.cause.counts,"/",PD.counts),paste0(noPD.cause.counts,"/",noPD.counts))
  total.counts_ad<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause))
  total.counts_ad
  
  #Cbind
  Table<-c("","Periodontal status","","")
  Table<-rbind(Table,c("", "No/Mild periodontitis","Moderate/Severe periodontitis","P"))
  Table<-rbind(Table,c("All cause*","","",""))
  Table<-rbind(Table,c("Deaths/total (Unweighted)",total.counts[1,1],total.counts[1,2],""))
  Table<-rbind(Table,c("Deaths/total (Weighted)",total.counts_ad[1,1],total.counts_ad[1,2],""))
  Table<-rbind(Table,c("Model 1†","1.00 [Reference]",paste0(result.cause[1,1]," (",result.cause[1,2],", ",result.cause[1,3],")"),result.cause[1,4]))
  Table<-rbind(Table,c("Model 2‡","1.00 [Reference]",paste0(result.cause[2,1]," (",result.cause[2,2],", ",result.cause[2,3],")"),result.cause[2,4]))
  Table<-rbind(Table,c("Model 3§","1.00 [Reference]",paste0(result.cause[3,1]," (",result.cause[3,2],", ",result.cause[3,3],")"),result.cause[3,4]))
  Table<-rbind(Table,c("Model 4¶","1.00 [Reference]",paste0(result.cause[4,1]," (",result.cause[4,2],", ",result.cause[4,3],")"),result.cause[4,4]))
  Table<-rbind(Table,c("CVD cause","","",""))
  Table<-rbind(Table,c("Deaths/total (Unweighted)",total.counts[2,1],total.counts[2,2],""))
  Table<-rbind(Table,c("Deaths/total (Weighted)",total.counts_ad[2,1],total.counts_ad[2,2],""))
  Table<-rbind(Table,c("Model 1","1.00 [Reference]",paste0(result.cause[5,1]," (",result.cause[5,2],", ",result.cause[5,3],")"),result.cause[5,4]))
  Table<-rbind(Table,c("Model 2","1.00 [Reference]",paste0(result.cause[6,1]," (",result.cause[6,2],", ",result.cause[6,3],")"),result.cause[6,4]))
  Table<-rbind(Table,c("Model 3","1.00 [Reference]",paste0(result.cause[7,1]," (",result.cause[7,2],", ",result.cause[7,3],")"),result.cause[7,4]))
  Table<-rbind(Table,c("Model 4","1.00 [Reference]",paste0(result.cause[8,1]," (",result.cause[8,2],", ",result.cause[8,3],")"),result.cause[8,4]))
  Table<-rbind(Table,c("Cancer cause","","",""))
  Table<-rbind(Table,c("Deaths/total (Unweighted)",total.counts[3,1],total.counts[3,2],""))
  Table<-rbind(Table,c("Deaths/total (Weighted)",total.counts_ad[3,1],total.counts_ad[3,2],""))
  Table<-rbind(Table,c("Model 1","1.00 [Reference]",paste0(result.cause[9,1]," (",result.cause[9,2],", ",result.cause[9,3],")"),result.cause[9,4]))
  Table<-rbind(Table,c("Model 2","1.00 [Reference]",paste0(result.cause[10,1]," (",result.cause[10,2],", ",result.cause[10,3],")"),result.cause[10,4]))
  Table<-rbind(Table,c("Model 3","1.00 [Reference]",paste0(result.cause[11,1]," (",result.cause[11,2],", ",result.cause[11,3],")"),result.cause[11,4]))
  Table<-rbind(Table,c("Model 4","1.00 [Reference]",paste0(result.cause[12,1]," (",result.cause[12,2],", ",result.cause[12,3],")"),result.cause[12,4]))
  Table<-as.data.frame(Table)
  Table_S13 = data.frame(lapply(Table, as.character), stringsAsFactors=FALSE)
  Table_S13
  write.table(Table_S13,sep = ",",file ="I:/NHANES study/PD&TYG&MO/Result/Supplementary Table 13.csv",row.names =F,col.names =F )
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 29 Complete data (Table S14) ####  
load(file="I:/NHANES study/PD&TYG&MO/Data/Complete_weighted.Rdata")
Interpolation_weighted<-Complete_weighted
colnames(Interpolation_weighted)

table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$MORT_stat)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
var<-c("MORT_stat","Age_status","Sex","Marital_status","Race_ethnicity","Education_levels","PIR",
       "Health_insurance","SEI","SES","Smoking_status","Drinking_status","Physical_status","HEI",
       "HPL_status","HTN_status","CVD_status",
       "BMI_status","T2D_status","Cancer_status","Cohort")
VAR<-c("MORT_stat","TYG","TyG_WC_mean","Age","Marital_status","Age_status","Sex","Race_ethnicity","Education_levels","PIR",
       "Health_insurance","SEI","SES","Smoking_status","Drinking_status","Physical_status","HEI", "HTN_status","HPL_status",
       "BMI","BMI_status","T2D_status","CVD_status","Cancer_status","Cohort")

options(survey.lonely.psu="adjust")
options(survey.adjust.domain.lonely=TRUE)
{ #* section 18.1 Over all ####
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,1),
                          'SE' = round(svymean$SE*100,1) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
                          'Mean' =round(svymean$mean,1),
                          'SE' = round(svymean$SE,1))
      return(model)
    }
  }  
  Over<- ldply(lapply(VAR, model))
}
options(survey.lonely.psu="adjust")
options(survey.adjust.domain.lonely=TRUE)

{ #* section 18.2 No/Mild periodontitis ####
  rhcSvy_HEI2PD<-subset(rhcSvy,PD_diagnosis=="No/Mild periodontitis")
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy_HEI2PD,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_HEI2PD, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,1),
                          'SE' = round(svymean$SE*100,1) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_HEI2PD, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
                          'Mean' =round(svymean$mean,1),
                          'SE' = round(svymean$SE,1))
      return(model)
    }
  }  
  noPD<- ldply(lapply(VAR, model))
}  
{ #* section 18.3 Moderate/Severe periodontitis ####
  rhcSvy_PD<-subset(rhcSvy,PD_diagnosis=="Moderate/Severe periodontitis")
  model<- function(x){
    
    if( x %in% var ) {
      Covariates<-as.formula(paste0("~",x))
      unwtd_count<-svyby(Covariates,Covariates,rhcSvy_PD,unwtd.count) 
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      model <- data.frame('Covariates'=x,
                          'grade' = gsub(x,"",rownames(svymean)),
                          'counts'=unwtd_count[2],
                          'Mean' = round(svymean$mean*100,1),
                          'SE' = round(svymean$SE*100,1) )
      return(model)
    } else {
      
      Covariates<-as.formula(paste0("~",x))
      svymean<-as.data.frame(svymean(Covariates,rhcSvy_PD, na.rm = TRUE))
      colnames(svymean)[2]<-"SE"
      model <- data.frame('Covariates'=x,
                          'grade' ="Mean ± SE",
                          'counts'=' ',
                          'Mean' =round(svymean$mean,1),
                          'SE' = round(svymean$SE,1))
      return(model)
    }
  }  
  PD<- ldply(lapply(VAR, model))
}
Table1<-cbind(Over,noPD[,c("counts","Mean","SE")],PD[,c("counts","Mean","SE")])
save(Table1,file = "I:/NHANES study/PD&TYG&MO/Data/Table1_Rdata")  
{ #* section 18.4 t-test and chi-test ####
  model<- function(x){
    
    if( x %in% var ) {
      formula<-as.formula(paste0("~",x,"+PD_diagnosis"))
      chi_test<-svychisq(formula,rhcSvy)
      model <- data.frame('Covariates'=x,
                          'P value' =chi_test[["p.value"]])
      return(model)
    } else {
      formula<-as.formula(paste0(x,"~PD_diagnosis"))
      t_test<-svyttest(formula,rhcSvy)
      model <- data.frame('Covariates'=x,
                          'P value' =t_test[["p.value"]])
      return(model)
    }
  }  
  test_data<- ldply(lapply(VAR, model))
  test_data$P.value<-round(test_data$P.value,3)
  test_data$P.value[test_data$P.value==0]<-"<0.001"
  new.function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  test_data$P.value<-lapply(test_data$P.value,new.function)
  test_data$P.value<-as.character(test_data$P.value)
}
load(file = "I:/NHANES study/PD&TYG&MO/Data/Table1_Rdata")
Table1<-merge(Table1,test_data,by="Covariates",all.x = T)
Table1$Covariates
Table1$Row<-paste0(Table1$Covariates," ",Table1$grade)
rownames(Table1)<-Table1$Row
colnames(Table1)<-c("Covariates","grade",
                    "counts_all","Mean_all","SE_all",
                    "counts_noPD","Mean_noPD","SE_noPD",
                    "counts_PD","Mean_PD","SE_PD",
                    "P.value","Row")
rownames(Table1)
{ #* section 18.5 Combine  Table S14 #####
  #Events
  #all.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  
  PD_MDD<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="Moderate/Severe periodontitis"),]
  noPD_MDD<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="No/Mild periodontitis"),]
  
  PD.counts_ad<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts_ad<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  Counts_ad<-format(round(sum(PD_MDD$weight))+round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  
  
  Table<-c("","","","Periodontal status","","","","")
  Table<-rbind(Table,c("","Over all","","No/Mild periodontitis","","Moderate/Severe periodontitis","",""))
  Table<-rbind(Table,c("Characteristics","Mean/ %*","SE*","Mean/ %","SE","Mean/ %","SE","P†"))
  Table<-rbind(Table,c("No.(Unweighted)",PD.counts[1]+PD.counts[2],"",PD.counts[1],"",PD.counts[2],"",""))
  Table<-rbind(Table,c("No.(Weighted)",Counts_ad,"",PD.counts_ad,"",noPD.counts_ad,"",""))
  #Age
  Table<-rbind(Table,c("Age (years), mean",
                       Table1["Age Mean ± SE","Mean_all"],Table1["Age Mean ± SE","SE_all"],
                       Table1["Age Mean ± SE","Mean_noPD"],Table1["Age Mean ± SE","SE_noPD"],
                       Table1["Age Mean ± SE","Mean_PD"],Table1["Age Mean ± SE","SE_PD"],
                       Table1["Age Mean ± SE","P.value"] ))
  Table<-rbind(Table,c("Age status, %","","","","","","",Table1["Age_status <45","P.value"]))
  Table<-rbind(Table,c("<45",
                       Table1["Age_status <45","Mean_all"],Table1["Age_status <45","SE_all"],
                       Table1["Age_status <45","Mean_noPD"],Table1["Age_status <45","SE_noPD"],
                       Table1["Age_status <45","Mean_PD"],Table1["Age_status <45","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("[45, 65)",
                       Table1["Age_status [45,65)","Mean_all"],Table1["Age_status [45,65)","SE_all"],
                       Table1["Age_status [45,65)","Mean_noPD"],Table1["Age_status [45,65)","SE_noPD"],
                       Table1["Age_status [45,65)","Mean_PD"],Table1["Age_status [45,65)","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("≥65",
                       Table1["Age_status >=65","Mean_all"],Table1["Age_status >=65","SE_all"],
                       Table1["Age_status >=65","Mean_noPD"],Table1["Age_status >=65","SE_noPD"],
                       Table1["Age_status >=65","Mean_PD"],Table1["Age_status >=65","SE_PD"],
                       "" ))
  
  #Sex
  Table<-rbind(Table,c("Sex, Female",
                       Table1["Sex Female","Mean_all"],Table1["Sex Female","SE_all"],
                       Table1["Sex Female","Mean_noPD"],Table1["Sex Female","SE_noPD"],
                       Table1["Sex Female","Mean_PD"],Table1["Sex Female","SE_PD"],
                       Table1["Sex Female","P.value"]))
  
  #Race/ ethnicity
  Table<-rbind(Table,c("Race/ ethnicity, %","","","","","","",Table1["Race_ethnicity Non-Hispanic White","P.value"]))
  Table<-rbind(Table,c("Non-Hispanic white",
                       Table1["Race_ethnicity Non-Hispanic White","Mean_all"],Table1["Race_ethnicity Non-Hispanic White","SE_all"],
                       Table1["Race_ethnicity Non-Hispanic White","Mean_noPD"],Table1["Race_ethnicity Non-Hispanic White","SE_noPD"],
                       Table1["Race_ethnicity Non-Hispanic White","Mean_PD"],Table1["Race_ethnicity Non-Hispanic White","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Non-Hispanic black",
                       Table1["Race_ethnicity Non-Hispanic Black","Mean_all"],Table1["Race_ethnicity Non-Hispanic Black","SE_all"],
                       Table1["Race_ethnicity Non-Hispanic Black","Mean_noPD"],Table1["Race_ethnicity Non-Hispanic Black","SE_noPD"],
                       Table1["Race_ethnicity Non-Hispanic Black","Mean_PD"],Table1["Race_ethnicity Non-Hispanic Black","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Hispanic",
                       Table1["Race_ethnicity Hispanic","Mean_all"],Table1["Race_ethnicity Hispanic","SE_all"],
                       Table1["Race_ethnicity Hispanic","Mean_noPD"],Table1["Race_ethnicity Hispanic","SE_noPD"],
                       Table1["Race_ethnicity Hispanic","Mean_PD"],Table1["Race_ethnicity Hispanic","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Other race/ ethnicity",
                       Table1["Race_ethnicity Other_Race","Mean_all"],Table1["Race_ethnicity Other_Race","SE_all"],
                       Table1["Race_ethnicity Other_Race","Mean_noPD"],Table1["Race_ethnicity Other_Race","SE_noPD"],
                       Table1["Race_ethnicity Other_Race","Mean_PD"],Table1["Race_ethnicity Other_Race","SE_PD"],
                       "" ))
  #Marital status
  Table<-rbind(Table,c("Marital status, %","","","","","","",Table1["Marital_status Married","P.value"]))
  Table<-rbind(Table,c("Married",
                       Table1["Marital_status Married","Mean_all"],Table1["Marital_status Married","SE_all"],
                       Table1["Marital_status Married","Mean_noPD"],Table1["Marital_status Married","SE_noPD"],
                       Table1["Marital_status Married","Mean_PD"],Table1["Marital_status Married","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Never married",
                       Table1["Marital_status Never_married","Mean_all"],Table1["Marital_status Never_married","SE_all"],
                       Table1["Marital_status Never_married","Mean_noPD"],Table1["Marital_status Never_married","SE_noPD"],
                       Table1["Marital_status Never_married","Mean_PD"],Table1["Marital_status Never_married","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Widowed/ Divorced/ Separated",
                       Table1["Marital_status Separated","Mean_all"],Table1["Marital_status Separated","SE_all"],
                       Table1["Marital_status Separated","Mean_noPD"],Table1["Marital_status Separated","SE_noPD"],
                       Table1["Marital_status Separated","Mean_PD"],Table1["Marital_status Separated","SE_PD"],
                       "" ))
  
  #Socioeconomic Status
  Table<-rbind(Table,c("Socioeconomic Status, %","","","","","","",Table1["SES low","P.value"]))
  Table<-rbind(Table,c("Low",
                       Table1["SES low","Mean_all"],Table1["SES low","SE_all"],
                       Table1["SES low","Mean_noPD"],Table1["SES low","SE_noPD"],
                       Table1["SES low","Mean_PD"],Table1["SES low","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Medium",
                       Table1["SES medium","Mean_all"],Table1["SES medium","SE_all"],
                       Table1["SES medium","Mean_noPD"],Table1["SES medium","SE_noPD"],
                       Table1["SES medium","Mean_PD"],Table1["SES medium","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("High",
                       Table1["SES high","Mean_all"],Table1["SES high","SE_all"],
                       Table1["SES high","Mean_noPD"],Table1["SES high","SE_noPD"],
                       Table1["SES high","Mean_PD"],Table1["SES high","SE_PD"],
                       "" ))
  
  #Smoking status
  Table<-rbind(Table,c("Smoking status, %","","","","","","",Table1["Smoking_status Never_smoker","P.value"]))
  Table<-rbind(Table,c("Never smoker",
                       Table1["Smoking_status Never_smoker","Mean_all"],Table1["Smoking_status Never_smoker","SE_all"],
                       Table1["Smoking_status Never_smoker","Mean_noPD"],Table1["Smoking_status Never_smoker","SE_noPD"],
                       Table1["Smoking_status Never_smoker","Mean_PD"],Table1["Smoking_status Never_smoker","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Former smoker",
                       Table1["Smoking_status Former_smoker","Mean_all"],Table1["Smoking_status Former_smoker","SE_all"],
                       Table1["Smoking_status Former_smoker","Mean_noPD"],Table1["Smoking_status Former_smoker","SE_noPD"],
                       Table1["Smoking_status Former_smoker","Mean_PD"],Table1["Smoking_status Former_smoker","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Current smoker",
                       Table1["Smoking_status Current_smoker","Mean_all"],Table1["Smoking_status Current_smoker","SE_all"],
                       Table1["Smoking_status Current_smoker","Mean_noPD"],Table1["Smoking_status Current_smoker","SE_noPD"],
                       Table1["Smoking_status Current_smoker","Mean_PD"],Table1["Smoking_status Current_smoker","SE_PD"],
                       "" ))
  
  #Drinking status
  Table<-rbind(Table,c("Drinking status, %","","","","","","",Table1["Drinking_status Nondrinker","P.value"]))
  Table<-rbind(Table,c("Nondrinker",
                       Table1["Drinking_status Nondrinker","Mean_all"],Table1["Drinking_status Nondrinker","SE_all"],
                       Table1["Drinking_status Nondrinker","Mean_noPD"],Table1["Drinking_status Nondrinker","SE_noPD"],
                       Table1["Drinking_status Nondrinker","Mean_PD"],Table1["Drinking_status Nondrinker","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Light/ moderate drinker",
                       Table1["Drinking_status Light/moderate_drinker","Mean_all"],Table1["Drinking_status Light/moderate_drinker","SE_all"],
                       Table1["Drinking_status Light/moderate_drinker","Mean_noPD"],Table1["Drinking_status Light/moderate_drinker","SE_noPD"],
                       Table1["Drinking_status Light/moderate_drinker","Mean_PD"],Table1["Drinking_status Light/moderate_drinker","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Heavier drinker",
                       Table1["Drinking_status Heavier_drinker","Mean_all"],Table1["Drinking_status Heavier_drinker","SE_all"],
                       Table1["Drinking_status Heavier_drinker","Mean_noPD"],Table1["Drinking_status Heavier_drinker","SE_noPD"],
                       Table1["Drinking_status Heavier_drinker","Mean_PD"],Table1["Drinking_status Heavier_drinker","SE_PD"],
                       "" ))
  #Physical status
  Table<-rbind(Table,c("Physical status, %","","","","","","",Table1["Physical_status Inactive","P.value"]))
  Table<-rbind(Table,c("Inactive",
                       Table1["Physical_status Inactive","Mean_all"],Table1["Physical_status Inactive","SE_all"],
                       Table1["Physical_status Inactive","Mean_noPD"],Table1["Physical_status Inactive","SE_noPD"],
                       Table1["Physical_status Inactive","Mean_PD"],Table1["Physical_status Inactive","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Insufficient",
                       Table1["Physical_status Insufficient","Mean_all"],Table1["Physical_status Insufficient","SE_all"],
                       Table1["Physical_status Insufficient","Mean_noPD"],Table1["Physical_status Insufficient","SE_noPD"],
                       Table1["Physical_status Insufficient","Mean_PD"],Table1["Physical_status Insufficient","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Recommended",
                       Table1["Physical_status Recommended","Mean_all"],Table1["Physical_status Recommended","SE_all"],
                       Table1["Physical_status Recommended","Mean_noPD"],Table1["Physical_status Recommended","SE_noPD"],
                       Table1["Physical_status Recommended","Mean_PD"],Table1["Physical_status Recommended","SE_PD"],
                       "" ))
  #BMI
  Table<-rbind(Table,c("BMI, Mean",
                       Table1["BMI Mean ± SE","Mean_all"],Table1["BMI Mean ± SE","SE_all"],
                       Table1["BMI Mean ± SE","Mean_noPD"],Table1["BMI Mean ± SE","SE_noPD"],
                       Table1["BMI Mean ± SE","Mean_PD"],Table1["BMI Mean ± SE","SE_PD"],
                       Table1["BMI Mean ± SE","P.value"] ))
  
  Table<-rbind(Table,c("BMI status (kg/m2), %","","","","","","",Table1["BMI_status (0,25)","P.value"]))
  Table<-rbind(Table,c("<25",
                       Table1["BMI_status (0,25)","Mean_all"],Table1["BMI_status (0,25)","SE_all"],
                       Table1["BMI_status (0,25)","Mean_noPD"],Table1["BMI_status (0,25)","SE_noPD"],
                       Table1["BMI_status (0,25)","Mean_PD"],Table1["BMI_status (0,25)","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("[25.0 -30)",
                       Table1["BMI_status [25.0-30)","Mean_all"],Table1["BMI_status [25.0-30)","SE_all"],
                       Table1["BMI_status [25.0-30)","Mean_noPD"],Table1["BMI_status [25.0-30)","SE_noPD"],
                       Table1["BMI_status [25.0-30)","Mean_PD"],Table1["BMI_status [25.0-30)","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("≥30",
                       Table1["BMI_status [30,inf)","Mean_all"],Table1["BMI_status [30,inf)","SE_all"],
                       Table1["BMI_status [30,inf)","Mean_noPD"],Table1["BMI_status [30,inf)","SE_noPD"],
                       Table1["BMI_status [30,inf)","Mean_PD"],Table1["BMI_status [30,inf)","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("Comorbidities, %","","","","","","",""))
  #Hypertension
  Table<-rbind(Table,c("Hypertension",
                       Table1["HTN_status YES","Mean_all"],Table1["HTN_status YES","SE_all"],
                       Table1["HTN_status YES","Mean_noPD"],Table1["HTN_status YES","SE_noPD"],
                       Table1["HTN_status YES","Mean_PD"],Table1["HTN_status YES","SE_PD"],
                       Table1["HTN_status YES","P.value"]))
  #Hyperlipidemia
  Table<-rbind(Table,c("Hyperlipidemia",
                       Table1["HPL_status YES","Mean_all"],Table1["HPL_status YES","SE_all"],
                       Table1["HPL_status YES","Mean_noPD"],Table1["HPL_status YES","SE_noPD"],
                       Table1["HPL_status YES","Mean_PD"],Table1["HPL_status YES","SE_PD"],
                       Table1["HPL_status YES","P.value"]))
  #Diabetes mellitus
  Table<-rbind(Table,c("Diabetes mellitus",
                       Table1["T2D_status YES","Mean_all"],Table1["T2D_status YES","SE_all"],
                       Table1["T2D_status YES","Mean_noPD"],Table1["T2D_status YES","SE_noPD"],
                       Table1["T2D_status YES","Mean_PD"],Table1["T2D_status YES","SE_PD"],
                       Table1["T2D_status YES","P.value"]))
  #Cohort
  Table<-rbind(Table,c("Cohort period, %","","","","","","",Table1["Cohort NHANES_CON1","P.value"]))
  
  Table<-rbind(Table,c("NHANES III",
                       Table1["Cohort NHANES_III","Mean_all"],Table1["Cohort NHANES_III","SE_all"],
                       Table1["Cohort NHANES_III","Mean_noPD"],Table1["Cohort NHANES_III","SE_noPD"],
                       Table1["Cohort NHANES_III","Mean_PD"],Table1["Cohort NHANES_III","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("NHANES 1999-2004",
                       Table1["Cohort NHANES_CON1","Mean_all"],Table1["Cohort NHANES_CON1","SE_all"],
                       Table1["Cohort NHANES_CON1","Mean_noPD"],Table1["Cohort NHANES_CON1","SE_noPD"],
                       Table1["Cohort NHANES_CON1","Mean_PD"],Table1["Cohort NHANES_CON1","SE_PD"],
                       "" ))
  Table<-rbind(Table,c("NHANES 2009-2014",
                       Table1["Cohort NHANES_CON2","Mean_all"],Table1["Cohort NHANES_CON2","SE_all"],
                       Table1["Cohort NHANES_CON2","Mean_noPD"],Table1["Cohort NHANES_CON2","SE_noPD"],
                       Table1["Cohort NHANES_CON2","Mean_PD"],Table1["Cohort NHANES_CON2","SE_PD"],
                       "" ))
  
  Table_S14<-Table
  write.table(Table_S14,sep = ",",file ="I:/NHANES study/PD&TYG&MO/Result/Supplementary Table 14.csv" ,row.names =F,col.names =F )
}


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 30 Complete data COX (Table S15) ####  
load(file="I:/NHANES study/PD&TYG&MO/Data/Complete_weighted.Rdata")
Interpolation_weighted<-Complete_weighted
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)
{ #* all model #####
  #all model1
  model1<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PD_diagnosis, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="All cause")
  result
  #all model2
  model2<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="All cause")
  result2
  result<-rbind(result,result2)
  #all model3
  model3<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_result<-summary(model3)
  
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="All cause")
  result<-rbind(result,result3)
  result
  #all model4
  model4<-svycoxph(Surv(peryear, MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result_All<-rbind(result,result4)
  result_All
}

{ #* CVD model #####
  
  #CVD Crude
  model1<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PD_diagnosis, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="CVD cause")
  result
  #CVD model1
  model2<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="CVD cause")
  result<-rbind(result,result2)
  result
  #CVD model2
  model3<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status, design =rhcSvy)
  model3_result<-summary(model3)
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="CVD cause")
  result<-rbind(result,result3)
  result
  #CVD model3
  model4<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result_CVD<-rbind(result,result4)
  result_CVD
  
}

{ #* Cancer model #####
  
  #Cancer model1
  model1<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis, design =rhcSvy)
  model1_result<-summary(model1)
  P<-model1_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model1_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                       'P value' =P,'model'="model1",'status'="Cancer cause")
  result
  #Cancer model2
  model2<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status, design =rhcSvy)
  model2_result<-summary(model2)
  P<-model2_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model2_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model2",'status'="Cancer cause")
  result<-rbind(result,result2)
  result
  #Cancer model3
  model3<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status,design =rhcSvy)
  model3_result<-summary(model3)
  P<-model3_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model3_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model3",'status'="Cancer cause")
  result<-rbind(result,result3)
  result
  #Cancer model4
  model4<-svycoxph(Surv(peryear, Cancer_MORT_stat==1) ~
                     PD_diagnosis+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                     Smoking_status+Drinking_status+BMI_status+Physical_status+
                     HTN_status+T2D_status+HPL_status+Cohort
                   , design =rhcSvy)
  model4_result<-summary(model4)
  
  P<-model4_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-as.numeric(model4_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="Cancer cause")
  result_Cancer<-rbind(result,result4)
  result_Cancer
}



{ #* Combine #####
  #Results
  result.cause<-rbind(result_All,result_CVD,result_Cancer)
  result.cause$HR<-round(result.cause$HR,2)
  result.cause$lower..95<-round(result.cause$lower..95,2)
  result.cause$upper..95<-round(result.cause$upper..95,2)
  result.cause$P.value<-round(result.cause$P.value,3)
  round_3_function <- function(x){
    while(nchar(x)<5){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  
  round_2_function <- function(x){
    while(nchar(x)<4){
      temp <- paste(x,0)
      x <- temp
      x <- gsub(" ","",x)
    }
    return(x)
  }
  
  result.cause$HR<-lapply(result.cause$HR,round_2_function)
  result.cause$lower..95<-lapply(result.cause$lower..95,round_2_function)
  result.cause$upper..95<-lapply(result.cause$upper..95,round_2_function)
  result.cause$P.value<-lapply(result.cause$P.value,round_3_function)
  
  result.cause[result.cause=="10000"]<-"1.000"
  result.cause[result.cause=="00000"]<-"<0.001"
  result.cause
  #Events
  #all.cause
  PD.counts<-table(Interpolation_weighted$PD_diagnosis,useNA = "ifany")
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$MORT_stat,useNA = "ifany")
  all.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #CVD.cause
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$CVD_MORT_stat,useNA = "ifany")
  CVD.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  #Cancer.cause
  PD.M.counts<-table(Interpolation_weighted$PD_diagnosis,Interpolation_weighted$Cancer_MORT_stat,useNA = "ifany")
  Cancer.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
  total.counts<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause))
  total.counts
  
  #PD_MDD
  PD_MDD<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="Moderate/Severe periodontitis"),]
  PD_MDD$year<-PD_MDD$peryear
  PD_MDD$Pyear<-PD_MDD$year*PD_MDD$weight
  PD_MDD_death_All<-PD_MDD[which(PD_MDD$MORT_stat==1),]
  PD_MDD_death_CVD<-PD_MDD[which(PD_MDD$CVD_MORT_stat==1),]
  PD_MDD_death_Cancer<-PD_MDD[which(PD_MDD$Cancer_MORT_stat==1),]
  
  #noPD_MDD
  noPD_MDD<-Interpolation_weighted[which(Interpolation_weighted$PD_diagnosis=="No/Mild periodontitis"),]
  noPD_MDD$year<-noPD_MDD$peryear
  noPD_MDD$Pyear<-noPD_MDD$year*noPD_MDD$weight
  noPD_MDD_death_All<-noPD_MDD[which(noPD_MDD$MORT_stat==1),]
  noPD_MDD_death_CVD<-noPD_MDD[which(noPD_MDD$CVD_MORT_stat==1),]
  noPD_MDD_death_Cancer<-noPD_MDD[which(noPD_MDD$Cancer_MORT_stat==1),]
  
  #all.cause
  PD.counts<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  PD.cause.counts<-format(round(sum(PD_MDD_death_All$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.cause.counts<-format(round(sum(noPD_MDD_death_All$weight)), big.mark = ",", scientific = FALSE)
  
  all.cause<-c(paste0(PD.cause.counts,"/",PD.counts),paste0(noPD.cause.counts,"/",noPD.counts))
  #CVD.cause
  PD.counts<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  PD.cause.counts<-format(round(sum(PD_MDD_death_CVD$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.cause.counts<-format(round(sum(noPD_MDD_death_CVD$weight)), big.mark = ",", scientific = FALSE)
  CVD.cause<-c(paste0(PD.cause.counts,"/",PD.counts),paste0(noPD.cause.counts,"/",noPD.counts))
  #Cancer.cause
  PD.counts<-format(round(sum(PD_MDD$weight)), big.mark = ",", scientific = FALSE)
  PD.cause.counts<-format(round(sum(PD_MDD_death_Cancer$weight)), big.mark = ",", scientific = FALSE)
  noPD.counts<-format(round(sum(noPD_MDD$weight)), big.mark = ",", scientific = FALSE)
  noPD.cause.counts<-format(round(sum(noPD_MDD_death_Cancer$weight)), big.mark = ",", scientific = FALSE)
  Cancer.cause<-c(paste0(PD.cause.counts,"/",PD.counts),paste0(noPD.cause.counts,"/",noPD.counts))
  total.counts_ad<-as.data.frame(rbind(all.cause,CVD.cause,Cancer.cause))
  total.counts_ad
  
  #Cbind
  Table<-c("","Periodontal status","","")
  Table<-rbind(Table,c("", "No/Mild periodontitis","Moderate/Severe periodontitis","P"))
  Table<-rbind(Table,c("All cause*","","",""))
  Table<-rbind(Table,c("Deaths/total (Unweighted)",total.counts[1,1],total.counts[1,2],""))
  Table<-rbind(Table,c("Deaths/total (Weighted)",total.counts_ad[1,1],total.counts_ad[1,2],""))
  Table<-rbind(Table,c("Model 1†","1.00 [Reference]",paste0(result.cause[1,1]," (",result.cause[1,2],", ",result.cause[1,3],")"),result.cause[1,4]))
  Table<-rbind(Table,c("Model 2‡","1.00 [Reference]",paste0(result.cause[2,1]," (",result.cause[2,2],", ",result.cause[2,3],")"),result.cause[2,4]))
  Table<-rbind(Table,c("Model 3§","1.00 [Reference]",paste0(result.cause[3,1]," (",result.cause[3,2],", ",result.cause[3,3],")"),result.cause[3,4]))
  Table<-rbind(Table,c("Model 4¶","1.00 [Reference]",paste0(result.cause[4,1]," (",result.cause[4,2],", ",result.cause[4,3],")"),result.cause[4,4]))
  Table<-rbind(Table,c("CVD cause","","",""))
  Table<-rbind(Table,c("Deaths/total (Unweighted)",total.counts[2,1],total.counts[2,2],""))
  Table<-rbind(Table,c("Deaths/total (Weighted)",total.counts_ad[2,1],total.counts_ad[2,2],""))
  Table<-rbind(Table,c("Model 1","1.00 [Reference]",paste0(result.cause[5,1]," (",result.cause[5,2],", ",result.cause[5,3],")"),result.cause[5,4]))
  Table<-rbind(Table,c("Model 2","1.00 [Reference]",paste0(result.cause[6,1]," (",result.cause[6,2],", ",result.cause[6,3],")"),result.cause[6,4]))
  Table<-rbind(Table,c("Model 3","1.00 [Reference]",paste0(result.cause[7,1]," (",result.cause[7,2],", ",result.cause[7,3],")"),result.cause[7,4]))
  Table<-rbind(Table,c("Model 4","1.00 [Reference]",paste0(result.cause[8,1]," (",result.cause[8,2],", ",result.cause[8,3],")"),result.cause[8,4]))
  Table<-rbind(Table,c("Cancer cause","","",""))
  Table<-rbind(Table,c("Deaths/total (Unweighted)",total.counts[3,1],total.counts[3,2],""))
  Table<-rbind(Table,c("Deaths/total (Weighted)",total.counts_ad[3,1],total.counts_ad[3,2],""))
  Table<-rbind(Table,c("Model 1","1.00 [Reference]",paste0(result.cause[9,1]," (",result.cause[9,2],", ",result.cause[9,3],")"),result.cause[9,4]))
  Table<-rbind(Table,c("Model 2","1.00 [Reference]",paste0(result.cause[10,1]," (",result.cause[10,2],", ",result.cause[10,3],")"),result.cause[10,4]))
  Table<-rbind(Table,c("Model 3","1.00 [Reference]",paste0(result.cause[11,1]," (",result.cause[11,2],", ",result.cause[11,3],")"),result.cause[11,4]))
  Table<-rbind(Table,c("Model 4","1.00 [Reference]",paste0(result.cause[12,1]," (",result.cause[12,2],", ",result.cause[12,3],")"),result.cause[12,4]))
  Table<-as.data.frame(Table)
  Table_S15 = data.frame(lapply(Table, as.character), stringsAsFactors=FALSE)
  Table_S15
  write.table(Table_S15,sep = ",",file ="I:/NHANES study/PD&TYG&MO/Result/Supplementary Table 15.csv",row.names =F,col.names =F )
}
