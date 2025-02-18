# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# ++++++++++++++++++++Orginal data++++++++++++++++++++++ ####
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section x.Table S14  ####
load(file="I:/NHANES study/PD&TYG&MO/Data/Original_weighted.Rdata")
Interpolation_weighted<-Original_weighted
table(Interpolation_weighted$TYG_quantile)
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
Table_orginal<-cbind(Over,Quantile1[,c("counts","Mean","SE")],
              Quantile2[,c("counts","Mean","SE")],
              Quantile3[,c("counts","Mean","SE")],
              Quantile4[,c("counts","Mean","SE")])
table(Interpolation_weighted$TYG_quantile)
save(Table_orginal,file = "I:/NHANES study/PD&TYG&MO/Data/Table_orginal_Rdata")  
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
load(file = "I:/NHANES study/PD&TYG&MO/Data/Table_orginal_Rdata")
Table_orginal<-merge(Table_orginal,test_data,by="Covariates",all.x = T)
Table_orginal$Covariates
Table_orginal$Row<-paste0(Table_orginal$Covariates," ",Table_orginal$grade)
rownames(Table_orginal)<-Table_orginal$Row
colnames(Table_orginal)<-c("Covariates","grade",
                    "counts_all","Mean_all","SE_all",
                    "counts_TYG_quantile1","Mean_TYG_quantile1","SE_TYG_quantile1",
                    "counts_TYG_quantile2","Mean_TYG_quantile2","SE_TYG_quantile2",
                    "counts_TYG_quantile3","Mean_TYG_quantile3","SE_TYG_quantile3",
                    "counts_TYG_quantile4","Mean_TYG_quantile4","SE_TYG_quantile4",
                    "P.value","Row")
rownames(Table_orginal)
{ #* section 18.5 Combine  Table #####
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
                       Table_orginal["Age Mean ± SE","Mean_all"],Table_orginal["Age Mean ± SE","SE_all"],
                       Table_orginal["Age Mean ± SE","Mean_TYG_quantile1"],Table_orginal["Age Mean ± SE","SE_TYG_quantile1"],
                       Table_orginal["Age Mean ± SE","Mean_TYG_quantile2"],Table_orginal["Age Mean ± SE","SE_TYG_quantile2"],
                       Table_orginal["Age Mean ± SE","Mean_TYG_quantile3"],Table_orginal["Age Mean ± SE","SE_TYG_quantile3"],
                       Table_orginal["Age Mean ± SE","Mean_TYG_quantile4"],Table_orginal["Age Mean ± SE","SE_TYG_quantile4"],
                       Table_orginal["Age Mean ± SE","P.value"] ))
  Table<-rbind(Table,c("Age status, %","","","","","","","","","","",Table_orginal["Age_status <45","P.value"]))
  Table<-rbind(Table,c("<45",
                       Table_orginal["Age_status <45","Mean_all"],Table_orginal["Age_status <45","SE_all"],
                       Table_orginal["Age_status <45","Mean_TYG_quantile1"],Table_orginal["Age_status <45","SE_TYG_quantile1"],
                       Table_orginal["Age_status <45","Mean_TYG_quantile2"],Table_orginal["Age_status <45","SE_TYG_quantile2"],
                       Table_orginal["Age_status <45","Mean_TYG_quantile3"],Table_orginal["Age_status <45","SE_TYG_quantile3"],
                       Table_orginal["Age_status <45","Mean_TYG_quantile4"],Table_orginal["Age_status <45","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("[45, 65)",
                       Table_orginal["Age_status [45,65)","Mean_all"],Table_orginal["Age_status <45","SE_all"],
                       Table_orginal["Age_status [45,65)","Mean_TYG_quantile1"],Table_orginal["Age_status [45,65)","SE_TYG_quantile1"],
                       Table_orginal["Age_status [45,65)","Mean_TYG_quantile2"],Table_orginal["Age_status [45,65)","SE_TYG_quantile2"],
                       Table_orginal["Age_status [45,65)","Mean_TYG_quantile3"],Table_orginal["Age_status [45,65)","SE_TYG_quantile3"],
                       Table_orginal["Age_status [45,65)","Mean_TYG_quantile4"],Table_orginal["Age_status [45,65)","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("≥65",
                       Table_orginal["Age_status >=65","Mean_all"],Table_orginal["Age_status >=65","SE_all"],
                       Table_orginal["Age_status >=65","Mean_TYG_quantile1"],Table_orginal["Age_status >=65","SE_TYG_quantile1"],
                       Table_orginal["Age_status >=65","Mean_TYG_quantile2"],Table_orginal["Age_status >=65","SE_TYG_quantile2"],
                       Table_orginal["Age_status >=65","Mean_TYG_quantile3"],Table_orginal["Age_status >=65","SE_TYG_quantile3"],
                       Table_orginal["Age_status >=65","Mean_TYG_quantile4"],Table_orginal["Age_status >=65","SE_TYG_quantile4"],
                       "" ))
  
  #Sex
  Table<-rbind(Table,c("Sex, Female",
                       Table_orginal["Sex Female","Mean_all"],Table_orginal["Sex Female","SE_all"],
                       Table_orginal["Sex Female","Mean_TYG_quantile1"],Table_orginal["Sex Female","SE_TYG_quantile1"],
                       Table_orginal["Sex Female","Mean_TYG_quantile2"],Table_orginal["Sex Female","SE_TYG_quantile2"],
                       Table_orginal["Sex Female","Mean_TYG_quantile3"],Table_orginal["Sex Female","SE_TYG_quantile3"],
                       Table_orginal["Sex Female","Mean_TYG_quantile4"],Table_orginal["Sex Female","SE_TYG_quantile4"],
                       Table_orginal["Sex Female","P.value"]))
  
  #Race/ ethnicity
  Table<-rbind(Table,c("Race/ ethnicity, %","","","","","","","","","","",Table_orginal["Race_ethnicity Non-Hispanic White","P.value"]))
  Table<-rbind(Table,c("Non-Hispanic white",
                       Table_orginal["Race_ethnicity Non-Hispanic White","Mean_all"],Table_orginal["Race_ethnicity Non-Hispanic White","SE_all"],
                       Table_orginal["Race_ethnicity Non-Hispanic White","Mean_TYG_quantile1"],Table_orginal["Race_ethnicity Non-Hispanic White","SE_TYG_quantile1"],
                       Table_orginal["Race_ethnicity Non-Hispanic White","Mean_TYG_quantile2"],Table_orginal["Race_ethnicity Non-Hispanic White","SE_TYG_quantile2"],
                       Table_orginal["Race_ethnicity Non-Hispanic White","Mean_TYG_quantile3"],Table_orginal["Race_ethnicity Non-Hispanic White","SE_TYG_quantile3"],
                       Table_orginal["Race_ethnicity Non-Hispanic White","Mean_TYG_quantile4"],Table_orginal["Race_ethnicity Non-Hispanic White","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Non-Hispanic black",
                       Table_orginal["Race_ethnicity Non-Hispanic Black","Mean_all"],Table_orginal["Race_ethnicity Non-Hispanic Black","SE_all"],
                       Table_orginal["Race_ethnicity Non-Hispanic Black","Mean_TYG_quantile1"],Table_orginal["Race_ethnicity Non-Hispanic Black","SE_TYG_quantile1"],
                       Table_orginal["Race_ethnicity Non-Hispanic Black","Mean_TYG_quantile2"],Table_orginal["Race_ethnicity Non-Hispanic Black","SE_TYG_quantile2"],
                       Table_orginal["Race_ethnicity Non-Hispanic Black","Mean_TYG_quantile3"],Table_orginal["Race_ethnicity Non-Hispanic Black","SE_TYG_quantile3"],
                       Table_orginal["Race_ethnicity Non-Hispanic Black","Mean_TYG_quantile4"],Table_orginal["Race_ethnicity Non-Hispanic Black","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Hispanic",
                       Table_orginal["Race_ethnicity Hispanic","Mean_all"],Table_orginal["Race_ethnicity Hispanic","SE_all"],
                       Table_orginal["Race_ethnicity Hispanic","Mean_TYG_quantile1"],Table_orginal["Race_ethnicity Hispanic","SE_TYG_quantile1"],
                       Table_orginal["Race_ethnicity Hispanic","Mean_TYG_quantile2"],Table_orginal["Race_ethnicity Hispanic","SE_TYG_quantile2"],
                       Table_orginal["Race_ethnicity Hispanic","Mean_TYG_quantile3"],Table_orginal["Race_ethnicity Hispanic","SE_TYG_quantile3"],
                       Table_orginal["Race_ethnicity Hispanic","Mean_TYG_quantile4"],Table_orginal["Race_ethnicity Hispanic","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Other race/ ethnicity",
                       Table_orginal["Race_ethnicity Other_Race","Mean_all"],Table_orginal["Race_ethnicity Other_Race","SE_all"],
                       Table_orginal["Race_ethnicity Other_Race","Mean_TYG_quantile1"],Table_orginal["Race_ethnicity Other_Race","SE_TYG_quantile1"],
                       Table_orginal["Race_ethnicity Other_Race","Mean_TYG_quantile2"],Table_orginal["Race_ethnicity Other_Race","SE_TYG_quantile2"],
                       Table_orginal["Race_ethnicity Other_Race","Mean_TYG_quantile3"],Table_orginal["Race_ethnicity Other_Race","SE_TYG_quantile3"],
                       Table_orginal["Race_ethnicity Other_Race","Mean_TYG_quantile4"],Table_orginal["Race_ethnicity Other_Race","SE_TYG_quantile4"],
                       "" ))
  #Marital status
  Table<-rbind(Table,c("Marital status, %","","","","","","","","","","",Table_orginal["Marital_status Married","P.value"]))
  Table<-rbind(Table,c("Married",
                       Table_orginal["Marital_status Married","Mean_all"],Table_orginal["Marital_status Married","SE_all"],
                       Table_orginal["Marital_status Married","Mean_TYG_quantile1"],Table_orginal["Marital_status Married","SE_TYG_quantile1"],
                       Table_orginal["Marital_status Married","Mean_TYG_quantile2"],Table_orginal["Marital_status Married","SE_TYG_quantile2"],
                       Table_orginal["Marital_status Married","Mean_TYG_quantile3"],Table_orginal["Marital_status Married","SE_TYG_quantile3"],
                       Table_orginal["Marital_status Married","Mean_TYG_quantile4"],Table_orginal["Marital_status Married","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Never married",
                       Table_orginal["Marital_status Never_married","Mean_all"],Table_orginal["Marital_status Never_married","SE_all"],
                       Table_orginal["Marital_status Never_married","Mean_TYG_quantile1"],Table_orginal["Marital_status Never_married","SE_TYG_quantile1"],
                       Table_orginal["Marital_status Never_married","Mean_TYG_quantile2"],Table_orginal["Marital_status Never_married","SE_TYG_quantile2"],
                       Table_orginal["Marital_status Never_married","Mean_TYG_quantile3"],Table_orginal["Marital_status Never_married","SE_TYG_quantile3"],
                       Table_orginal["Marital_status Never_married","Mean_TYG_quantile4"],Table_orginal["Marital_status Never_married","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Widowed/ Divorced/ Separated",
                       Table_orginal["Marital_status Separated","Mean_all"],Table_orginal["Marital_status Separated","SE_all"],
                       Table_orginal["Marital_status Separated","Mean_TYG_quantile1"],Table_orginal["Marital_status Separated","SE_TYG_quantile1"],
                       Table_orginal["Marital_status Separated","Mean_TYG_quantile2"],Table_orginal["Marital_status Separated","SE_TYG_quantile2"],
                       Table_orginal["Marital_status Separated","Mean_TYG_quantile3"],Table_orginal["Marital_status Separated","SE_TYG_quantile3"],
                       Table_orginal["Marital_status Separated","Mean_TYG_quantile4"],Table_orginal["Marital_status Separated","SE_TYG_quantile4"],
                       "" ))
  
  #Socioeconomic Status
  Table<-rbind(Table,c("Socioeconomic Status, %","","","","","","","","","","",Table_orginal["SES low","P.value"]))
  Table<-rbind(Table,c("Low",
                       Table_orginal["SES low","Mean_all"],Table_orginal["SES low","SE_all"],
                       Table_orginal["SES low","Mean_TYG_quantile1"],Table_orginal["SES low","SE_TYG_quantile1"],
                       Table_orginal["SES low","Mean_TYG_quantile2"],Table_orginal["SES low","SE_TYG_quantile2"],
                       Table_orginal["SES low","Mean_TYG_quantile3"],Table_orginal["SES low","SE_TYG_quantile3"],
                       Table_orginal["SES low","Mean_TYG_quantile4"],Table_orginal["SES low","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Low_medium",
                       Table_orginal["SES low_medium","Mean_all"],Table_orginal["SES low_medium","SE_all"],
                       Table_orginal["SES low_medium","Mean_TYG_quantile1"],Table_orginal["SES low_medium","SE_TYG_quantile1"],
                       Table_orginal["SES low_medium","Mean_TYG_quantile2"],Table_orginal["SES low_medium","SE_TYG_quantile2"],
                       Table_orginal["SES low_medium","Mean_TYG_quantile3"],Table_orginal["SES low_medium","SE_TYG_quantile3"],
                       Table_orginal["SES low_medium","Mean_TYG_quantile4"],Table_orginal["SES low_medium","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Medium_high",
                       Table_orginal["SES medium_high","Mean_all"],Table_orginal["SES medium_high","SE_all"],
                       Table_orginal["SES medium_high","Mean_TYG_quantile1"],Table_orginal["SES medium_high","SE_TYG_quantile1"],
                       Table_orginal["SES medium_high","Mean_TYG_quantile2"],Table_orginal["SES medium_high","SE_TYG_quantile2"],
                       Table_orginal["SES medium_high","Mean_TYG_quantile3"],Table_orginal["SES medium_high","SE_TYG_quantile3"],
                       Table_orginal["SES medium_high","Mean_TYG_quantile4"],Table_orginal["SES medium_high","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("High",
                       Table_orginal["SES high","Mean_all"],Table_orginal["SES high","SE_all"],
                       Table_orginal["SES high","Mean_TYG_quantile1"],Table_orginal["SES high","SE_TYG_quantile1"],
                       Table_orginal["SES high","Mean_TYG_quantile2"],Table_orginal["SES high","SE_TYG_quantile2"],
                       Table_orginal["SES high","Mean_TYG_quantile3"],Table_orginal["SES high","SE_TYG_quantile3"],
                       Table_orginal["SES high","Mean_TYG_quantile4"],Table_orginal["SES high","SE_TYG_quantile4"],
                       "" ))
  
  #Smoking status
  Table<-rbind(Table,c("Smoking status, %","","","","","","","","","","",Table_orginal["Smoking_status Never_smoker","P.value"]))
  Table<-rbind(Table,c("Never smoker",
                       Table_orginal["Smoking_status Never_smoker","Mean_all"],Table_orginal["Smoking_status Never_smoker","SE_all"],
                       Table_orginal["Smoking_status Never_smoker","Mean_TYG_quantile1"],Table_orginal["Smoking_status Never_smoker","SE_TYG_quantile1"],
                       Table_orginal["Smoking_status Never_smoker","Mean_TYG_quantile2"],Table_orginal["Smoking_status Never_smoker","SE_TYG_quantile2"],
                       Table_orginal["Smoking_status Never_smoker","Mean_TYG_quantile3"],Table_orginal["Smoking_status Never_smoker","SE_TYG_quantile3"],
                       Table_orginal["Smoking_status Never_smoker","Mean_TYG_quantile4"],Table_orginal["Smoking_status Never_smoker","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Former smoker",
                       Table_orginal["Smoking_status Former_smoker","Mean_all"],Table_orginal["Smoking_status Former_smoker","SE_all"],
                       Table_orginal["Smoking_status Former_smoker","Mean_TYG_quantile1"],Table_orginal["Smoking_status Former_smoker","SE_TYG_quantile1"],
                       Table_orginal["Smoking_status Former_smoker","Mean_TYG_quantile2"],Table_orginal["Smoking_status Former_smoker","SE_TYG_quantile2"],
                       Table_orginal["Smoking_status Former_smoker","Mean_TYG_quantile3"],Table_orginal["Smoking_status Former_smoker","SE_TYG_quantile3"],
                       Table_orginal["Smoking_status Former_smoker","Mean_TYG_quantile4"],Table_orginal["Smoking_status Former_smoker","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Current smoker",
                       Table_orginal["Smoking_status Current_smoker","Mean_all"],Table_orginal["Smoking_status Current_smoker","SE_all"],
                       Table_orginal["Smoking_status Current_smoker","Mean_TYG_quantile1"],Table_orginal["Smoking_status Current_smoker","SE_TYG_quantile1"],
                       Table_orginal["Smoking_status Current_smoker","Mean_TYG_quantile2"],Table_orginal["Smoking_status Current_smoker","SE_TYG_quantile2"],
                       Table_orginal["Smoking_status Current_smoker","Mean_TYG_quantile3"],Table_orginal["Smoking_status Current_smoker","SE_TYG_quantile3"],
                       Table_orginal["Smoking_status Current_smoker","Mean_TYG_quantile4"],Table_orginal["Smoking_status Current_smoker","SE_TYG_quantile4"],
                       "" ))
  
  #Drinking status
  Table<-rbind(Table,c("Drinking status, %","","","","","","","","","","",Table_orginal["Drinking_status Nondrinker","P.value"]))
  Table<-rbind(Table,c("Nondrinker",
                       Table_orginal["Drinking_status Nondrinker","Mean_all"],Table_orginal["Drinking_status Nondrinker","SE_all"],
                       Table_orginal["Drinking_status Nondrinker","Mean_TYG_quantile1"],Table_orginal["Drinking_status Nondrinker","SE_TYG_quantile1"],
                       Table_orginal["Drinking_status Nondrinker","Mean_TYG_quantile2"],Table_orginal["Drinking_status Nondrinker","SE_TYG_quantile2"],
                       Table_orginal["Drinking_status Nondrinker","Mean_TYG_quantile3"],Table_orginal["Drinking_status Nondrinker","SE_TYG_quantile3"],
                       Table_orginal["Drinking_status Nondrinker","Mean_TYG_quantile4"],Table_orginal["Drinking_status Nondrinker","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Light/ moderate drinker",
                       Table_orginal["Drinking_status Light/moderate_drinker","Mean_all"],Table_orginal["Drinking_status Light/moderate_drinker","SE_all"],
                       Table_orginal["Drinking_status Light/moderate_drinker","Mean_TYG_quantile1"],Table_orginal["Drinking_status Light/moderate_drinker","SE_TYG_quantile1"],
                       Table_orginal["Drinking_status Light/moderate_drinker","Mean_TYG_quantile2"],Table_orginal["Drinking_status Light/moderate_drinker","SE_TYG_quantile2"],
                       Table_orginal["Drinking_status Light/moderate_drinker","Mean_TYG_quantile3"],Table_orginal["Drinking_status Light/moderate_drinker","SE_TYG_quantile3"],
                       Table_orginal["Drinking_status Light/moderate_drinker","Mean_TYG_quantile4"],Table_orginal["Drinking_status Light/moderate_drinker","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Heavier drinker",
                       Table_orginal["Drinking_status Heavier_drinker","Mean_all"],Table_orginal["Drinking_status Heavier_drinker","SE_all"],
                       Table_orginal["Drinking_status Heavier_drinker","Mean_TYG_quantile1"],Table_orginal["Drinking_status Heavier_drinker","SE_TYG_quantile1"],
                       Table_orginal["Drinking_status Heavier_drinker","Mean_TYG_quantile2"],Table_orginal["Drinking_status Heavier_drinker","SE_TYG_quantile2"],
                       Table_orginal["Drinking_status Heavier_drinker","Mean_TYG_quantile3"],Table_orginal["Drinking_status Heavier_drinker","SE_TYG_quantile3"],
                       Table_orginal["Drinking_status Heavier_drinker","Mean_TYG_quantile4"],Table_orginal["Drinking_status Heavier_drinker","SE_TYG_quantile4"],
                       "" ))
  #Physical status
  Table<-rbind(Table,c("Physical status, %","","","","","","","","","","",Table_orginal["Physical_status Inactive","P.value"]))
  Table<-rbind(Table,c("Inactive",
                       Table_orginal["Physical_status Inactive","Mean_all"],Table_orginal["Physical_status Inactive","SE_all"],
                       Table_orginal["Physical_status Inactive","Mean_TYG_quantile1"],Table_orginal["Physical_status Inactive","SE_TYG_quantile1"],
                       Table_orginal["Physical_status Inactive","Mean_TYG_quantile2"],Table_orginal["Physical_status Inactive","SE_TYG_quantile2"],
                       Table_orginal["Physical_status Inactive","Mean_TYG_quantile3"],Table_orginal["Physical_status Inactive","SE_TYG_quantile3"],
                       Table_orginal["Physical_status Inactive","Mean_TYG_quantile4"],Table_orginal["Physical_status Inactive","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Insufficient",
                       Table_orginal["Physical_status Insufficient","Mean_all"],Table_orginal["Physical_status Insufficient","SE_all"],
                       Table_orginal["Physical_status Insufficient","Mean_TYG_quantile1"],Table_orginal["Physical_status Insufficient","SE_TYG_quantile1"],
                       Table_orginal["Physical_status Insufficient","Mean_TYG_quantile2"],Table_orginal["Physical_status Insufficient","SE_TYG_quantile2"],
                       Table_orginal["Physical_status Insufficient","Mean_TYG_quantile3"],Table_orginal["Physical_status Insufficient","SE_TYG_quantile3"],
                       Table_orginal["Physical_status Insufficient","Mean_TYG_quantile4"],Table_orginal["Physical_status Insufficient","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Recommended",
                       Table_orginal["Physical_status Recommended","Mean_all"],Table_orginal["Physical_status Recommended","SE_all"],
                       Table_orginal["Physical_status Recommended","Mean_TYG_quantile1"],Table_orginal["Physical_status Recommended","SE_TYG_quantile1"],
                       Table_orginal["Physical_status Recommended","Mean_TYG_quantile2"],Table_orginal["Physical_status Recommended","SE_TYG_quantile2"],
                       Table_orginal["Physical_status Recommended","Mean_TYG_quantile3"],Table_orginal["Physical_status Recommended","SE_TYG_quantile3"],
                       Table_orginal["Physical_status Recommended","Mean_TYG_quantile4"],Table_orginal["Physical_status Recommended","SE_TYG_quantile4"],
                       "" ))
  
  #Healthy Eating Index
  #Table<-rbind(Table,c("Healthy eating index, %","","","","","","",Table_orginal["HEI Quintile 1","P.value"]))
  
  # Table<-rbind(Table,c("Quintile 1",
  #                      Table_orginal["HEI Quintile 1","Mean_all"],Table_orginal["HEI Quintile 1","SE_all"],
  #                      Table_orginal["HEI Quintile 1","Mean_noPD"],Table_orginal["HEI Quintile 1","SE_noPD"],
  #                      Table_orginal["HEI Quintile 1","Mean_PD"],Table_orginal["HEI Quintile 1","SE_PD"],
  #                      "" ))
  # Table<-rbind(Table,c("Quintile 2",
  #                      Table_orginal["HEI Quintile 2","Mean_all"],Table_orginal["HEI Quintile 2","SE_all"],
  #                      Table_orginal["HEI Quintile 2","Mean_noPD"],Table_orginal["HEI Quintile 2","SE_noPD"],
  #                      Table_orginal["HEI Quintile 2","Mean_PD"],Table_orginal["HEI Quintile 2","SE_PD"],
  #                      "" ))
  #
  # Table<-rbind(Table,c("Quintile 3",
  #                      Table_orginal["HEI Quintile 3","Mean_all"],Table_orginal["HEI Quintile 3","SE_all"],
  #                      Table_orginal["HEI Quintile 3","Mean_noPD"],Table_orginal["HEI Quintile 3","SE_noPD"],
  #                      Table_orginal["HEI Quintile 3","Mean_PD"],Table_orginal["HEI Quintile 3","SE_PD"],
  #                      "" ))
  # Table<-rbind(Table,c("Quintile 4",
  #                      Table_orginal["HEI Quintile 4","Mean_all"],Table_orginal["HEI Quintile 4","SE_all"],
  #                      Table_orginal["HEI Quintile 4","Mean_noPD"],Table_orginal["HEI Quintile 4","SE_noPD"],
  #                      Table_orginal["HEI Quintile 4","Mean_PD"],Table_orginal["HEI Quintile 4","SE_PD"],
  #                      "" ))
  # Table<-rbind(Table,c("Quintile 5",
  #                      Table_orginal["HEI Quintile 5","Mean_all"],Table_orginal["HEI Quintile 5","SE_all"],
  #                      Table_orginal["HEI Quintile 5","Mean_noPD"],Table_orginal["HEI Quintile 5","SE_noPD"],
  #                      Table_orginal["HEI Quintile 5","Mean_PD"],Table_orginal["HEI Quintile 5","SE_PD"],
  #                      "" ))
  
  #BMI
  Table<-rbind(Table,c("BMI, Mean",
                       Table_orginal["BMI Mean ± SE","Mean_all"],Table_orginal["BMI Mean ± SE","SE_all"],
                       Table_orginal["BMI Mean ± SE","Mean_TYG_quantile1"],Table_orginal["BMI Mean ± SE","SE_TYG_quantile1"],
                       Table_orginal["BMI Mean ± SE","Mean_TYG_quantile2"],Table_orginal["BMI Mean ± SE","SE_TYG_quantile2"],
                       Table_orginal["BMI Mean ± SE","Mean_TYG_quantile3"],Table_orginal["BMI Mean ± SE","SE_TYG_quantile3"],
                       Table_orginal["BMI Mean ± SE","Mean_TYG_quantile4"],Table_orginal["BMI Mean ± SE","SE_TYG_quantile4"],
                       Table_orginal["BMI Mean ± SE","P.value"] ))
  
  Table<-rbind(Table,c("BMI status (kg/m2), %","","","","","","","","","","",Table_orginal["BMI_status (0,25)","P.value"]))
  Table<-rbind(Table,c("<25",
                       Table_orginal["BMI_status (0,25)","Mean_all"],Table_orginal["BMI_status (0,25)","SE_all"],
                       Table_orginal["BMI_status (0,25)","Mean_TYG_quantile1"],Table_orginal["BMI_status (0,25)","SE_TYG_quantile1"],
                       Table_orginal["BMI_status (0,25)","Mean_TYG_quantile2"],Table_orginal["BMI_status (0,25)","SE_TYG_quantile2"],
                       Table_orginal["BMI_status (0,25)","Mean_TYG_quantile3"],Table_orginal["BMI_status (0,25)","SE_TYG_quantile3"],
                       Table_orginal["BMI_status (0,25)","Mean_TYG_quantile4"],Table_orginal["BMI_status (0,25)","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("[25.0 -30)",
                       Table_orginal["BMI_status [25.0-30)","Mean_all"],Table_orginal["BMI_status [25.0-30)","SE_all"],
                       Table_orginal["BMI_status [25.0-30)","Mean_TYG_quantile1"],Table_orginal["BMI_status [25.0-30)","SE_TYG_quantile1"],
                       Table_orginal["BMI_status [25.0-30)","Mean_TYG_quantile2"],Table_orginal["BMI_status [25.0-30)","SE_TYG_quantile2"],
                       Table_orginal["BMI_status [25.0-30)","Mean_TYG_quantile3"],Table_orginal["BMI_status [25.0-30)","SE_TYG_quantile3"],
                       Table_orginal["BMI_status [25.0-30)","Mean_TYG_quantile4"],Table_orginal["BMI_status [25.0-30)","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("≥30",
                       Table_orginal["BMI_status [30,inf)","Mean_all"],Table_orginal["BMI_status [30,inf)","SE_all"],
                       Table_orginal["BMI_status [30,inf)","Mean_TYG_quantile1"],Table_orginal["BMI_status [30,inf)","SE_TYG_quantile1"],
                       Table_orginal["BMI_status [30,inf)","Mean_TYG_quantile2"],Table_orginal["BMI_status [30,inf)","SE_TYG_quantile2"],
                       Table_orginal["BMI_status [30,inf)","Mean_TYG_quantile3"],Table_orginal["BMI_status [30,inf)","SE_TYG_quantile3"],
                       Table_orginal["BMI_status [30,inf)","Mean_TYG_quantile4"],Table_orginal["BMI_status [30,inf)","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Comorbidities, %","","","","","","","","","","",""))
  #Hypertension
  Table<-rbind(Table,c("Hypertension",
                       Table_orginal["HTN_status YES","Mean_all"],Table_orginal["HTN_status YES","SE_all"],
                       Table_orginal["HTN_status YES","Mean_TYG_quantile1"],Table_orginal["HTN_status YES","SE_TYG_quantile1"],
                       Table_orginal["HTN_status YES","Mean_TYG_quantile2"],Table_orginal["HTN_status YES","SE_TYG_quantile2"],
                       Table_orginal["HTN_status YES","Mean_TYG_quantile3"],Table_orginal["HTN_status YES","SE_TYG_quantile3"],
                       Table_orginal["HTN_status YES","Mean_TYG_quantile4"],Table_orginal["HTN_status YES","SE_TYG_quantile4"],
                       Table_orginal["HTN_status YES","P.value"]))
  #Hyperlipidemia
  Table<-rbind(Table,c("Hyperlipidemia",
                       Table_orginal["HPL_status YES","Mean_all"],Table_orginal["HPL_status YES","SE_all"],
                       Table_orginal["HPL_status YES","Mean_TYG_quantile1"],Table_orginal["HPL_status YES","SE_TYG_quantile1"],
                       Table_orginal["HPL_status YES","Mean_TYG_quantile2"],Table_orginal["HPL_status YES","SE_TYG_quantile2"],
                       Table_orginal["HPL_status YES","Mean_TYG_quantile3"],Table_orginal["HPL_status YES","SE_TYG_quantile3"],
                       Table_orginal["HPL_status YES","Mean_TYG_quantile4"],Table_orginal["HPL_status YES","SE_TYG_quantile4"],
                       Table_orginal["HPL_status YES","P.value"]))
  #Diabetes mellitus
  Table<-rbind(Table,c("Diabetes mellitus",
                       Table_orginal["T2D_status YES","Mean_all"],Table_orginal["T2D_status YES","SE_all"],
                       Table_orginal["T2D_status YES","Mean_TYG_quantile1"],Table_orginal["T2D_status YES","SE_TYG_quantile1"],
                       Table_orginal["T2D_status YES","Mean_TYG_quantile2"],Table_orginal["T2D_status YES","SE_TYG_quantile2"],
                       Table_orginal["T2D_status YES","Mean_TYG_quantile3"],Table_orginal["T2D_status YES","SE_TYG_quantile3"],
                       Table_orginal["T2D_status YES","Mean_TYG_quantile4"],Table_orginal["T2D_status YES","SE_TYG_quantile4"],
                       Table_orginal["T2D_status YES","P.value"]))
  #Cohort
  Table<-rbind(Table,c("Cohort period, %","","","","","","","","","","",Table_orginal["Cohort NHANES_CON1","P.value"]))
  
  Table<-rbind(Table,c("NHANES III",
                       Table_orginal["Cohort NHANES_III","Mean_all"],Table_orginal["Cohort NHANES_III","SE_all"],
                       Table_orginal["Cohort NHANES_III","Mean_TYG_quantile1"],Table_orginal["Cohort NHANES_III","SE_TYG_quantile1"],
                       Table_orginal["Cohort NHANES_III","Mean_TYG_quantile2"],Table_orginal["Cohort NHANES_III","SE_TYG_quantile2"],
                       Table_orginal["Cohort NHANES_III","Mean_TYG_quantile3"],Table_orginal["Cohort NHANES_III","SE_TYG_quantile3"],
                       Table_orginal["Cohort NHANES_III","Mean_TYG_quantile4"],Table_orginal["Cohort NHANES_III","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("NHANES 1999-2004",
                       Table_orginal["Cohort NHANES_CON1","Mean_all"],Table_orginal["Cohort NHANES_CON1","SE_all"],
                       Table_orginal["Cohort NHANES_CON1","Mean_TYG_quantile1"],Table_orginal["Cohort NHANES_CON1","SE_TYG_quantile1"],
                       Table_orginal["Cohort NHANES_CON1","Mean_TYG_quantile2"],Table_orginal["Cohort NHANES_CON1","SE_TYG_quantile2"],
                       Table_orginal["Cohort NHANES_CON1","Mean_TYG_quantile3"],Table_orginal["Cohort NHANES_CON1","SE_TYG_quantile3"],
                       Table_orginal["Cohort NHANES_CON1","Mean_TYG_quantile4"],Table_orginal["Cohort NHANES_CON1","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("NHANES 2009-2014",
                       Table_orginal["Cohort NHANES_CON2","Mean_all"],Table_orginal["Cohort NHANES_CON2","SE_all"],
                       Table_orginal["Cohort NHANES_CON2","Mean_TYG_quantile1"],Table_orginal["Cohort NHANES_CON2","SE_TYG_quantile1"],
                       Table_orginal["Cohort NHANES_CON2","Mean_TYG_quantile2"],Table_orginal["Cohort NHANES_CON2","SE_TYG_quantile2"],
                       Table_orginal["Cohort NHANES_CON2","Mean_TYG_quantile3"],Table_orginal["Cohort NHANES_CON2","SE_TYG_quantile3"],
                       Table_orginal["Cohort NHANES_CON2","Mean_TYG_quantile4"],Table_orginal["Cohort NHANES_CON2","SE_TYG_quantile4"],
                       "" ))
  
  write.table(Table,sep = ",",file ="I:/NHANES study/PD&TYG&MO/Result/Supplementary Table 14.csv" ,row.names =F,col.names =F )
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 19 Load data ####  
load(file="I:/NHANES study/PD&TYG&MO/Data/Original_weighted.Rdata")
Interpolation_weighted<-Original_weighted
table(Interpolation_weighted$TYG_quantile)
Interpolation_weighted$TyG_WC<-Interpolation_weighted$TyG_WC/100
Interpolation_weighted$TyG_BMI<-Interpolation_weighted$TyG_BMI/100
Interpolation_weighted$TYG_quantile<-as.character(Interpolation_weighted$TYG_quantile)
Interpolation_weighted$TYG_qua[Interpolation_weighted$TYG_quantile=="Quantile 1"]<-0
Interpolation_weighted$TYG_qua[Interpolation_weighted$TYG_quantile=="Quantile 2"]<-1
Interpolation_weighted$TYG_qua[Interpolation_weighted$TYG_quantile=="Quantile 3"]<-2
Interpolation_weighted$TYG_qua[Interpolation_weighted$TYG_quantile=="Quantile 4"]<-3
table(Interpolation_weighted$TyG_WC_quantile)
Interpolation_weighted$TyG_WC_quantile<-as.character(Interpolation_weighted$TyG_WC_quantile)
Interpolation_weighted$TyG_WC_qua[Interpolation_weighted$TyG_WC_quantile=="Quantile 1"]<-0
Interpolation_weighted$TyG_WC_qua[Interpolation_weighted$TyG_WC_quantile=="Quantile 2"]<-1
Interpolation_weighted$TyG_WC_qua[Interpolation_weighted$TyG_WC_quantile=="Quantile 3"]<-2
Interpolation_weighted$TyG_WC_qua[Interpolation_weighted$TyG_WC_quantile=="Quantile 4"]<-3
table(Interpolation_weighted$TyG_WHtR_quantile)
Interpolation_weighted$TyG_WHtR_quantile<-as.character(Interpolation_weighted$TyG_WHtR_quantile)
Interpolation_weighted$TyG_WHtR_qua[Interpolation_weighted$TyG_WHtR_quantile=="Quantile 1"]<-0
Interpolation_weighted$TyG_WHtR_qua[Interpolation_weighted$TyG_WHtR_quantile=="Quantile 2"]<-1
Interpolation_weighted$TyG_WHtR_qua[Interpolation_weighted$TyG_WHtR_quantile=="Quantile 3"]<-2
Interpolation_weighted$TyG_WHtR_qua[Interpolation_weighted$TyG_WHtR_quantile=="Quantile 4"]<-3
table(Interpolation_weighted$TyG_WHtR_qua)
Interpolation_weighted$TyG_BMI_quantile<-as.character(Interpolation_weighted$TyG_BMI_quantile)
Interpolation_weighted$TyG_BMI_qua[Interpolation_weighted$TyG_BMI_quantile=="Quantile 1"]<-0
Interpolation_weighted$TyG_BMI_qua[Interpolation_weighted$TyG_BMI_quantile=="Quantile 2"]<-1
Interpolation_weighted$TyG_BMI_qua[Interpolation_weighted$TyG_BMI_quantile=="Quantile 3"]<-2
Interpolation_weighted$TyG_BMI_qua[Interpolation_weighted$TyG_BMI_quantile=="Quantile 4"]<-3
table(Interpolation_weighted$TyG_BMI_qua)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 19 TYG  ####  
{ #* all model #####
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
  result_All<-result4
}
{ #* CVD model #####
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
  result_CVD<-result4
}
{ #* Cancer model #####
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
  result_Cancer<-result4
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
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  Table_TyG<-as.data.frame(result.cause)
  Table_TyG= data.frame(lapply(Table_TyG, as.character), stringsAsFactors=FALSE)
}
Table_TyG
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 20 TYG WC ####  
{ #* all model #####
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
  result_All<-result4
}
{ #* CVD model #####
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
  result_CVD<-result4
}

{ #* Cancer model #####
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
  result_Cancer<-result4
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
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  Table_TyG_WC<-as.data.frame(result.cause)
  Table_TyG_WC= data.frame(lapply(Table_TyG_WC, as.character), stringsAsFactors=FALSE)
}
Table_TyG_WC
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 21 TYG WHtR ####  
{ #* all model #####
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
  result_All<-result4
  result_All
}
{ #* CVD model #####
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
  result_CVD<-result4
}

{ #* Cancer model #####
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
  result_Cancer<-result4
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
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  Table_TyG_WHtR<-as.data.frame(result.cause)
  Table_TyG_WHtR = data.frame(lapply(Table_TyG_WHtR, as.character), stringsAsFactors=FALSE)
  }
Table_TyG_WHtR

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 22 TYG BMI ####  
{ #* all model #####
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
  result_All<-result4
}

{ #* CVD model #####
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
  result_CVD<-result4
}
{ #* Cancer model #####
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
  result_Cancer<-result4
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
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  Table_TyG_BMI<-as.data.frame(result.cause)
  Table_TyG_BMI= data.frame(lapply(Table_TyG_BMI, as.character), stringsAsFactors=FALSE)
  }
Table_TyG_BMI
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 23 TYG quantile ####    

{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TYG_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}
{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TYG_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
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
  result.Cancer<-result4
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
  Table_TyG_quantile<-result.cause
  Table_TyG_quantile
}

{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TYG_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}
{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TYG_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
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
  result.Cancer<-result4
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
  Table_TyG_qua<-result.cause
  Table_TyG_qua
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 24 TYG WC quantile ####    
{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_WC_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}
{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_WC_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  P<-model4_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
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
  result.Cancer<-result4
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
  Table_TyG_WC_quantile<-result.cause
  Table_TyG_WC_quantile
}
{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_WC_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}
{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_WC_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
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
  result.Cancer<-result4
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
  Table_TyG_WC_qua<-result.cause
  Table_TyG_WC_qua
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 25 TYG WHtR quantile  ####    
{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_WHtR_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}
{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_WHtR_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
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
  result.Cancer<-result4
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
  Table_TyG_WHtR_quantile<-result.cause
  Table_TyG_WHtR_quantile
}
{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_WHtR_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}
{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_WHtR_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  P<-model4_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
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
  result.Cancer<-result4
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
  Table_TyG_WHtR_qua<-result.cause
  Table_TyG_WHtR_qua
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 26 TYG BMI quantile  #### 
{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_BMI_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}
{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_BMI_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
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
  result.Cancer<-result4
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
  Table_TyG_BMI_quantile<-result.cause
}

{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_BMI_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}

{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_BMI_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
 
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
  result.Cancer<-result4
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
  Table_TyG_BMI_qua<-result.cause
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 26 Total (Table S15) #### 
Table<-c("Exposure","All cause","","CVD-related","","Cancer-related","")
Table<-rbind(Table,c("","HR (95% CI)*","P†", "HR (95% CI)","P","HR (95% CI)","P"))
Table<-rbind(Table,c("TyG index","", "", "", "", "",""))
Table<-rbind(Table,c("Per 1-index‡",Table_TyG[1,7],Table_TyG[1,4], 
                     Table_TyG[2,7],Table_TyG[2,4],
                     Table_TyG[3,7],Table_TyG[3,4]))
Table<-rbind(Table,c("Quantile 1", 
                     "1.00 [Reference]","",
                     "1.00 [Reference]","",
                     "1.00 [Reference]",""))
Table<-rbind(Table,c("Quantile 2", 
                     Table_TyG_quantile[1,7],"",
                     Table_TyG_quantile[2,7],"",
                     Table_TyG_quantile[3,7],""))
Table<-rbind(Table,c("Quantile 3", 
                     Table_TyG_quantile[4,7],"",
                     Table_TyG_quantile[5,7],"",
                     Table_TyG_quantile[6,7],""))
Table<-rbind(Table,c("Quantile 4", 
                     Table_TyG_quantile[7,7],Table_TyG_qua[1,4],
                     Table_TyG_quantile[8,7],Table_TyG_qua[2,4],
                     Table_TyG_quantile[9,7],Table_TyG_qua[3,4]))

Table<-rbind(Table,c("TyG-WC index","", "", "", "", "",""))
Table<-rbind(Table,c("Per 100-index",Table_TyG_WC[1,7],Table_TyG_WC[1,4], 
                     Table_TyG_WC[2,7],Table_TyG_WC[2,4],
                     Table_TyG_WC[3,7],Table_TyG_WC[3,4]))
Table<-rbind(Table,c("Quantile 1", 
                     "1.00 [Reference]","",
                     "1.00 [Reference]","",
                     "1.00 [Reference]",""))
Table<-rbind(Table,c("Quantile 2", 
                     Table_TyG_WC_quantile[1,7],"",
                     Table_TyG_WC_quantile[2,7],"",
                     Table_TyG_WC_quantile[3,7],""))
Table<-rbind(Table,c("Quantile 3", 
                     Table_TyG_WC_quantile[4,7],"",
                     Table_TyG_WC_quantile[5,7],"",
                     Table_TyG_WC_quantile[6,7],""))
Table<-rbind(Table,c("Quantile 4", 
                     Table_TyG_WC_quantile[7,7],Table_TyG_WC_qua[1,4],
                     Table_TyG_WC_quantile[8,7],Table_TyG_WC_qua[2,4],
                     Table_TyG_WC_quantile[9,7],Table_TyG_WC_qua[3,4]))

Table<-rbind(Table,c("TyG-WHtR index","", "", "", "", "",""))
Table<-rbind(Table,c("Per 1-index",Table_TyG_WHtR[1,7],Table_TyG_WHtR[1,4], 
                     Table_TyG_WHtR[2,7],Table_TyG_WHtR[2,4],
                     Table_TyG_WHtR[3,7],Table_TyG_WHtR[3,4]))
Table<-rbind(Table,c("Quantile 1", 
                     "1.00 [Reference]","",
                     "1.00 [Reference]","",
                     "1.00 [Reference]",""))
Table<-rbind(Table,c("Quantile 2", 
                     Table_TyG_WHtR_quantile[1,7],"",
                     Table_TyG_WHtR_quantile[2,7],"",
                     Table_TyG_WHtR_quantile[3,7],""))
Table<-rbind(Table,c("Quantile 3", 
                     Table_TyG_WHtR_quantile[4,7],"",
                     Table_TyG_WHtR_quantile[5,7],"",
                     Table_TyG_WHtR_quantile[6,7],""))
Table<-rbind(Table,c("Quantile 4", 
                     Table_TyG_WHtR_quantile[7,7],Table_TyG_WHtR_qua[1,4],
                     Table_TyG_WHtR_quantile[8,7],Table_TyG_WHtR_qua[2,4],
                     Table_TyG_WHtR_quantile[9,7],Table_TyG_WHtR_qua[3,4]))

Table<-rbind(Table,c("TyG-BMI index","", "", "", "", "",""))
Table<-rbind(Table,c("Per 100-index",Table_TyG_BMI[1,7],Table_TyG_BMI[1,4], 
                     Table_TyG_BMI[2,7],Table_TyG_BMI[2,4],
                     Table_TyG_BMI[3,7],Table_TyG_BMI[3,4]))
Table<-rbind(Table,c("Quantile 1", 
                     "1.00 [Reference]","",
                     "1.00 [Reference]","",
                     "1.00 [Reference]",""))
Table<-rbind(Table,c("Quantile 2", 
                     Table_TyG_BMI_quantile[1,7],"",
                     Table_TyG_BMI_quantile[2,7],"",
                     Table_TyG_BMI_quantile[3,7],""))
Table<-rbind(Table,c("Quantile 3", 
                     Table_TyG_BMI_quantile[4,7],"",
                     Table_TyG_BMI_quantile[5,7],"",
                     Table_TyG_BMI_quantile[6,7],""))
Table<-rbind(Table,c("Quantile 4", 
                     Table_TyG_BMI_quantile[7,7],Table_TyG_BMI_qua[1,4],
                     Table_TyG_BMI_quantile[8,7],Table_TyG_BMI_qua[2,4],
                     Table_TyG_BMI_quantile[9,7],Table_TyG_BMI_qua[3,4]))

Table<-as.data.frame(Table)
Table= data.frame(lapply(Table, as.character), stringsAsFactors=FALSE)
Table
write.table(Table,sep = ",",file ="I:/NHANES study/PD&TYG&MO/Result/Supplementary Table 15.csv",row.names =F,col.names =F )


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# ++++++++++++++++++++Complete data++++++++++++++++++++++ ####
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section x.Table S16  ####
load(file="I:/NHANES study/PD&TYG&MO/Data/Complete_weighted.Rdata")
Interpolation_weighted<-Complete_weighted
table(Interpolation_weighted$TYG_quantile)
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
Table_orginal<-cbind(Over,Quantile1[,c("counts","Mean","SE")],
                Quantile2[,c("counts","Mean","SE")],
                Quantile3[,c("counts","Mean","SE")],
                Quantile4[,c("counts","Mean","SE")])
table(Interpolation_weighted$TYG_quantile)
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
Table_orginal<-merge(Table_orginal,test_data,by="Covariates",all.x = T)
Table_orginal$Covariates
Table_orginal$Row<-paste0(Table_orginal$Covariates," ",Table_orginal$grade)
rownames(Table_orginal)<-Table_orginal$Row
colnames(Table_orginal)<-c("Covariates","grade",
                      "counts_all","Mean_all","SE_all",
                      "counts_TYG_quantile1","Mean_TYG_quantile1","SE_TYG_quantile1",
                      "counts_TYG_quantile2","Mean_TYG_quantile2","SE_TYG_quantile2",
                      "counts_TYG_quantile3","Mean_TYG_quantile3","SE_TYG_quantile3",
                      "counts_TYG_quantile4","Mean_TYG_quantile4","SE_TYG_quantile4",
                      "P.value","Row")
rownames(Table_orginal)
{ #* section 18.5 Combine  Table #####
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
                       Table_orginal["Age Mean ± SE","Mean_all"],Table_orginal["Age Mean ± SE","SE_all"],
                       Table_orginal["Age Mean ± SE","Mean_TYG_quantile1"],Table_orginal["Age Mean ± SE","SE_TYG_quantile1"],
                       Table_orginal["Age Mean ± SE","Mean_TYG_quantile2"],Table_orginal["Age Mean ± SE","SE_TYG_quantile2"],
                       Table_orginal["Age Mean ± SE","Mean_TYG_quantile3"],Table_orginal["Age Mean ± SE","SE_TYG_quantile3"],
                       Table_orginal["Age Mean ± SE","Mean_TYG_quantile4"],Table_orginal["Age Mean ± SE","SE_TYG_quantile4"],
                       Table_orginal["Age Mean ± SE","P.value"] ))
  Table<-rbind(Table,c("Age status, %","","","","","","","","","","",Table_orginal["Age_status <45","P.value"]))
  Table<-rbind(Table,c("<45",
                       Table_orginal["Age_status <45","Mean_all"],Table_orginal["Age_status <45","SE_all"],
                       Table_orginal["Age_status <45","Mean_TYG_quantile1"],Table_orginal["Age_status <45","SE_TYG_quantile1"],
                       Table_orginal["Age_status <45","Mean_TYG_quantile2"],Table_orginal["Age_status <45","SE_TYG_quantile2"],
                       Table_orginal["Age_status <45","Mean_TYG_quantile3"],Table_orginal["Age_status <45","SE_TYG_quantile3"],
                       Table_orginal["Age_status <45","Mean_TYG_quantile4"],Table_orginal["Age_status <45","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("[45, 65)",
                       Table_orginal["Age_status [45,65)","Mean_all"],Table_orginal["Age_status <45","SE_all"],
                       Table_orginal["Age_status [45,65)","Mean_TYG_quantile1"],Table_orginal["Age_status [45,65)","SE_TYG_quantile1"],
                       Table_orginal["Age_status [45,65)","Mean_TYG_quantile2"],Table_orginal["Age_status [45,65)","SE_TYG_quantile2"],
                       Table_orginal["Age_status [45,65)","Mean_TYG_quantile3"],Table_orginal["Age_status [45,65)","SE_TYG_quantile3"],
                       Table_orginal["Age_status [45,65)","Mean_TYG_quantile4"],Table_orginal["Age_status [45,65)","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("≥65",
                       Table_orginal["Age_status >=65","Mean_all"],Table_orginal["Age_status >=65","SE_all"],
                       Table_orginal["Age_status >=65","Mean_TYG_quantile1"],Table_orginal["Age_status >=65","SE_TYG_quantile1"],
                       Table_orginal["Age_status >=65","Mean_TYG_quantile2"],Table_orginal["Age_status >=65","SE_TYG_quantile2"],
                       Table_orginal["Age_status >=65","Mean_TYG_quantile3"],Table_orginal["Age_status >=65","SE_TYG_quantile3"],
                       Table_orginal["Age_status >=65","Mean_TYG_quantile4"],Table_orginal["Age_status >=65","SE_TYG_quantile4"],
                       "" ))
  
  #Sex
  Table<-rbind(Table,c("Sex, Female",
                       Table_orginal["Sex Female","Mean_all"],Table_orginal["Sex Female","SE_all"],
                       Table_orginal["Sex Female","Mean_TYG_quantile1"],Table_orginal["Sex Female","SE_TYG_quantile1"],
                       Table_orginal["Sex Female","Mean_TYG_quantile2"],Table_orginal["Sex Female","SE_TYG_quantile2"],
                       Table_orginal["Sex Female","Mean_TYG_quantile3"],Table_orginal["Sex Female","SE_TYG_quantile3"],
                       Table_orginal["Sex Female","Mean_TYG_quantile4"],Table_orginal["Sex Female","SE_TYG_quantile4"],
                       Table_orginal["Sex Female","P.value"]))
  
  #Race/ ethnicity
  Table<-rbind(Table,c("Race/ ethnicity, %","","","","","","","","","","",Table_orginal["Race_ethnicity Non-Hispanic White","P.value"]))
  Table<-rbind(Table,c("Non-Hispanic white",
                       Table_orginal["Race_ethnicity Non-Hispanic White","Mean_all"],Table_orginal["Race_ethnicity Non-Hispanic White","SE_all"],
                       Table_orginal["Race_ethnicity Non-Hispanic White","Mean_TYG_quantile1"],Table_orginal["Race_ethnicity Non-Hispanic White","SE_TYG_quantile1"],
                       Table_orginal["Race_ethnicity Non-Hispanic White","Mean_TYG_quantile2"],Table_orginal["Race_ethnicity Non-Hispanic White","SE_TYG_quantile2"],
                       Table_orginal["Race_ethnicity Non-Hispanic White","Mean_TYG_quantile3"],Table_orginal["Race_ethnicity Non-Hispanic White","SE_TYG_quantile3"],
                       Table_orginal["Race_ethnicity Non-Hispanic White","Mean_TYG_quantile4"],Table_orginal["Race_ethnicity Non-Hispanic White","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Non-Hispanic black",
                       Table_orginal["Race_ethnicity Non-Hispanic Black","Mean_all"],Table_orginal["Race_ethnicity Non-Hispanic Black","SE_all"],
                       Table_orginal["Race_ethnicity Non-Hispanic Black","Mean_TYG_quantile1"],Table_orginal["Race_ethnicity Non-Hispanic Black","SE_TYG_quantile1"],
                       Table_orginal["Race_ethnicity Non-Hispanic Black","Mean_TYG_quantile2"],Table_orginal["Race_ethnicity Non-Hispanic Black","SE_TYG_quantile2"],
                       Table_orginal["Race_ethnicity Non-Hispanic Black","Mean_TYG_quantile3"],Table_orginal["Race_ethnicity Non-Hispanic Black","SE_TYG_quantile3"],
                       Table_orginal["Race_ethnicity Non-Hispanic Black","Mean_TYG_quantile4"],Table_orginal["Race_ethnicity Non-Hispanic Black","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Hispanic",
                       Table_orginal["Race_ethnicity Hispanic","Mean_all"],Table_orginal["Race_ethnicity Hispanic","SE_all"],
                       Table_orginal["Race_ethnicity Hispanic","Mean_TYG_quantile1"],Table_orginal["Race_ethnicity Hispanic","SE_TYG_quantile1"],
                       Table_orginal["Race_ethnicity Hispanic","Mean_TYG_quantile2"],Table_orginal["Race_ethnicity Hispanic","SE_TYG_quantile2"],
                       Table_orginal["Race_ethnicity Hispanic","Mean_TYG_quantile3"],Table_orginal["Race_ethnicity Hispanic","SE_TYG_quantile3"],
                       Table_orginal["Race_ethnicity Hispanic","Mean_TYG_quantile4"],Table_orginal["Race_ethnicity Hispanic","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Other race/ ethnicity",
                       Table_orginal["Race_ethnicity Other_Race","Mean_all"],Table_orginal["Race_ethnicity Other_Race","SE_all"],
                       Table_orginal["Race_ethnicity Other_Race","Mean_TYG_quantile1"],Table_orginal["Race_ethnicity Other_Race","SE_TYG_quantile1"],
                       Table_orginal["Race_ethnicity Other_Race","Mean_TYG_quantile2"],Table_orginal["Race_ethnicity Other_Race","SE_TYG_quantile2"],
                       Table_orginal["Race_ethnicity Other_Race","Mean_TYG_quantile3"],Table_orginal["Race_ethnicity Other_Race","SE_TYG_quantile3"],
                       Table_orginal["Race_ethnicity Other_Race","Mean_TYG_quantile4"],Table_orginal["Race_ethnicity Other_Race","SE_TYG_quantile4"],
                       "" ))
  #Marital status
  Table<-rbind(Table,c("Marital status, %","","","","","","","","","","",Table_orginal["Marital_status Married","P.value"]))
  Table<-rbind(Table,c("Married",
                       Table_orginal["Marital_status Married","Mean_all"],Table_orginal["Marital_status Married","SE_all"],
                       Table_orginal["Marital_status Married","Mean_TYG_quantile1"],Table_orginal["Marital_status Married","SE_TYG_quantile1"],
                       Table_orginal["Marital_status Married","Mean_TYG_quantile2"],Table_orginal["Marital_status Married","SE_TYG_quantile2"],
                       Table_orginal["Marital_status Married","Mean_TYG_quantile3"],Table_orginal["Marital_status Married","SE_TYG_quantile3"],
                       Table_orginal["Marital_status Married","Mean_TYG_quantile4"],Table_orginal["Marital_status Married","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Never married",
                       Table_orginal["Marital_status Never_married","Mean_all"],Table_orginal["Marital_status Never_married","SE_all"],
                       Table_orginal["Marital_status Never_married","Mean_TYG_quantile1"],Table_orginal["Marital_status Never_married","SE_TYG_quantile1"],
                       Table_orginal["Marital_status Never_married","Mean_TYG_quantile2"],Table_orginal["Marital_status Never_married","SE_TYG_quantile2"],
                       Table_orginal["Marital_status Never_married","Mean_TYG_quantile3"],Table_orginal["Marital_status Never_married","SE_TYG_quantile3"],
                       Table_orginal["Marital_status Never_married","Mean_TYG_quantile4"],Table_orginal["Marital_status Never_married","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Widowed/ Divorced/ Separated",
                       Table_orginal["Marital_status Separated","Mean_all"],Table_orginal["Marital_status Separated","SE_all"],
                       Table_orginal["Marital_status Separated","Mean_TYG_quantile1"],Table_orginal["Marital_status Separated","SE_TYG_quantile1"],
                       Table_orginal["Marital_status Separated","Mean_TYG_quantile2"],Table_orginal["Marital_status Separated","SE_TYG_quantile2"],
                       Table_orginal["Marital_status Separated","Mean_TYG_quantile3"],Table_orginal["Marital_status Separated","SE_TYG_quantile3"],
                       Table_orginal["Marital_status Separated","Mean_TYG_quantile4"],Table_orginal["Marital_status Separated","SE_TYG_quantile4"],
                       "" ))
  
  #Socioeconomic Status
  Table<-rbind(Table,c("Socioeconomic Status, %","","","","","","","","","","",Table_orginal["SES low","P.value"]))
  Table<-rbind(Table,c("Low",
                       Table_orginal["SES low","Mean_all"],Table_orginal["SES low","SE_all"],
                       Table_orginal["SES low","Mean_TYG_quantile1"],Table_orginal["SES low","SE_TYG_quantile1"],
                       Table_orginal["SES low","Mean_TYG_quantile2"],Table_orginal["SES low","SE_TYG_quantile2"],
                       Table_orginal["SES low","Mean_TYG_quantile3"],Table_orginal["SES low","SE_TYG_quantile3"],
                       Table_orginal["SES low","Mean_TYG_quantile4"],Table_orginal["SES low","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Medium",
                       Table_orginal["SES medium","Mean_all"],Table_orginal["SES medium","SE_all"],
                       Table_orginal["SES medium","Mean_TYG_quantile1"],Table_orginal["SES medium","SE_TYG_quantile1"],
                       Table_orginal["SES medium","Mean_TYG_quantile2"],Table_orginal["SES medium","SE_TYG_quantile2"],
                       Table_orginal["SES medium","Mean_TYG_quantile3"],Table_orginal["SES medium","SE_TYG_quantile3"],
                       Table_orginal["SES medium","Mean_TYG_quantile4"],Table_orginal["SES medium","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("High",
                       Table_orginal["SES high","Mean_all"],Table_orginal["SES high","SE_all"],
                       Table_orginal["SES high","Mean_TYG_quantile1"],Table_orginal["SES high","SE_TYG_quantile1"],
                       Table_orginal["SES high","Mean_TYG_quantile2"],Table_orginal["SES high","SE_TYG_quantile2"],
                       Table_orginal["SES high","Mean_TYG_quantile3"],Table_orginal["SES high","SE_TYG_quantile3"],
                       Table_orginal["SES high","Mean_TYG_quantile4"],Table_orginal["SES high","SE_TYG_quantile4"],
                       "" ))
  
  #Smoking status
  Table<-rbind(Table,c("Smoking status, %","","","","","","","","","","",Table_orginal["Smoking_status Never_smoker","P.value"]))
  Table<-rbind(Table,c("Never smoker",
                       Table_orginal["Smoking_status Never_smoker","Mean_all"],Table_orginal["Smoking_status Never_smoker","SE_all"],
                       Table_orginal["Smoking_status Never_smoker","Mean_TYG_quantile1"],Table_orginal["Smoking_status Never_smoker","SE_TYG_quantile1"],
                       Table_orginal["Smoking_status Never_smoker","Mean_TYG_quantile2"],Table_orginal["Smoking_status Never_smoker","SE_TYG_quantile2"],
                       Table_orginal["Smoking_status Never_smoker","Mean_TYG_quantile3"],Table_orginal["Smoking_status Never_smoker","SE_TYG_quantile3"],
                       Table_orginal["Smoking_status Never_smoker","Mean_TYG_quantile4"],Table_orginal["Smoking_status Never_smoker","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Former smoker",
                       Table_orginal["Smoking_status Former_smoker","Mean_all"],Table_orginal["Smoking_status Former_smoker","SE_all"],
                       Table_orginal["Smoking_status Former_smoker","Mean_TYG_quantile1"],Table_orginal["Smoking_status Former_smoker","SE_TYG_quantile1"],
                       Table_orginal["Smoking_status Former_smoker","Mean_TYG_quantile2"],Table_orginal["Smoking_status Former_smoker","SE_TYG_quantile2"],
                       Table_orginal["Smoking_status Former_smoker","Mean_TYG_quantile3"],Table_orginal["Smoking_status Former_smoker","SE_TYG_quantile3"],
                       Table_orginal["Smoking_status Former_smoker","Mean_TYG_quantile4"],Table_orginal["Smoking_status Former_smoker","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Current smoker",
                       Table_orginal["Smoking_status Current_smoker","Mean_all"],Table_orginal["Smoking_status Current_smoker","SE_all"],
                       Table_orginal["Smoking_status Current_smoker","Mean_TYG_quantile1"],Table_orginal["Smoking_status Current_smoker","SE_TYG_quantile1"],
                       Table_orginal["Smoking_status Current_smoker","Mean_TYG_quantile2"],Table_orginal["Smoking_status Current_smoker","SE_TYG_quantile2"],
                       Table_orginal["Smoking_status Current_smoker","Mean_TYG_quantile3"],Table_orginal["Smoking_status Current_smoker","SE_TYG_quantile3"],
                       Table_orginal["Smoking_status Current_smoker","Mean_TYG_quantile4"],Table_orginal["Smoking_status Current_smoker","SE_TYG_quantile4"],
                       "" ))
  
  #Drinking status
  Table<-rbind(Table,c("Drinking status, %","","","","","","","","","","",Table_orginal["Drinking_status Nondrinker","P.value"]))
  Table<-rbind(Table,c("Nondrinker",
                       Table_orginal["Drinking_status Nondrinker","Mean_all"],Table_orginal["Drinking_status Nondrinker","SE_all"],
                       Table_orginal["Drinking_status Nondrinker","Mean_TYG_quantile1"],Table_orginal["Drinking_status Nondrinker","SE_TYG_quantile1"],
                       Table_orginal["Drinking_status Nondrinker","Mean_TYG_quantile2"],Table_orginal["Drinking_status Nondrinker","SE_TYG_quantile2"],
                       Table_orginal["Drinking_status Nondrinker","Mean_TYG_quantile3"],Table_orginal["Drinking_status Nondrinker","SE_TYG_quantile3"],
                       Table_orginal["Drinking_status Nondrinker","Mean_TYG_quantile4"],Table_orginal["Drinking_status Nondrinker","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Light/ moderate drinker",
                       Table_orginal["Drinking_status Light/moderate_drinker","Mean_all"],Table_orginal["Drinking_status Light/moderate_drinker","SE_all"],
                       Table_orginal["Drinking_status Light/moderate_drinker","Mean_TYG_quantile1"],Table_orginal["Drinking_status Light/moderate_drinker","SE_TYG_quantile1"],
                       Table_orginal["Drinking_status Light/moderate_drinker","Mean_TYG_quantile2"],Table_orginal["Drinking_status Light/moderate_drinker","SE_TYG_quantile2"],
                       Table_orginal["Drinking_status Light/moderate_drinker","Mean_TYG_quantile3"],Table_orginal["Drinking_status Light/moderate_drinker","SE_TYG_quantile3"],
                       Table_orginal["Drinking_status Light/moderate_drinker","Mean_TYG_quantile4"],Table_orginal["Drinking_status Light/moderate_drinker","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Heavier drinker",
                       Table_orginal["Drinking_status Heavier_drinker","Mean_all"],Table_orginal["Drinking_status Heavier_drinker","SE_all"],
                       Table_orginal["Drinking_status Heavier_drinker","Mean_TYG_quantile1"],Table_orginal["Drinking_status Heavier_drinker","SE_TYG_quantile1"],
                       Table_orginal["Drinking_status Heavier_drinker","Mean_TYG_quantile2"],Table_orginal["Drinking_status Heavier_drinker","SE_TYG_quantile2"],
                       Table_orginal["Drinking_status Heavier_drinker","Mean_TYG_quantile3"],Table_orginal["Drinking_status Heavier_drinker","SE_TYG_quantile3"],
                       Table_orginal["Drinking_status Heavier_drinker","Mean_TYG_quantile4"],Table_orginal["Drinking_status Heavier_drinker","SE_TYG_quantile4"],
                       "" ))
  #Physical status
  Table<-rbind(Table,c("Physical status, %","","","","","","","","","","",Table_orginal["Physical_status Inactive","P.value"]))
  Table<-rbind(Table,c("Inactive",
                       Table_orginal["Physical_status Inactive","Mean_all"],Table_orginal["Physical_status Inactive","SE_all"],
                       Table_orginal["Physical_status Inactive","Mean_TYG_quantile1"],Table_orginal["Physical_status Inactive","SE_TYG_quantile1"],
                       Table_orginal["Physical_status Inactive","Mean_TYG_quantile2"],Table_orginal["Physical_status Inactive","SE_TYG_quantile2"],
                       Table_orginal["Physical_status Inactive","Mean_TYG_quantile3"],Table_orginal["Physical_status Inactive","SE_TYG_quantile3"],
                       Table_orginal["Physical_status Inactive","Mean_TYG_quantile4"],Table_orginal["Physical_status Inactive","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Insufficient",
                       Table_orginal["Physical_status Insufficient","Mean_all"],Table_orginal["Physical_status Insufficient","SE_all"],
                       Table_orginal["Physical_status Insufficient","Mean_TYG_quantile1"],Table_orginal["Physical_status Insufficient","SE_TYG_quantile1"],
                       Table_orginal["Physical_status Insufficient","Mean_TYG_quantile2"],Table_orginal["Physical_status Insufficient","SE_TYG_quantile2"],
                       Table_orginal["Physical_status Insufficient","Mean_TYG_quantile3"],Table_orginal["Physical_status Insufficient","SE_TYG_quantile3"],
                       Table_orginal["Physical_status Insufficient","Mean_TYG_quantile4"],Table_orginal["Physical_status Insufficient","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Recommended",
                       Table_orginal["Physical_status Recommended","Mean_all"],Table_orginal["Physical_status Recommended","SE_all"],
                       Table_orginal["Physical_status Recommended","Mean_TYG_quantile1"],Table_orginal["Physical_status Recommended","SE_TYG_quantile1"],
                       Table_orginal["Physical_status Recommended","Mean_TYG_quantile2"],Table_orginal["Physical_status Recommended","SE_TYG_quantile2"],
                       Table_orginal["Physical_status Recommended","Mean_TYG_quantile3"],Table_orginal["Physical_status Recommended","SE_TYG_quantile3"],
                       Table_orginal["Physical_status Recommended","Mean_TYG_quantile4"],Table_orginal["Physical_status Recommended","SE_TYG_quantile4"],
                       "" ))
  
  #Healthy Eating Index
  #Table<-rbind(Table,c("Healthy eating index, %","","","","","","",Table_orginal["HEI Quintile 1","P.value"]))
  
  # Table<-rbind(Table,c("Quintile 1",
  #                      Table_orginal["HEI Quintile 1","Mean_all"],Table_orginal["HEI Quintile 1","SE_all"],
  #                      Table_orginal["HEI Quintile 1","Mean_noPD"],Table_orginal["HEI Quintile 1","SE_noPD"],
  #                      Table_orginal["HEI Quintile 1","Mean_PD"],Table_orginal["HEI Quintile 1","SE_PD"],
  #                      "" ))
  # Table<-rbind(Table,c("Quintile 2",
  #                      Table_orginal["HEI Quintile 2","Mean_all"],Table_orginal["HEI Quintile 2","SE_all"],
  #                      Table_orginal["HEI Quintile 2","Mean_noPD"],Table_orginal["HEI Quintile 2","SE_noPD"],
  #                      Table_orginal["HEI Quintile 2","Mean_PD"],Table_orginal["HEI Quintile 2","SE_PD"],
  #                      "" ))
  #
  # Table<-rbind(Table,c("Quintile 3",
  #                      Table_orginal["HEI Quintile 3","Mean_all"],Table_orginal["HEI Quintile 3","SE_all"],
  #                      Table_orginal["HEI Quintile 3","Mean_noPD"],Table_orginal["HEI Quintile 3","SE_noPD"],
  #                      Table_orginal["HEI Quintile 3","Mean_PD"],Table_orginal["HEI Quintile 3","SE_PD"],
  #                      "" ))
  # Table<-rbind(Table,c("Quintile 4",
  #                      Table_orginal["HEI Quintile 4","Mean_all"],Table_orginal["HEI Quintile 4","SE_all"],
  #                      Table_orginal["HEI Quintile 4","Mean_noPD"],Table_orginal["HEI Quintile 4","SE_noPD"],
  #                      Table_orginal["HEI Quintile 4","Mean_PD"],Table_orginal["HEI Quintile 4","SE_PD"],
  #                      "" ))
  # Table<-rbind(Table,c("Quintile 5",
  #                      Table_orginal["HEI Quintile 5","Mean_all"],Table_orginal["HEI Quintile 5","SE_all"],
  #                      Table_orginal["HEI Quintile 5","Mean_noPD"],Table_orginal["HEI Quintile 5","SE_noPD"],
  #                      Table_orginal["HEI Quintile 5","Mean_PD"],Table_orginal["HEI Quintile 5","SE_PD"],
  #                      "" ))
  
  #BMI
  Table<-rbind(Table,c("BMI, Mean",
                       Table_orginal["BMI Mean ± SE","Mean_all"],Table_orginal["BMI Mean ± SE","SE_all"],
                       Table_orginal["BMI Mean ± SE","Mean_TYG_quantile1"],Table_orginal["BMI Mean ± SE","SE_TYG_quantile1"],
                       Table_orginal["BMI Mean ± SE","Mean_TYG_quantile2"],Table_orginal["BMI Mean ± SE","SE_TYG_quantile2"],
                       Table_orginal["BMI Mean ± SE","Mean_TYG_quantile3"],Table_orginal["BMI Mean ± SE","SE_TYG_quantile3"],
                       Table_orginal["BMI Mean ± SE","Mean_TYG_quantile4"],Table_orginal["BMI Mean ± SE","SE_TYG_quantile4"],
                       Table_orginal["BMI Mean ± SE","P.value"] ))
  
  Table<-rbind(Table,c("BMI status (kg/m2), %","","","","","","","","","","",Table_orginal["BMI_status (0,25)","P.value"]))
  Table<-rbind(Table,c("<25",
                       Table_orginal["BMI_status (0,25)","Mean_all"],Table_orginal["BMI_status (0,25)","SE_all"],
                       Table_orginal["BMI_status (0,25)","Mean_TYG_quantile1"],Table_orginal["BMI_status (0,25)","SE_TYG_quantile1"],
                       Table_orginal["BMI_status (0,25)","Mean_TYG_quantile2"],Table_orginal["BMI_status (0,25)","SE_TYG_quantile2"],
                       Table_orginal["BMI_status (0,25)","Mean_TYG_quantile3"],Table_orginal["BMI_status (0,25)","SE_TYG_quantile3"],
                       Table_orginal["BMI_status (0,25)","Mean_TYG_quantile4"],Table_orginal["BMI_status (0,25)","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("[25.0 -30)",
                       Table_orginal["BMI_status [25.0-30)","Mean_all"],Table_orginal["BMI_status [25.0-30)","SE_all"],
                       Table_orginal["BMI_status [25.0-30)","Mean_TYG_quantile1"],Table_orginal["BMI_status [25.0-30)","SE_TYG_quantile1"],
                       Table_orginal["BMI_status [25.0-30)","Mean_TYG_quantile2"],Table_orginal["BMI_status [25.0-30)","SE_TYG_quantile2"],
                       Table_orginal["BMI_status [25.0-30)","Mean_TYG_quantile3"],Table_orginal["BMI_status [25.0-30)","SE_TYG_quantile3"],
                       Table_orginal["BMI_status [25.0-30)","Mean_TYG_quantile4"],Table_orginal["BMI_status [25.0-30)","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("≥30",
                       Table_orginal["BMI_status [30,inf)","Mean_all"],Table_orginal["BMI_status [30,inf)","SE_all"],
                       Table_orginal["BMI_status [30,inf)","Mean_TYG_quantile1"],Table_orginal["BMI_status [30,inf)","SE_TYG_quantile1"],
                       Table_orginal["BMI_status [30,inf)","Mean_TYG_quantile2"],Table_orginal["BMI_status [30,inf)","SE_TYG_quantile2"],
                       Table_orginal["BMI_status [30,inf)","Mean_TYG_quantile3"],Table_orginal["BMI_status [30,inf)","SE_TYG_quantile3"],
                       Table_orginal["BMI_status [30,inf)","Mean_TYG_quantile4"],Table_orginal["BMI_status [30,inf)","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Comorbidities, %","","","","","","","","","","",""))
  #Hypertension
  Table<-rbind(Table,c("Hypertension",
                       Table_orginal["HTN_status YES","Mean_all"],Table_orginal["HTN_status YES","SE_all"],
                       Table_orginal["HTN_status YES","Mean_TYG_quantile1"],Table_orginal["HTN_status YES","SE_TYG_quantile1"],
                       Table_orginal["HTN_status YES","Mean_TYG_quantile2"],Table_orginal["HTN_status YES","SE_TYG_quantile2"],
                       Table_orginal["HTN_status YES","Mean_TYG_quantile3"],Table_orginal["HTN_status YES","SE_TYG_quantile3"],
                       Table_orginal["HTN_status YES","Mean_TYG_quantile4"],Table_orginal["HTN_status YES","SE_TYG_quantile4"],
                       Table_orginal["HTN_status YES","P.value"]))
  #Hyperlipidemia
  Table<-rbind(Table,c("Hyperlipidemia",
                       Table_orginal["HPL_status YES","Mean_all"],Table_orginal["HPL_status YES","SE_all"],
                       Table_orginal["HPL_status YES","Mean_TYG_quantile1"],Table_orginal["HPL_status YES","SE_TYG_quantile1"],
                       Table_orginal["HPL_status YES","Mean_TYG_quantile2"],Table_orginal["HPL_status YES","SE_TYG_quantile2"],
                       Table_orginal["HPL_status YES","Mean_TYG_quantile3"],Table_orginal["HPL_status YES","SE_TYG_quantile3"],
                       Table_orginal["HPL_status YES","Mean_TYG_quantile4"],Table_orginal["HPL_status YES","SE_TYG_quantile4"],
                       Table_orginal["HPL_status YES","P.value"]))
  #Diabetes mellitus
  Table<-rbind(Table,c("Diabetes mellitus",
                       Table_orginal["T2D_status YES","Mean_all"],Table_orginal["T2D_status YES","SE_all"],
                       Table_orginal["T2D_status YES","Mean_TYG_quantile1"],Table_orginal["T2D_status YES","SE_TYG_quantile1"],
                       Table_orginal["T2D_status YES","Mean_TYG_quantile2"],Table_orginal["T2D_status YES","SE_TYG_quantile2"],
                       Table_orginal["T2D_status YES","Mean_TYG_quantile3"],Table_orginal["T2D_status YES","SE_TYG_quantile3"],
                       Table_orginal["T2D_status YES","Mean_TYG_quantile4"],Table_orginal["T2D_status YES","SE_TYG_quantile4"],
                       Table_orginal["T2D_status YES","P.value"]))
  #Cohort
  Table<-rbind(Table,c("Cohort period, %","","","","","","","","","","",Table_orginal["Cohort NHANES_CON1","P.value"]))
  
  Table<-rbind(Table,c("NHANES III",
                       Table_orginal["Cohort NHANES_III","Mean_all"],Table_orginal["Cohort NHANES_III","SE_all"],
                       Table_orginal["Cohort NHANES_III","Mean_TYG_quantile1"],Table_orginal["Cohort NHANES_III","SE_TYG_quantile1"],
                       Table_orginal["Cohort NHANES_III","Mean_TYG_quantile2"],Table_orginal["Cohort NHANES_III","SE_TYG_quantile2"],
                       Table_orginal["Cohort NHANES_III","Mean_TYG_quantile3"],Table_orginal["Cohort NHANES_III","SE_TYG_quantile3"],
                       Table_orginal["Cohort NHANES_III","Mean_TYG_quantile4"],Table_orginal["Cohort NHANES_III","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("NHANES 1999-2004",
                       Table_orginal["Cohort NHANES_CON1","Mean_all"],Table_orginal["Cohort NHANES_CON1","SE_all"],
                       Table_orginal["Cohort NHANES_CON1","Mean_TYG_quantile1"],Table_orginal["Cohort NHANES_CON1","SE_TYG_quantile1"],
                       Table_orginal["Cohort NHANES_CON1","Mean_TYG_quantile2"],Table_orginal["Cohort NHANES_CON1","SE_TYG_quantile2"],
                       Table_orginal["Cohort NHANES_CON1","Mean_TYG_quantile3"],Table_orginal["Cohort NHANES_CON1","SE_TYG_quantile3"],
                       Table_orginal["Cohort NHANES_CON1","Mean_TYG_quantile4"],Table_orginal["Cohort NHANES_CON1","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("NHANES 2009-2014",
                       Table_orginal["Cohort NHANES_CON2","Mean_all"],Table_orginal["Cohort NHANES_CON2","SE_all"],
                       Table_orginal["Cohort NHANES_CON2","Mean_TYG_quantile1"],Table_orginal["Cohort NHANES_CON2","SE_TYG_quantile1"],
                       Table_orginal["Cohort NHANES_CON2","Mean_TYG_quantile2"],Table_orginal["Cohort NHANES_CON2","SE_TYG_quantile2"],
                       Table_orginal["Cohort NHANES_CON2","Mean_TYG_quantile3"],Table_orginal["Cohort NHANES_CON2","SE_TYG_quantile3"],
                       Table_orginal["Cohort NHANES_CON2","Mean_TYG_quantile4"],Table_orginal["Cohort NHANES_CON2","SE_TYG_quantile4"],
                       "" ))
  
  Table
  write.table(Table,sep = ",",file ="I:/NHANES study/PD&TYG&MO/Result/Supplementary Table 16.csv" ,row.names =F,col.names =F )
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 19 Load data ####  
load(file="I:/NHANES study/PD&TYG&MO/Data/Complete_weighted.Rdata")
Interpolation_weighted<-Complete_weighted
table(Interpolation_weighted$TYG_quantile)
Interpolation_weighted$TyG_WC<-Interpolation_weighted$TyG_WC/100
Interpolation_weighted$TyG_BMI<-Interpolation_weighted$TyG_BMI/100
Interpolation_weighted$TYG_quantile<-as.character(Interpolation_weighted$TYG_quantile)
Interpolation_weighted$TYG_qua[Interpolation_weighted$TYG_quantile=="Quantile 1"]<-0
Interpolation_weighted$TYG_qua[Interpolation_weighted$TYG_quantile=="Quantile 2"]<-1
Interpolation_weighted$TYG_qua[Interpolation_weighted$TYG_quantile=="Quantile 3"]<-2
Interpolation_weighted$TYG_qua[Interpolation_weighted$TYG_quantile=="Quantile 4"]<-3
table(Interpolation_weighted$TyG_WC_quantile)
Interpolation_weighted$TyG_WC_quantile<-as.character(Interpolation_weighted$TyG_WC_quantile)
Interpolation_weighted$TyG_WC_qua[Interpolation_weighted$TyG_WC_quantile=="Quantile 1"]<-0
Interpolation_weighted$TyG_WC_qua[Interpolation_weighted$TyG_WC_quantile=="Quantile 2"]<-1
Interpolation_weighted$TyG_WC_qua[Interpolation_weighted$TyG_WC_quantile=="Quantile 3"]<-2
Interpolation_weighted$TyG_WC_qua[Interpolation_weighted$TyG_WC_quantile=="Quantile 4"]<-3
table(Interpolation_weighted$TyG_WHtR_quantile)
Interpolation_weighted$TyG_WHtR_quantile<-as.character(Interpolation_weighted$TyG_WHtR_quantile)
Interpolation_weighted$TyG_WHtR_qua[Interpolation_weighted$TyG_WHtR_quantile=="Quantile 1"]<-0
Interpolation_weighted$TyG_WHtR_qua[Interpolation_weighted$TyG_WHtR_quantile=="Quantile 2"]<-1
Interpolation_weighted$TyG_WHtR_qua[Interpolation_weighted$TyG_WHtR_quantile=="Quantile 3"]<-2
Interpolation_weighted$TyG_WHtR_qua[Interpolation_weighted$TyG_WHtR_quantile=="Quantile 4"]<-3
table(Interpolation_weighted$TyG_WHtR_qua)
Interpolation_weighted$TyG_BMI_quantile<-as.character(Interpolation_weighted$TyG_BMI_quantile)
Interpolation_weighted$TyG_BMI_qua[Interpolation_weighted$TyG_BMI_quantile=="Quantile 1"]<-0
Interpolation_weighted$TyG_BMI_qua[Interpolation_weighted$TyG_BMI_quantile=="Quantile 2"]<-1
Interpolation_weighted$TyG_BMI_qua[Interpolation_weighted$TyG_BMI_quantile=="Quantile 3"]<-2
Interpolation_weighted$TyG_BMI_qua[Interpolation_weighted$TyG_BMI_quantile=="Quantile 4"]<-3
table(Interpolation_weighted$TyG_BMI_qua)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 19 TYG  ####  
{ #* all model #####
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
  result_All<-result4
}
{ #* CVD model #####
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
  result_CVD<-result4
}
{ #* Cancer model #####
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
  result_Cancer<-result4
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
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  Table_TyG<-as.data.frame(result.cause)
  Table_TyG= data.frame(lapply(Table_TyG, as.character), stringsAsFactors=FALSE)
}
Table_TyG
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 20 TYG WC ####  
{ #* all model #####
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
  result_All<-result4
}
{ #* CVD model #####
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
  result_CVD<-result4
}

{ #* Cancer model #####
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
  result_Cancer<-result4
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
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  Table_TyG_WC<-as.data.frame(result.cause)
  Table_TyG_WC= data.frame(lapply(Table_TyG_WC, as.character), stringsAsFactors=FALSE)
}
Table_TyG_WC
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 21 TYG WHtR ####  
{ #* all model #####
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
  result_All<-result4
  result_All
}
{ #* CVD model #####
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
  result_CVD<-result4
}

{ #* Cancer model #####
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
  result_Cancer<-result4
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
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  Table_TyG_WHtR<-as.data.frame(result.cause)
  Table_TyG_WHtR = data.frame(lapply(Table_TyG_WHtR, as.character), stringsAsFactors=FALSE)
}
Table_TyG_WHtR

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 22 TYG BMI ####  
{ #* all model #####
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
  result_All<-result4
}

{ #* CVD model #####
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
  result_CVD<-result4
}
{ #* Cancer model #####
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
  result_Cancer<-result4
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
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  Table_TyG_BMI<-as.data.frame(result.cause)
  Table_TyG_BMI= data.frame(lapply(Table_TyG_BMI, as.character), stringsAsFactors=FALSE)
}
Table_TyG_BMI
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 23 TYG quantile ####    

{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TYG_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}
{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TYG_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
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
  result.Cancer<-result4
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
  Table_TyG_quantile<-result.cause
  Table_TyG_quantile
}

{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TYG_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}
{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TYG_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
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
  result.Cancer<-result4
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
  Table_TyG_qua<-result.cause
  Table_TyG_qua
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 24 TYG WC quantile ####    
{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_WC_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}
{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_WC_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  P<-model4_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
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
  result.Cancer<-result4
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
  Table_TyG_WC_quantile<-result.cause
  Table_TyG_WC_quantile
}
{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_WC_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}
{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_WC_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
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
  result.Cancer<-result4
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
  Table_TyG_WC_qua<-result.cause
  Table_TyG_WC_qua
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 25 TYG WHtR quantile  ####    
{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_WHtR_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}
{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_WHtR_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
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
  result.Cancer<-result4
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
  Table_TyG_WHtR_quantile<-result.cause
  Table_TyG_WHtR_quantile
}
{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_WHtR_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}
{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_WHtR_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  P<-model4_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
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
  result.Cancer<-result4
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
  Table_TyG_WHtR_qua<-result.cause
  Table_TyG_WHtR_qua
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 26 TYG BMI quantile  #### 
{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_BMI_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}
{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_BMI_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
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
  result.Cancer<-result4
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
  Table_TyG_BMI_quantile<-result.cause
}

{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_BMI_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}

{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_BMI_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
  
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
  result.Cancer<-result4
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
  Table_TyG_BMI_qua<-result.cause
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 26 Total (Table S17) #### 
Table<-c("Exposure","All cause","","CVD-related","","Cancer-related","")
Table<-rbind(Table,c("","HR (95% CI)*","P†", "HR (95% CI)","P","HR (95% CI)","P"))
Table<-rbind(Table,c("TyG index","", "", "", "", "",""))
Table<-rbind(Table,c("Per 1-index‡",Table_TyG[1,7],Table_TyG[1,4], 
                     Table_TyG[2,7],Table_TyG[2,4],
                     Table_TyG[3,7],Table_TyG[3,4]))
Table<-rbind(Table,c("Quantile 1", 
                     "1.00 [Reference]","",
                     "1.00 [Reference]","",
                     "1.00 [Reference]",""))
Table<-rbind(Table,c("Quantile 2", 
                     Table_TyG_quantile[1,7],"",
                     Table_TyG_quantile[2,7],"",
                     Table_TyG_quantile[3,7],""))
Table<-rbind(Table,c("Quantile 3", 
                     Table_TyG_quantile[4,7],"",
                     Table_TyG_quantile[5,7],"",
                     Table_TyG_quantile[6,7],""))
Table<-rbind(Table,c("Quantile 4", 
                     Table_TyG_quantile[7,7],Table_TyG_qua[1,4],
                     Table_TyG_quantile[8,7],Table_TyG_qua[2,4],
                     Table_TyG_quantile[9,7],Table_TyG_qua[3,4]))

Table<-rbind(Table,c("TyG-WC index","", "", "", "", "",""))
Table<-rbind(Table,c("Per 100-index",Table_TyG_WC[1,7],Table_TyG_WC[1,4], 
                     Table_TyG_WC[2,7],Table_TyG_WC[2,4],
                     Table_TyG_WC[3,7],Table_TyG_WC[3,4]))
Table<-rbind(Table,c("Quantile 1", 
                     "1.00 [Reference]","",
                     "1.00 [Reference]","",
                     "1.00 [Reference]",""))
Table<-rbind(Table,c("Quantile 2", 
                     Table_TyG_WC_quantile[1,7],"",
                     Table_TyG_WC_quantile[2,7],"",
                     Table_TyG_WC_quantile[3,7],""))
Table<-rbind(Table,c("Quantile 3", 
                     Table_TyG_WC_quantile[4,7],"",
                     Table_TyG_WC_quantile[5,7],"",
                     Table_TyG_WC_quantile[6,7],""))
Table<-rbind(Table,c("Quantile 4", 
                     Table_TyG_WC_quantile[7,7],Table_TyG_WC_qua[1,4],
                     Table_TyG_WC_quantile[8,7],Table_TyG_WC_qua[2,4],
                     Table_TyG_WC_quantile[9,7],Table_TyG_WC_qua[3,4]))

Table<-rbind(Table,c("TyG-WHtR index","", "", "", "", "",""))
Table<-rbind(Table,c("Per 1-index",Table_TyG_WHtR[1,7],Table_TyG_WHtR[1,4], 
                     Table_TyG_WHtR[2,7],Table_TyG_WHtR[2,4],
                     Table_TyG_WHtR[3,7],Table_TyG_WHtR[3,4]))
Table<-rbind(Table,c("Quantile 1", 
                     "1.00 [Reference]","",
                     "1.00 [Reference]","",
                     "1.00 [Reference]",""))
Table<-rbind(Table,c("Quantile 2", 
                     Table_TyG_WHtR_quantile[1,7],"",
                     Table_TyG_WHtR_quantile[2,7],"",
                     Table_TyG_WHtR_quantile[3,7],""))
Table<-rbind(Table,c("Quantile 3", 
                     Table_TyG_WHtR_quantile[4,7],"",
                     Table_TyG_WHtR_quantile[5,7],"",
                     Table_TyG_WHtR_quantile[6,7],""))
Table<-rbind(Table,c("Quantile 4", 
                     Table_TyG_WHtR_quantile[7,7],Table_TyG_WHtR_qua[1,4],
                     Table_TyG_WHtR_quantile[8,7],Table_TyG_WHtR_qua[2,4],
                     Table_TyG_WHtR_quantile[9,7],Table_TyG_WHtR_qua[3,4]))

Table<-rbind(Table,c("TyG-BMI index","", "", "", "", "",""))
Table<-rbind(Table,c("Per 100-index",Table_TyG_BMI[1,7],Table_TyG_BMI[1,4], 
                     Table_TyG_BMI[2,7],Table_TyG_BMI[2,4],
                     Table_TyG_BMI[3,7],Table_TyG_BMI[3,4]))
Table<-rbind(Table,c("Quantile 1", 
                     "1.00 [Reference]","",
                     "1.00 [Reference]","",
                     "1.00 [Reference]",""))
Table<-rbind(Table,c("Quantile 2", 
                     Table_TyG_BMI_quantile[1,7],"",
                     Table_TyG_BMI_quantile[2,7],"",
                     Table_TyG_BMI_quantile[3,7],""))
Table<-rbind(Table,c("Quantile 3", 
                     Table_TyG_BMI_quantile[4,7],"",
                     Table_TyG_BMI_quantile[5,7],"",
                     Table_TyG_BMI_quantile[6,7],""))
Table<-rbind(Table,c("Quantile 4", 
                     Table_TyG_BMI_quantile[7,7],Table_TyG_BMI_qua[1,4],
                     Table_TyG_BMI_quantile[8,7],Table_TyG_BMI_qua[2,4],
                     Table_TyG_BMI_quantile[9,7],Table_TyG_BMI_qua[3,4]))

Table<-as.data.frame(Table)
Table= data.frame(lapply(Table, as.character), stringsAsFactors=FALSE)
Table
write.table(Table,sep = ",",file ="I:/NHANES study/PD&TYG&MO/Result/Supplementary Table 17.csv",row.names =F,col.names =F )

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# ++++++++++++++++++++Complete data++++++++++++++++++++++ ####
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section x.Table S16  ####
load(file="I:/NHANES study/PD&TYG&MO/Data/Complete_weighted.Rdata")
Interpolation_weighted<-Complete_weighted
table(Interpolation_weighted$TYG_quantile)
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
Table_orginal<-cbind(Over,Quantile1[,c("counts","Mean","SE")],
                     Quantile2[,c("counts","Mean","SE")],
                     Quantile3[,c("counts","Mean","SE")],
                     Quantile4[,c("counts","Mean","SE")])
table(Interpolation_weighted$TYG_quantile)
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
Table_orginal<-merge(Table_orginal,test_data,by="Covariates",all.x = T)
Table_orginal$Covariates
Table_orginal$Row<-paste0(Table_orginal$Covariates," ",Table_orginal$grade)
rownames(Table_orginal)<-Table_orginal$Row
colnames(Table_orginal)<-c("Covariates","grade",
                           "counts_all","Mean_all","SE_all",
                           "counts_TYG_quantile1","Mean_TYG_quantile1","SE_TYG_quantile1",
                           "counts_TYG_quantile2","Mean_TYG_quantile2","SE_TYG_quantile2",
                           "counts_TYG_quantile3","Mean_TYG_quantile3","SE_TYG_quantile3",
                           "counts_TYG_quantile4","Mean_TYG_quantile4","SE_TYG_quantile4",
                           "P.value","Row")
rownames(Table_orginal)
{ #* section 18.5 Combine  Table #####
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
                       Table_orginal["Age Mean ± SE","Mean_all"],Table_orginal["Age Mean ± SE","SE_all"],
                       Table_orginal["Age Mean ± SE","Mean_TYG_quantile1"],Table_orginal["Age Mean ± SE","SE_TYG_quantile1"],
                       Table_orginal["Age Mean ± SE","Mean_TYG_quantile2"],Table_orginal["Age Mean ± SE","SE_TYG_quantile2"],
                       Table_orginal["Age Mean ± SE","Mean_TYG_quantile3"],Table_orginal["Age Mean ± SE","SE_TYG_quantile3"],
                       Table_orginal["Age Mean ± SE","Mean_TYG_quantile4"],Table_orginal["Age Mean ± SE","SE_TYG_quantile4"],
                       Table_orginal["Age Mean ± SE","P.value"] ))
  Table<-rbind(Table,c("Age status, %","","","","","","","","","","",Table_orginal["Age_status <45","P.value"]))
  Table<-rbind(Table,c("<45",
                       Table_orginal["Age_status <45","Mean_all"],Table_orginal["Age_status <45","SE_all"],
                       Table_orginal["Age_status <45","Mean_TYG_quantile1"],Table_orginal["Age_status <45","SE_TYG_quantile1"],
                       Table_orginal["Age_status <45","Mean_TYG_quantile2"],Table_orginal["Age_status <45","SE_TYG_quantile2"],
                       Table_orginal["Age_status <45","Mean_TYG_quantile3"],Table_orginal["Age_status <45","SE_TYG_quantile3"],
                       Table_orginal["Age_status <45","Mean_TYG_quantile4"],Table_orginal["Age_status <45","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("[45, 65)",
                       Table_orginal["Age_status [45,65)","Mean_all"],Table_orginal["Age_status <45","SE_all"],
                       Table_orginal["Age_status [45,65)","Mean_TYG_quantile1"],Table_orginal["Age_status [45,65)","SE_TYG_quantile1"],
                       Table_orginal["Age_status [45,65)","Mean_TYG_quantile2"],Table_orginal["Age_status [45,65)","SE_TYG_quantile2"],
                       Table_orginal["Age_status [45,65)","Mean_TYG_quantile3"],Table_orginal["Age_status [45,65)","SE_TYG_quantile3"],
                       Table_orginal["Age_status [45,65)","Mean_TYG_quantile4"],Table_orginal["Age_status [45,65)","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("≥65",
                       Table_orginal["Age_status >=65","Mean_all"],Table_orginal["Age_status >=65","SE_all"],
                       Table_orginal["Age_status >=65","Mean_TYG_quantile1"],Table_orginal["Age_status >=65","SE_TYG_quantile1"],
                       Table_orginal["Age_status >=65","Mean_TYG_quantile2"],Table_orginal["Age_status >=65","SE_TYG_quantile2"],
                       Table_orginal["Age_status >=65","Mean_TYG_quantile3"],Table_orginal["Age_status >=65","SE_TYG_quantile3"],
                       Table_orginal["Age_status >=65","Mean_TYG_quantile4"],Table_orginal["Age_status >=65","SE_TYG_quantile4"],
                       "" ))
  
  #Sex
  Table<-rbind(Table,c("Sex, Female",
                       Table_orginal["Sex Female","Mean_all"],Table_orginal["Sex Female","SE_all"],
                       Table_orginal["Sex Female","Mean_TYG_quantile1"],Table_orginal["Sex Female","SE_TYG_quantile1"],
                       Table_orginal["Sex Female","Mean_TYG_quantile2"],Table_orginal["Sex Female","SE_TYG_quantile2"],
                       Table_orginal["Sex Female","Mean_TYG_quantile3"],Table_orginal["Sex Female","SE_TYG_quantile3"],
                       Table_orginal["Sex Female","Mean_TYG_quantile4"],Table_orginal["Sex Female","SE_TYG_quantile4"],
                       Table_orginal["Sex Female","P.value"]))
  
  #Race/ ethnicity
  Table<-rbind(Table,c("Race/ ethnicity, %","","","","","","","","","","",Table_orginal["Race_ethnicity Non-Hispanic White","P.value"]))
  Table<-rbind(Table,c("Non-Hispanic white",
                       Table_orginal["Race_ethnicity Non-Hispanic White","Mean_all"],Table_orginal["Race_ethnicity Non-Hispanic White","SE_all"],
                       Table_orginal["Race_ethnicity Non-Hispanic White","Mean_TYG_quantile1"],Table_orginal["Race_ethnicity Non-Hispanic White","SE_TYG_quantile1"],
                       Table_orginal["Race_ethnicity Non-Hispanic White","Mean_TYG_quantile2"],Table_orginal["Race_ethnicity Non-Hispanic White","SE_TYG_quantile2"],
                       Table_orginal["Race_ethnicity Non-Hispanic White","Mean_TYG_quantile3"],Table_orginal["Race_ethnicity Non-Hispanic White","SE_TYG_quantile3"],
                       Table_orginal["Race_ethnicity Non-Hispanic White","Mean_TYG_quantile4"],Table_orginal["Race_ethnicity Non-Hispanic White","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Non-Hispanic black",
                       Table_orginal["Race_ethnicity Non-Hispanic Black","Mean_all"],Table_orginal["Race_ethnicity Non-Hispanic Black","SE_all"],
                       Table_orginal["Race_ethnicity Non-Hispanic Black","Mean_TYG_quantile1"],Table_orginal["Race_ethnicity Non-Hispanic Black","SE_TYG_quantile1"],
                       Table_orginal["Race_ethnicity Non-Hispanic Black","Mean_TYG_quantile2"],Table_orginal["Race_ethnicity Non-Hispanic Black","SE_TYG_quantile2"],
                       Table_orginal["Race_ethnicity Non-Hispanic Black","Mean_TYG_quantile3"],Table_orginal["Race_ethnicity Non-Hispanic Black","SE_TYG_quantile3"],
                       Table_orginal["Race_ethnicity Non-Hispanic Black","Mean_TYG_quantile4"],Table_orginal["Race_ethnicity Non-Hispanic Black","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Hispanic",
                       Table_orginal["Race_ethnicity Hispanic","Mean_all"],Table_orginal["Race_ethnicity Hispanic","SE_all"],
                       Table_orginal["Race_ethnicity Hispanic","Mean_TYG_quantile1"],Table_orginal["Race_ethnicity Hispanic","SE_TYG_quantile1"],
                       Table_orginal["Race_ethnicity Hispanic","Mean_TYG_quantile2"],Table_orginal["Race_ethnicity Hispanic","SE_TYG_quantile2"],
                       Table_orginal["Race_ethnicity Hispanic","Mean_TYG_quantile3"],Table_orginal["Race_ethnicity Hispanic","SE_TYG_quantile3"],
                       Table_orginal["Race_ethnicity Hispanic","Mean_TYG_quantile4"],Table_orginal["Race_ethnicity Hispanic","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Other race/ ethnicity",
                       Table_orginal["Race_ethnicity Other_Race","Mean_all"],Table_orginal["Race_ethnicity Other_Race","SE_all"],
                       Table_orginal["Race_ethnicity Other_Race","Mean_TYG_quantile1"],Table_orginal["Race_ethnicity Other_Race","SE_TYG_quantile1"],
                       Table_orginal["Race_ethnicity Other_Race","Mean_TYG_quantile2"],Table_orginal["Race_ethnicity Other_Race","SE_TYG_quantile2"],
                       Table_orginal["Race_ethnicity Other_Race","Mean_TYG_quantile3"],Table_orginal["Race_ethnicity Other_Race","SE_TYG_quantile3"],
                       Table_orginal["Race_ethnicity Other_Race","Mean_TYG_quantile4"],Table_orginal["Race_ethnicity Other_Race","SE_TYG_quantile4"],
                       "" ))
  #Marital status
  Table<-rbind(Table,c("Marital status, %","","","","","","","","","","",Table_orginal["Marital_status Married","P.value"]))
  Table<-rbind(Table,c("Married",
                       Table_orginal["Marital_status Married","Mean_all"],Table_orginal["Marital_status Married","SE_all"],
                       Table_orginal["Marital_status Married","Mean_TYG_quantile1"],Table_orginal["Marital_status Married","SE_TYG_quantile1"],
                       Table_orginal["Marital_status Married","Mean_TYG_quantile2"],Table_orginal["Marital_status Married","SE_TYG_quantile2"],
                       Table_orginal["Marital_status Married","Mean_TYG_quantile3"],Table_orginal["Marital_status Married","SE_TYG_quantile3"],
                       Table_orginal["Marital_status Married","Mean_TYG_quantile4"],Table_orginal["Marital_status Married","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Never married",
                       Table_orginal["Marital_status Never_married","Mean_all"],Table_orginal["Marital_status Never_married","SE_all"],
                       Table_orginal["Marital_status Never_married","Mean_TYG_quantile1"],Table_orginal["Marital_status Never_married","SE_TYG_quantile1"],
                       Table_orginal["Marital_status Never_married","Mean_TYG_quantile2"],Table_orginal["Marital_status Never_married","SE_TYG_quantile2"],
                       Table_orginal["Marital_status Never_married","Mean_TYG_quantile3"],Table_orginal["Marital_status Never_married","SE_TYG_quantile3"],
                       Table_orginal["Marital_status Never_married","Mean_TYG_quantile4"],Table_orginal["Marital_status Never_married","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Widowed/ Divorced/ Separated",
                       Table_orginal["Marital_status Separated","Mean_all"],Table_orginal["Marital_status Separated","SE_all"],
                       Table_orginal["Marital_status Separated","Mean_TYG_quantile1"],Table_orginal["Marital_status Separated","SE_TYG_quantile1"],
                       Table_orginal["Marital_status Separated","Mean_TYG_quantile2"],Table_orginal["Marital_status Separated","SE_TYG_quantile2"],
                       Table_orginal["Marital_status Separated","Mean_TYG_quantile3"],Table_orginal["Marital_status Separated","SE_TYG_quantile3"],
                       Table_orginal["Marital_status Separated","Mean_TYG_quantile4"],Table_orginal["Marital_status Separated","SE_TYG_quantile4"],
                       "" ))
  
  #Socioeconomic Status
  Table<-rbind(Table,c("Socioeconomic Status, %","","","","","","","","","","",Table_orginal["SES low","P.value"]))
  Table<-rbind(Table,c("Low",
                       Table_orginal["SES low","Mean_all"],Table_orginal["SES low","SE_all"],
                       Table_orginal["SES low","Mean_TYG_quantile1"],Table_orginal["SES low","SE_TYG_quantile1"],
                       Table_orginal["SES low","Mean_TYG_quantile2"],Table_orginal["SES low","SE_TYG_quantile2"],
                       Table_orginal["SES low","Mean_TYG_quantile3"],Table_orginal["SES low","SE_TYG_quantile3"],
                       Table_orginal["SES low","Mean_TYG_quantile4"],Table_orginal["SES low","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Low_medium",
                       Table_orginal["SES low_medium","Mean_all"],Table_orginal["SES low_medium","SE_all"],
                       Table_orginal["SES low_medium","Mean_TYG_quantile1"],Table_orginal["SES low_medium","SE_TYG_quantile1"],
                       Table_orginal["SES low_medium","Mean_TYG_quantile2"],Table_orginal["SES low_medium","SE_TYG_quantile2"],
                       Table_orginal["SES low_medium","Mean_TYG_quantile3"],Table_orginal["SES low_medium","SE_TYG_quantile3"],
                       Table_orginal["SES low_medium","Mean_TYG_quantile4"],Table_orginal["SES low_medium","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Medium_high",
                       Table_orginal["SES medium_high","Mean_all"],Table_orginal["SES medium_high","SE_all"],
                       Table_orginal["SES medium_high","Mean_TYG_quantile1"],Table_orginal["SES medium_high","SE_TYG_quantile1"],
                       Table_orginal["SES medium_high","Mean_TYG_quantile2"],Table_orginal["SES medium_high","SE_TYG_quantile2"],
                       Table_orginal["SES medium_high","Mean_TYG_quantile3"],Table_orginal["SES medium_high","SE_TYG_quantile3"],
                       Table_orginal["SES medium_high","Mean_TYG_quantile4"],Table_orginal["SES medium_high","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("High",
                       Table_orginal["SES high","Mean_all"],Table_orginal["SES high","SE_all"],
                       Table_orginal["SES high","Mean_TYG_quantile1"],Table_orginal["SES high","SE_TYG_quantile1"],
                       Table_orginal["SES high","Mean_TYG_quantile2"],Table_orginal["SES high","SE_TYG_quantile2"],
                       Table_orginal["SES high","Mean_TYG_quantile3"],Table_orginal["SES high","SE_TYG_quantile3"],
                       Table_orginal["SES high","Mean_TYG_quantile4"],Table_orginal["SES high","SE_TYG_quantile4"],
                       "" ))
  
  #Smoking status
  Table<-rbind(Table,c("Smoking status, %","","","","","","","","","","",Table_orginal["Smoking_status Never_smoker","P.value"]))
  Table<-rbind(Table,c("Never smoker",
                       Table_orginal["Smoking_status Never_smoker","Mean_all"],Table_orginal["Smoking_status Never_smoker","SE_all"],
                       Table_orginal["Smoking_status Never_smoker","Mean_TYG_quantile1"],Table_orginal["Smoking_status Never_smoker","SE_TYG_quantile1"],
                       Table_orginal["Smoking_status Never_smoker","Mean_TYG_quantile2"],Table_orginal["Smoking_status Never_smoker","SE_TYG_quantile2"],
                       Table_orginal["Smoking_status Never_smoker","Mean_TYG_quantile3"],Table_orginal["Smoking_status Never_smoker","SE_TYG_quantile3"],
                       Table_orginal["Smoking_status Never_smoker","Mean_TYG_quantile4"],Table_orginal["Smoking_status Never_smoker","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Former smoker",
                       Table_orginal["Smoking_status Former_smoker","Mean_all"],Table_orginal["Smoking_status Former_smoker","SE_all"],
                       Table_orginal["Smoking_status Former_smoker","Mean_TYG_quantile1"],Table_orginal["Smoking_status Former_smoker","SE_TYG_quantile1"],
                       Table_orginal["Smoking_status Former_smoker","Mean_TYG_quantile2"],Table_orginal["Smoking_status Former_smoker","SE_TYG_quantile2"],
                       Table_orginal["Smoking_status Former_smoker","Mean_TYG_quantile3"],Table_orginal["Smoking_status Former_smoker","SE_TYG_quantile3"],
                       Table_orginal["Smoking_status Former_smoker","Mean_TYG_quantile4"],Table_orginal["Smoking_status Former_smoker","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Current smoker",
                       Table_orginal["Smoking_status Current_smoker","Mean_all"],Table_orginal["Smoking_status Current_smoker","SE_all"],
                       Table_orginal["Smoking_status Current_smoker","Mean_TYG_quantile1"],Table_orginal["Smoking_status Current_smoker","SE_TYG_quantile1"],
                       Table_orginal["Smoking_status Current_smoker","Mean_TYG_quantile2"],Table_orginal["Smoking_status Current_smoker","SE_TYG_quantile2"],
                       Table_orginal["Smoking_status Current_smoker","Mean_TYG_quantile3"],Table_orginal["Smoking_status Current_smoker","SE_TYG_quantile3"],
                       Table_orginal["Smoking_status Current_smoker","Mean_TYG_quantile4"],Table_orginal["Smoking_status Current_smoker","SE_TYG_quantile4"],
                       "" ))
  
  #Drinking status
  Table<-rbind(Table,c("Drinking status, %","","","","","","","","","","",Table_orginal["Drinking_status Nondrinker","P.value"]))
  Table<-rbind(Table,c("Nondrinker",
                       Table_orginal["Drinking_status Nondrinker","Mean_all"],Table_orginal["Drinking_status Nondrinker","SE_all"],
                       Table_orginal["Drinking_status Nondrinker","Mean_TYG_quantile1"],Table_orginal["Drinking_status Nondrinker","SE_TYG_quantile1"],
                       Table_orginal["Drinking_status Nondrinker","Mean_TYG_quantile2"],Table_orginal["Drinking_status Nondrinker","SE_TYG_quantile2"],
                       Table_orginal["Drinking_status Nondrinker","Mean_TYG_quantile3"],Table_orginal["Drinking_status Nondrinker","SE_TYG_quantile3"],
                       Table_orginal["Drinking_status Nondrinker","Mean_TYG_quantile4"],Table_orginal["Drinking_status Nondrinker","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Light/ moderate drinker",
                       Table_orginal["Drinking_status Light/moderate_drinker","Mean_all"],Table_orginal["Drinking_status Light/moderate_drinker","SE_all"],
                       Table_orginal["Drinking_status Light/moderate_drinker","Mean_TYG_quantile1"],Table_orginal["Drinking_status Light/moderate_drinker","SE_TYG_quantile1"],
                       Table_orginal["Drinking_status Light/moderate_drinker","Mean_TYG_quantile2"],Table_orginal["Drinking_status Light/moderate_drinker","SE_TYG_quantile2"],
                       Table_orginal["Drinking_status Light/moderate_drinker","Mean_TYG_quantile3"],Table_orginal["Drinking_status Light/moderate_drinker","SE_TYG_quantile3"],
                       Table_orginal["Drinking_status Light/moderate_drinker","Mean_TYG_quantile4"],Table_orginal["Drinking_status Light/moderate_drinker","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Heavier drinker",
                       Table_orginal["Drinking_status Heavier_drinker","Mean_all"],Table_orginal["Drinking_status Heavier_drinker","SE_all"],
                       Table_orginal["Drinking_status Heavier_drinker","Mean_TYG_quantile1"],Table_orginal["Drinking_status Heavier_drinker","SE_TYG_quantile1"],
                       Table_orginal["Drinking_status Heavier_drinker","Mean_TYG_quantile2"],Table_orginal["Drinking_status Heavier_drinker","SE_TYG_quantile2"],
                       Table_orginal["Drinking_status Heavier_drinker","Mean_TYG_quantile3"],Table_orginal["Drinking_status Heavier_drinker","SE_TYG_quantile3"],
                       Table_orginal["Drinking_status Heavier_drinker","Mean_TYG_quantile4"],Table_orginal["Drinking_status Heavier_drinker","SE_TYG_quantile4"],
                       "" ))
  #Physical status
  Table<-rbind(Table,c("Physical status, %","","","","","","","","","","",Table_orginal["Physical_status Inactive","P.value"]))
  Table<-rbind(Table,c("Inactive",
                       Table_orginal["Physical_status Inactive","Mean_all"],Table_orginal["Physical_status Inactive","SE_all"],
                       Table_orginal["Physical_status Inactive","Mean_TYG_quantile1"],Table_orginal["Physical_status Inactive","SE_TYG_quantile1"],
                       Table_orginal["Physical_status Inactive","Mean_TYG_quantile2"],Table_orginal["Physical_status Inactive","SE_TYG_quantile2"],
                       Table_orginal["Physical_status Inactive","Mean_TYG_quantile3"],Table_orginal["Physical_status Inactive","SE_TYG_quantile3"],
                       Table_orginal["Physical_status Inactive","Mean_TYG_quantile4"],Table_orginal["Physical_status Inactive","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Insufficient",
                       Table_orginal["Physical_status Insufficient","Mean_all"],Table_orginal["Physical_status Insufficient","SE_all"],
                       Table_orginal["Physical_status Insufficient","Mean_TYG_quantile1"],Table_orginal["Physical_status Insufficient","SE_TYG_quantile1"],
                       Table_orginal["Physical_status Insufficient","Mean_TYG_quantile2"],Table_orginal["Physical_status Insufficient","SE_TYG_quantile2"],
                       Table_orginal["Physical_status Insufficient","Mean_TYG_quantile3"],Table_orginal["Physical_status Insufficient","SE_TYG_quantile3"],
                       Table_orginal["Physical_status Insufficient","Mean_TYG_quantile4"],Table_orginal["Physical_status Insufficient","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Recommended",
                       Table_orginal["Physical_status Recommended","Mean_all"],Table_orginal["Physical_status Recommended","SE_all"],
                       Table_orginal["Physical_status Recommended","Mean_TYG_quantile1"],Table_orginal["Physical_status Recommended","SE_TYG_quantile1"],
                       Table_orginal["Physical_status Recommended","Mean_TYG_quantile2"],Table_orginal["Physical_status Recommended","SE_TYG_quantile2"],
                       Table_orginal["Physical_status Recommended","Mean_TYG_quantile3"],Table_orginal["Physical_status Recommended","SE_TYG_quantile3"],
                       Table_orginal["Physical_status Recommended","Mean_TYG_quantile4"],Table_orginal["Physical_status Recommended","SE_TYG_quantile4"],
                       "" ))
  
  #Healthy Eating Index
  #Table<-rbind(Table,c("Healthy eating index, %","","","","","","",Table_orginal["HEI Quintile 1","P.value"]))
  
  # Table<-rbind(Table,c("Quintile 1",
  #                      Table_orginal["HEI Quintile 1","Mean_all"],Table_orginal["HEI Quintile 1","SE_all"],
  #                      Table_orginal["HEI Quintile 1","Mean_noPD"],Table_orginal["HEI Quintile 1","SE_noPD"],
  #                      Table_orginal["HEI Quintile 1","Mean_PD"],Table_orginal["HEI Quintile 1","SE_PD"],
  #                      "" ))
  # Table<-rbind(Table,c("Quintile 2",
  #                      Table_orginal["HEI Quintile 2","Mean_all"],Table_orginal["HEI Quintile 2","SE_all"],
  #                      Table_orginal["HEI Quintile 2","Mean_noPD"],Table_orginal["HEI Quintile 2","SE_noPD"],
  #                      Table_orginal["HEI Quintile 2","Mean_PD"],Table_orginal["HEI Quintile 2","SE_PD"],
  #                      "" ))
  #
  # Table<-rbind(Table,c("Quintile 3",
  #                      Table_orginal["HEI Quintile 3","Mean_all"],Table_orginal["HEI Quintile 3","SE_all"],
  #                      Table_orginal["HEI Quintile 3","Mean_noPD"],Table_orginal["HEI Quintile 3","SE_noPD"],
  #                      Table_orginal["HEI Quintile 3","Mean_PD"],Table_orginal["HEI Quintile 3","SE_PD"],
  #                      "" ))
  # Table<-rbind(Table,c("Quintile 4",
  #                      Table_orginal["HEI Quintile 4","Mean_all"],Table_orginal["HEI Quintile 4","SE_all"],
  #                      Table_orginal["HEI Quintile 4","Mean_noPD"],Table_orginal["HEI Quintile 4","SE_noPD"],
  #                      Table_orginal["HEI Quintile 4","Mean_PD"],Table_orginal["HEI Quintile 4","SE_PD"],
  #                      "" ))
  # Table<-rbind(Table,c("Quintile 5",
  #                      Table_orginal["HEI Quintile 5","Mean_all"],Table_orginal["HEI Quintile 5","SE_all"],
  #                      Table_orginal["HEI Quintile 5","Mean_noPD"],Table_orginal["HEI Quintile 5","SE_noPD"],
  #                      Table_orginal["HEI Quintile 5","Mean_PD"],Table_orginal["HEI Quintile 5","SE_PD"],
  #                      "" ))
  
  #BMI
  Table<-rbind(Table,c("BMI, Mean",
                       Table_orginal["BMI Mean ± SE","Mean_all"],Table_orginal["BMI Mean ± SE","SE_all"],
                       Table_orginal["BMI Mean ± SE","Mean_TYG_quantile1"],Table_orginal["BMI Mean ± SE","SE_TYG_quantile1"],
                       Table_orginal["BMI Mean ± SE","Mean_TYG_quantile2"],Table_orginal["BMI Mean ± SE","SE_TYG_quantile2"],
                       Table_orginal["BMI Mean ± SE","Mean_TYG_quantile3"],Table_orginal["BMI Mean ± SE","SE_TYG_quantile3"],
                       Table_orginal["BMI Mean ± SE","Mean_TYG_quantile4"],Table_orginal["BMI Mean ± SE","SE_TYG_quantile4"],
                       Table_orginal["BMI Mean ± SE","P.value"] ))
  
  Table<-rbind(Table,c("BMI status (kg/m2), %","","","","","","","","","","",Table_orginal["BMI_status (0,25)","P.value"]))
  Table<-rbind(Table,c("<25",
                       Table_orginal["BMI_status (0,25)","Mean_all"],Table_orginal["BMI_status (0,25)","SE_all"],
                       Table_orginal["BMI_status (0,25)","Mean_TYG_quantile1"],Table_orginal["BMI_status (0,25)","SE_TYG_quantile1"],
                       Table_orginal["BMI_status (0,25)","Mean_TYG_quantile2"],Table_orginal["BMI_status (0,25)","SE_TYG_quantile2"],
                       Table_orginal["BMI_status (0,25)","Mean_TYG_quantile3"],Table_orginal["BMI_status (0,25)","SE_TYG_quantile3"],
                       Table_orginal["BMI_status (0,25)","Mean_TYG_quantile4"],Table_orginal["BMI_status (0,25)","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("[25.0 -30)",
                       Table_orginal["BMI_status [25.0-30)","Mean_all"],Table_orginal["BMI_status [25.0-30)","SE_all"],
                       Table_orginal["BMI_status [25.0-30)","Mean_TYG_quantile1"],Table_orginal["BMI_status [25.0-30)","SE_TYG_quantile1"],
                       Table_orginal["BMI_status [25.0-30)","Mean_TYG_quantile2"],Table_orginal["BMI_status [25.0-30)","SE_TYG_quantile2"],
                       Table_orginal["BMI_status [25.0-30)","Mean_TYG_quantile3"],Table_orginal["BMI_status [25.0-30)","SE_TYG_quantile3"],
                       Table_orginal["BMI_status [25.0-30)","Mean_TYG_quantile4"],Table_orginal["BMI_status [25.0-30)","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("≥30",
                       Table_orginal["BMI_status [30,inf)","Mean_all"],Table_orginal["BMI_status [30,inf)","SE_all"],
                       Table_orginal["BMI_status [30,inf)","Mean_TYG_quantile1"],Table_orginal["BMI_status [30,inf)","SE_TYG_quantile1"],
                       Table_orginal["BMI_status [30,inf)","Mean_TYG_quantile2"],Table_orginal["BMI_status [30,inf)","SE_TYG_quantile2"],
                       Table_orginal["BMI_status [30,inf)","Mean_TYG_quantile3"],Table_orginal["BMI_status [30,inf)","SE_TYG_quantile3"],
                       Table_orginal["BMI_status [30,inf)","Mean_TYG_quantile4"],Table_orginal["BMI_status [30,inf)","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("Comorbidities, %","","","","","","","","","","",""))
  #Hypertension
  Table<-rbind(Table,c("Hypertension",
                       Table_orginal["HTN_status YES","Mean_all"],Table_orginal["HTN_status YES","SE_all"],
                       Table_orginal["HTN_status YES","Mean_TYG_quantile1"],Table_orginal["HTN_status YES","SE_TYG_quantile1"],
                       Table_orginal["HTN_status YES","Mean_TYG_quantile2"],Table_orginal["HTN_status YES","SE_TYG_quantile2"],
                       Table_orginal["HTN_status YES","Mean_TYG_quantile3"],Table_orginal["HTN_status YES","SE_TYG_quantile3"],
                       Table_orginal["HTN_status YES","Mean_TYG_quantile4"],Table_orginal["HTN_status YES","SE_TYG_quantile4"],
                       Table_orginal["HTN_status YES","P.value"]))
  #Hyperlipidemia
  Table<-rbind(Table,c("Hyperlipidemia",
                       Table_orginal["HPL_status YES","Mean_all"],Table_orginal["HPL_status YES","SE_all"],
                       Table_orginal["HPL_status YES","Mean_TYG_quantile1"],Table_orginal["HPL_status YES","SE_TYG_quantile1"],
                       Table_orginal["HPL_status YES","Mean_TYG_quantile2"],Table_orginal["HPL_status YES","SE_TYG_quantile2"],
                       Table_orginal["HPL_status YES","Mean_TYG_quantile3"],Table_orginal["HPL_status YES","SE_TYG_quantile3"],
                       Table_orginal["HPL_status YES","Mean_TYG_quantile4"],Table_orginal["HPL_status YES","SE_TYG_quantile4"],
                       Table_orginal["HPL_status YES","P.value"]))
  #Diabetes mellitus
  Table<-rbind(Table,c("Diabetes mellitus",
                       Table_orginal["T2D_status YES","Mean_all"],Table_orginal["T2D_status YES","SE_all"],
                       Table_orginal["T2D_status YES","Mean_TYG_quantile1"],Table_orginal["T2D_status YES","SE_TYG_quantile1"],
                       Table_orginal["T2D_status YES","Mean_TYG_quantile2"],Table_orginal["T2D_status YES","SE_TYG_quantile2"],
                       Table_orginal["T2D_status YES","Mean_TYG_quantile3"],Table_orginal["T2D_status YES","SE_TYG_quantile3"],
                       Table_orginal["T2D_status YES","Mean_TYG_quantile4"],Table_orginal["T2D_status YES","SE_TYG_quantile4"],
                       Table_orginal["T2D_status YES","P.value"]))
  #Cohort
  Table<-rbind(Table,c("Cohort period, %","","","","","","","","","","",Table_orginal["Cohort NHANES_CON1","P.value"]))
  
  Table<-rbind(Table,c("NHANES III",
                       Table_orginal["Cohort NHANES_III","Mean_all"],Table_orginal["Cohort NHANES_III","SE_all"],
                       Table_orginal["Cohort NHANES_III","Mean_TYG_quantile1"],Table_orginal["Cohort NHANES_III","SE_TYG_quantile1"],
                       Table_orginal["Cohort NHANES_III","Mean_TYG_quantile2"],Table_orginal["Cohort NHANES_III","SE_TYG_quantile2"],
                       Table_orginal["Cohort NHANES_III","Mean_TYG_quantile3"],Table_orginal["Cohort NHANES_III","SE_TYG_quantile3"],
                       Table_orginal["Cohort NHANES_III","Mean_TYG_quantile4"],Table_orginal["Cohort NHANES_III","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("NHANES 1999-2004",
                       Table_orginal["Cohort NHANES_CON1","Mean_all"],Table_orginal["Cohort NHANES_CON1","SE_all"],
                       Table_orginal["Cohort NHANES_CON1","Mean_TYG_quantile1"],Table_orginal["Cohort NHANES_CON1","SE_TYG_quantile1"],
                       Table_orginal["Cohort NHANES_CON1","Mean_TYG_quantile2"],Table_orginal["Cohort NHANES_CON1","SE_TYG_quantile2"],
                       Table_orginal["Cohort NHANES_CON1","Mean_TYG_quantile3"],Table_orginal["Cohort NHANES_CON1","SE_TYG_quantile3"],
                       Table_orginal["Cohort NHANES_CON1","Mean_TYG_quantile4"],Table_orginal["Cohort NHANES_CON1","SE_TYG_quantile4"],
                       "" ))
  Table<-rbind(Table,c("NHANES 2009-2014",
                       Table_orginal["Cohort NHANES_CON2","Mean_all"],Table_orginal["Cohort NHANES_CON2","SE_all"],
                       Table_orginal["Cohort NHANES_CON2","Mean_TYG_quantile1"],Table_orginal["Cohort NHANES_CON2","SE_TYG_quantile1"],
                       Table_orginal["Cohort NHANES_CON2","Mean_TYG_quantile2"],Table_orginal["Cohort NHANES_CON2","SE_TYG_quantile2"],
                       Table_orginal["Cohort NHANES_CON2","Mean_TYG_quantile3"],Table_orginal["Cohort NHANES_CON2","SE_TYG_quantile3"],
                       Table_orginal["Cohort NHANES_CON2","Mean_TYG_quantile4"],Table_orginal["Cohort NHANES_CON2","SE_TYG_quantile4"],
                       "" ))
  
  Table
  write.table(Table,sep = ",",file ="I:/NHANES study/PD&TYG&MO/Result/Supplementary Table 16.csv" ,row.names =F,col.names =F )
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 19 Load data ####  
load(file="I:/NHANES study/PD&TYG&MO/Data/Complete_weighted.Rdata")
Interpolation_weighted<-Complete_weighted
table(Interpolation_weighted$TYG_quantile)
Interpolation_weighted$TyG_WC<-Interpolation_weighted$TyG_WC/100
Interpolation_weighted$TyG_BMI<-Interpolation_weighted$TyG_BMI/100
Interpolation_weighted$TYG_quantile<-as.character(Interpolation_weighted$TYG_quantile)
Interpolation_weighted$TYG_qua[Interpolation_weighted$TYG_quantile=="Quantile 1"]<-0
Interpolation_weighted$TYG_qua[Interpolation_weighted$TYG_quantile=="Quantile 2"]<-1
Interpolation_weighted$TYG_qua[Interpolation_weighted$TYG_quantile=="Quantile 3"]<-2
Interpolation_weighted$TYG_qua[Interpolation_weighted$TYG_quantile=="Quantile 4"]<-3
table(Interpolation_weighted$TyG_WC_quantile)
Interpolation_weighted$TyG_WC_quantile<-as.character(Interpolation_weighted$TyG_WC_quantile)
Interpolation_weighted$TyG_WC_qua[Interpolation_weighted$TyG_WC_quantile=="Quantile 1"]<-0
Interpolation_weighted$TyG_WC_qua[Interpolation_weighted$TyG_WC_quantile=="Quantile 2"]<-1
Interpolation_weighted$TyG_WC_qua[Interpolation_weighted$TyG_WC_quantile=="Quantile 3"]<-2
Interpolation_weighted$TyG_WC_qua[Interpolation_weighted$TyG_WC_quantile=="Quantile 4"]<-3
table(Interpolation_weighted$TyG_WHtR_quantile)
Interpolation_weighted$TyG_WHtR_quantile<-as.character(Interpolation_weighted$TyG_WHtR_quantile)
Interpolation_weighted$TyG_WHtR_qua[Interpolation_weighted$TyG_WHtR_quantile=="Quantile 1"]<-0
Interpolation_weighted$TyG_WHtR_qua[Interpolation_weighted$TyG_WHtR_quantile=="Quantile 2"]<-1
Interpolation_weighted$TyG_WHtR_qua[Interpolation_weighted$TyG_WHtR_quantile=="Quantile 3"]<-2
Interpolation_weighted$TyG_WHtR_qua[Interpolation_weighted$TyG_WHtR_quantile=="Quantile 4"]<-3
table(Interpolation_weighted$TyG_WHtR_qua)
Interpolation_weighted$TyG_BMI_quantile<-as.character(Interpolation_weighted$TyG_BMI_quantile)
Interpolation_weighted$TyG_BMI_qua[Interpolation_weighted$TyG_BMI_quantile=="Quantile 1"]<-0
Interpolation_weighted$TyG_BMI_qua[Interpolation_weighted$TyG_BMI_quantile=="Quantile 2"]<-1
Interpolation_weighted$TyG_BMI_qua[Interpolation_weighted$TyG_BMI_quantile=="Quantile 3"]<-2
Interpolation_weighted$TyG_BMI_qua[Interpolation_weighted$TyG_BMI_quantile=="Quantile 4"]<-3
table(Interpolation_weighted$TyG_BMI_qua)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 19 TYG  ####  
{ #* all model #####
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
  result_All<-result4
}
{ #* CVD model #####
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
  result_CVD<-result4
}
{ #* Cancer model #####
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
  result_Cancer<-result4
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
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  Table_TyG<-as.data.frame(result.cause)
  Table_TyG= data.frame(lapply(Table_TyG, as.character), stringsAsFactors=FALSE)
}
Table_TyG
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 20 TYG WC ####  
{ #* all model #####
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
  result_All<-result4
}
{ #* CVD model #####
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
  result_CVD<-result4
}

{ #* Cancer model #####
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
  result_Cancer<-result4
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
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  Table_TyG_WC<-as.data.frame(result.cause)
  Table_TyG_WC= data.frame(lapply(Table_TyG_WC, as.character), stringsAsFactors=FALSE)
}
Table_TyG_WC
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 21 TYG WHtR ####  
{ #* all model #####
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
  result_All<-result4
  result_All
}
{ #* CVD model #####
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
  result_CVD<-result4
}

{ #* Cancer model #####
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
  result_Cancer<-result4
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
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  Table_TyG_WHtR<-as.data.frame(result.cause)
  Table_TyG_WHtR = data.frame(lapply(Table_TyG_WHtR, as.character), stringsAsFactors=FALSE)
}
Table_TyG_WHtR

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 22 TYG BMI ####  
{ #* all model #####
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
  result_All<-result4
}

{ #* CVD model #####
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
  result_CVD<-result4
}
{ #* Cancer model #####
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
  result_Cancer<-result4
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
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  Table_TyG_BMI<-as.data.frame(result.cause)
  Table_TyG_BMI= data.frame(lapply(Table_TyG_BMI, as.character), stringsAsFactors=FALSE)
}
Table_TyG_BMI
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 23 TYG quantile ####    

{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TYG_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}
{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TYG_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
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
  result.Cancer<-result4
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
  Table_TyG_quantile<-result.cause
  Table_TyG_quantile
}

{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TYG_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}
{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TYG_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
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
  result.Cancer<-result4
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
  Table_TyG_qua<-result.cause
  Table_TyG_qua
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 24 TYG WC quantile ####    
{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_WC_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}
{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_WC_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  P<-model4_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
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
  result.Cancer<-result4
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
  Table_TyG_WC_quantile<-result.cause
  Table_TyG_WC_quantile
}
{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_WC_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}
{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_WC_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
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
  result.Cancer<-result4
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
  Table_TyG_WC_qua<-result.cause
  Table_TyG_WC_qua
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 25 TYG WHtR quantile  ####    
{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_WHtR_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}
{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_WHtR_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
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
  result.Cancer<-result4
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
  Table_TyG_WHtR_quantile<-result.cause
  Table_TyG_WHtR_quantile
}
{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_WHtR_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}
{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_WHtR_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  P<-model4_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
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
  result.Cancer<-result4
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
  Table_TyG_WHtR_qua<-result.cause
  Table_TyG_WHtR_qua
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 26 TYG BMI quantile  #### 
{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_BMI_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}
{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_BMI_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
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
  result.Cancer<-result4
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
  Table_TyG_BMI_quantile<-result.cause
}

{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_BMI_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}

{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_BMI_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
  
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
  result.Cancer<-result4
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
  Table_TyG_BMI_qua<-result.cause
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 26 Total (Table S17) #### 
Table<-c("Exposure","All cause","","CVD-related","","Cancer-related","")
Table<-rbind(Table,c("","HR (95% CI)*","P†", "HR (95% CI)","P","HR (95% CI)","P"))
Table<-rbind(Table,c("TyG index","", "", "", "", "",""))
Table<-rbind(Table,c("Per 1-index‡",Table_TyG[1,7],Table_TyG[1,4], 
                     Table_TyG[2,7],Table_TyG[2,4],
                     Table_TyG[3,7],Table_TyG[3,4]))
Table<-rbind(Table,c("Quantile 1", 
                     "1.00 [Reference]","",
                     "1.00 [Reference]","",
                     "1.00 [Reference]",""))
Table<-rbind(Table,c("Quantile 2", 
                     Table_TyG_quantile[1,7],"",
                     Table_TyG_quantile[2,7],"",
                     Table_TyG_quantile[3,7],""))
Table<-rbind(Table,c("Quantile 3", 
                     Table_TyG_quantile[4,7],"",
                     Table_TyG_quantile[5,7],"",
                     Table_TyG_quantile[6,7],""))
Table<-rbind(Table,c("Quantile 4", 
                     Table_TyG_quantile[7,7],Table_TyG_qua[1,4],
                     Table_TyG_quantile[8,7],Table_TyG_qua[2,4],
                     Table_TyG_quantile[9,7],Table_TyG_qua[3,4]))

Table<-rbind(Table,c("TyG-WC index","", "", "", "", "",""))
Table<-rbind(Table,c("Per 100-index",Table_TyG_WC[1,7],Table_TyG_WC[1,4], 
                     Table_TyG_WC[2,7],Table_TyG_WC[2,4],
                     Table_TyG_WC[3,7],Table_TyG_WC[3,4]))
Table<-rbind(Table,c("Quantile 1", 
                     "1.00 [Reference]","",
                     "1.00 [Reference]","",
                     "1.00 [Reference]",""))
Table<-rbind(Table,c("Quantile 2", 
                     Table_TyG_WC_quantile[1,7],"",
                     Table_TyG_WC_quantile[2,7],"",
                     Table_TyG_WC_quantile[3,7],""))
Table<-rbind(Table,c("Quantile 3", 
                     Table_TyG_WC_quantile[4,7],"",
                     Table_TyG_WC_quantile[5,7],"",
                     Table_TyG_WC_quantile[6,7],""))
Table<-rbind(Table,c("Quantile 4", 
                     Table_TyG_WC_quantile[7,7],Table_TyG_WC_qua[1,4],
                     Table_TyG_WC_quantile[8,7],Table_TyG_WC_qua[2,4],
                     Table_TyG_WC_quantile[9,7],Table_TyG_WC_qua[3,4]))

Table<-rbind(Table,c("TyG-WHtR index","", "", "", "", "",""))
Table<-rbind(Table,c("Per 1-index",Table_TyG_WHtR[1,7],Table_TyG_WHtR[1,4], 
                     Table_TyG_WHtR[2,7],Table_TyG_WHtR[2,4],
                     Table_TyG_WHtR[3,7],Table_TyG_WHtR[3,4]))
Table<-rbind(Table,c("Quantile 1", 
                     "1.00 [Reference]","",
                     "1.00 [Reference]","",
                     "1.00 [Reference]",""))
Table<-rbind(Table,c("Quantile 2", 
                     Table_TyG_WHtR_quantile[1,7],"",
                     Table_TyG_WHtR_quantile[2,7],"",
                     Table_TyG_WHtR_quantile[3,7],""))
Table<-rbind(Table,c("Quantile 3", 
                     Table_TyG_WHtR_quantile[4,7],"",
                     Table_TyG_WHtR_quantile[5,7],"",
                     Table_TyG_WHtR_quantile[6,7],""))
Table<-rbind(Table,c("Quantile 4", 
                     Table_TyG_WHtR_quantile[7,7],Table_TyG_WHtR_qua[1,4],
                     Table_TyG_WHtR_quantile[8,7],Table_TyG_WHtR_qua[2,4],
                     Table_TyG_WHtR_quantile[9,7],Table_TyG_WHtR_qua[3,4]))

Table<-rbind(Table,c("TyG-BMI index","", "", "", "", "",""))
Table<-rbind(Table,c("Per 100-index",Table_TyG_BMI[1,7],Table_TyG_BMI[1,4], 
                     Table_TyG_BMI[2,7],Table_TyG_BMI[2,4],
                     Table_TyG_BMI[3,7],Table_TyG_BMI[3,4]))
Table<-rbind(Table,c("Quantile 1", 
                     "1.00 [Reference]","",
                     "1.00 [Reference]","",
                     "1.00 [Reference]",""))
Table<-rbind(Table,c("Quantile 2", 
                     Table_TyG_BMI_quantile[1,7],"",
                     Table_TyG_BMI_quantile[2,7],"",
                     Table_TyG_BMI_quantile[3,7],""))
Table<-rbind(Table,c("Quantile 3", 
                     Table_TyG_BMI_quantile[4,7],"",
                     Table_TyG_BMI_quantile[5,7],"",
                     Table_TyG_BMI_quantile[6,7],""))
Table<-rbind(Table,c("Quantile 4", 
                     Table_TyG_BMI_quantile[7,7],Table_TyG_BMI_qua[1,4],
                     Table_TyG_BMI_quantile[8,7],Table_TyG_BMI_qua[2,4],
                     Table_TyG_BMI_quantile[9,7],Table_TyG_BMI_qua[3,4]))

Table<-as.data.frame(Table)
Table= data.frame(lapply(Table, as.character), stringsAsFactors=FALSE)
Table
write.table(Table,sep = ",",file ="I:/NHANES study/PD&TYG&MO/Result/Supplementary Table 17.csv",row.names =F,col.names =F )


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# ++++++++++++omitted cancer and CVD patients+++++++++++ ####
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 19 Load data ####  
load(file="I:/NHANES study/PD&TYG&MO/Data/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
Interpolation_weighted<-subset(Interpolation_weighted,Cancer_status!="YES"&CVD_status!="YES")
Interpolation_weighted$TyG_WC<-Interpolation_weighted$TyG_WC/100
Interpolation_weighted$TyG_BMI<-Interpolation_weighted$TyG_BMI/100
Interpolation_weighted$TYG_quantile<-as.character(Interpolation_weighted$TYG_quantile)
Interpolation_weighted$TYG_qua[Interpolation_weighted$TYG_quantile=="Quantile 1"]<-0
Interpolation_weighted$TYG_qua[Interpolation_weighted$TYG_quantile=="Quantile 2"]<-1
Interpolation_weighted$TYG_qua[Interpolation_weighted$TYG_quantile=="Quantile 3"]<-2
Interpolation_weighted$TYG_qua[Interpolation_weighted$TYG_quantile=="Quantile 4"]<-3
table(Interpolation_weighted$TyG_WC_quantile)
Interpolation_weighted$TyG_WC_quantile<-as.character(Interpolation_weighted$TyG_WC_quantile)
Interpolation_weighted$TyG_WC_qua[Interpolation_weighted$TyG_WC_quantile=="Quantile 1"]<-0
Interpolation_weighted$TyG_WC_qua[Interpolation_weighted$TyG_WC_quantile=="Quantile 2"]<-1
Interpolation_weighted$TyG_WC_qua[Interpolation_weighted$TyG_WC_quantile=="Quantile 3"]<-2
Interpolation_weighted$TyG_WC_qua[Interpolation_weighted$TyG_WC_quantile=="Quantile 4"]<-3
table(Interpolation_weighted$TyG_WHtR_quantile)
Interpolation_weighted$TyG_WHtR_quantile<-as.character(Interpolation_weighted$TyG_WHtR_quantile)
Interpolation_weighted$TyG_WHtR_qua[Interpolation_weighted$TyG_WHtR_quantile=="Quantile 1"]<-0
Interpolation_weighted$TyG_WHtR_qua[Interpolation_weighted$TyG_WHtR_quantile=="Quantile 2"]<-1
Interpolation_weighted$TyG_WHtR_qua[Interpolation_weighted$TyG_WHtR_quantile=="Quantile 3"]<-2
Interpolation_weighted$TyG_WHtR_qua[Interpolation_weighted$TyG_WHtR_quantile=="Quantile 4"]<-3
table(Interpolation_weighted$TyG_WHtR_qua)
Interpolation_weighted$TyG_BMI_quantile<-as.character(Interpolation_weighted$TyG_BMI_quantile)
Interpolation_weighted$TyG_BMI_qua[Interpolation_weighted$TyG_BMI_quantile=="Quantile 1"]<-0
Interpolation_weighted$TyG_BMI_qua[Interpolation_weighted$TyG_BMI_quantile=="Quantile 2"]<-1
Interpolation_weighted$TyG_BMI_qua[Interpolation_weighted$TyG_BMI_quantile=="Quantile 3"]<-2
Interpolation_weighted$TyG_BMI_qua[Interpolation_weighted$TyG_BMI_quantile=="Quantile 4"]<-3
table(Interpolation_weighted$TyG_BMI_qua)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 19 TYG  ####  
{ #* all model #####
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
  result_All<-result4
}
{ #* CVD model #####
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
  result_CVD<-result4
}
{ #* Cancer model #####
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
  result_Cancer<-result4
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
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  Table_TyG<-as.data.frame(result.cause)
  Table_TyG= data.frame(lapply(Table_TyG, as.character), stringsAsFactors=FALSE)
}
Table_TyG
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 20 TYG WC ####  
{ #* all model #####
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
  result_All<-result4
}
{ #* CVD model #####
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
  result_CVD<-result4
}

{ #* Cancer model #####
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
  result_Cancer<-result4
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
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  Table_TyG_WC<-as.data.frame(result.cause)
  Table_TyG_WC= data.frame(lapply(Table_TyG_WC, as.character), stringsAsFactors=FALSE)
}
Table_TyG_WC
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 21 TYG WHtR ####  
{ #* all model #####
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
  result_All<-result4
  result_All
}
{ #* CVD model #####
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
  result_CVD<-result4
}

{ #* Cancer model #####
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
  result_Cancer<-result4
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
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  Table_TyG_WHtR<-as.data.frame(result.cause)
  Table_TyG_WHtR = data.frame(lapply(Table_TyG_WHtR, as.character), stringsAsFactors=FALSE)
}
Table_TyG_WHtR

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 22 TYG BMI ####  
{ #* all model #####
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
  result_All<-result4
}

{ #* CVD model #####
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
  result_CVD<-result4
}
{ #* Cancer model #####
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
  result_Cancer<-result4
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
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  Table_TyG_BMI<-as.data.frame(result.cause)
  Table_TyG_BMI= data.frame(lapply(Table_TyG_BMI, as.character), stringsAsFactors=FALSE)
}
Table_TyG_BMI
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 23 TYG quantile ####    

{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TYG_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}
{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TYG_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
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
  result.Cancer<-result4
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
  Table_TyG_quantile<-result.cause
  Table_TyG_quantile
}

{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TYG_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}
{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TYG_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
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
  result.Cancer<-result4
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
  Table_TyG_qua<-result.cause
  Table_TyG_qua
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 24 TYG WC quantile ####    
{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_WC_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}
{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_WC_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  P<-model4_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
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
  result.Cancer<-result4
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
  Table_TyG_WC_quantile<-result.cause
  Table_TyG_WC_quantile
}
{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_WC_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}
{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_WC_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
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
  result.Cancer<-result4
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
  Table_TyG_WC_qua<-result.cause
  Table_TyG_WC_qua
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 25 TYG WHtR quantile  ####    
{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_WHtR_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}
{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_WHtR_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
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
  result.Cancer<-result4
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
  Table_TyG_WHtR_quantile<-result.cause
  Table_TyG_WHtR_quantile
}
{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_WHtR_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}
{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_WHtR_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  P<-model4_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
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
  result.Cancer<-result4
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
  Table_TyG_WHtR_qua<-result.cause
  Table_TyG_WHtR_qua
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 26 TYG BMI quantile  #### 
{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_BMI_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}
{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_BMI_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
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
  result.Cancer<-result4
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
  Table_TyG_BMI_quantile<-result.cause
}

{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_BMI_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}

{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_BMI_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
  
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
  result.Cancer<-result4
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
  Table_TyG_BMI_qua<-result.cause
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 26 Total (Table S18) #### 
Table<-c("Exposure","All cause","","CVD-related","","Cancer-related","")
Table<-rbind(Table,c("","HR (95% CI)*","P†", "HR (95% CI)","P","HR (95% CI)","P"))
Table<-rbind(Table,c("TyG index","", "", "", "", "",""))
Table<-rbind(Table,c("Per 1-index‡",Table_TyG[1,7],Table_TyG[1,4], 
                     Table_TyG[2,7],Table_TyG[2,4],
                     Table_TyG[3,7],Table_TyG[3,4]))
Table<-rbind(Table,c("Quantile 1", 
                     "1.00 [Reference]","",
                     "1.00 [Reference]","",
                     "1.00 [Reference]",""))
Table<-rbind(Table,c("Quantile 2", 
                     Table_TyG_quantile[1,7],"",
                     Table_TyG_quantile[2,7],"",
                     Table_TyG_quantile[3,7],""))
Table<-rbind(Table,c("Quantile 3", 
                     Table_TyG_quantile[4,7],"",
                     Table_TyG_quantile[5,7],"",
                     Table_TyG_quantile[6,7],""))
Table<-rbind(Table,c("Quantile 4", 
                     Table_TyG_quantile[7,7],Table_TyG_qua[1,4],
                     Table_TyG_quantile[8,7],Table_TyG_qua[2,4],
                     Table_TyG_quantile[9,7],Table_TyG_qua[3,4]))

Table<-rbind(Table,c("TyG-WC index","", "", "", "", "",""))
Table<-rbind(Table,c("Per 100-index",Table_TyG_WC[1,7],Table_TyG_WC[1,4], 
                     Table_TyG_WC[2,7],Table_TyG_WC[2,4],
                     Table_TyG_WC[3,7],Table_TyG_WC[3,4]))
Table<-rbind(Table,c("Quantile 1", 
                     "1.00 [Reference]","",
                     "1.00 [Reference]","",
                     "1.00 [Reference]",""))
Table<-rbind(Table,c("Quantile 2", 
                     Table_TyG_WC_quantile[1,7],"",
                     Table_TyG_WC_quantile[2,7],"",
                     Table_TyG_WC_quantile[3,7],""))
Table<-rbind(Table,c("Quantile 3", 
                     Table_TyG_WC_quantile[4,7],"",
                     Table_TyG_WC_quantile[5,7],"",
                     Table_TyG_WC_quantile[6,7],""))
Table<-rbind(Table,c("Quantile 4", 
                     Table_TyG_WC_quantile[7,7],Table_TyG_WC_qua[1,4],
                     Table_TyG_WC_quantile[8,7],Table_TyG_WC_qua[2,4],
                     Table_TyG_WC_quantile[9,7],Table_TyG_WC_qua[3,4]))

Table<-rbind(Table,c("TyG-WHtR index","", "", "", "", "",""))
Table<-rbind(Table,c("Per 1-index",Table_TyG_WHtR[1,7],Table_TyG_WHtR[1,4], 
                     Table_TyG_WHtR[2,7],Table_TyG_WHtR[2,4],
                     Table_TyG_WHtR[3,7],Table_TyG_WHtR[3,4]))
Table<-rbind(Table,c("Quantile 1", 
                     "1.00 [Reference]","",
                     "1.00 [Reference]","",
                     "1.00 [Reference]",""))
Table<-rbind(Table,c("Quantile 2", 
                     Table_TyG_WHtR_quantile[1,7],"",
                     Table_TyG_WHtR_quantile[2,7],"",
                     Table_TyG_WHtR_quantile[3,7],""))
Table<-rbind(Table,c("Quantile 3", 
                     Table_TyG_WHtR_quantile[4,7],"",
                     Table_TyG_WHtR_quantile[5,7],"",
                     Table_TyG_WHtR_quantile[6,7],""))
Table<-rbind(Table,c("Quantile 4", 
                     Table_TyG_WHtR_quantile[7,7],Table_TyG_WHtR_qua[1,4],
                     Table_TyG_WHtR_quantile[8,7],Table_TyG_WHtR_qua[2,4],
                     Table_TyG_WHtR_quantile[9,7],Table_TyG_WHtR_qua[3,4]))

Table<-rbind(Table,c("TyG-BMI index","", "", "", "", "",""))
Table<-rbind(Table,c("Per 100-index",Table_TyG_BMI[1,7],Table_TyG_BMI[1,4], 
                     Table_TyG_BMI[2,7],Table_TyG_BMI[2,4],
                     Table_TyG_BMI[3,7],Table_TyG_BMI[3,4]))
Table<-rbind(Table,c("Quantile 1", 
                     "1.00 [Reference]","",
                     "1.00 [Reference]","",
                     "1.00 [Reference]",""))
Table<-rbind(Table,c("Quantile 2", 
                     Table_TyG_BMI_quantile[1,7],"",
                     Table_TyG_BMI_quantile[2,7],"",
                     Table_TyG_BMI_quantile[3,7],""))
Table<-rbind(Table,c("Quantile 3", 
                     Table_TyG_BMI_quantile[4,7],"",
                     Table_TyG_BMI_quantile[5,7],"",
                     Table_TyG_BMI_quantile[6,7],""))
Table<-rbind(Table,c("Quantile 4", 
                     Table_TyG_BMI_quantile[7,7],Table_TyG_BMI_qua[1,4],
                     Table_TyG_BMI_quantile[8,7],Table_TyG_BMI_qua[2,4],
                     Table_TyG_BMI_quantile[9,7],Table_TyG_BMI_qua[3,4]))

Table<-as.data.frame(Table)
Table= data.frame(lapply(Table, as.character), stringsAsFactors=FALSE)
Table
write.table(Table,sep = ",",file ="I:/NHANES study/PD&TYG&MO/Result/Supplementary Table 18.csv",row.names =F,col.names =F )



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# +++++++++++++++++++++++Peryear>=2+++++++++++++++++++++ ####
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 19 Load data ####  
load(file="I:/NHANES study/PD&TYG&MO/Data/Interpolation_weighted.Rdata")
colnames(Interpolation_weighted)
Interpolation_weighted<-subset(Interpolation_weighted,peryear>=2)
Interpolation_weighted$TyG_WC<-Interpolation_weighted$TyG_WC/100
Interpolation_weighted$TyG_BMI<-Interpolation_weighted$TyG_BMI/100
Interpolation_weighted$TYG_quantile<-as.character(Interpolation_weighted$TYG_quantile)
Interpolation_weighted$TYG_qua[Interpolation_weighted$TYG_quantile=="Quantile 1"]<-0
Interpolation_weighted$TYG_qua[Interpolation_weighted$TYG_quantile=="Quantile 2"]<-1
Interpolation_weighted$TYG_qua[Interpolation_weighted$TYG_quantile=="Quantile 3"]<-2
Interpolation_weighted$TYG_qua[Interpolation_weighted$TYG_quantile=="Quantile 4"]<-3
table(Interpolation_weighted$TyG_WC_quantile)
Interpolation_weighted$TyG_WC_quantile<-as.character(Interpolation_weighted$TyG_WC_quantile)
Interpolation_weighted$TyG_WC_qua[Interpolation_weighted$TyG_WC_quantile=="Quantile 1"]<-0
Interpolation_weighted$TyG_WC_qua[Interpolation_weighted$TyG_WC_quantile=="Quantile 2"]<-1
Interpolation_weighted$TyG_WC_qua[Interpolation_weighted$TyG_WC_quantile=="Quantile 3"]<-2
Interpolation_weighted$TyG_WC_qua[Interpolation_weighted$TyG_WC_quantile=="Quantile 4"]<-3
table(Interpolation_weighted$TyG_WHtR_quantile)
Interpolation_weighted$TyG_WHtR_quantile<-as.character(Interpolation_weighted$TyG_WHtR_quantile)
Interpolation_weighted$TyG_WHtR_qua[Interpolation_weighted$TyG_WHtR_quantile=="Quantile 1"]<-0
Interpolation_weighted$TyG_WHtR_qua[Interpolation_weighted$TyG_WHtR_quantile=="Quantile 2"]<-1
Interpolation_weighted$TyG_WHtR_qua[Interpolation_weighted$TyG_WHtR_quantile=="Quantile 3"]<-2
Interpolation_weighted$TyG_WHtR_qua[Interpolation_weighted$TyG_WHtR_quantile=="Quantile 4"]<-3
table(Interpolation_weighted$TyG_WHtR_qua)
Interpolation_weighted$TyG_BMI_quantile<-as.character(Interpolation_weighted$TyG_BMI_quantile)
Interpolation_weighted$TyG_BMI_qua[Interpolation_weighted$TyG_BMI_quantile=="Quantile 1"]<-0
Interpolation_weighted$TyG_BMI_qua[Interpolation_weighted$TyG_BMI_quantile=="Quantile 2"]<-1
Interpolation_weighted$TyG_BMI_qua[Interpolation_weighted$TyG_BMI_quantile=="Quantile 3"]<-2
Interpolation_weighted$TyG_BMI_qua[Interpolation_weighted$TyG_BMI_quantile=="Quantile 4"]<-3
table(Interpolation_weighted$TyG_BMI_qua)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =Interpolation_weighted,strata=~sdmvstra,weights = ~ weight)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 19 TYG  ####  
{ #* all model #####
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
  result_All<-result4
}
{ #* CVD model #####
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
  result_CVD<-result4
}
{ #* Cancer model #####
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
  result_Cancer<-result4
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
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  Table_TyG<-as.data.frame(result.cause)
  Table_TyG= data.frame(lapply(Table_TyG, as.character), stringsAsFactors=FALSE)
}
Table_TyG
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 20 TYG WC ####  
{ #* all model #####
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
  result_All<-result4
}
{ #* CVD model #####
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
  result_CVD<-result4
}

{ #* Cancer model #####
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
  result_Cancer<-result4
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
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  Table_TyG_WC<-as.data.frame(result.cause)
  Table_TyG_WC= data.frame(lapply(Table_TyG_WC, as.character), stringsAsFactors=FALSE)
}
Table_TyG_WC
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 21 TYG WHtR ####  
{ #* all model #####
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
  result_All<-result4
  result_All
}
{ #* CVD model #####
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
  result_CVD<-result4
}

{ #* Cancer model #####
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
  result_Cancer<-result4
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
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  Table_TyG_WHtR<-as.data.frame(result.cause)
  Table_TyG_WHtR = data.frame(lapply(Table_TyG_WHtR, as.character), stringsAsFactors=FALSE)
}
Table_TyG_WHtR

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 22 TYG BMI ####  
{ #* all model #####
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
  result_All<-result4
}

{ #* CVD model #####
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
  result_CVD<-result4
}
{ #* Cancer model #####
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
  result_Cancer<-result4
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
  result.cause$HR_CI<-paste0(result.cause$HR," (",
                             result.cause$lower..95,", ",
                             result.cause$upper..95,")")
  Table_TyG_BMI<-as.data.frame(result.cause)
  Table_TyG_BMI= data.frame(lapply(Table_TyG_BMI, as.character), stringsAsFactors=FALSE)
}
Table_TyG_BMI
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 23 TYG quantile ####    

{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TYG_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}
{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TYG_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
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
  result.Cancer<-result4
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
  Table_TyG_quantile<-result.cause
  Table_TyG_quantile
}

{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TYG_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}
{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TYG_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
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
  result.Cancer<-result4
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
  Table_TyG_qua<-result.cause
  Table_TyG_qua
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 24 TYG WC quantile ####    
{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_WC_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}
{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_WC_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  P<-model4_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
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
  result.Cancer<-result4
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
  Table_TyG_WC_quantile<-result.cause
  Table_TyG_WC_quantile
}
{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_WC_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}
{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_WC_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
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
  result.Cancer<-result4
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
  Table_TyG_WC_qua<-result.cause
  Table_TyG_WC_qua
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 25 TYG WHtR quantile  ####    
{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_WHtR_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}
{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_WHtR_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
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
  result.Cancer<-result4
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
  Table_TyG_WHtR_quantile<-result.cause
  Table_TyG_WHtR_quantile
}
{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_WHtR_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}
{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_WHtR_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  P<-model4_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
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
  result.Cancer<-result4
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
  Table_TyG_WHtR_qua<-result.cause
  Table_TyG_WHtR_qua
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 26 TYG BMI quantile  #### 
{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_BMI_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}
{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_BMI_quantile+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1:3,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1:3,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[,1],'lower .95'=HR[,2],'upper .95'=HR[,3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
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
  result.Cancer<-result4
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
  Table_TyG_BMI_quantile<-result.cause
}

{ #* all model #####
  model4_all<-svycoxph(Surv(peryear, MORT_stat==1) ~
                         TyG_BMI_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort,design =rhcSvy)
  model4_all_result<-summary(model4_all)
  
  P<-model4_all_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="All cause")
  result.all<-result4
}

{ #* CVD model #####
  model4_CVD<-svycoxph(Surv(peryear, CVD_MORT_stat==1) ~
                         TyG_BMI_qua+Age_status+Sex+Race_ethnicity+SES+Marital_status+
                         Smoking_status+Drinking_status+BMI_status+Physical_status+
                         HTN_status+T2D_status+HPL_status+Cohort, design =rhcSvy)
  model4_CVD_result<-summary(model4_CVD)
  
  P<-model4_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
  HR<-model4_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")]
  result4 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                        'P value' =P,'model'="model4",'status'="CVD cause")
  result.CVD<-result4
}
{ #* Cancer model #####
  
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
  result.Cancer<-result4
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
  Table_TyG_BMI_qua<-result.cause
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 26 Total (Table S19) #### 
Table<-c("Exposure","All cause","","CVD-related","","Cancer-related","")
Table<-rbind(Table,c("","HR (95% CI)*","P†", "HR (95% CI)","P","HR (95% CI)","P"))
Table<-rbind(Table,c("TyG index","", "", "", "", "",""))
Table<-rbind(Table,c("Per 1-index‡",Table_TyG[1,7],Table_TyG[1,4], 
                     Table_TyG[2,7],Table_TyG[2,4],
                     Table_TyG[3,7],Table_TyG[3,4]))
Table<-rbind(Table,c("Quantile 1", 
                     "1.00 [Reference]","",
                     "1.00 [Reference]","",
                     "1.00 [Reference]",""))
Table<-rbind(Table,c("Quantile 2", 
                     Table_TyG_quantile[1,7],"",
                     Table_TyG_quantile[2,7],"",
                     Table_TyG_quantile[3,7],""))
Table<-rbind(Table,c("Quantile 3", 
                     Table_TyG_quantile[4,7],"",
                     Table_TyG_quantile[5,7],"",
                     Table_TyG_quantile[6,7],""))
Table<-rbind(Table,c("Quantile 4", 
                     Table_TyG_quantile[7,7],Table_TyG_qua[1,4],
                     Table_TyG_quantile[8,7],Table_TyG_qua[2,4],
                     Table_TyG_quantile[9,7],Table_TyG_qua[3,4]))

Table<-rbind(Table,c("TyG-WC index","", "", "", "", "",""))
Table<-rbind(Table,c("Per 100-index",Table_TyG_WC[1,7],Table_TyG_WC[1,4], 
                     Table_TyG_WC[2,7],Table_TyG_WC[2,4],
                     Table_TyG_WC[3,7],Table_TyG_WC[3,4]))
Table<-rbind(Table,c("Quantile 1", 
                     "1.00 [Reference]","",
                     "1.00 [Reference]","",
                     "1.00 [Reference]",""))
Table<-rbind(Table,c("Quantile 2", 
                     Table_TyG_WC_quantile[1,7],"",
                     Table_TyG_WC_quantile[2,7],"",
                     Table_TyG_WC_quantile[3,7],""))
Table<-rbind(Table,c("Quantile 3", 
                     Table_TyG_WC_quantile[4,7],"",
                     Table_TyG_WC_quantile[5,7],"",
                     Table_TyG_WC_quantile[6,7],""))
Table<-rbind(Table,c("Quantile 4", 
                     Table_TyG_WC_quantile[7,7],Table_TyG_WC_qua[1,4],
                     Table_TyG_WC_quantile[8,7],Table_TyG_WC_qua[2,4],
                     Table_TyG_WC_quantile[9,7],Table_TyG_WC_qua[3,4]))

Table<-rbind(Table,c("TyG-WHtR index","", "", "", "", "",""))
Table<-rbind(Table,c("Per 1-index",Table_TyG_WHtR[1,7],Table_TyG_WHtR[1,4], 
                     Table_TyG_WHtR[2,7],Table_TyG_WHtR[2,4],
                     Table_TyG_WHtR[3,7],Table_TyG_WHtR[3,4]))
Table<-rbind(Table,c("Quantile 1", 
                     "1.00 [Reference]","",
                     "1.00 [Reference]","",
                     "1.00 [Reference]",""))
Table<-rbind(Table,c("Quantile 2", 
                     Table_TyG_WHtR_quantile[1,7],"",
                     Table_TyG_WHtR_quantile[2,7],"",
                     Table_TyG_WHtR_quantile[3,7],""))
Table<-rbind(Table,c("Quantile 3", 
                     Table_TyG_WHtR_quantile[4,7],"",
                     Table_TyG_WHtR_quantile[5,7],"",
                     Table_TyG_WHtR_quantile[6,7],""))
Table<-rbind(Table,c("Quantile 4", 
                     Table_TyG_WHtR_quantile[7,7],Table_TyG_WHtR_qua[1,4],
                     Table_TyG_WHtR_quantile[8,7],Table_TyG_WHtR_qua[2,4],
                     Table_TyG_WHtR_quantile[9,7],Table_TyG_WHtR_qua[3,4]))

Table<-rbind(Table,c("TyG-BMI index","", "", "", "", "",""))
Table<-rbind(Table,c("Per 100-index",Table_TyG_BMI[1,7],Table_TyG_BMI[1,4], 
                     Table_TyG_BMI[2,7],Table_TyG_BMI[2,4],
                     Table_TyG_BMI[3,7],Table_TyG_BMI[3,4]))
Table<-rbind(Table,c("Quantile 1", 
                     "1.00 [Reference]","",
                     "1.00 [Reference]","",
                     "1.00 [Reference]",""))
Table<-rbind(Table,c("Quantile 2", 
                     Table_TyG_BMI_quantile[1,7],"",
                     Table_TyG_BMI_quantile[2,7],"",
                     Table_TyG_BMI_quantile[3,7],""))
Table<-rbind(Table,c("Quantile 3", 
                     Table_TyG_BMI_quantile[4,7],"",
                     Table_TyG_BMI_quantile[5,7],"",
                     Table_TyG_BMI_quantile[6,7],""))
Table<-rbind(Table,c("Quantile 4", 
                     Table_TyG_BMI_quantile[7,7],Table_TyG_BMI_qua[1,4],
                     Table_TyG_BMI_quantile[8,7],Table_TyG_BMI_qua[2,4],
                     Table_TyG_BMI_quantile[9,7],Table_TyG_BMI_qua[3,4]))

Table<-as.data.frame(Table)
Table= data.frame(lapply(Table, as.character), stringsAsFactors=FALSE)
Table
write.table(Table,sep = ",",file ="I:/NHANES study/PD&TYG&MO/Result/Supplementary Table 19.csv",row.names =F,col.names =F )

