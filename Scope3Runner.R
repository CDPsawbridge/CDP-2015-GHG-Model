# S1templ$sec_hybrid.f<-"Food & Staples Retailing"
# 
# S3Runner(S1templ,58918.28,NA,336000,"Europe")
# # use the above as a test to check you get the same as account numer 2906, which is CASINO
#Run sequential regressions by revenue
require(plyr)
require(ggplot2)
require(stringr)
require(stringi)
require(reshape2)

S3Runner<-function(EnvSam, TotalRevenue,Capex,Employees,cty){
load('S3modelsBest.R')
load('S3modelsind.R')
load('S3modelsindg.R')
load('IObysub.R')
load('GICSmap.R')
load('OutR2postS3.R')
load('regtry7.R')
EnvSamsub <- EnvSam
EnvSamsub$ctytier2<-cty
# EnvSamsub <- subset(EnvSam,select=c("account_id","sub_industry_name","Industry_Group","Industry","TotalRevenue","Employees","Capex","S1.Gross.mtCO2e","S2.Gross.mtCO2e","ctytier2"))
GICSsub2 <- unique(subset(GICS,select=c("sec_hybrid","Industry.Group","Industry","Sub.Industry","sec_hybridS3")))

names(GICSsub2) <- c("sec_hybrid","Industry_Group","Industry" ,"sub_industry","sec_hybridS3")
EnvSamsub <- merge(EnvSamsub,GICSsub2,by.x="sec_hybrid.f",by.y="sec_hybrid") #add GICS sec_hybridS3 to EnvSam
EnvSamsub<-EnvSamsub[1,]
EnvSamsub$account_id<-87 
EnvSamsub$TotalRevenuet <- log(TotalRevenue)

EnvSamsub$Capext <- log(-1*Capex)
EnvSamsub$Employeest <- log(Employees)

#estimate EnvSam S3 using IO
EnvSamS3est <- merge(EnvSamsub,IObysub,by.x="sub_industry",by.y="Sub.Industry",all.x=TRUE,all.y=FALSE)
EnvSamS3est[c("IOmed01","IOmed03","IOmed04","IOmed05","IOmed06","IOmed09")] <- TotalRevenue*EnvSamS3est[c("IOmed01","IOmed03","IOmed04","IOmed05","IOmed06","IOmed09")]
#estimate EnvSam using regressions
l <- length(EnvSam$TotalRevenue)
seqreg01 = data.frame(fitval = numeric(l))#,lwrb = numeric(l),uprb = numeric(l))
seqreg02 = data.frame(fitval = numeric(l))#,lwrb = numeric(l),uprb = numeric(l))
seqreg03 = data.frame(fitval = numeric(l))#,lwrb = numeric(l),uprb = numeric(l))
seqreg04 = data.frame(fitval = numeric(l))#,lwrb = numeric(l),uprb = numeric(l))
seqreg05 = data.frame(fitval = numeric(l))#,lwrb = numeric(l),uprb = numeric(l))
seqreg06 = data.frame(fitval = numeric(l))#,lwrb = numeric(l),uprb = numeric(l))
seqreg07 = data.frame(fitval = numeric(l))#,lwrb = numeric(l),uprb = numeric(l))
seqreg09 = data.frame(fitval = numeric(l))#,lwrb = numeric(l),uprb = numeric(l))
seqreg11 = data.frame(fitval = numeric(l))#,lwrb = numeric(l),uprb = numeric(l))
seqreg12 = data.frame(fitval = numeric(l))#,lwrb = numeric(l),uprb = numeric(l))

acctidcheck <- data.frame(account_id = numeric(l))

#function to test for null or na
isnothing = function(x) {
  any(is.null(x))  | any(is.na(x))   
}
# was a loop now just one iteration
i<-1
modelname01<-NA
modelname02<-NA
modelname03<-NA
modelname04<-NA
modelname05<-NA
modelname06<-NA
modelname07<-NA
modelname09<-NA
modelname11<-NA
modelname12<-NA
  seccode <- EnvSamsub$sec_hybridS3[i]
  indcode <- EnvSamsub$Industry[i]
  indgcode <- EnvSamsub$Industry_Group[i]
  acctidcheck[i,1] <- EnvSamsub$account_id[i]
  r2row <- subset(r2out,sech  %in%  seccode)
  if(nrow(r2row)>1) {r2row <- r2row[1,]}
  r2row[which(is.na(r2row))] <- 0
  
  #predict using best lm, testing for existence and R2. For cat02 and cat07, first try best then sech(revenue)
  if (!isnothing(modelsbest[[paste(seccode,".01",sep='')]])& r2row$"01.x">0.500) {
    fit01 <- predict(modelsbest[[paste(seccode,".01",sep='')]], newdata=EnvSamsub[i,],interval="prediction",se.fit=TRUE, level=0.9)
    modelname01<-seccode} else if(!isnothing(modelsind[[paste(indcode,".01",sep='')]])& r2row$"01.y">0.500) {
      fit01 <- predict(modelsind[[paste(indcode,".01",sep='')]], newdata=EnvSamsub[i,],interval="prediction",se.fit=TRUE, level=0.9)
 modelname01<-indcode} else if(!isnothing(modelsindg[[paste(indgcode,".01",sep='')]])& r2row$"01">0.500) {
        fit01 <- predict(modelsindg[[paste(indgcode,".01",sep='')]], newdata=EnvSamsub[i,],interval="prediction",se.fit=TRUE, level=0.9)
   modelname01<-indgcode} else fit01 <- NA
  if (!isnothing(modelsbest[[paste(seccode,".02",sep='')]])& r2row$"02.x">0.500 & !is.na(EnvSamsub$Capext[i])) {
    fit02 <- predict(modelsbest[[paste(seccode,".02",sep='')]], newdata=EnvSamsub[i,],interval="prediction",se.fit=TRUE, level=0.9)
    modelname02<-seccode} else if(!isnothing(modelsind[[paste(indcode,".02",sep='')]])& r2row$"02.y">0.500) {
      fit02 <- predict(modelsind[[paste(indcode,".02",sep='')]], newdata=EnvSamsub[i,],interval="prediction",se.fit=TRUE, level=0.9)
      modelname02<-indcode} else if(!isnothing(modelsindg[[paste(indgcode,".02",sep='')]])& r2row$"02">0.500) {
        modelname01<-indgcode
        fit02 <- predict(modelsindg[[paste(indgcode,".02",sep='')]], newdata=EnvSamsub[i,],interval="prediction",se.fit=TRUE, level=0.9)} else fit02 <- NA
  if (!isnothing(modelsbest[[paste(seccode,".03",sep='')]])& r2row$"03.x">0.500) {
    modelname03<-seccode
    fit03 <- predict(modelsbest[[paste(seccode,".03",sep='')]], newdata=EnvSamsub[i,],interval="prediction",se.fit=TRUE, level=0.9)} else if(!isnothing(modelsind[[paste(indcode,".03",sep='')]])& r2row$"03.y">0.500) {
      modelname03<-indcode
      fit03 <- predict(modelsind[[paste(indcode,".03",sep='')]], newdata=EnvSamsub[i,],interval="prediction",se.fit=TRUE, level=0.9)} else if(!isnothing(modelsindg[[paste(indgcode,".03",sep='')]])& r2row$"03">0.500) {     
        modelname03<-indgcode
        fit03 <- predict(modelsindg[[paste(indgcode,".03",sep='')]], newdata=EnvSamsub[i,],interval="prediction",se.fit=TRUE, level=0.9)} else fit03 <- NA
  if (!isnothing(modelsbest[[paste(seccode,".04",sep='')]])& r2row$"04.x">0.500) {
    modelname04<-seccode
    fit04 <- predict(modelsbest[[paste(seccode,".04",sep='')]], newdata=EnvSamsub[i,],interval="prediction",se.fit=TRUE, level=0.9)} else if(!isnothing(modelsind[[paste(indcode,".04",sep='')]])& r2row$"04.y">0.500) {
      modelname04<-indcode
      fit04 <- predict(modelsind[[paste(indcode,".04",sep='')]], newdata=EnvSamsub[i,],interval="prediction",se.fit=TRUE, level=0.9)} else if(!isnothing(modelsindg[[paste(indgcode,".04",sep='')]])& r2row$"04">0.500) {
        modelname04<-indgcode
        fit04 <- predict(modelsindg[[paste(indgcode,".04",sep='')]], newdata=EnvSamsub[i,],interval="prediction",se.fit=TRUE, level=0.9)} else fit04 <- NA
  if (!isnothing(modelsbest[[paste(seccode,".05",sep='')]])& r2row$"05.x">0.500) {
    modelname05<-seccode
    fit05 <- predict(modelsbest[[paste(seccode,".05",sep='')]], newdata=EnvSamsub[i,],interval="prediction",se.fit=TRUE, level=0.9)} else if(!isnothing(modelsind[[paste(indcode,".05",sep='')]])& r2row$"05.y">0.500) {
      modelname05<-indcode
      fit05 <- predict(modelsind[[paste(indcode,".05",sep='')]], newdata=EnvSamsub[i,],interval="prediction",se.fit=TRUE, level=0.9)} else if(!isnothing(modelsindg[[paste(indgcode,".05",sep='')]])& r2row$"05">0.500) {
        modelname05<-indgcode
        fit05 <- predict(modelsindg[[paste(indgcode,".05",sep='')]], newdata=EnvSamsub[i,],interval="prediction",se.fit=TRUE, level=0.9)} else fit05 <- NA
  if (!isnothing(modelsbest[[paste(seccode,".06",sep='')]])& r2row$"06.x">0.500) {
    modelname06<-seccode
    fit06 <- predict(modelsbest[[paste(seccode,".06",sep='')]], newdata=EnvSamsub[i,],interval="prediction",se.fit=TRUE, level=0.9)} else if(!isnothing(modelsind[[paste(indcode,".06",sep='')]])& r2row$"06.y">0.500) {
      modelname06<-indcode
      fit06 <- predict(modelsind[[paste(indcode,".06",sep='')]], newdata=EnvSamsub[i,],interval="prediction",se.fit=TRUE, level=0.9)} else if(!isnothing(modelsindg[[paste(indgcode,".06",sep='')]])& r2row$"06">0.500) {
        modelname06<-indgcode
        fit06 <- predict(modelsindg[[paste(indgcode,".06",sep='')]], newdata=EnvSamsub[i,],interval="prediction",se.fit=TRUE, level=0.9)} else fit06 <- NA
  if (!isnothing(modelsbest[[paste(seccode,".07",sep='')]])& r2row$"07.x">0.500 & !is.na(EnvSamsub$Employeest[i])) {
    modelname07<-seccode
    fit07 <- predict(modelsbest[[paste(seccode,".07",sep='')]], newdata=EnvSamsub[i,],interval="prediction",se.fit=TRUE, level=0.9)} else if(!isnothing(modelsindg[[paste(indcode,".07",sep='')]])& r2row$"07">0.500) {
      modelname07<-indcode
      fit07 <- predict(modelsindg[[paste(indcode,".07",sep='')]], newdata=EnvSamsub[i,],interval="prediction",se.fit=TRUE, level=0.9)} else if(!isnothing(EnvSamsub$Employeest[i])) {
        modelname07<-indcode
        fit07 <- predict(regtry7, newdata=EnvSamsub[i,],interval="prediction",se.fit=TRUE, level=0.9)} else fit07 <- NA
  if (!isnothing(modelsbest[[paste(seccode,".09",sep='')]])& r2row$"09.x">0.500) {
    modelname09<-seccode
    fit09 <- predict(modelsbest[[paste(seccode,".09",sep='')]], newdata=EnvSamsub[i,],interval="prediction",se.fit=TRUE, level=0.9)} else if(!isnothing(modelsind[[paste(indcode,".09",sep='')]])& r2row$"09.y">0.500) {
      modelname09<-indcode
      fit09 <- predict(modelsind[[paste(indcode,".09",sep='')]], newdata=EnvSamsub[i,],interval="prediction",se.fit=TRUE, level=0.9)} else if(!isnothing(modelsindg[[paste(indgcode,".09",sep='')]])& r2row$"09">0.500) {
        modelname09<-indgcode
        fit09 <- predict(modelsindg[[paste(indgcode,".09",sep='')]], newdata=EnvSamsub[i,],interval="prediction",se.fit=TRUE, level=0.9)} else fit09 <- NA
  if (!isnothing(modelsbest[[paste(seccode,".11",sep='')]])& r2row$"11.x">0.500) {
    modelname11<-seccode
    fit11 <- predict(modelsbest[[paste(seccode,".11",sep='')]], newdata=EnvSamsub[i,],interval="prediction",se.fit=TRUE, level=0.9)} else if(!isnothing(modelsind[[paste(indcode,".11",sep='')]])& r2row$"11.y">0.500) {
      modelname11<-indcode
      fit11 <- predict(modelsind[[paste(indcode,".11",sep='')]], newdata=EnvSamsub[i,],interval="prediction",se.fit=TRUE, level=0.9)} else if(!isnothing(modelsindg[[paste(indgcode,".11",sep='')]])& r2row$"11">0.500) {
        modelname11<-indgcode
        fit11 <- predict(modelsindg[[paste(indgcode,".11",sep='')]], newdata=EnvSamsub[i,],interval="prediction",se.fit=TRUE, level=0.9)} else fit11 <- NA
  if (!isnothing(modelsbest[[paste(seccode,".12",sep='')]])& r2row$"12.x">0.500) {
    modelname12<-seccode
    fit12 <- predict(modelsbest[[paste(seccode,".12",sep='')]], newdata=EnvSamsub[i,],interval="prediction",se.fit=TRUE, level=0.9)} else if(!isnothing(modelsind[[paste(indcode,".12",sep='')]])& r2row$"12.y">0.500) {
      modelname12<-indcode
      fit12 <- predict(modelsind[[paste(indcode,".12",sep='')]], newdata=EnvSamsub[i,],interval="prediction",se.fit=TRUE, level=0.9)} else if(!isnothing(modelsindg[[paste(indgcode,".12",sep='')]])& r2row$"12">0.500) {
        modelname12<-indgcode
        fit12 <- predict(modelsindg[[paste(indgcode,".12",sep='')]], newdata=EnvSamsub[i,],interval="prediction",se.fit=TRUE, level=0.9)} else fit12 <- NA
  
  if(length(fit01)< 2) {seqreg01[i,1] <- NA} else {seqreg01[i,1] <- exp(fit01$fit[1])}
  if(length(fit02)< 2) {seqreg02[i,1] <- NA} else {seqreg02[i,1] <- exp(fit02$fit[1])}
  if(length(fit03)< 2) {seqreg03[i,1] <- NA} else {seqreg03[i,1] <- exp(fit03$fit[1])}
  if(length(fit04)< 2) {seqreg04[i,1] <- NA} else {seqreg04[i,1] <- exp(fit04$fit[1])}
  if(length(fit05)< 2) {seqreg05[i,1] <- NA} else {seqreg05[i,1] <- exp(fit05$fit[1])}
  if(length(fit06)< 2) {seqreg06[i,1] <- NA} else {seqreg06[i,1] <- exp(fit06$fit[1])}
  if(length(fit07)< 2) {seqreg07[i,1] <- NA} else {seqreg07[i,1] <- exp(fit07$fit[1])}
  if(length(fit09)< 2) {seqreg09[i,1] <- NA} else {seqreg09[i,1] <- exp(fit09$fit[1])}
  if(length(fit11)< 2) {seqreg11[i,1] <- NA} else {seqreg11[i,1] <- exp(fit11$fit[1])}
  if(length(fit12)< 2) {seqreg12[i,1] <- NA} else {seqreg12[i,1] <- exp(fit12$fit[1])}


seqreg <- cbind(acctidcheck,seqreg01,seqreg02,seqreg03,seqreg04,seqreg05,seqreg06,seqreg07,seqreg09,seqreg11,seqreg12)
names(seqreg) <- c("account_id","reg01","reg02","reg03","reg04","reg05","reg06","reg07","reg09","reg11","reg12")
EnvSamS3est <- merge(EnvSamS3est,seqreg,by="account_id")

#pick best estimate, if reg exists take reg otherwise take IO
EnvSamS3est$Best01 <- EnvSamS3est$reg01
EnvSamS3est$Best02 <- EnvSamS3est$reg02
EnvSamS3est$Best03 <- EnvSamS3est$reg03
EnvSamS3est$Best04 <- EnvSamS3est$reg04
EnvSamS3est$Best05 <- EnvSamS3est$reg05
EnvSamS3est$Best06 <- EnvSamS3est$reg06
EnvSamS3est$Best07 <- EnvSamS3est$reg07
EnvSamS3est$Best09 <- EnvSamS3est$reg09
EnvSamS3est$Best11 <- EnvSamS3est$reg11
EnvSamS3est$Best12 <- EnvSamS3est$reg12
EnvSamS3est$Best01[which(is.na(EnvSamS3est$Best01))] <- EnvSamS3est$IOmed01[which(is.na(EnvSamS3est$Best01))]
EnvSamS3est$Best03[which(is.na(EnvSamS3est$Best03))] <- EnvSamS3est$IOmed03[which(is.na(EnvSamS3est$Best03))]
EnvSamS3est$Best04[which(is.na(EnvSamS3est$Best04))] <- EnvSamS3est$IOmed04[which(is.na(EnvSamS3est$Best04))]
EnvSamS3est$Best05[which(is.na(EnvSamS3est$Best05))] <- EnvSamS3est$IOmed05[which(is.na(EnvSamS3est$Best05))]
EnvSamS3est$Best06[which(is.na(EnvSamS3est$Best06))] <- EnvSamS3est$IOmed06[which(is.na(EnvSamS3est$Best06))]
EnvSamS3est$Best09[which(is.na(EnvSamS3est$Best09))] <- EnvSamS3est$IOmed09[which(is.na(EnvSamS3est$Best09))]
##############################################

Best01 <- EnvSamS3est$Best01
Best02 <- EnvSamS3est$Best02
Best03 <- EnvSamS3est$Best03
Best04 <- EnvSamS3est$Best04
Best05 <- EnvSamS3est$Best05
Best06 <- EnvSamS3est$Best06
Best07 <- EnvSamS3est$Best07
Best09 <- EnvSamS3est$Best09
Best11 <- EnvSamS3est$Best11
Best12 <- EnvSamS3est$Best12

reg01 <- EnvSamS3est$reg01
reg02 <- EnvSamS3est$reg02
reg03 <- EnvSamS3est$reg03
reg04 <- EnvSamS3est$reg04
reg05 <- EnvSamS3est$reg05
reg06 <- EnvSamS3est$reg06
reg07 <- EnvSamS3est$reg07
reg09 <- EnvSamS3est$reg09
reg11 <- EnvSamS3est$reg11
reg12 <- EnvSamS3est$reg12

IOmed01 <- EnvSamS3est$IOmed01
IOmed03 <- EnvSamS3est$IOmed03
IOmed04 <- EnvSamS3est$IOmed04
IOmed05 <- EnvSamS3est$IOmed05
IOmed06 <- EnvSamS3est$IOmed06
IOmed09 <- EnvSamS3est$IOmed09

#RUNS UP TO HERE#############
data.frame(Metric=c("Scope 3: Purchased goods and services",
                    "Scope 3: Capital goods",
                    "Scope 3: Fuel and energy-related activities",
                    "Scope 3: Upstream transportation and distribution",
                    "Scope 3: Waste generated in operations",
                    "Scope 3: Business travel",
                    "Scope 3: Employee commuting",
                    "Scope 3: Downstream transportation and distribution",
                    "Scope 3: Use of sold products",
                    "Scope 3: End-of-life treatment of sold products"),
           Value=c(signif(max(0,Best01),3),
                   signif(max(0,Best02),3),
                   signif(max(0,Best03),3),
                   signif(max(0,Best04),3),
                   signif(max(0,Best05),3),
                   signif(max(0,Best06),3),
                   signif(max(0,Best07),3),
                   signif(max(0,Best09),3),
                   signif(max(0,Best11),3),
                   signif(max(0,Best12),3)),
           Unit="metric Tonnes of CO2 equivalent",
           Model=c(if(!is.na(Best01) & Best01 %in% reg01){paste(modelname01,"- Sector Regression")}else if(!is.na(Best01) & Best01 %in% IOmed01){"Input-Output"} else {"No Model returned"},
                   if(!is.na(Best02) &Best02 %in% reg02){paste(modelname02,"- Sector Regression")}else {"No Model returned"},
                   if(!is.na(Best03) &Best03 %in% reg03){paste(modelname03,"- Sector Regression")}else if(!is.na(Best03) & Best03 %in% IOmed03){"Input-Output"} else {"No Model returned"},
                   if(!is.na(Best04) &Best04 %in% reg04){paste(modelname04,"- Sector Regression")}else if(!is.na(Best04) & Best04 %in% IOmed04){"Input-Output"} else {"No Model returned"},
                   if(!is.na(Best05) &Best05 %in% reg05){paste(modelname05,"- Sector Regression")}else if(!is.na(Best05) & Best05 %in% IOmed05){"Input-Output"} else {"No Model returned"},
                   if(!is.na(Best06) &Best06 %in% reg06){paste(modelname06,"- Sector Regression")}else if(!is.na(Best06) & Best06 %in% IOmed06){"Input-Output"} else {"No Model returned"},
                   if(!is.na(Best07) &Best07 %in% reg07){paste(modelname07,"- Sector Regression")}else {"No Model returned"},
                   if(!is.na(Best09) &Best09 %in% reg09){paste(modelname09,"- Sector Regression")}else if(!is.na(Best09) & Best09 %in% IOmed09){"Input-Output"} else {"No Model returned"},
                   if(!is.na(Best11) &Best11 %in% reg11){paste(modelname11,"- Sector Regression")}else {"No Model returned"},
                   if(!is.na(Best12) &Best12 %in% reg12){paste(modelname12,"- Sector Regression")}else {"No Model returned"}))
}
