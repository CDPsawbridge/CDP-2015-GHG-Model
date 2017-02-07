S1S2pred<-function(S1templ,perc_vec,sec_vec){
  S1templ[1,names(S1templ) %in% sec_vec]<-if(sum(perc_vec)>0){log(perc_vec)}else{S1templ[1,names(S1templ) %in% sec_vec]}
  S1templ$ctytier2.f[1]<-input$region
  S1templ$sec_hybrid.f[1]<-input$gics
  # estimate and transform
  EstimatedScope1<-signif(exp(predict.lm(reg4S1t,newdata=S1templ)),3)
  EstimatedScope2<-signif(exp(predict.lm(reg4S2t,newdata=S1templ)),3)
  S3<-S3Runner(S1templ,sum(perc_vec),input$Capex,input$Employees,S1templ$ctytier2.f)
  
  whatWeSee<-if(!input$sec1perc==0){S1S2<-data.frame(Metric=c("Scope1 (Direct emissions)","Scope2 (Emissions from electricity use)"),
                                                     Value=c(EstimatedScope1,EstimatedScope2),
                                                     Unit=c("million metric Tonnes of CO2 equivalent","million metric tonnes of CO2 equivalent"),
                                                     Model=c("Scope 1 Multi Sector","Scope 2 Multi Sector"))
  
  rbind(S1S2,S3)
  }else{whatWeSee<-data.frame(Error="Please enter revenue for at least one Activity")}
  
  whatWeSee}