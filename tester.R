
coeffs<-coefficients(reg4S1t)%>%names
secFac<-coeffs[grepl("sec_hyb",coeffs)] %>% substr(nchar("sec_hybrid.f")+1,1000)
ctyFac<-coeffs[grepl("ctytier2",coeffs)] %>% substr(nchar("ctytier2.f")+1,1000)
variabs<-subset(coeffs,!grepl("sec_hyb",coeffs)) 
variabs<-subset(variabs,!grepl("ctytier2",variabs))
variabs<-subset(variabs,!grepl("Intercept",variabs))
S1templ<-data.frame(matrix(rep(0,length(variabs)),nrow=1))
names(S1templ)<-variabs

for(j in 20:length(variabs)) {
  x<-variabs[j]
  
  if (!is.na(str_extract_all(substr(x,2,5000),"[:upper:]")[[1]][1])) {
    for (i in 2:length(str_extract_all(x,"[:upper:]")[[1]])) {
      
      x <- str_replace_all(x,str_extract_all(x,"[:upper:]")[[1]][i],
                           paste("",str_extract_all(x,"[:upper:]")[[1]][i]))
    }
  }
  x<- str_replace_all(x,"and"," &")
  variabs[j]<-x
}

