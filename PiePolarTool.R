
wesGraph<-function(obj){

GrandBudapest = c("#F1BB7B", "#FD6467", "#5B1A18", "#D67236")
Moonrise1 = c("#F3DF6C", "#CEAB07", "#D5D5D3", "#24281A")
Royal1 = c("#899DA4", "#C93312", "#FAEFD1", "#DC863B")
Moonrise2 = c("#798E87", "#C27D38", "#CCC591", "#29211F")
Cavalcanti = c("#D8B70A", "#02401B", "#A2A475", "#81A88D", "#972D15")
Royal2 = c("#9A8822", "#F5CDB4", "#F8AFA8", "#FDDDA0", "#74A089")
GrandBudapest2 = c("#E6A0C4", "#C6CDF7", "#D8A499", "#7294D4")
Moonrise3 = c("#85D4E3", "#F4B5BD", "#9C964A", "#CDC08C", "#FAD77B")
Chevalier = c("#446455", "#FDD262", "#D3DDDC", "#C7B19C")
Zissou = c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00")
FantasticFox = c("#DD8D29", "#E2D200", "#46ACC8", "#E58601", "#B40F20")
Darjeeling = c("#FF0000", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6")
Rushmore = c("#E1BD6D", "#EABE94", "#0B775E", "#35274A" ,"#F2300F")
BottleRocket = c("#A42820", "#5F5647", "#9B110E", "#3F5151", "#4E2A1E", "#550307", "#0C1707")
Darjeeling2 = c("#ECCBAE", "#046C9A", "#D69C4E", "#ABDDDE", "#000000")
BottleRocket2 = c("#FAD510", "#CB2314", "#273046", "#354823", "#1E1E1E")



obj<-cbind(obj,plot=c("Scope1","Scope2","Scope3",rep("",9)),Scope=c("Scope1","Scope2",rep("Scope3",10)))
obj<-obj[!is.na(obj$Value),]
obj$ymax<-obj$Value/sum(obj[obj$Scope=="Scope3",]$Value,na.rm=T)*100
obj$ymax[1:2]<-100
obj$ymin<-c(0,0,0,obj$ymax[3:(nrow(obj)-1)])
obj$ymax[3:(nrow(obj))]<-cumsum(obj$ymax[3:(nrow(obj))])
obj$ymin[3:(nrow(obj))]<-cumsum(obj$ymin[3:(nrow(obj))])



obj$xmin<-0
obj$xmax<-0
obj[obj$Scope=="Scope1",]$xmax<-obj$Value[1]/sum(obj$Value,na.rm=T)*100

obj[obj$Scope=="Scope2",]$xmin<-obj$Value[1]/sum(obj$Value,na.rm=T)*100
obj[obj$Scope=="Scope2",]$xmax<-sum(obj$Value[1:2],na.rm=T)/sum(obj$Value,na.rm=T)*100

obj[obj$Scope=="Scope3",]$xmin<-sum(obj$Value[1:2],na.rm=T)/sum(obj$Value,na.rm=T)*100
obj[obj$Scope=="Scope3",]$xmax<-100


P<-ggplot(obj)+  geom_rect(alpha=0.5,colour = I("white"),aes(ymax=ymax,ymin=ymin,xmin=xmin,xmax=xmax,fill=Metric))+
  geom_text(aes(x=(xmin+(xmax-xmin)/2),y=120,label=ifelse((xmax-xmin)>3,as.character(plot),"")))+
  coord_polar()+   
  scale_fill_manual(values=c(Darjeeling2,Royal1,Rushmore ))+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(), 
        axis.title.x=element_blank(),
        axis.title.y=element_blank())


P
}





