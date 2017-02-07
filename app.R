# Global variables can go here
require(gridExtra) 
require(ggplot2)
require(dplyr)
source("Scope3Runner.R")
source('PiePolarTool.R')
library(shiny)
library(shinyapps)

load("BICS_S1.R")
load("BICS_S2.R")
#####################
# reads moel structure from lm object
coeffs<-coefficients(reg4S1t)%>%names
secFac<-coeffs[grepl("sec_hyb",coeffs)] %>% substr(nchar("sec_hybrid.f")+1,1000)
ctyFac<-coeffs[grepl("ctytier2",coeffs)] %>% substr(nchar("ctytier2.f")+1,1000)
variabs<-subset(coeffs,!grepl("sec_hyb",coeffs)) 
variabs<-subset(variabs,!grepl("ctytier2",variabs))
variabs<-subset(variabs,!grepl("Intercept",variabs))
S1templ<-data.frame(matrix(rep(0,length(variabs)),nrow=1))
names(S1templ)<-variabs
spaceNames<-variabs
for(j in 1:length(spaceNames)) {
  x<-spaceNames[j]
  
  if (!is.na(str_extract_all(substr(x,2,5000),"[:upper:]")[[1]][1])) {
    for (i in 2:length(str_extract_all(x,"[:upper:]")[[1]])) {
      
      x <- str_replace_all(x,str_extract_all(x,"[:upper:]")[[1]][i],
                           paste("",str_extract_all(x,"[:upper:]")[[1]][i]))
    }
  }
  x<- str_replace_all(x,"and"," &")
  spaceNames[j]<-x
}

#####Takes model frame and creates a mapping table from BICS To GICS


mapframe <- model.frame(reg4S1t) %>% select(-S1.Gross.mtCO2et,-ctytier2.f)

mapframe[2:ncol(mapframe)] <- exp(mapframe[2:ncol(mapframe)])-1

mapframe <- melt(mapframe,id.vars="sec_hybrid.f",variable.name="BICS_hyb") %>% dcast(BICS_hyb~sec_hybrid.f,fun.aggregate=sum)

mapframe<-cbind(BICS_hyb=mapframe["BICS_hyb"],
                       GICS_hyb=colnames(mapframe[2:ncol(mapframe)])[max.col(mapframe[2:ncol(mapframe)],ties.method="first")])



############################
# Now need to make function to textclean BicsCleanNames to be Bics Clean Names
############################

nameMap<-data.frame(ShortNames=variabs,LongNames=spaceNames)
nameMap$ShortNames<-as.character(nameMap$ShortNames)
nameMap$LongNames<-as.character(nameMap$LongNames)


ui <- fluidPage(theme = "CDP.css",
               conditionalPanel("input.SeeModel ==0", 
                                h2("Corporate GHG Emissions Profile Calculator"),
                                h6("This tool allows you to estimate the emissions of a company, by taking into account it's revenue in different sectors."),
                                h4("Instructions:"),
                                p('Choose Division/Sector of the company you are trying to estimate for'),
                                p('You can search for the right Industry Sector and Divisions by typing into the drop down menus.'),
                                p("All the information required for calculating these estimates can be found in a company's annual reports."),
                                p('These models are able to estimate the emissions of companies with more than one activity, 
                                  for companies with more than one activity just add a new Division for each activity and
                                  enter the revenue from that business activity or division.'),
                                h4("Notes:"),
                                p('These models use statistical methods based on data reported to CDP and should only be considered more reliable than
                                  the data reported by companies when there is evidence to suggest that a company has not followed the GHG Protocol.'),
                                p('These models were built using emissions data submitted by  companies to CDP in 2015. 
                                  Using these models to estimate the emissions of any other type of entity during any other time period will not
                                  yield reasonable results.'),
                                p('This is the most ubiquitous of the models used to create the CDP Complete Dataset, in many cases other models were selected to
                                  give more accurate estimates for specific companies and sectors.'),
                                p("Full details of how all the models were built can be found",
                                  a("here.", href="https://www.cdp.net/en-US/Programmes/Pages/ghg-emissions-dataset.aspx")),
                                p('Definitions of Scope 1, 2 & 3 can be found in the ',a("GHG Protocol.", href="http://www.ghgprotocol.org/")),
                                actionButton("SeeModel","Use the Model")
                                
                                
                                ),
               conditionalPanel("input.SeeModel >0",
    sidebarLayout(

    sidebarPanel(

      fluidRow(
        fluidRow(column(p("Follow the two steps and press the 'Calculate' button to run models"),width=8),
                 column(actionButton("goButton", "Calculate"),width=3)),
        
        h4("Step 1:"), 
        h5("Select Region."),

        selectInput("region", "Region:",ctyFac,selected="Europe"),
        

        p(""),
        h4("Step 2:"),
        h5("Select at least one Division and enter the revenue from each Division/Sector in Millions of USD."),
        
        fluidRow(column(width=8,selectInput("sec1", "Division/Sector 1:",c("",nameMap$LongNames))),
                 column(width=4,numericInput("sec1perc", "Revenue:", min = 0, value=0))),
        
        fluidRow(column(width=8,selectInput("sec2", "Division/Sector 2:",c("",nameMap$LongNames))),
                 column(width=4,numericInput("sec2perc", "Revenue:", min = 0, value=0))),
        
        fluidRow(column(width=8,selectInput("sec3", "Division/Sector 3:",c("",nameMap$LongNames))),
                 column(width=4,numericInput("sec3perc", "Revenue:", min = 0, value=0))),
        
        fluidRow(column(width=8,selectInput("sec4", "Division/Sector 4:",c("",nameMap$LongNames))),
                 column(width=4,numericInput("sec4perc", "Revenue:", min = 0, value=0))),
        
        fluidRow(column(width=8,selectInput("sec5", "Division/Sector 5:",c("",nameMap$LongNames))),
                 column(width=4, numericInput("sec5perc", "Revenue:", min = 0, value=0))),
        
        p(""),
        h4("Step 3:"),
        h5("If known enter the Capital Expenditure (Capex), and the number of Employees."),
        h6("(Capex is negative in company accounts because it is an expense)."),
        fluidRow(column(width=6,numericInput("Capex", "Capex:", max = 0, value=NA))
                 ,column(width=6,numericInput("Employees", "Employees:", min = 0, value=NA))),
        actionButton("expl"," Show Explanation"))
      ),
    mainPanel(
      h1("CDP 2015 Corporate GHG Emissions Model"),
      p("(This App will give you an estimate of a company's emissions profile if you fill in the details on the left and press calculate.)"),
      plotOutput("PiePlot"),
      tableOutput("nText"),

      ####################
      conditionalPanel("input.expl >0",
      h4("Explanation"),

      p("When comparing the output from these models with actual data reported by a particular company, remember that these figures effectively 
         show how other similar companies reported their emissions to CDP not what that company's emissions should be. 
         Externally verified company reported data should always be used in place of modelled estimates."),
      p("For Example; This model over estimates Apple's emissions for two reasons; "),
      p("1. Apple have made considerable efforts to reduce their emissions compared to their peers. "),
      p("2. Their products tend to cost more and so they earn more revenue per unit sold than their peers. This model assumes that the emissions per unit sold are the same within each sector. "),
      p("The reason the estimate for Apple is much higher is not that the model is 'wrong' it's that the model is not a good fit for Apple because 
        Apple are not comparable with their peers, obviously there are many examples of other companies that are not comparable with their peers
        and you should always consider these issues when using this emissions model (or any other one)."),
      p("Please keep this in mind when using these models, they are best used for providing emissions figures in absence of any other information."),
      p(''),
      h4("Instructions:"),
      h4("Notes:"),
      p('These models use statistical methods based on data reported to CDP and should only be considered more reliable than
        the data reported by companies when there is evidence to suggest that a company has not followed the GHG Protocol.'),
      p('These models were built using emissions data submitted by  companies to CDP in 2015. 
        Using these models to estimate the emissions of any other type of entity during any other time period will not
        yield reasonable results.'),
      p('This is the most ubiquitous of the models used to create the CDP Complete Dataset, in many cases other models were selected to
         give more accurate estimates for specific companies and sectors.'),
      p("Full details of how all the models were built can be found",
        a("here.", href="https://www.cdp.net/en-US/Programmes/Pages/ghg-emissions-dataset.aspx")),
      p('Definitions of Scope 1, 2 & 3 can be found in the ',a("GHG Protocol.", href="http://www.ghgprotocol.org/")),
      p('This app was created by Hugh Sawbridge at CDP. (hugh.sawbridge@cdp.net)')
      )))
  ))

server <- function(input, output) {
  
  framePred <- eventReactive(input$goButton, {
    # # put params into template
    sec_vec<-c()
    perc_vec<-c()
    if (!input$sec1=="" & input$sec1perc > 0 ){
      sec_vec<-c(sec_vec,nameMap$ShortNames[nameMap$LongNames==input$sec1])
      perc_vec<-c(perc_vec,input$sec1perc)
    }
    # add extra sectors
    if (!input$sec2=="" & input$sec2perc > 0 ){
      sec_vec<-c(sec_vec,nameMap$ShortNames[nameMap$LongNames==input$sec2])
      perc_vec<-c(perc_vec,input$sec2perc)
    }
    
    if (!input$sec3=="" & input$sec3perc > 0 ){
      sec_vec<-c(sec_vec,nameMap$ShortNames[nameMap$LongNames==input$sec3])
      perc_vec<-c(perc_vec,input$sec3perc)
    }
    
    if (!input$sec4=="" & input$sec4perc > 0 ){
      sec_vec<-c(sec_vec,nameMap$ShortNames[nameMap$LongNames==input$sec4])
      perc_vec<-c(perc_vec,input$sec4perc)
    }
    
    if (!input$sec5=="" & input$sec5perc > 0 ){
      sec_vec<-c(sec_vec,nameMap$ShortNames[nameMap$LongNames==input$sec5])
      perc_vec<-c(perc_vec,input$sec5perc)
    }
    
  
      
    if(length(sec_vec)==0){stop('Please select at least one Division/Sector and be sure to enter a revenue value.')}

    TotalRevenue<-sum(perc_vec)
    perc_vec<-perc_vec/TotalRevenue*log(TotalRevenue)
    S1templ[1,names(S1templ) %in% sec_vec]<-perc_vec

    S1templ$ctytier2.f[1]<-input$region
    mapper <- as.data.frame(cbind(div=sec_vec,rev=perc_vec))
    mapper$rev <- as.numeric(mapper$rev)
    mapper$div <- as.character(mapper$div)
    mapper <- merge(mapper,mapframe,by.x="div",by.y="BICS_hyb")
    mapper$GICS_hyb <- as.character(mapper$GICS_hyb)
    print(as.character(mapper[mapper$rev==max(mapper$rev),]$div))
    S1templ$sec_hybrid.f[1]<-as.character(mapper[mapper$rev==max(mapper$rev),]$GICS_hyb)
    print(as.character(mapper[mapper$rev==max(mapper$rev),]$div))
    #HERERE HUGH

    # estimate and transform
    EstimatedScope1<-signif(exp(predict.lm(reg4S1t,newdata=S1templ)),3)
    EstimatedScope2<-signif(exp(predict.lm(reg4S2t,newdata=S1templ)),3)
    S3<-S3Runner(S1templ,TotalRevenue,input$Capex,input$Employees,S1templ$ctytier2.f)

    whatWeSee<-if(!input$sec1perc==0){S1S2<-data.frame(Metric=c("Scope1 (Direct emissions)","Scope2 (Emissions from purchased energy)"),
                                                       Value=c(EstimatedScope1,EstimatedScope2),
                                                       Unit=c("metric Tonnes of CO2 equivalent","metric Tonnes of CO2 equivalent"),
                                                       Model=c("Scope 1 Multi Sector","Scope 2 Multi Sector"))
    
    rbind(S1S2,S3)
    }else{whatWeSee<-data.frame(Error="Please enter revenue for at least one Activity")}
    whatWeSee
  })

  output$PiePlot <- renderPlot({
    wesGraph(framePred())
  })
  output$nText <- renderTable({
    framePred()
  })
}

shinyApp(ui, server)

