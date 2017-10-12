rm(list=ls())

gc()

library(shiny)
library(dplyr)
library(readxl)
library(reshape2)



######## note: excel calculations will not give the same value before April 2016 because of a change in grouping of 3 futures contracts together. (NK, NS and NU)


options(stringsAsFactors = FALSE)

server<-function(input,output,session){
  
  # --------------------------------------------------------------------------------
  ##### SECTION:  Option to change default values for calculation
  
  output$conditional<-renderUI({
    list(
      numericInput("riskWeight","Risk Weight",value=0.2),
      numericInput("capitalRatio","Capital Ratio",value=0.08),
      numericInput("alpha","Alpha",value=1.4) ,
      numericInput("floor","Floor",value=0.05),
      numericInput("haircut","Haircut",value=0.08),
      numericInput("daysYear","Number of Days in Year",value=365),
      numericInput("workingDays","Number of Working Days in Year",value=250)
    )
  })
  
  data<-eventReactive(input$run,{
    
    if(input$filename==""){return(NULL)}
   
    # --------------------------------------------------------------------------------
    ##### SECTION: get file input parameters from GUI
    
    genv <- .GlobalEnv
    genv$filename<-input$filename
    genv$currentDate <- input$currentDate
    genv$DFccp <-as.numeric(gsub(input$DFccp, pattern = ",", replacement = ""))
    genv$floor =input$floor
    genv$haircut =  input$haircut
    genv$alpha<-input$alpha
    genv$daysYear<-input$daysYear
    genv$workingDays<-input$workingDays
    
    genv$margin<-10
    
    #supervisory haircut for collateral
    genv$bucket1<-0.005
    genv$bucket2<-0.02
    genv$bucket3<-0.04
    
    #default for KCCP
    genv$riskWeight<-input$riskWeight
    genv$capitalRatio<-input$capitalRatio
    
    
    # supervisory factor
    genv$SFinterestRate <- 0.005
    genv$SFequity<-0.2
    genv$SFComm<-0.18
    genv$SFFx<-0.04
    genv$SFCommEL<-0.4
    
    #Correlations
    genv$commCorr<-0.4
    genv$equityCorr<-0.8
    
    
    genv$ supOptionVolatility <-data.frame("Classification" = c("Equities","Energy","Metals","Agricultural","Other"),
                                           "volatility" = c(0.75,0.7,0.7,0.7,0.7))
   
    
    print("Script Executed")
  
    source("d:\\projects\\cfactor\\library_cfactor.R",local=T)
    source("d:\\projects\\cfactor\\cfactor_mainscript.R",local=T)
  
    print("Calculation Completed")
    
    list(KCCP, cfactor,df_result)  
   
  })
  
  # --------------------------------------------------------------------------------
  ##### SECTION:  Output to GUI
  
  output$KCCP<-renderText({
    paste("KCCP :",data()[1])
  })
  
  output$cfactor<-renderText({ 
    paste("C factor:",data()[2])})
  
  output$memberEAD<-renderDataTable({
    as.data.frame(data()[3])
  })
 
  
}


    # --------------------------------------------------------------------------------
    ##### SECTION:  User Interface for GUI

ui<-  fluidPage(
  headerPanel("C-factor Calculation"),
  
  sidebarPanel(
  tags$head(tags$style(type="text/css", "
                           #loadmessage {
                                    position: fixed;
                                    top: 0px;
                                    left: 0px;
                                    width: 100%;
                                    padding: 5px 0px 5px 0px;
                                    text-align: center;
                                    font-weight: bold;
                                    font-size: 100%;
                                    color: #000000;
                                    background-color: #ffebd6;
                                    z-index: 105;
                                    }
                                    ")),
    textInput("filename","File name :",value="new c-factor 29 feb 16.xlsx"),
  textInput("DFccp","Please enter DCCP",value=85106382.98),
    dateInput("currentDate", "Date (yyyy-mm-dd) :", value = as.Date("16/02/29",format="%y/%m/%d"), min = NULL, max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en", width = NULL),
    actionButton("run", "Run"),
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                   tags$div("Loading...",id="loadmessage")),
    br(),
    selectInput(
      "defaultValues", "Use Default Values?",
      c(Yes = "yes",
        No = "no")),
    conditionalPanel(
      condition = "input.defaultValues == 'no'",
      uiOutput("conditional")
      
    )
  ),
  
  
  
  mainPanel(h3(textOutput("KCCP")),
            h3(textOutput("cfactor")),
            dataTableOutput("memberEAD")
            
  )
)


shinyApp(ui = ui, server = server)

