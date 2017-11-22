library(shiny)
#library(shinyapps)
library(googleVis)
library(knitr)
#install.packages('shinysky')
#library(shinysky)
library (MASS)
library(e1071)
library(rsconnect)
#install.packages('rsconnect')
#library(tidyverse)
library(GGally)
library(psych)
library(MASS)
library(glmnet)
library(pls)
library(FNN)
library(car)
library(tree)
library(randomForest)
library(gbm)
require(tweedie)
require(statmod)
require(censReg)
library("PerformanceAnalytics")
library(RODBC)
library(shinythemes)
library(shinydashboard)
library(shinyTree)
library(dygraphs)
library(DT)
library(xts)
library(zoo)  
library(plotly)
library(ggplot2)
library(Hmisc)
library('dplyr')      # for data manipulation
library('tidyr')      # for reshaping data

library('scales')     # for scale_y_continuous(label = percent)
library('ggthemes')   # for scale_fill_few('medium')
library('ztable')   
#library(plyr)
#install.packages('RLumShiny')
library(RLumShiny)
#install.packages("broom")
library(broom)
#install.packages("devtools")
library("devtools")
require("devtools")
#devtools::install_github("AnalytixWare/ShinySky")
library(TeachingDemos)
library(car)
library(corrplot) # We'll use corrplot later on in this example too.
library(visreg) # This library will allow us to show multivariate graphs.
library(rgl)
library(scatterplot3d)




#setwd("C:/Users/UHPAD/Desktop/SFBCICSymbolsApp")

load("VehiclesSymbolDatNew.RData")


ui <- fluidPage(theme = shinytheme("yeti"),# cerulean,sandstone,slate ,united,cosmo,cyborg,darkly,flatly,lumen,journal,slate ,superhero,journal,paper,yeti
                
                
                # tags$style(".well {background-color: wine ;}"), # lightgray
                
                titlePanel("report",title=div(img(src='sfbcic.png', align = "left",height = 10, width = 10), "SFBCIC Symbols Modelling Projekt")),
                
                # Sidebar 
                
                sidebarLayout(
                  sidebarPanel(  width=3,    
                                 textInput("name", label = h5("Name"), value = "Name"),
                                 HTML('</br>'),
                                 selectInput("dataset", h5("Choose a dataset:"), choices = c("Full Data")),
                                 HTML('</br>'),
                                 uiOutput('dv'),    
                                 HTML('</br>'),
                                 uiOutput('iv'),
                                 HTML('</br>'),
                                 radioButtons("p", "Select Modelling Method:",
                                              list("Tweedie Model6"='gg')),  
                                 HTML('</br>'),
                                 radioButtons('report_format', h5('Document format'), c('PDF', 'HTML', 'Word'), inline = TRUE),
                                 conditionalPanel("report.Rmd", downloadButton("downloadReport"))),

                  
                  mainPanel( width=8, tabsetPanel( type = "tabs", 
                                                   
                                                   tabPanel("Data", DT::dataTableOutput("view")),
                                                   
                                                   tabPanel("SummaryStats",
                                                            verbatimTextOutput("summary"),
                                                            textInput("text_summary", label = "Interpretation", value = "Enter text..."),
                                                            
                                                            htmlOutput("summary2"),
                                                            HTML('</br> </br>'),
                                                            textInput("text_summay2", label = "Interpretation", value = "Enter text...")),
                                                   
                                                   
                                                   tabPanel("Histograms",                   
                                                            plotOutput("distPlot_dv"),
                                                            sliderInput("bins_dv", "Number of bins:", min = 1, max = 50, value = 7),  
                                                            textInput("text_hist_dv", label = "Interpretation", value = "Enter text..."),
                                                            
                                                            plotOutput("distPlot_iv"),
                                                            sliderInput("bins_iv", "Number of bins:", min = 1, max = 50, value = 7),
                                                            textInput("text_hist_iv", label = "Interpretation", value = "Enter text..."),
                                                            
                                                            plotlyOutput("summbar",height = "650px"),
                                                            textInput("text_sumbar", label = "Interpretation", value = "Enter text...")),
                                                   
                                                   
                                                   tabPanel("ScatterPlot",                   
                                                            plotlyOutput("scatter",height = "650px"),
                                                            textInput("text_scatter", label = "Interpretation", value = "Enter text...")),  
                                                   
                                               
                                                   
                                                   tabPanel("Correlations",                   
                                                            htmlOutput("corr"),
                                                            HTML('</br> </br>'),
                                                            textInput("text_correlation", label = "Interpretation", value = "Enter text...")),
                                                   
                                                   tabPanel("Model",                   
                                                            verbatimTextOutput("model"),
                                                            textInput("text_model", label = "Interpretation", value = "Enter text...")),
                                                   
                                                   tabPanel("ModelFactors",                   
                                                            verbatimTextOutput("modelfactors"),
                                                            textInput("text_modelfactors", label = "Interpretation", value = "Enter text...")),
                                                   
                                                   
                                                   tabPanel("Fitted", DT::dataTableOutput("Preds"),
                                                            tableOutput("Preds_mse")) ,
                                                   
                                                   tabPanel("FittedGraphs",                   
                                                            plotlyOutput("obsvrsfitted",height = "350px"),
                                                            textInput("text_residuals", label = "Interpretation", value = "Enter text..."),
                                                            
                                                            plotOutput("obsvrsfitted_obs"),
                                                            sliderInput("bins_dv2", "Number of bins:", min = 1, max = 50, value = 7),
                                                            textInput("text_hist_dv2", label = "Interpretation", value = "Enter text..."), 
                                                            
                                                            plotOutput("obsvrsfitted_fits"),
                                                            sliderInput("bins_dv3", "Number of bins:", min = 1, max = 50, value = 7),
                                                            textInput("text_hist_dv3", label = "Interpretation", value = "Enter text...")),
                                                   
                                                   
                                                   tabPanel("SlopePlots",                   
                                                            plotOutput("regressline",height = "850px"),
                                                            textInput("text_regress1", label = "Interpretation", value = "Enter text..."),
                                                            
                                                            plotOutput("parpplot2",height = "1000px"),
                                                            textInput("text_par2", label = "Interpretation", value = "Enter text..."),
                                                            
                                                            plotOutput("parpplot",height = "850px"),
                                                            textInput("text_par1", label = "Interpretation", value = "Enter text...")),
                                                   
                                                   
                                                   tabPanel("AverageFitsGraphs",
                                                            htmlOutput("AvgFitTables"),
                                                            HTML('</br> </br>'),
                                                            textInput("text_AvgTabs", label = "Interpretation", value = "Enter text..."),
                                                            
                                                            plotOutput("AvgFitPlots",height = "700px"),
                                                            textInput("text_AvgGraph", label = "Interpretation", value = "Enter text...")),
                                                   
                                                   tabPanel("ScoreGraphs",
                                                            htmlOutput("AvgScoreTables"),
                                                            HTML('</br> </br>'),
                                                            textInput("text_ScoreTabs", label = "Interpretation", value = "Enter text..."),
                                                            
                                                            plotOutput("AvgScorePlots",height = "700px"),
                                                            textInput("text_ScoreGraph", label = "Interpretation", value = "Enter text...")),
                                                   
                                                   tabPanel("VinsPredict", DT::dataTableOutput("PredsVins") ) 
                                                   
                                                   
                                                   
                  )                         
                  ))
)

#install.packages("magrittr")

library(magrittr)

server <- function(input, output) {
  
  # list of data sets
  datasetInput <- reactive({
    switch( input$dataset ,
            "Full Data"= VehiclesSymbolDatNew  , #VehiclesSymbolDatRevised4
            "Your Data" = df()) 
  })
  
  # dependent variable
  output$dv = renderUI({
    selectInput('dv', h5('Dependent Variable'), choices = names(datasetInput()))
  })
  
  # independent variable
  output$iv = renderUI({
    selectInput('iv', h5('Independent Variable'), choices = names(datasetInput()))
  })
  
  
  model <-reactive({

      #Tweedie Model6 
      if(input$p=='gg'){
        
        glm( LossCostMetric   ~  factor(VehicleSizeName)  + factor(VehicleClassName) + Age   + EngineCylinders  + 
               CurbWeight +  Wheelbase  +  Length +  Width +  BasePrice 
             , family = tweedie(var.power = 1.5, link.power = 0), data = datasetInput()) 
        
      }

    else 
      # Fit an inverse-Gaussion glm with log-link
      if(input$p=='hh'){
        
        glm( pmax(LossCostMetric,1)   ~    Age   + pmax(EngineCylinders,1) + 
               CurbWeight +  Wheelbase  +  Length +  Width +  BasePrice 
             , family = tweedie(var.power = 3, link.power = 1), data = datasetInput()) 
      }  
    
    
    
  })
  
  # create graphics 
  
  
  # data view 
  output$view <- DT::renderDataTable({ 
    DT::datatable(datasetInput()[,]) #input$show_vars, drop = FALSE
  })
  
  
  
  # summary statistics
  output$summary <- renderPrint({
    summary(cbind(datasetInput()[input$dv], datasetInput()[input$iv]))
  })  
  
  
  
  
  output$summary2  <- renderGvis({
    y = count(datasetInput(), input$iv )
    tot= sum(y$freq)
    PercentFreq = y$freq/tot*100
    y = cbind(y , PercentFreq )
    gvisTable(y)
  })  
  
  
  output$summbar  <- renderPlotly({
    
    counts <- table(datasetInput()[input$iv])
    counts <- as.data.frame(prop.table(counts))
    
    plot_ly(counts)  %>%
      
      add_trace(counts, x = ~ counts$Var1, y =counts$Freq ,
                type='bar', marker = list(color='chartreuse'))  %>%  
      
      layout(title = paste(input$iv ,"vs Observed Percent Frequency"),   
             xaxis = list(title = input$iv),
             yaxis = list(title = 'Percent Frequency') )
    
  }) 
  
  
  
  # histograms   
  output$distPlot_dv <- renderPlot({
    x    <- datasetInput()[,input$dv]  
    bins <- seq(min(x), max(x), length.out = input$bins_dv + 1)
    hist(x, breaks = bins, col = 'green', border = 'blue', main = 'Dependent Variable', xlab = input$dv,  prob = TRUE)
  })
  
  
  output$distPlot_iv <- renderPlot({
    x    <- datasetInput()[,input$iv]  
    bins <- seq(min(x), max(x), length.out = input$bins_iv + 1)
    hist(x, breaks = bins, col = "green", border = 'blue', main = 'Independent Variable', xlab = input$iv,  prob = TRUE)
  })
  
  
  
  
  
  
  output$scatter <- renderPlotly({
    
    fit <- lm( datasetInput()[,input$dv] ~ datasetInput()[,input$iv], data = datasetInput())
    
    datasetInput()%>%
      plot_ly(x = ~datasetInput()[,input$iv]) %>%
      add_markers(y = ~datasetInput()[,input$dv] ,text = ~paste("MakeName: ", MakeName, '<br>ModelYear:',ModelYear, '<br>SeriesName:',SeriesName,'<br> TotalClaims :',totalclaims,'<br> TotalPayments $:',totalpayments),
                  marker = list(size = 10)) %>%
      add_lines(x = ~datasetInput()[,input$iv], y = fitted(fit)) %>%
      layout(title = paste(input$iv ,"vs", input$dv),   
             xaxis = list(title = input$iv ),
             yaxis = list(title = input$dv),
             dragmode =  "select") 
  })
  
  
  
  output$corr <- renderGvis({
    d <- datasetInput()[,sapply(datasetInput(),is.integer)|sapply(datasetInput(),is.numeric)] 
    cor <- as.data.frame(round(cor(d), 2))
    cor <- cbind(Variables = rownames(cor), cor)
    gvisTable(cor,options=list(titleTextStyle="{color:'black', fontName:'Courier', fontSize:16}"))
    
  })  
  
  
  # selected model
  output$model <- renderPrint({
    options(scipen = 5)
    summary(model()) 
  })
  
  
  
  # selected model factors
  output$modelfactors <- renderPrint({
    
    
    if(input$p=='hh'|input$p=='ii' ){
      
      options(scipen = 999)
      round(coef(model()),digits=4)
    }
    else
    {
      options(scipen = 999)
      round(exp(coef(model())),digits=4)
    }  
    
  })
  
  
  
  ##Predsdataset <- reactive(list({ predict(model(),test_data)}))
  
  ###Observed for Test data and predicted 
  
  
  # Observed Vrs Fitted for Train data 
  # Predictions  view 
  output$Preds <- DT::renderDataTable({ 

      datasetInputfit= cbind(datasetInput()[,], FittedLossCost=model()$fitted.values)
      datasetInputfit$ScoreGrp <- 
        cut(datasetInputfit$FittedLossCost, breaks=c(0,100,150,200,250,300,400,500,600,800,
                                                     5000), right = FALSE,dig.lab=6)  
      datasetInputfit$ScoreGrp<-ifelse(datasetInputfit$ScoreGrp=='[0,100)',1,datasetInputfit$ScoreGrp)
      
      DT::datatable(datasetInputfit[,])  #input$show_vars, drop = FALSE
 
  })
  
  
  
  output$Preds_mse <- renderTable({

      head(cbind( Train_MSE=mean((datasetInput()$LossCostMetric - model()$fitted.values)^2),AICValue=AICtweedie(model(),k=2)))
 
  })
  
  
  
  
  
  
  output$obsvrsfitted <- renderPlotly({
    
    predobsdat= cbind(datasetInput()[,],FittedLossCost=model()$fitted.values)  # Predsdataset
    
    predobsdat <- predobsdat %>% filter(!is.na(FittedLossCost))
    
    
    plot_ly(predobsdat, x = ~ predobsdat[,input$iv], y = ~ FittedLossCost, 
            type='scatter',mode='markers',text = ~paste("MakeName: ", MakeName, '<br>ModelYear:',ModelYear, '<br>SeriesName:',SeriesName,'<br> TotalClaims :',totalclaims,'<br> ObservedLossCost $:',LossCostMetric),
            marker = list(size = 10,color='#d62728'))  %>%    #  mode='lines'       
      layout(title = paste(input$iv ,"vs Fitted Loss Cost"),   
             xaxis = list(title = input$iv ),
             yaxis = list(title = "Fitted Loss Cost"),
             dragmode =  "select" ) #
    # plot_bgcolor = "6A446A")
    
  }) 
  
  
  output$obsvrsfitted_obs <- renderPlot({
    x    <- datasetInput()[,input$dv]  
    bins <- seq(min(x), max(x), length.out = input$bins_dv2 + 1)
    hist(x, breaks = bins, col = 'yellow', border = 'blue', main = 'Distribution of Observed Values', xlab = input$dv)
  })
  
  output$obsvrsfitted_fits <- renderPlot({
    
    predobsdat= cbind(datasetInput()[,],FittedLossCost=model()$fitted.values)  # Predsdataset
    predobsdat <- predobsdat %>% filter(!is.na(FittedLossCost))
    
    x    <- predobsdat$FittedLossCost  
    bins <- seq(min(x), max(x), length.out =  input$bins_dv3 + 1)
    hist(x, breaks = bins, col = 'yellow', border = 'blue', main = 'Distribution of Fitted Values', xlab = 'FittedLossCost',  prob = TRUE)
  })
  
  
 
  
  output$regressline <- renderPlot({
    par(mfrow = c(3,4),bg = 'olivedrab1')
    termplot(model(),main="Slope of Selected Predictor") 
    
  })
  
  output$parpplot <- renderPlot({
    
    
    par(mar=c(5, 4, 4, 6) + .2,bg = 'olivedrab1')
    hist(x=datasetInput()[input$iv], ylim=c(0,1),axes=FALSE,ylab=" ", xlab=input$iv,freq=FALSE ,col="darkblue", yaxt = "n")  
    mtext("Density",side=4,col="darkblue",line=4) 
    axis(4,ylim=c(0,1),col="darkblue",col.axis="darkblue",las=1)
    par(new=TRUE)
    termplot(model(), terms=input$iv ,lwd.term = 2 ,xlab=" ", main = "Predictor Distibution with Slope")
  })
  
  
  
  
  output$parpplot2 <- renderPlot({
    
    par(mfrow = c(1,2))
    termplot(model(), terms=input$iv,main='Partial Plot for Predictor')    
    termplot(model(), terms=input$iv, partial.resid = TRUE,smooth = panel.smooth,main='Scatter plot with Fitted Model Line')    
  })
  
  
  
  output$AvgFitTables <- renderGvis({ 

      datasetInputfit= cbind(datasetInput()[,], FittedLossCost=model()$fitted.values)
 
      AggtableObsFit<- aggregate(cbind(LossCostMetric,FittedLossCost ) ~ datasetInputfit[,input$iv] , data = datasetInputfit, mean)
      gvisTable(AggtableObsFit)
      
  
  })
  
  
  output$AvgFitPlots <- renderPlot({ 

      datasetInputfit= cbind(datasetInput()[,], FittedLossCost=model()$fitted.values)
      
      AggtableObsFit<- aggregate(cbind(LossCostMetric,FittedLossCost ) ~ datasetInputfit[,input$iv], data = datasetInputfit, mean)
      counts <- table(datasetInputfit[,input$iv])
      counts <- prop.table(counts)
      par(mar=c(5, 4, 4, 6) + 0.1)
      
      barplot(counts, pch=15,  xlab=" ", ylab=" ", ylim=c(0,max(counts)+.1), 
              axes=FALSE,  col="green")
      mtext("Proportion",side=4,col="green",line=4) 
      axis(4,  col="green",col.axis="green",las=1)
      par(new=TRUE)
      plot(1:length(AggtableObsFit$LossCostMetric),AggtableObsFit$LossCostMetric, col="red", type="b",pch=19,lwd=2, ylab=" ", xlab=" ",xaxt = "n",
           ylim=c(min(AggtableObsFit$LossCostMetric,AggtableObsFit$FittedLossCost),max(AggtableObsFit$LossCostMetric,AggtableObsFit$FittedLossCost)))
      par(new=TRUE)
      plot(1:length(AggtableObsFit$FittedLossCost),AggtableObsFit$FittedLossCost, col="blue", type="b",pch=18,xlab=" ", ylab=" ",lwd=2, axes=FALSE,xaxt = "n",
           ylim=c(min(AggtableObsFit$LossCostMetric,AggtableObsFit$FittedLossCost),max(AggtableObsFit$LossCostMetric,AggtableObsFit$FittedLossCost)))
      legend("topleft", legend=c("Observed Average", "Fitted Model Average"),
             col=c("red", "blue"), lty=1:3, cex=1)
      grid(nx=NULL,ny=NULL)
 
  })
  
  
  ##Score Tables 
  output$AvgScoreTables <- renderGvis({ 
    
    datasetInputfit= cbind(datasetInput()[,], FittedLossCost=model()$fitted.values)
    
    datasetInputfit$ScoreGrp <- 
      cut(datasetInputfit$FittedLossCost, breaks=c(0,100,150,200,250,300,400,500,600,800,
                                                   5000), right = FALSE,dig.lab=6)  
    datasetInputfit$ScoreGrp<-ifelse(datasetInputfit$ScoreGrp=='[0,100)',1,datasetInputfit$ScoreGrp)
    
    
    AggtableObsFit<- aggregate(cbind(LossCostMetric,FittedLossCost ) ~ datasetInputfit$ScoreGrp, data = datasetInputfit, mean)
    gvisTable(AggtableObsFit)
    
  })
  
  
  ###Score Graphs 
  
  
  output$AvgScorePlots <- renderPlot({ 
    
    datasetInputfit= cbind(datasetInput()[,], FittedLossCost=model()$fitted.values)
    datasetInputfit$ScoreGrp <- cut(datasetInputfit$FittedLossCost, breaks=c(0,100,150,200,250,300,400,500,600,800,
                                                                             5000), right = FALSE,dig.lab=6)  
    datasetInputfit$ScoreGrp<-ifelse(datasetInputfit$ScoreGrp=='[0,100)',1,datasetInputfit$ScoreGrp)
    
    
    AggtableObsFit<- aggregate(cbind(LossCostMetric,FittedLossCost ) ~ datasetInputfit$ScoreGrp, data = datasetInputfit, mean)
    counts <- table(datasetInputfit$ScoreGrp)
    counts <- prop.table(counts)
    par(mar=c(5, 4, 4, 6) + 0.1)
    
    barplot(counts, pch=15,  xlab=" ", ylab=" ", ylim=c(0,max(counts)+.1), 
            axes=FALSE,  col="green")
    mtext("Proportion",side=4,col="green",line=4) 
    axis(4,  col="green",col.axis="green",las=1)
    par(new=TRUE)
    plot(1:length(AggtableObsFit$LossCostMetric),AggtableObsFit$LossCostMetric, col="red", type="b",pch=19,lwd=2, ylab=" ", xlab=" ",xaxt = "n",
         ylim=c(min(AggtableObsFit$LossCostMetric,AggtableObsFit$FittedLossCost),max(AggtableObsFit$LossCostMetric,AggtableObsFit$FittedLossCost)))
    par(new=TRUE)
    plot(1:length(AggtableObsFit$FittedLossCost),AggtableObsFit$FittedLossCost, col="blue", type="b",pch=18,xlab=" ", ylab=" ",lwd=2, axes=FALSE,xaxt = "n",
         ylim=c(min(AggtableObsFit$LossCostMetric,AggtableObsFit$FittedLossCost),max(AggtableObsFit$LossCostMetric,AggtableObsFit$FittedLossCost)))
    legend("topleft", legend=c("Observed Average", "Fitted Model Average"),
           col=c("red", "blue"), lty=1:3, cex=1)
    grid(nx=NULL,ny=NULL)
    
    
  })
  
  
  
  # Predictions  with VIN
  output$PredsVins <- DT::renderDataTable({ 
    
    load("Fullsymbols.RData")
    
    preddat<-predict(model(),Fullsymbols,type="response")
    
    predobsdatfull= cbind(Fullsymbols[,],FittedLossCost=preddat) 
    
    predobsdatfull$ScoreGrp <- 
      cut(predobsdatfull$FittedLossCost, breaks=c(0,100,150,200,250,300,400,500,600,800,
                                                  18000000), right = FALSE,dig.lab=6)    
    
    predobsdatfull$ScoreGrp<-ifelse(predobsdatfull$ScoreGrp=='[0,100)',1, predobsdatfull$ScoreGrp)
    
    
    DT::datatable(predobsdatfull[,])  
    
  })
  
  
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$report_format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('report.Rmd')
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd')
      
      library(rmarkdown)
      out <- render('report.Rmd', switch(
        input$report_format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    })
  
  
}





shinyApp(ui = ui, server = server)


###Gini Index in random forest 


