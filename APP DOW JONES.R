#----
#title: "APPLICATION dans R POUR LE CALCUL DES INDICATEURS TECHNIQUES DU DOW JONES "
#author: "Ikram Fella, Sara Elaasri"
#date: "19/12/2021"
#----

library(tidyverse)
library(flexdashboard)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(quantmod)
library(DT)

ui <- dashboardPage(skin="red",
  dashboardHeader (title = "Analyse technique DJI"),
  dashboardSidebar(helpText("  Choisir intervalle de temps"),
   dateRangeInput("dates","intervalle de temps",start = "2015-01-01",
            end = as.character(Sys.Date())),
   br(),
   br(),
   sidebarMenu(
     menuItem("Afficher les donnees", tabName = "ViewData", icon = icon("info")),
     
     br(),
     
     
     menuItem("Les indicateurs de tendance", tabName = "indtech", icon = icon("chart-line")),
   br(),
     menuItem("Graph", tabName = "graph", icon = icon("poll")) )
   ),
   
   dashboardBody(img(src ="https://www.pefondes.eu/wp-content/uploads/2019/10/trader-indice-dow-jones.jpg", height = 140, width = 500),
     tabItems(
       #graph
       tabItem(
         tabName = "graph",
         h1("Graph de Dowjones avec les indicateurs"),
         tabsetPanel(
             tabPanel("graphe complet", plotOutput("plot5",height= 500) )
           )
           
         ),
     # ViewData
     tabItem(tabName = "ViewData",
             h1("Les donnes de Dowjones"),
             tabsetPanel(
               
               tabPanel("cours",tableOutput("table1") ),   
               tabPanel("indicateurs techniques",tableOutput("table2") )
             )
             
     ),
     
    
     
     #indtech
     tabItem(tabName ="indtech",
             h1("Quelque indicateurs de tendances"),
             tabsetPanel(
               tabPanel("cours", plotOutput("plot",height= 500) ),
               tabPanel("SMA", plotOutput("plot1",height= 500) ),
               tabPanel("MACD", plotOutput("plot2",height= 500) ),
               tabPanel("RSI", plotOutput("plot3",height= 500) ),
               tabPanel("ROC", plotOutput("plot4",height= 500) )
              
               
             ) 
     )
     
   )
   )
  )


server <- function(input, output) {
  output$table1 <- renderTable(dataInput <- getSymbols("^DJI", src = "yahoo",
                                                      from = input$dates[1],
                                                      to = input$dates[2],
                                                      period     = "daily",
                                                      auto.assign = FALSE))
  
  output$table2 <- renderTable ( {
    dataInput<-getSymbols("^DJI", src = "yahoo",
                          from = input$dates[1],
                          to = input$dates[2],
                          auto.assign = FALSE) 
    
# fonctions
                                                      
#simple moving average (SMA)                                          
 SMA20 <-SMA(Cl(dataInput),n=20) 
 colnames(SMA20) <- "SMA20"
 
SMA50 <-SMA(Cl(dataInput),n=50)
colnames(SMA50) <- "SMA50"
 #MACD
macd<- MACD(Cl(dataInput), nFast=12, nSlow=26,
             nSig=9, percent=FALSE)

#RSI
rsi <- RSI(Cl(dataInput), SMA, n=14)
colnames(rsi) <- "RSI"
#Rate of Change (ROC)
roc <- ROC(Cl(dataInput),n=2)
colnames(roc) <- "ROC"
data1 <-cbind(SMA20,SMA50,roc,rsi,macd,dataInput$date)
data1
})
  output$plot <- renderPlot({
    dataInput <- getSymbols("^DJI", src = "yahoo",
                            from = input$dates[1],
                            to = input$dates[2],
                            auto.assign = FALSE)
    chartSeries( dataInput,type="candlesticks",name="^DJI" ,theme=chartTheme('white'),addVo())
    
    }) 
  output$plot1 <- renderPlot({
      dataInput <- getSymbols("^DJI", src = "yahoo",
                              from = input$dates[1],
                              to = input$dates[2],
                              auto.assign = FALSE)
      
      chartSeries (dataInput,name= "^DJI",theme = chartTheme("black"), 
                   TA=c(
                     addVo(), 
                     addSMA(n=200,on=1, col = 'white'),
                     addSMA(n=50,on=1,col = 'orange'),
                     addSMA(n=20,on=1,col = 'red')
                   )
                   
      )
  })
  output$plot2 <- renderPlot({
    dataInput <- getSymbols("^DJI", src = "yahoo",
                            from = input$dates[1],
                            to = input$dates[2],
                            auto.assign = FALSE)
    chartSeries (dataInput,name= "^DJI",theme = chartTheme('white'), 
                 TA=c(
                   addVo(), 
                   addMACD(fast=12,slow=26,signal=9,type="EMA")
                 ))
    
  })
  output$plot3 <- renderPlot({
    dataInput <- getSymbols("^DJI", src = "yahoo",
                            from = input$dates[1],
                            to = input$dates[2],
                            auto.assign = FALSE)
    
    chartSeries (dataInput,name= "^DJI",theme=chartTheme("black"), 
                 TA=c( addVo(), addRSI())
                 
    )
    
    
    
  })
  output$plot4 <- renderPlot({
    dataInput <- getSymbols("^DJI", src = "yahoo",
                            from = input$dates[1],
                            to = input$dates[2],
                            auto.assign = FALSE)
    
    chartSeries (dataInput,name= "^DJI",theme = chartTheme("white"), 
                 TA=c(addVo(), addROC(n=50,col = 'red'))
                      
                 
    )
    
    
  })
  output$plot5 <- renderPlot({
    dataInput <- getSymbols("^DJI", src = "yahoo",
                            from = input$dates[1],
                            to = input$dates[2],
                            auto.assign = FALSE)
    
    chartSeries (dataInput,name= "^DJI",theme = chartTheme("white"), 
                 TA=c(addVo(),addMACD(fast=12,slow=26,signal=9,type="EMA"),
                   addSMA(n=200,col = 'orange'),
                   addSMA(n=50,col = 'green'),
                   addSMA(n=20,col = 'black'),
                  addROC(n=50,col = 'red'),
                    addRSI()
                  
                 
    ))
    
    
  })
}
shinyApp(ui, server)


