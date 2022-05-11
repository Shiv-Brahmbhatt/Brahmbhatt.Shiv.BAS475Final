library(plotly)
library(dplyr)
library(ggplot2)     
library(ggeasy) 
library(fpp3)
library(tidyverse)
library(lubridate)
library(fable)
library(fabletools)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(ggeasy)
library(ggthemes)

file_path <- 'Federer_Nadal.csv'
g_trends <- read.csv(file_path, skip = 2)

names(g_trends) <- c("Month", "FedererInterest", "NadalInterest")
g_trends$Month <- yearmonth(g_trends$Month)
g_trends <- tsibble(g_trends)


g_trends$FedererInterest <- as.numeric(
  ifelse(g_trends$FedererInterest == "<1", 0, g_trends$FedererInterest)
)

g_trends$NadalInterest <- as.numeric(
  ifelse(g_trends$NadalInterest == "<1", 0, g_trends$NadalInterest)
)





library(shiny)

ui <- dashboardPage(
  dashboardHeader(title = "Federer vs. Nadal Interest"),
  dashboardSidebar(
    
    # Menu Tabs
    sidebarMenu(
      menuItem("Instructions", tabName = "instructions", icon = icon("fa-solid fa-book")),
      menuItem("Time Series Plot", tabName = "timeseriesplot", icon = icon("fa-regular fa-chart-line")),
      menuItem("Varied Plots", tabName = "variedplots", icon = icon("fa-regular fa-chart-line")),
      menuItem("Forecast", tabName = "forecast", icon = icon("fa-regular fa-chart-line")),
      menuItem("Simple Models", tabName = "simplemodels", icon = icon("fa-regular fa-chart-line")),
      menuItem("Exponential Smoothing", tabName = "ets", icon = icon("fa-regular fa-chart-line")),
      menuItem("ARIMA Models", tabName = "arima", icon = icon("fa-regular fa-chart-line")),
      menuItem("Interpretation", tabName = "interpretation", icon = icon("fa-solid fa-book-open")),
      
      dateRangeInput("dates", "Date Range", start = ("2004-01-01"), end = NULL, min = NULL,
                     max = NULL, format = "yyyy-mm", startview = "month", weekstart = 0,
                     language = "en", separator = " to ", width = NULL),
      
      selectizeInput("chooseplot", choices = NULL, label = h3("Choose a Plot")),
      
      selectizeInput("choosesimplemodel", choices = NULL, label = h3("Choose a Simple Model")),
      
      selectizeInput("chooseetsmodel", choices = NULL, label = h3("Choose a ETS Model")),
      
      selectizeInput("choosearimamodel", choices = NULL, label = h3("Choose a ARIMA Model"))
      
      )
    
    
  ),
  
  dashboardBody(
    
    tabItems(
      tabItem(
        tabName = "instructions",
        textOutput('instructions')
      ),
      
      tabItem(
        tabName = "timeseriesplot",
        box(plotlyOutput("TSP"), width = 16)
        
      ),
      
      tabItem(
        tabName = "variedplots",
        hr(),
        fluidRow(column(5, verbatimTextOutput("value"))),
        box(plotOutput("plot"), width = 16)
      ),
      
      tabItem(
        tabName = "forecast",
        box(plotOutput("FedForecast"), width = 16),
        box(plotOutput("NadForecast"), width = 16)
      ),
      
      tabItem(
        tabName = "simplemodels",
        hr(),
       # fluidRow(column(5, verbatimTextOutput("value"))),
        box(plotOutput("simplemodelsplot"), width = 16)
      ),
      
      tabItem(
        tabName = "ets",
        hr(),
      #  fluidRow(column(5, verbatimTextOutput("value"))),
        box(plotOutput("etsplot"), width = 16)
      ),
      
      tabItem(
        tabName = "arima",
        hr(),
        #  fluidRow(column(5, verbatimTextOutput("value"))),
        box(plotOutput("arimaplot"), width = 16)
      ),
      
      tabItem(
        tabName = "interpretation",
        textOutput('interpretation')
      )
    )
  )
)

server <- function(input, output, session) {
  output$value <- renderPrint({ input$dates })
  
  output$instructions <- renderText({paste("This app displays contains a full time series of Roger Federer and Rafael Nadal from 2004-Present.The app includes seasonal plots, autocorrelation plots, and decomposition plots for the respective players. Additionally, the app also includes a forecasting feature that predicts 3 years into the future for each player. Furthermore, a whole host of forecasting models have been added such as, naive, seasonal naive, mean, drift, ETS, and ARIMA models. On the left hand side, you will find various tabs and a date input feature. Click the Time Series Plot tab and input a month and year. The graph will update based on your input. This graph is interactive, so you can hover over the respective lines. You will also to be able to choose a plot on the left hand side. This corresponds to the Varied Plots tab. Pick the graph you would like to view and the plot will change accordingly. Similarly, the Simple Models, ETS, and ARIMA tabs correspond with their respective models. Upon clicking on the tab, you can change the corresponding graph output. You are also able to view forecasting plots for both players by clicking on the Forecast tab. This will display a forecast for interest for the next 3 years using the TSLM method. Lastly, time series interpretations can be found under the Interpretations tab on the left hand side.")})
  output$interpretation <- renderText({paste("Federer Interpretation: There is a mostly upward linear trend seen. Federer's interest has been declining over the past few months. This is most likely due to the fact that he is not on tour right now and is recovering from surgery. There seems to be a strong seasonal pattern, especially in times where Federer wins a grand slam title. This is most noticeable in June-July when Federer wins the Wimbledon title. Nearly every June-July, there is an increase in Federer searches. After running the decomposition, it can be noted that the variation in the data is most likely due to random chance. Most of the autocorrelations are not statistically significant, meaning the series is not correlated with itself given the lag. The manual ARIMA was seasonally differenced and the ACF and PACF charts were evaluated. Non-seasonal AR was set to 0, the data was differened once, and it had a moving average of 1. The seasonal AR was set to 2, it was differenced once, and had a moving average of 1.  \nNadal Interpretations: Similar to Federer, there is an upward linear trend, although it is not as strong as Federer's. There seems to be a strong seasonal pattern for Nadal as well. This can be seen when he wins major titles, such as grand slams. Nadal is synonymous with the French Open, which is typically played in late May to early June. This is shown on the seasonal plot, as Nadal's interest generally increases there. After running the decomposition for Nadal, it can be noted that the variation in the data is most likely due to randomness. The majority of autocorrelations are not statistically significant, meaning the series is not correlated with itself given the lag. The manual ARIMA was seasonally differenced and the ACF and PACF charts were evaluated. Non-seasonal AR was set to 0, the data was differened once, and it had a moving average of 1. The seasonal AR was set to 0, it was differenced once, and had a moving average of 1.")})
  

  
  updateSelectizeInput(session, "chooseplot", selected= "Federer_Seasonal",choices = list("Federer Seasonal" = "Federer_Seasonal",
                                                                                          "Nadal Seasonal" = "Nadal_Seasonal",
                                                                                          "Federer Decomposition" = "Federer_Decomposition",
                                                                                          "Nadal Decomposition" = "Nadal_Decomposition",
                                                                                          "Federer ACF" = "Federer_ACF",
                                                                                          "Nadal ACF" = "Nadal_ACF"), 
                       server = T)
  
  updateSelectizeInput(session, "choosesimplemodel", selected= "Federer_Naive",choices = list("Federer Naive" = "Federer_Naive",
                                                                                          "Nadal Naive" = "Nadal_Naive",
                                                                                          "Federer Seasonal Naive" = "Federer_SNaive",
                                                                                          "Nadal Seasonal Naive" = "Nadal_SNaive",
                                                                                          "Federer Mean" = "Federer_Mean",
                                                                                          "Nadal Mean" = "Nadal_Mean",
                                                                                          "Federer Drift" = "Federer_Drift",
                                                                                          "Nadal Drift" = "Nadal_Drift"), 
                       server = T)
  
  updateSelectizeInput(session, "chooseetsmodel", selected= "Federer_Holts",choices = list("Federer Holts" = "Federer_Holts",
                                                                                              "Nadal Holts" = "Nadal_Holts",
                                                                                              "Federer Holts Winter" = "Federer_HWinter",
                                                                                              "Nadal Holts Winter" = "Nadal_HWinter"), 
                       server = T)
  
  updateSelectizeInput(session, "choosearimamodel", selected= "Federer_Manual",choices = list("Federer Manual ARIMA" = "Federer_Manual",
                                                                                           "Nadal Manual ARIMA" = "Nadal_Manual",
                                                                                           "Federer Auto ARIMA" = "Federer_Auto",
                                                                                           "Nadal Auto ARIMA" = "Nadal_Auto"), 
                       server = T)
  
  output$TSP <- renderPlotly({
    ggplot()+
      geom_line(data = g_trends, aes(x=as.Date(Month), y=FedererInterest), color = "Blue")+
      geom_line(data = g_trends, aes(x=as.Date(Month), y=NadalInterest), color = "Red")+
      labs(x="Month", y="Interest") + 
      ggtitle("Federer vs Nadal Interest: 2004 - Present") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme_fivethirtyeight() +
      theme(axis.title = element_text())+
      xlim(c(input$dates[1],input$dates[2]))
  })
  
  
  
  observe({
    
    
    if(input$chooseplot=="Federer_Seasonal")
    {
      output$plot<-renderPlot({ 
        p=g_trends %>% 
          gg_season(FedererInterest, labels = "none") +
          labs( title = "Seasonal Plot: Roger Federer Interest", x = "Month", y = "Federer Interest") +
          theme_fivethirtyeight() + 
          theme(axis.title = element_text())
        plot(p)
      })
    }
    
    
    if(input$chooseplot=="Nadal_Seasonal")
    { 
      output$plot<-renderPlot({ 
        p=g_trends %>% 
          gg_season(NadalInterest, labels = "none") +
          labs( title = "Seasonal Plot: Rafael Nadal Interest", x = "Month", y = "Nadal Interest") +
          theme_fivethirtyeight() + 
          theme(axis.title = element_text())
        plot(p)
      }) 
    }
    
    if(input$chooseplot=="Federer_ACF")
    {
      output$plot<-renderPlot({ 
        p=g_trends %>% 
          ACF(FedererInterest, lag = 7) %>% 
          autoplot() + labs (title = "Autocorrelation: Federer Interest") +
          theme_fivethirtyeight() + 
          theme(axis.title = element_text())
        plot(p)
      })
    }
    
    if(input$chooseplot=="Nadal_ACF")
    {
      output$plot<-renderPlot({ 
        p=g_trends %>% 
          ACF(NadalInterest, lag = 7) %>% 
          autoplot()+ labs (title = "Autocorrelation: Nadal Interest") +
          theme_fivethirtyeight() + 
          theme(axis.title = element_text())
        plot(p)
      })
    }
    
    if(input$chooseplot=="Federer_Decomposition")
    {
      output$plot<-renderPlot({ 
        p=g_trends %>% 
          model(
            STL(FedererInterest ~ trend(window = 21) + 
                  season (window = 12),
                robust = TRUE)) %>% 
          components() %>% 
          autoplot() +
          labs(title = "Federer STL Decomposition")
        plot(p)
      })
    }
    
    if(input$chooseplot=="Nadal_Decomposition")
    {
      output$plot<-renderPlot({ 
        p=g_trends %>% 
          model(
            STL(NadalInterest ~ trend(window = 21) + 
                  season (window = 12),
                robust = TRUE)) %>% 
          components() %>% 
          autoplot() +
          labs(title = "Nadal STL Decomposition")
        plot(p)
      })
    }
    
    if(input$choosesimplemodel=="Federer_Naive")
    {
      output$simplemodelsplot<-renderPlot({ 
        p=g_trends %>% 
          model(naive = NAIVE(FedererInterest)) %>% 
          forecast(h = "3 years") %>% 
          autoplot(g_trends)+
          labs(title = "3 Year Forecast of Federer Interest: Naive", x = "Month", y = "Federer Interest") +
          easy_center_title()
        plot(p)
      })
    }
    
    if(input$choosesimplemodel=="Nadal_Naive")
    {
      output$simplemodelsplot<-renderPlot({ 
        p=g_trends %>% 
          model(naive = NAIVE(NadalInterest)) %>% 
          forecast(h = "3 years") %>% 
          autoplot(g_trends) +
          labs(title = "3 Year Forecast of Nadal Interest: Naive", x = "Month", y = "Nadal Interest") +
          easy_center_title()
        plot(p)
      })
    }
    
    if(input$choosesimplemodel=="Federer_SNaive")
    {
      output$simplemodelsplot<-renderPlot({ 
        p=FedSeasonalModel <- g_trends %>% 
          model(snaive = SNAIVE(FedererInterest ~ lag(12)))
        FedSeasonalModel %>% forecast(h = "3 years") %>% 
          autoplot(g_trends) +
          labs(title = "3 Year Forecast of Federer Interest: Seasonal Naive", x = "Month", y = "Federer Interest") +
          easy_center_title()
      })
    }
    
    if(input$choosesimplemodel=="Nadal_SNaive")
    {
      output$simplemodelsplot<-renderPlot({ 
        p=NadSeasonalModel <- g_trends %>% 
          model(snaive = SNAIVE(NadalInterest ~ lag(12)))
        NadSeasonalModel %>% forecast(h = "3 years") %>% 
          autoplot(g_trends) +
          labs(title = "3 Year Forecast of Nadal Interest: Seasonal Naive", x = "Month", y = "Nadal Interest") +
          easy_center_title()
      })
    }
    
    if(input$choosesimplemodel=="Federer_Mean")
    {
      output$simplemodelsplot<-renderPlot({ 
        p=FedMeanModel <- g_trends %>% 
          model(mean = MEAN(FedererInterest))
        FedMeanModel %>% forecast(h = "3 years") %>% 
          autoplot(g_trends) +
          labs(title = "3 Year Forecast of Federer Interest: Mean", x = "Month", y = "Federer Interest") +
          easy_center_title()
      })
    }
    
    if(input$choosesimplemodel=="Nadal_Mean")
    {
      output$simplemodelsplot<-renderPlot({ 
        p=NadMeanModel <- g_trends %>% 
          model(mean = MEAN(NadalInterest))
        NadMeanModel %>% forecast(h = "3 years") %>% 
          autoplot(g_trends) +
          labs(title = "3 Year Forecast of Nadal Interest: Mean", x = "Month", y = "Nadal Interest") +
          easy_center_title()
      })
    }
    
    if(input$choosesimplemodel=="Federer_Drift")
    {
      output$simplemodelsplot<-renderPlot({ 
        p=FedDriftModel <- g_trends %>% 
          model(Drift = NAIVE(FedererInterest~drift()))
        FedDriftModel %>% forecast(h = "3 years") %>% 
          autoplot(g_trends) +
          labs(title = "3 Year Forecast of Federer Interest: Drift", x = "Month", y = "Federer Interest") +
          easy_center_title()
      })
    }
    
    if(input$choosesimplemodel=="Nadal_Drift")
    {
      output$simplemodelsplot<-renderPlot({ 
        p=NadDriftModel <- g_trends %>% 
          model(Drift = NAIVE(NadalInterest~drift()))
        NadDriftModel %>% forecast(h = "3 years") %>% 
          autoplot(g_trends) +
          labs(title = "3 Year Forecast of Nadal Interest: Drift", x = "Month", y = "Nadal Interest") +
          easy_center_title()
      })
    }
    
    if(input$chooseetsmodel=="Federer_Holts")
    {
      output$etsplot<-renderPlot({ 
        p=FedHoltModel <- g_trends %>% 
          model(HoltsMethod = ETS(FedererInterest~ error("A") +
                                    trend("A") + season("N")))
        FedHoltModel %>% forecast(h = "3 years") %>% 
          autoplot(g_trends) +
          labs(title = "3 Year Forecast of Federer Interest: Holt's", x = "Month", y = "Federer Interest") +
          easy_center_title()
      })
    }
    
    if(input$chooseetsmodel=="Nadal_Holts")
    {
      output$etsplot<-renderPlot({ 
        p=NadHoltModel <- g_trends %>% 
          model(HoltsMethod = ETS(NadalInterest~ error("A") +
                                    trend("A") + season("N")))
        NadHoltModel %>% forecast(h = "3 years") %>% 
          autoplot(g_trends) +
          labs(title = "3 Year Forecast of Nadal Interest: Holt's", x = "Month", y = "Nadal Interest") +
          easy_center_title()
      })
    }
    
    if(input$chooseetsmodel=="Federer_HWinter")
    {
      output$etsplot<-renderPlot({ 
        p=FedHoltWinter <- g_trends %>%
          model(
            additive = ETS(FedererInterest ~ error("A") + trend("A") +
                             season("A")),
            multiplicative = ETS(FedererInterest ~ error("M") + trend("A") +
                                   season("M"))
          )
        FedHoltWinter %>% 
          forecast(h = "3 years") %>%
          autoplot(g_trends, level = NULL) +
          labs(title="3 Year Forecast of Federer Interest: Holt's Winters",
               x="Month",
               y="Federer Interest") +
          guides(colour = guide_legend(title = "Forecast")) +
          theme_economist_white() + easy_center_title()
      })
    }
    
    if(input$chooseetsmodel=="Nadal_HWinter")
    {
      output$etsplot<-renderPlot({ 
        p=NadHoltWinter <- g_trends %>%
          model(
            additive = ETS(NadalInterest ~ error("A") + trend("A") +
                             season("A")),
            multiplicative = ETS(NadalInterest ~ error("M") + trend("A") +
                                   season("M"))
          )
        NadHoltWinter %>% 
          forecast(h = "3 years") %>%
          autoplot(g_trends, level = NULL) +
          labs(title="3 Year Forecast of Nadal Interest: Holt's Winters",
               x="Month",
               y="Nadal Interest") +
          guides(colour = guide_legend(title = "Forecast")) + 
          theme_economist_white() + easy_center_title()
      })
    }
    
    if(input$choosearimamodel=="Federer_Manual")
    {
      output$arimaplot<-renderPlot({ 
        p=Fed_MArima <- g_trends %>% 
          model(
            arima011211 = ARIMA(FedererInterest ~ pdq(0,1,1) + PDQ(2,1,1)),
          )
        Fed_MArima %>% forecast(h = "3 years") %>% 
          autoplot(g_trends) +
          labs(title = "3 Year Forecast of Federer Interest: Manual ARIMA", x = "Month", y = "Federer Interest") +
          easy_center_title()
      })
    }
    
    if(input$choosearimamodel=="Nadal_Manual")
    {
      output$arimaplot<-renderPlot({ 
        p=Nad_MArima <- g_trends %>% 
          model(
            arima011011 = ARIMA(NadalInterest ~ pdq(0,1,1) + PDQ(0,1,1)),
          )
        Nad_MArima %>% forecast(h = "3 years") %>% 
          autoplot(g_trends) +
          labs(title = "3 Year Forecast of Nadal Interest: Manual ARIMA", x = "Month", y = "Nadal Interest") +
          easy_center_title()
      })
    }
      
      if(input$choosearimamodel=="Federer_Auto")
      {
        output$arimaplot<-renderPlot({ 
          p=Fed_Arima <- g_trends %>% 
            model(
              auto = ARIMA(FedererInterest, stepwise = FALSE, approx = FALSE),
            )
          Fed_Arima %>% forecast(h = "3 years") %>% 
            autoplot(g_trends) +
            labs(title = "3 Year Forecast of Federer Interest: Auto ARIMA", x = "Month", y = "Federer Interest") +
            easy_center_title()
        })
      
     }
    
    if(input$choosearimamodel=="Nadal_Auto")
    {
      output$arimaplot<-renderPlot({ 
        p=Nad_Arima <- g_trends %>% 
          model(
            auto = ARIMA(NadalInterest, stepwise = FALSE, approx = FALSE),
          )
        Nad_Arima %>% forecast(h = "3 years") %>% 
          autoplot(g_trends) +
          labs(title = "3 Year Forecast of Nadal Interest: Auto ARIMA", x = "Month", y = "Nadal Interest") +
          easy_center_title()
      })
      
    }
    
    
  })
  
  
  
  output$FedForecast <- renderPlot({
    
    FedForecast <- g_trends %>% 
      model(trend_model = TSLM(FedererInterest ~trend()))
    FedForecast %>% forecast(h = "3 years") %>% 
      autoplot(g_trends) + labs(title = "3 Year Federer Forecast: TSLM", x = "Month", y = "Federer Interest")
  })
  
  output$NadForecast <- renderPlot({
    
    NadForecast <- g_trends %>% 
      model(trend_model = TSLM(NadalInterest ~trend()))
    NadForecast %>% forecast(h = "3 years") %>% 
      autoplot(g_trends) + labs(title = "3 Year Nadal Forecast: TSLM", x = "Month", y = "Nadal Interest")
  })
  
}

shinyApp(ui, server)
