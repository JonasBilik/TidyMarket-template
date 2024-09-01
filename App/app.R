
if(!require(shiny)) install.packages("shiny")
  library(shiny)
if(!require(shinydashboard)) install.packages("shinydashboard")
  library(shinydashboard)

if(!require(quantmod)) install.packages("quantmod")
  library(quantmod)


if(!require(memoise)) install.packages("memoise") # required for ??
if(!require(sass)) install.packages("sass") # required for ??
if(!require(pkgconfig)) install.packages("pkgconfig") # required for ggplot2

if(!require(ggplot2)) install.packages("ggplot2") 
  library(ggplot2) # required for plotly
if(!require(plotly)) install.packages("plotly")
  library(plotly)


if(!require(httr)) install.packages("jsonlite")
  library(httr)
if(!require(jsonlite)) install.packages("httr")
  library(jsonlite)
if(!require(dplyr)) install.packages("dplyr")
  library(dplyr)


#Key Schulte
  api_key = "#"




symbols = read.csv("nasdaq_screener_2024.01.10.csv")[,1:2] # Stock symbols from: https://www.nasdaq.com/market-activity/stocks/screener
stock_symbol_inputs = symbols %>% 
  mutate(symbolandname = paste0(Symbol, " (", Name, ")")) %>%
  select(symbolandname) %>% .[[1]]


ui <- dashboardPage(
  
  dashboardHeader(title = "TidyMarkets App"),
  
  dashboardSidebar(
    selectInput("stock_symbol", "Choose a stock:", 
                choices = stock_symbol_inputs
                , multiple = TRUE
    )
    , selectInput("time_interval", "Choose a time interval (in sec.):", 
                  choices = c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 10)
                  , selected = 1
    )
    , box(width = 12,
          verbatimTextOutput("Calc_API_Calls_per_min") # verbatimTextOutput() required! textOutput() not working
    ) 
    , sliderInput("x_range", 
                  "Choose Time Range:", 
                  min = as.POSIXct("03:00:00", format="%H:%M"),
                  max = as.POSIXct("19:00:00", format="%H:%M"),
                  value = c(as.POSIXct("06:00:00", format="%H:%M"), as.POSIXct("18:00:00", format="%H:%M")),
                  timeFormat = "%H:%M", ticks = T)
    , sidebarMenu(
      id = "tabs",
      menuItem("Dashboard", icon = icon("dashboard"), tabName = "about")
      , menuItem("Info", icon = icon("info"), tabName = "info")
    )
  ),
  
  dashboardBody(
    tags$script("
      Shiny.addCustomMessageHandler('stock_symbol', function(value) {
      Shiny.setInputValue('stock_symbol', value);
      });
    "),
    fluidRow(
      tabBox(
        width = 4,
        title = textOutput("box1_name"),
        id = "box1",
        selected = "Chart",
        tabPanel("Chart"
                 , span(textOutput("box1_price"), style = "font-size:300%; font-weight:bold")
                 , span(htmlOutput("box1_change"), style = "font-size:200%; font-weight:bold")
                 , textOutput("box1_previousClose_and_open")
                 , plotlyOutput("box1_chart", width = "100%", height = "100%")
                 , textOutput("box1_dayLowHIgh")
                 , textOutput("box1_yearLowHIgh")
                 , textOutput("box1_volume")
        ),
        tabPanel("News", "... hier folgen die News")
      ),
      tabBox(
        width = 4,
        title = textOutput("box2_name"), 
        id = "box2",
        selected = "Chart",
        tabPanel("Chart" 
                 , span(textOutput("box2_price"), style = "font-size:300%; font-weight:bold")
                 , span(htmlOutput("box2_change"), style = "font-size:200%; font-weight:bold")
                 , textOutput("box2_previousClose_and_open")
                 , plotlyOutput("box2_chart", width = "100%", height = "100%")
                 , textOutput("box2_dayLowHIgh")
                 , textOutput("box2_yearLowHIgh")
                 , textOutput("box2_volume")
        ),
        tabPanel("News", "... hier folgen die News")
      ),
      tabBox(
        width = 4,
        title = textOutput("box3_name"), 
        id = "box3",
        selected = "Chart",
        tabPanel("Chart" 
                 , span(textOutput("box3_price"), style = "font-size:300%; font-weight:bold")
                 , span(htmlOutput("box3_change"), style = "font-size:200%; font-weight:bold")
                 , textOutput("box3_previousClose_and_open")
                 , plotlyOutput("box3_chart", width = "100%", height = "100%")
                 , textOutput("box3_dayLowHIgh")
                 , textOutput("box3_yearLowHIgh")
                 , textOutput("box3_volume")
        ),
        tabPanel("News", "... hier folgen die News")
      ),
      tabBox(
        width = 4,
        title = textOutput("box4_name"), 
        id = "box4",
        selected = "Chart",
        tabPanel("Chart" 
                 , span(textOutput("box4_price"), style = "font-size:300%; font-weight:bold")
                 , span(htmlOutput("box4_change"), style = "font-size:200%; font-weight:bold")
                 , textOutput("box4_previousClose_and_open")
                 , plotlyOutput("box4_chart", width = "100%", height = "100%")
                 , textOutput("box4_dayLowHIgh")
                 , textOutput("box4_yearLowHIgh")
                 , textOutput("box4_volume")
        ),
        tabPanel("News", "... hier folgen die News")
      ),
      tabBox(
        width = 4,
        title = textOutput("box5_name"),
        id = "box5",
        selected = "Chart",
        tabPanel("Chart"
                 , span(textOutput("box5_price"), style = "font-size:300%; font-weight:bold")
                 , span(htmlOutput("box5_change"), style = "font-size:200%; font-weight:bold")
                 , textOutput("box5_previousClose_and_open")
                 , plotlyOutput("box5_chart", width = "100%", height = "100%")
                 , textOutput("box5_dayLowHIgh")
                 , textOutput("box5_yearLowHIgh")
                 , textOutput("box5_volume")
        ),
        tabPanel("News", "... hier folgen die News")
      ),
    
      tabBox(
        width = 4,
        title = textOutput("box6_name"),
        id = "box6",
        selected = "Chart",
        tabPanel("Chart"
                 , textOutput("box6_price")
                 , htmlOutput("box6_change")
                 , textOutput("box6_previousClose_and_open")
                 , plotlyOutput("box6_chart", width = "100%", height = "100%")
                 , textOutput("box6_dayLowHIgh")
                 , textOutput("box6_yearLowHIgh")
                 , textOutput("box6_volume")
        ),
        tabPanel("News", "... hier folgen die News")
      ),
      tabBox(
        width = 4,
        title = textOutput("box7_name"),
        id = "box7",
        selected = "Chart",
        tabPanel("Chart"
                 , textOutput("box7_price")
                 , htmlOutput("box7_change")
                 , textOutput("box7_previousClose_and_open")
                 , plotlyOutput("box7_chart", width = "100%", height = "100%")
                 , textOutput("box7_dayLowHIgh")
                 , textOutput("box7_yearLowHIgh")
                 , textOutput("box7_volume")
        ),
        tabPanel("News", "... hier folgen die News")
      ),
      tabBox(
        width = 4,
        title = textOutput("box8_name"),
        id = "box8",
        selected = "Chart",
        tabPanel("Chart"
                 , textOutput("box8_price")
                 , htmlOutput("box8_change")
                 , textOutput("box8_previousClose_and_open")
                 , plotlyOutput("box8_chart", width = "100%", height = "100%")
                 , textOutput("box8_dayLowHIgh")
                 , textOutput("box8_yearLowHIgh")
                 , textOutput("box8_volume")
        ),
        tabPanel("News", "... hier folgen die News")
      ),
      tabBox(
        width = 4,
        title = textOutput("box9_name"),
        id = "box9",
        selected = "Chart",
        tabPanel("Chart"
                 , textOutput("box9_price")
                 , htmlOutput("box9_change")
                 , textOutput("box9_previousClose_and_open")
                 , plotlyOutput("box9_chart", width = "100%", height = "100%")
                 , textOutput("box9_dayLowHIgh")
                 , textOutput("box9_yearLowHIgh")
                 , textOutput("box9_volume")
        ),
        tabPanel("News", "... hier folgen die News")
      ),
      tabBox(
        width = 4,
        title = textOutput("box10_name"),
        id = "box10",
        selected = "Chart",
        tabPanel("Chart"
                 , textOutput("box10_price")
                 , htmlOutput("box10_change")
                 , textOutput("box10_previousClose_and_open")
                 , plotlyOutput("box10_chart", width = "100%", height = "100%")
                 , textOutput("box10_dayLowHIgh")
                 , textOutput("box10_yearLowHIgh")
                 , textOutput("box10_volume")
        ),
        tabPanel("News", "... hier folgen die News")
      ),
      tabBox(
        width = 4,
        title = textOutput("box11_name"),
        id = "box11",
        selected = "Chart",
        tabPanel("Chart"
                 , textOutput("box11_price")
                 , htmlOutput("box11_change")
                 , textOutput("box11_previousClose_and_open")
                 , plotlyOutput("box11_chart", width = "100%", height = "100%")
                 , textOutput("box11_dayLowHIgh")
                 , textOutput("box11_yearLowHIgh")
                 , textOutput("box11_volume")
        ),
        tabPanel("News", "... hier folgen die News")
      ),
      tabBox(
        width = 4,
        title = textOutput("box12_name"),
        id = "box12",
        selected = "Chart",
        tabPanel("Chart"
                 , textOutput("box12_price")
                 , htmlOutput("box12_change")
                 , textOutput("box12_previousClose_and_open")
                 , plotlyOutput("box12_chart", width = "100%", height = "100%")
                 , textOutput("box12_dayLowHIgh")
                 , textOutput("box12_yearLowHIgh")
                 , textOutput("box12_volume")
        ),
        tabPanel("News", "... hier folgen die News")
      ),
      tabBox(
        width = 4,
        title = textOutput("box13_name"),
        id = "box13",
        selected = "Chart",
        tabPanel("Chart"
                 , textOutput("box13_price")
                 , htmlOutput("box13_change")
                 , textOutput("box13_previousClose_and_open")
                 , plotlyOutput("box13_chart", width = "100%", height = "100%")
                 , textOutput("box13_dayLowHIgh")
                 , textOutput("box13_yearLowHIgh")
                 , textOutput("box13_volume")
        ),
        tabPanel("News", "... hier folgen die News")
      ),
      tabBox(
        width = 4,
        title = textOutput("box14_name"),
        id = "box14",
        selected = "Chart",
        tabPanel("Chart"
                 , textOutput("box14_price")
                 , htmlOutput("box14_change")
                 , textOutput("box14_previousClose_and_open")
                 , plotlyOutput("box14_chart", width = "100%", height = "100%")
                 , textOutput("box14_dayLowHIgh")
                 , textOutput("box14_yearLowHIgh")
                 , textOutput("box14_volume")
        ),
        tabPanel("News", "... hier folgen die News")
      ),
      tabBox(
        width = 4,
        title = textOutput("box15_name"),
        id = "box15",
        selected = "Chart",
        tabPanel("Chart"
                 , textOutput("box15_price")
                 , htmlOutput("box15_change")
                 , textOutput("box15_previousClose_and_open")
                 , plotlyOutput("box15_chart", width = "100%", height = "100%")
                 , textOutput("box15_dayLowHIgh")
                 , textOutput("box15_yearLowHIgh")
                 , textOutput("box15_volume")
        ),
        tabPanel("News", "... hier folgen die News")
      ),
      tabBox(
        width = 4,
        title = textOutput("box16_name"),
        id = "box16",
        selected = "Chart",
        tabPanel("Chart"
                 , textOutput("box16_price")
                 , htmlOutput("box16_change")
                 , textOutput("box16_previousClose_and_open")
                 , plotlyOutput("box16_chart", width = "100%", height = "100%")
                 , textOutput("box16_dayLowHIgh")
                 , textOutput("box16_yearLowHIgh")
                 , textOutput("box16_volume")
        ),
        tabPanel("News", "... hier folgen die News")
      )
      
    )

    # Requirement from FMP (see first paragraph: https://site.financialmodelingprep.com/developer/docs)
    , HTML('<a href="https://financialmodelingprep.com/developer/docs/">Data provided by Financial Modeling Prep</a>')
  )
)

server <- function(input, output, session) {
  
  # Storage of single values 
    rv = reactiveValues()
  
  #Create empty dataframe of required structure (LIVE/API DATA)
    # values <- reactiveValues(test = data.frame())
    values <- reactiveValues()
  
  #Render calculated currently used API-Calls (based on selected Stocks & API-Call frequency)
    output$Calc_API_Calls_per_min <- renderText({
      if(length(input$stock_symbol)>=1) {
        rv$n = (60/as.numeric(input$time_interval)) * length(input$stock_symbol)
      } else rv$n = 0
      
      # print(paste0("API-Calls/Min: ", n))
      paste0(rv$n, "/300 API-Calls/Min ")
    })
    
    
    
    
  #Func for loading new data (LIVE/API DATA)
    get_new_data <- function(symbol) {
      
      #Download
        res = GET(paste0("https://financialmodelingprep.com/api/v3/quote/",symbol,"?apikey=",api_key))
      
      #Saving
        df = fromJSON(rawToChar(res$content)) %>% 
                     mutate(time = as.POSIXct(timestamp, tz="EST", origin = "1970-01-01")) %>% # convert time format, "EST": Time tone NEW YORK
                     mutate(price = round(price, 2),
                            changesPercentage = round(changesPercentage, 2),
                            change = round(change, 2),
                            dayLow = round(dayLow, 2),
                            dayHigh = round(dayHigh, 2),
                            yearLow = round(yearLow, 2),
                            yearHigh = round(yearHigh, 2),
                            open = round(open, 2),
                            previousClose = round(previousClose, 2)) %>%
                     select(price, changesPercentage, change, dayLow, dayHigh, 
                            yearLow, yearHigh, volume, open, previousClose, time) #keep required cols & order them


        
      #load full day data, if no dataset exists
        if(is.null(values[[symbol]])){
          #Settings
            interval = "1min" # 1min, 5min, 15min, 30min, 1hour, 4hour
            start = Sys.Date()
            end = start
          #Request
            res = GET(paste0("https://financialmodelingprep.com/api/v3/historical-chart/",interval,"/",
                             symbol,"?from=",start,"&to=",end,"&apikey=",api_key))
            
            #Check whether data is available or not
            if(is.data.frame(fromJSON(rawToChar(res$content)))){
              
              #Adjust formatting
                previous_data = fromJSON(rawToChar(res$content)) %>% 
                  # select(open, low, high, close, volume, date) %>%
                  mutate(price = round(close, 2), changesPercentage = NA, change = NA, 
                         dayLow = NA, dayHigh = NA, 
                         yearLow = NA, yearHigh = NA, 
                         volume = NA, open = NA, previousClose = NA,
                         time = as.POSIXct(date, origin = "1970-01-01", tz="EST")) %>%
                  select(price, changesPercentage, change, dayLow, dayHigh, 
                         yearLow, yearHigh, volume, open, previousClose, time) %>% #keep required cols & order them %>%
                  arrange(time)
              
              #Bind datasets
                df = rbind(previous_data, df)
            }
              
        }
        
      return(df)
    }  
    
    
    
    
  #save current stock_symbol (e.g. "AMZN") and stock_symbol_and_name (e.g. "AMZN (Amazon.com Inc...)")
    observeEvent(input$stock_symbol, {
      if(length(input$stock_symbol)>=1) {
        for(i in 1:length(input$stock_symbol)) {
          
          #stock_symbol_and_name
            if(nchar(input$stock_symbol[i]) > 26) {
              rv[["stock_symbol_and_name"]][i] = paste0(substr(input$stock_symbol[i], 1, 20), "...)")
            } else rv[["stock_symbol_and_name"]][i] = input$stock_symbol[i]
          
          #stock_symbol
            rv[["stock_symbol"]][i] = sub("\\s*\\(.*", "", input$stock_symbol[i])
          
        }
      }
      
    }, priority = 10)  
    
  
    
  #---------------------------#  
  ## Time-repetitive tasks ####
  #---------------------------#
    observeEvent(reactiveTimer(as.numeric(input$time_interval)*1000)(), {
      
    #Loading new data points (LIVE/API DATA)  
      if(length(input$stock_symbol)>=1) {
        for(stock in input$stock_symbol) {
          stock = sub("\\s*\\(.*", "", stock)
          # print(stock)
          if(is.null(values[[stock]])) {
            values[[stock]] <- get_new_data(symbol = stock) %>%
              filter(!is.na(time))
          } else {
            # print("head(values[[stock]][[1]])")
            # print(head(values[[stock]][[1]]))
            # print("get_new_data(symbol = stock)")
            # print(head(get_new_data(symbol = stock)))
            # print("names(values[[stock]])")
            # print(names(values[[stock]]))
            # print("names(get_new_data(symbol = stock))")
            # print(names(get_new_data(symbol = stock)))
            
            values[[stock]] <- rbind(values[[stock]], get_new_data(symbol = stock)) %>%
              filter(!is.na(time))
            
            # print("working")
          }
          
        }
      }
      
    #Reorder Stocks shown by changesPercentage  
      if(length(input$stock_symbol)>=1 && length(names(values)) == length(input$stock_symbol)) {
        # print("names(values)")
        # print(names(values))
        values_list = reactiveValuesToList(values)
        
        # print("names(values_list):")
        # print(names(values_list))
        
        sort_vals = sapply(values_list, function(x) tail(x$changesPercentage, 1))
        ord = order(sort_vals, decreasing = T)
        # print("sort_vals")
        # print(sort_vals)
        # print("ord")
        # print(ord)
        
        # print("ord:")
        # print(ord)
        # print("c(names(values_list))[ord]")
        # print(c(names(values_list))[ord])
        # print("values_list[c(names(values_list))[ord]]")
        # print(head(values_list[c(names(values_list))[ord]]))
        # values_list = values_list[]
        
        
        values <<- reactiveValues()
        for(l in c(names(values_list))[ord]) {
          
          # print("l:")
          # print(l)
          # 
          values[[l]] <<- values_list[[l]]
          # print("in loop names(values[[l]])")
          # print(names(values))
          
          # print("values[[l]]")
          # print(head(values[[l]]))
        }
        
        # print("input$stock_symbol")
        # print(input$stock_symbol)
        # print("input$stock_symbol[ord]")
        # print(input$stock_symbol[ord])
        
        #reorder displayed inputs
        session$sendCustomMessage("stock_symbol", list(input$stock_symbol[ord]))
        
        # print("input$stock_symbol NEU")
        # print(input$stock_symbol)
        # print("values$names() NEU")
        # print(names(values))
        
        # values = values[order(sapply(values, function(x) tail(x$changesPercentage, 1)), decreasing = T)]
      }  
      
      
    }, priority = 9) 
    
    
  # #Reorder Stocks shown by changesPercentage
  #   observeEvent(reactiveTimer(as.numeric(input$time_interval)*1000)(), {
  # 
  #     if(length(input$stock_symbol)>=1 && length(names(values)) == length(input$stock_symbol)) {
  #       # print("names(values)")
  #       # print(names(values))
  #       values_list = reactiveValuesToList(values)
  #       
  #       # print("names(values_list):")
  #       # print(names(values_list))
  #       
  #       sort_vals = sapply(values_list, function(x) tail(x$changesPercentage, 1))
  #       ord = order(sort_vals, decreasing = T)
  #       # print("sort_vals")
  #       # print(sort_vals)
  #       # print("ord")
  #       # print(ord)
  #       
  #       # print("ord:")
  #       # print(ord)
  #       # print("c(names(values_list))[ord]")
  #       # print(c(names(values_list))[ord])
  #       # print("values_list[c(names(values_list))[ord]]")
  #       # print(head(values_list[c(names(values_list))[ord]]))
  #       # values_list = values_list[]
  #       
  #       
  #       values <<- reactiveValues()
  #       for(l in c(names(values_list))[ord]) {
  # 
  #         # print("l:")
  #         # print(l)
  #         # 
  #         values[[l]] <<- values_list[[l]]
  #         # print("in loop names(values[[l]])")
  #         # print(names(values))
  # 
  #         # print("values[[l]]")
  #         # print(head(values[[l]]))
  #       }
  #       
  #         # print("input$stock_symbol")
  #         # print(input$stock_symbol)
  #         # print("input$stock_symbol[ord]")
  #         # print(input$stock_symbol[ord])
  #       
  #       #reorder displayed inputs
  #       session$sendCustomMessage("stock_symbol", list(input$stock_symbol[ord]))
  #       
  #         # print("input$stock_symbol NEU")
  #         # print(input$stock_symbol)
  #         # print("values$names() NEU")
  #         # print(names(values))
  #       
  #       # values = values[order(sapply(values, function(x) tail(x$changesPercentage, 1)), decreasing = T)]
  #     }
  # 
  #   }, priority = 1)
    
    
    

  
  #Def Func: Rendering of Stock-Price Charts
    render_stock_chart <- function(temp_df, outputname, session) {
  
      #Create reactive range of x-axis
        rv$x_range = c(input$x_range[1]+60*60, input$x_range[2]+60*60)
      
      #Create basis line chart
      line_chart <<- plot_ly(data = temp_df 
                             , x = ~time 
                             , y = ~price
                             , type = "scatter" 
                             , mode = "lines"
                             , fill = "tonexty" # other options: "toself", "tonexty", 'tozeroy' see help
                             ) %>% layout(xaxis = list(title = "",
                                                       range = rv$x_range)
                                          , yaxis = list(title = "" 
                                                         , range = list(min(min(temp_df$price), tail(temp_df$previousClose, 1)) * 0.995
                                                                        , max(max(temp_df$price), tail(temp_df$previousClose, 1)) * 1.005
                                                                        )
                                                         )
                                          , showlegend = FALSE
                                          , plot_bgcolor='#e5ecf6'
                                          ) 
      
      
      # Add the previousClose as red line to the plot
        line_chart <<- line_chart %>% add_lines(
          y = tail(temp_df$previousClose, 1),
          x = range(rv$x_range), #range(temp_df$time),
          line = list(color = "red", dash = "dot"),
          inherit = FALSE,
          showlegend = FALSE
        )
      
      return(line_chart)
    }
  
  
  # Generic Chart & Info Generation ####
    max_length_stock_symbol <- 0
  
    #Keep the numnber of selected symbols up to date
      observeEvent(input$stock_symbol, {
        max_length_stock_symbol <<- ifelse(max_length_stock_symbol>length(input$stock_symbol), 
                                           max_length_stock_symbol, 
                                           length(input$stock_symbol)
                                           )
        })
  
    #
    observe({
      
      for(ii in 1:max_length_stock_symbol) {
        local({
          i <- ii # local() + assigning to new object (i) required, else just the last id would be used

        
        if(!is.null(input$stock_symbol[i])) {
          if(!is.na(input$stock_symbol[i])) {
            
            #Render current infos:
              #box_i_name
              output[[paste0("box",i,"_name")]] <- renderText({
                rv[["stock_symbol_and_name"]][i]
              })
          
              #box_i_price
              output[[paste0("box",i,"_price")]] <- renderText({paste0("$", tail(values[[rv[["stock_symbol"]][i]]]$price, 1))})
            
              #box_i_change
              output[[paste0("box",i,"_change")]] <- renderText({
                if(is.null(values[[rv[["stock_symbol"]][i]]]) | is.null(tail(values[[rv[["stock_symbol"]][i]]]$change, 1))) {
                  out = HTML("")
                } else {
                  if(tail(values[[rv[["stock_symbol"]][i]]]$change, 1) < 0) {
                    out = HTML(paste0("<span style='color:red;'>","-","$", tail(values[[rv[["stock_symbol"]][i]]]$change, 1), " (", tail(values[[rv[["stock_symbol"]][i]]]$changesPercentage, 1), "%)", "</span>"))
                  }
                  if(tail(values[[rv[["stock_symbol"]][i]]]$change, 1) == 0) {
                    out = HTML(paste0("<span style='color:grey;'>","$", tail(values[[rv[["stock_symbol"]][i]]]$change, 1), " (", tail(values[[rv[["stock_symbol"]][i]]]$changesPercentage, 1), "%)", "</span>"))
                  }
                  if(tail(values[[rv[["stock_symbol"]][i]]]$change, 1) > 0) {
                    out = HTML(paste0("<span style='color:green;'>","+","$", tail(values[[rv[["stock_symbol"]][i]]]$change, 1), " (", tail(values[[rv[["stock_symbol"]][i]]]$changesPercentage, 1), "%)", "</span>"))
                  }
                }
                #return:
                out
              })
              
              output[[paste0("box",i,"_dayLowHIgh")]] <- renderText({paste0("Day: ", "$", tail(values[[rv[["stock_symbol"]][i]]]$dayLow, 1), " - ", "$", tail(values[[rv[["stock_symbol"]][i]]]$dayHigh, 1))})
              output[[paste0("box",i,"_yearLowHIgh")]] <- renderText({paste0("Year: ", "$", tail(values[[rv[["stock_symbol"]][i]]]$yearLow, 1), " - ", "$", tail(values[[rv[["stock_symbol"]][i]]]$yearHigh, 1))})
              
              output[[paste0("box",i,"_volume")]] <- renderText({paste0("Volume: ", tail(values[[rv[["stock_symbol"]][i]]]$volume, 1))})
              
              output[[paste0("box",i,"_previousClose_and_open")]] <- renderText({
                paste0("previousClose: ", "$", tail(values[[rv[["stock_symbol"]][i]]]$previousClose, 1), " | ","Open: ", "$", tail(values[[rv[["stock_symbol"]][i]]]$open, 1))
              })
              
              #Render chart
              output[[paste0("box",i,"_chart")]] <- renderPlotly({
                if(is.null(values[[rv[["stock_symbol"]][i]]])) {
                  
                } else {
                  # print('rv[["stock_symbol"]][i]:')
                  # print(rv[["stock_symbol"]][i])
                  # print("names(values):")
                  # print(names(values))
                  # print('values[[rv[["stock_symbol"]][i]]]:')
                  # print(values[[rv[["stock_symbol"]][i]]])
                  render_stock_chart(values[[rv[["stock_symbol"]][i]]], paste0("box",i,"_chart"), session)
                }
              })
              
              
          } else {
            #Render stock name
            output[[paste0("box",i,"_name")]] = renderText({
            })
            #Render current infos
            output[[paste0("box",i,"_price")]] <- renderText({})
            output[[paste0("box",i,"_change")]] <- renderText({})
            output[[paste0("box",i,"_dayLowHIgh")]] <- renderText({})
            output[[paste0("box",i,"_yearLowHIgh")]] <- renderText({})
            output[[paste0("box",i,"_volume")]] <- renderText({})
            output[[paste0("box",i,"_open")]] <- renderText({})
            output[[paste0("box",i,"_previousClose_and_open")]] <- renderText({})
            
            #Render chart
            output[[paste0("box",i,"_chart")]] <- renderPlotly({
              
            })
          }
        } else {
          #Render stock name
          output[[paste0("box",i,"_name")]] = renderText({
          })
          #Render current infos
          output[[paste0("box",i,"_price")]] <- renderText({})
          output[[paste0("box",i,"_change")]] <- renderText({})
          output[[paste0("box",i,"_dayLowHIgh")]] <- renderText({})
          output[[paste0("box",i,"_yearLowHIgh")]] <- renderText({})
          output[[paste0("box",i,"_volume")]] <- renderText({})
          output[[paste0("box",i,"_open")]] <- renderText({})
          output[[paste0("box",i,"_previousClose_and_open")]] <- renderText({})
          
          #Render chart
          output[[paste0("box",i,"_chart")]] <- renderPlotly({
            
          })
        }
        })
      }
      
      
    })
  
} # End server({})


shinyApp(ui, server)

