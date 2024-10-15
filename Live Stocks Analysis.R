# Required Libraries
library(shiny)
library(quantmod)
library(plotly)
library(DT)
library(TTR)

# UI Code
ui <- fluidPage(
  titlePanel("Stock Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      textInput("stockSymbol", "Enter Stock Symbol (e.g., AAPL):", value = "AAPL"),
      dateInput("startDate", "Start Date:", value = Sys.Date() - 365), 
      dateInput("endDate", "End Date:", value = Sys.Date()),  
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Table",
                 DTOutput("stockTable")
        ),
        tabPanel("Stock Charts",
                 plotlyOutput("closingVsOpeningChart"),
                 plotlyOutput("highVsLowChart"),
                 plotlyOutput("volumeChart"),
                 plotlyOutput("candlestickChart"),
                 plotlyOutput("movingAvgChart"),
                 plotlyOutput("rsiChart")
        )
      ),
      width = 9
    )
  )
)

# Server Code
server <- function(input, output, session) {
  
  
  stock_data <- reactive({
    req(input$stockSymbol, input$startDate, input$endDate)  
    stock_symbol <- input$stockSymbol
    start_date <- as.Date(input$startDate)
    end_date <- as.Date(input$endDate)
    
    # Fetch the stock data dynamically
    stock_df <- quantmod::getSymbols(stock_symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
    stock_df <- data.frame(Date = index(stock_df), coredata(stock_df))  # Convert to data frame
    colnames(stock_df) <- gsub(paste0("^", stock_symbol, "."), "", colnames(stock_df))  
    return(stock_df)
  })
  
  
  output$stockTable <- renderDT({
    df <- stock_data()
    req(df)
    
    datatable(df, options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  # chart No. 1 - Closing vs Opening chart (dynamic)
  output$closingVsOpeningChart <- renderPlotly({
    df <- stock_data()
    req(df)
    
    plot_ly(df, x = ~Date, y = ~Close, type = 'scatter', mode = 'lines', name = 'Close') %>%
      add_lines(y = ~Open, name = 'Open') %>%
      layout(title = paste(input$stockSymbol, "Closing vs Opening Prices"))
  })
  
  # chart No. 2 - High vs Low chart
  output$highVsLowChart <- renderPlotly({
    df <- stock_data()
    req(df)
    
    plot_ly(df, x = ~Date, y = ~High, type = 'scatter', mode = 'lines', name = 'High') %>%
      add_lines(y = ~Low, name = 'Low') %>%
      layout(title = paste(input$stockSymbol, "High vs Low Prices"))
  })
  
  # chart No. 3 -  Volume chart
  output$volumeChart <- renderPlotly({
    df <- stock_data()
    req(df)
    
    plot_ly(df, x = ~Date, y = ~Volume, type = 'bar', name = 'Volume') %>%
      layout(title = paste(input$stockSymbol, "Trading Volume"))
  })
  
  # chart No. 4 -  Candlestick chart
  output$candlestickChart <- renderPlotly({
    df <- stock_data()
    req(df)
    
    plot_ly(df, x = ~Date, type = "candlestick", open = ~Open, close = ~Close, high = ~High, low = ~Low) %>%
      layout(title = paste(input$stockSymbol, "Candlestick Chart"))
  })
  
  # chart No. 5 - Moving average chart (50-day SMA)
  output$movingAvgChart <- renderPlotly({
    df <- stock_data()
    req(df)
    
    df$MA50 <- TTR::SMA(df[, "Close"], n = 50)
    
    plot_ly(df, x = ~Date, y = ~Close, type = 'scatter', mode = 'lines', name = 'Close') %>%
      add_lines(y = ~MA50, name = '50-Day Moving Average') %>%
      layout(title = paste(input$stockSymbol, "50-Day Moving Average"))
  })
  
  # chart No. 6 - RSI chart (14-day RSI)
  output$rsiChart <- renderPlotly({
    df <- stock_data()
    req(df)
    
    df$RSI <- RSI(df[, "Close"], n = 14)
    
    plot_ly(df, x = ~Date, y = ~RSI, type = 'scatter', mode = 'lines', name = 'RSI') %>%
      layout(title = paste(input$stockSymbol, "Relative Strength Index (RSI)"))
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
