library(shiny)
library(shinythemes)
library(tidyverse)
library(tidytext)
library(syuzhet)
library(wordcloud2)
library(plotly)
library(DT)
library(stringi)
library(lubridate)

# Define UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("X DATA Sentiment Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File", accept = ".csv"),
      actionButton("analyze", "Analyze"),
      tags$hr(),
      tags$p("Upload a CSV file containing a 'text' column for sentiment analysis."),
      sliderInput("word_freq", "Minimum Word Frequency:", min = 1, max = 50, value = 25),
      tags$style(HTML("
        .well { background-color: #f9f9f9; }
        .shiny-input-container { margin-bottom: 20px; }
      "))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Sentiment Analysis",
                 plotOutput("sentiment_plot", height = "600px"),
                 plotOutput("sentiment_pie", height = "600px")),
        tabPanel("Word Cloud",
                 wordcloud2Output("wordcloud")),
        tabPanel("Word Frequencies Bar Plot",
                 plotOutput("word_freq_plot", height = "600px")),
        tabPanel("All Tweets",
                 dataTableOutput("tweet_table")),
        tabPanel("Scatter Plots",
                 fluidRow(
                   column(6, plotlyOutput("scatter_anger")),
                   column(6, plotlyOutput("scatter_anticipation")),
                   column(6, plotlyOutput("scatter_disgust")),
                   column(6, plotlyOutput("scatter_fear"))
                 )),
        tabPanel("Download Report",
                 downloadButton("download_report", "Download Report"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$analyze, {
    req(input$file1)
    
    # Read the CSV file
    tweets <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
    
    # Ensure there is a text column
    if (!"text" %in% colnames(tweets)) {
      showNotification("The CSV file must contain a 'text' column.", type = "error")
      return()
    }
    
    # Clean and preprocess text
    tweets$text <- stri_replace_all_regex(tweets$text, "\\d+", "")
    tweets$text <- iconv(tweets$text, from = "UTF-8", to = "ASCII", sub = "")
    
    # Perform sentiment analysis
    tweets_sentiment <- get_nrc_sentiment(tweets$text)
    tweets$sentiment <- colnames(tweets_sentiment)[max.col(tweets_sentiment, ties.method = "first")]
    
    # Convert sentiment columns to numeric
    tweets_sentiment <- as.data.frame(lapply(tweets_sentiment, as.numeric))
    
    # Add sentiment scores for each tweet
    tweets <- cbind(tweets, tweets_sentiment)
    
    output$tweet_table <- renderDataTable({
      datatable(tweets[, c("text", "sentiment", "anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")], options = list(pageLength = 10))
    })
    
    output$sentiment_plot <- renderPlot({
      sentiment_counts <- colSums(tweets_sentiment)
      barplot(sentiment_counts, las = 2, col = rainbow(10), main = "Sentiment Analysis", ylab = "Count")
    })
    
    output$wordcloud <- renderWordcloud2({
      tweet_words <- tweets %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words, by = "word")
      
      word_freq <- tweet_words %>%
        count(word, sort = TRUE) %>%
        filter(n > input$word_freq)
      
      if (nrow(word_freq) == 0) {
        showNotification("No words found with the specified frequency. Please lower the minimum word frequency.", type = "error")
        return(NULL)
      }
      
      wordcloud2(data = word_freq, size = 0.7, shape = 'circle')
    })
    
    output$word_freq_plot <- renderPlot({
      tweet_words <- tweets %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words, by = "word")
      
      word_freq <- tweet_words %>%
        count(word, sort = TRUE) %>%
        filter(n > input$word_freq)
      
      if (nrow(word_freq) == 0) {
        showNotification("No words found with the specified frequency. Please lower the minimum word frequency.", type = "error")
        return(NULL)
      }
      
      barplot(word_freq$n, names.arg = word_freq$word, las = 2, col = rainbow(10), main = "Word Frequencies", ylab = "Frequency")
    })
    
    output$sentiment_pie <- renderPlot({
      sentiment_counts <- colSums(tweets_sentiment)
      labels <- names(sentiment_counts)
      pie(sentiment_counts, labels = labels, main = "Sentiment Distribution", col = rainbow(length(labels)))
    })
    
    scatter_plot <- function(tweets, sentiment, color) {
      tweet_words <- tweets %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words, by = "word") %>%
        group_by(word) %>%
        summarise(frequency = n(), sentiment_score = mean(!!sym(sentiment))) %>%
        filter(frequency > input$word_freq)
      
      plot_ly(tweet_words, x = ~frequency, y = ~sentiment_score, text = ~word, type = 'scatter', mode = 'markers',
              marker = list(color = color, size = 10, opacity = 0.6)) %>%
        layout(title = paste("Scatter Plot of", sentiment, "Sentiment"),
               xaxis = list(title = "Word Frequency"),
               yaxis = list(title = paste(sentiment, "Score")),
               hovermode = "closest")
    }
    
    output$scatter_anger <- renderPlotly({
      scatter_plot(tweets, "anger", "red")
    })
    
    output$scatter_anticipation <- renderPlotly({
      scatter_plot(tweets, "anticipation", "orange")
    })
    
    output$scatter_disgust <- renderPlotly({
      scatter_plot(tweets, "disgust", "green")
    })
    
    output$scatter_fear <- renderPlotly({
      scatter_plot(tweets, "fear", "blue")
    })
    
    output$download_report <- downloadHandler(
      filename = function() {
        paste("sentiment_analysis_report", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(tweets, file)
      }
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
