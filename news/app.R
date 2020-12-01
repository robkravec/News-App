# Load libraries
library(shiny)
library(shinyalert)
library(shinythemes)
library(tidyverse)
library(httr)
library(jsonlite)
library(DT)

# Define allowable categories for user searching
categories <- c("all", "business", "entertainment", "general", "health")
categories <- append(categories,c("science", "sports", "technology"))

# Load api helper functions
source("api_wrappers.R")

# user interface
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  useShinyalert(),
  navbarPage(
    title = strong("Headline Sentiment and Source Analysis"),
    tabPanel(
      em("Instructions"),
      htmlOutput(outputId = "instructions", style = "font-size:20px")
    ), # tabPanel1
    tabPanel(
      em("Top headlines"),
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = "country",
            label = "World news or US-only?",
            choices = c("World", "US")
          ), # radioButtons

          selectInput(
            inputId = "category",
            label = "News category (optional)",
            choices = categories
          ), # selectInput

          textInput(
            inputId = "keyword",
            label = "Keyword search (optional)",
            value = ""
          ), # textInput - keyword search

          sliderInput(
            inputId = "entries",
            label = "Number of headlines",
            min = 1,
            max = 100,
            value = 20
          ), # sliderInput

          textInput(
            inputId = "api",
            label = "Please enter your API key",
            value = ""
          ), # textInput - API key

          actionButton(
            inputId = "getdata",
            label = strong("Pull data")
          ) # actionButton
        ), # sidebarPanel

        mainPanel(
          tags$style(
            type = "text/css",
            ".shiny-output-error { visibility: hidden; }",
            ".shiny-output-error:before { visibility: hidden; }"
          ),
          htmlOutput(outputId = "actioncounter"),
          hr(),
          dataTableOutput(outputId = "topheadlines")
        )
      ) # tabPanel2
    ), # sidebarLayout

    tabPanel(
      em("Sources"),
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = "country2",
            label = "World news or US-only?",
            choices = c("World", "US")
          ), # radioButtons

          selectInput(
            inputId = "category2",
            label = "News category (optional)",
            choices = categories
          ), # selectInput

          textInput(
            inputId = "api2",
            label = "Please enter your API key",
            value = ""
          ), # textInput - API key

          actionButton(
            inputId = "getdata2",
            label = strong("Pull data")
          ) # actionButton
        ), # sidebarPanel

        mainPanel(
          tags$style(
            type = "text/css",
            ".shiny-output-error { visibility: hidden; }",
            ".shiny-output-error:before { visibility: hidden; }"
          ),
          htmlOutput(outputId = "actioncounter2"),
          hr(),
          dataTableOutput(outputId = "sources")
        ) # mainPanel
      ) # sidebarLayout
    ) # tabPanel3
  ) # navbarPage
)

# server function
server <- function(input, output) {

  # Counts number of unsuccessful API calls to adjust total shown in app
  count_deduction <- reactiveValues(deduction = 0)

  # Create instructions for instructions page
  output$instructions <- renderText({
    paste(
      "Thank you for visiting this R Shiny app, which displays the top",
      "headlines and news sources from around the world. This app serves as",
      "a wrapper to the",
      "News API (<a href = 'https://newsapi.org'>http://newsapi.org</a>)",
      "and SENTIM-API (<a href = 'https://sentim-api.herokuapp.com'",
      ">https://sentim-api.herokuapp.com</a>). Please note that use of the",
      "News API requires a valid API key, which can be",
      "acquired for free at the linked web address. A free account grants",
      "500 requests per day and allows queries of articles up to one month",
      "old. <br/> <br/>",
      "This app contains two different tabs that accept user input: <br/>",
      "<br/> - <em> Top headlines: </em> Returns top headlines (either",
      "from the US or around the world). Users may filter based on category",
      "or keyword, while also specifying the desired number of headlines to",
      "return (up to 100). The resulting data table contains the headline,",
      "a sentiment analysis of that headline (ranging from -1: very",
      "negative to 1: very positive), the news source, and a clickable URL",
      "<br/> <br/> - <em> Sources: </em> Returns a list of news publishers",
      "from whom top headlines are available (either from the US or around",
      "the world). Users may filter based on the category of news article",
      "produced by that source"
    )
  })

  # Pull top headline data when action button is clicked
  top_headlines <- eventReactive(input$getdata, {

    # Set country based on user input
    country <- ifelse(input$country == "US", "us", "")

    # Set category based on user input
    category <- ifelse(input$category == "all", "", input$category)

    # Call News API
    tryCatch(
      {
        top_headlines <- get_top_headlines(
          apiKey = input$api,
          country = country,
          category = category,
          pageSize = input$entries,
          page = 1,
          keyword = input$keyword
        )

        # If API call was successful, perform sentiment analysis, and modify URL
        if (nrow(top_headlines) > 0) {

          # Get polarity rating for each headline
          sentim <- map(top_headlines$title, get_sentim) %>%
            unlist()

          # Add polarity ratings and modify url so that it is clickable
          top_headlines <- top_headlines %>%
            mutate(
              sentiment = sentim,
              url = str_c("<a href='", url, "'>", url, "</a>")
            )
        }
      },
      error = function(e) {
        shinyalert(
          title = "We've encountered an error!",
          text = "Please enter a valid API key",
          type = "error"
        )
        # Remove 1 from API call counter because call with unsuccessful
        count_deduction$deduction <- count_deduction$deduction + 1

        # R wants a data frame output
        data.frame()
      }
    )
  })

  # Create data table of top headlines
  output$topheadlines <- DT::renderDataTable({
    # Get top headline data and put in datatable
    headline_data <- top_headlines()

    # Extract relevant columns, and display top headlines in datatable
    top_hl_df <- tibble(
      Headline = headline_data$title,
      Source = headline_data$source$name,
      "Sentiment (-1 to 1)" = headline_data$sentiment,
      URL = headline_data$url
    ) %>%
      datatable(escape = F)
  })

  # Pull data about news sources
  sources <- eventReactive(input$getdata2, {

    # Set country based on user input
    country2 <- ifelse(input$country2 == "US", "us", "")

    # Set category based on user input
    category2 <- ifelse(input$category2 == "all", "", input$category2)

    # Pull source data from News API
    tryCatch(
      {
        sources <- get_sources(
          apiKey = input$api2,
          country = country2,
          category = category2
        ) %>%
          select(Name = name, URL = url, Category = category, Country = country) %>%
          mutate(URL = str_c("<a href='", URL, "'>", URL, "</a>"))
      },
      error = function(e) {
        shinyalert(
          title = "We've encountered an error!",
          text = "Please enter a valid API key",
          type = "error"
        )
        # Remove 1 from API call counter because call with unsuccessful
        count_deduction$deduction <- count_deduction$deduction + 1

        # R wants the output of this function to be a data frame
        data.frame()
      }
    )
  })

  # Display sources datatable
  output$sources <- DT::renderDataTable({
    datatable(sources(), escape = F)
  })

  # Display number of API queries for top headlines
  output$actioncounter <- renderText(paste("<b>Successful API calls in current",
    "session:", input$getdata + input$getdata2 - count_deduction$deduction,
    "</b>"
  ))

  # Display number of API queries for sources
  output$actioncounter2 <- renderText(paste("<b>Successful API calls in current",
    "session:", input$getdata + input$getdata2 - count_deduction$deduction,
    "</b>"
  ))
}

# Run the application 
shinyApp(ui = ui, server = server)
