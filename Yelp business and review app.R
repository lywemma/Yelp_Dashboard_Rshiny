#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(XML)
library(stringr)
library(RCurl)
library(rsconnect)
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
require(scales)
library(DT)
library(BH)
library(leaflet)
library(RColorBrewer)
library(tidyverse)
library(wordcloud)
library(memoise)
library(tm)

# Data Prepare
yelp_data <- readRDS("yelp_data.RDS")
yelp_tip <- readRDS("tip_sample.RDS")
yelp_data$categories <- as.factor(yelp_data$categories)
#yelp_Rest <- yelp_data %>% filter(categories == "Restaurants")

#categories <- yelp_data %>% distinct(categories)
num_categories <- nrow(yelp_data %>% distinct(categories)) 

# getTermMatrix Function
getTermMatrix <- memoise(function(category_text) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  mytext <- yelp_tip %>% 
    filter(categories == category_text) %>%
    select(text)
  
  myCorpus = Corpus(VectorSource(mytext))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,stopwords("english"))
  myCorpus <- tm_map(myCorpus, removeWords, c("restaurant","restaurants","shopping","hotel","hotels","travel")) 
  myCorpus = tm_map(myCorpus, stripWhitespace)
  myDTM = TermDocumentMatrix(myCorpus)
  
  m = as.matrix(myDTM)
  
  v<-sort(rowSums(m), decreasing = TRUE)
  data.frame(word=names(v),freq=v)
})

# Define UI for application
ui <- fluidPage(
  tabsetPanel(
    tabPanel("Category Overivew",
             # Sidebar layout with a input and output definitions
             sidebarLayout(
               
               # Inputs
               sidebarPanel(
                 width = 4,
                 
                 # Text instructions
                 h4("Top N Categories"),
                 HTML(paste("Enter a value between 1 and", num_categories)),
                 
                 # Numeric input for sample size
                 numericInput(inputId = "n",
                              label = "Top N Categories:",
                              value = 10,
                              min = 1, max = num_categories,
                              step = 1)
               ),
               
               
               # Output: Show data table
               mainPanel(
                 width = 8,
                 
                 h3("Top Categories by Review Count"),
                 plotOutput(outputId = "top_n",click = "plot_click"),
                 
                 h3("Select One Category to view Store details"),
                 htmlOutput("x_value"),
                 # Show data table
                 dataTableOutput(outputId = "selected")
                 
               )
             )
    ),
    
    tabPanel("Word Cloud for Categories",
             
             sidebarLayout(
               # Inputs
               sidebarPanel(
                 width = 4,
                 
                 # Text instructions
                 h5("Select one or more categories:"),
                 
                 # Select Categories
                 selectInput(inputId = "cat_for_wc",
                             label = "Categories for WordCloud:",
                             choices = c("Restaurants","Shopping","Hotels & Travel"),
                             selected = "Restaurants"
                             ),
                 
                 # Select Max Words to show
                 sliderInput(inputId = 'max_words', 
                             label = "Maximum number of words", 
                             min = 1, 
                             max = 200,
                             value = 80),
                 br(),br(),
                 HTML("WordCloud Caculation might be slow.")
                 
                 
               ),
               
               mainPanel(
                 width = 8,
                 
                 h3("Word Cloud for Selected Categories"),
                 plotOutput(outputId = "wc")
               )
             )
    )
    )
  )


# Define server function required to create the scatterplot
server <- function(input, output, session) {
  
  # Create data table
  yelp_topn <- reactive({
    req(input$n)
    yelp_data %>%
      group_by(categories)%>%
      summarize(total_count = sum(review_count))%>%
      arrange(desc(total_count))%>%
      mutate(categories = factor(categories, unique(categories)))%>%
      ungroup() %>% 
      head(input$n)
  })
  
  output$top_n <- renderPlot({
    yelp_topn() %>% 
      ggplot(aes(x=reorder(categories, -total_count),y=total_count)) + 
      geom_bar(stat="identity",fill="steelblue4") + 
      labs(x="Categories",y="Total review count") +
      scale_y_continuous(labels = comma) +
      theme_hc()  +  
      theme(axis.text.x = element_text(angle = 45,hjust=1))
  })
  
  selected_category <-reactive({
    if (is.null(input$plot_click$x)) return("")
    else {
      lvls <- levels(yelp_topn()$categories)
      #select_name <- lvls[round(input$plot_click$x)]
      return (lvls[round(input$plot_click$x)])
    }
  })
  
  output$x_value <- renderText({
    HTML("You've selected <code>", selected_category(), "</code>",
         "<br><br>Here are the Stores in this category ranked by view count:")
  })
  
  observe({
  selected_business <- reactive({
    req(selected_category())
    yelp_data %>% 
      filter(categories == selected_category())%>%
      group_by(name)%>%
      summarize(total_reviewcount = sum(review_count), avg_stars = round(mean(stars),2))%>%
      arrange(desc(total_reviewcount))
  })
  
  output$selected <- DT::renderDataTable({
    DT::datatable(data = selected_business(), 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  })
  })
  
  # wordcloud_categories <- reactive({
  #   req(input$cat_for_wc)
  #   req(input$max_words)
  #   yelp_tip %>%
  #     filter(categories %in% input$cat_for_wc)%>%
  #     select(text)%>%
  #     unnest_tokens(word, text) %>%
  #     filter(!word %in% stop_words$word) %>%
  #     count(word,sort = TRUE) %>%
  #     ungroup()  %>%
  #     head(input$max_words)
  # })
  
  # output$wc <-renderPlot({
  #   wordcloud_categories() %>%
  #     with(wordcloud(word, n, max.words = input$max_words,colors=brewer.pal(8, "Dark2")))
  # })
  
  wordcloud_categories <- reactive({
     req(input$cat_for_wc)
     req(input$max_words)
     category_text <- input$cat_for_wc
     getTermMatrix(category_text)
  })
     
     
  output$wc <- renderPlot({
    wordcloud(words = wordcloud_categories()$word,
              freq=wordcloud_categories()$freq,
              min.freq=1,random.order=FALSE,
              rot.per=0.35, 
              max.words=input$max_words,
              colors=brewer.pal(8, "Dark2"))
  })
  
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
