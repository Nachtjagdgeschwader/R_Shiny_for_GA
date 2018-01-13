library(googleAuthR)
library(shiny)
library(googleAnalyticsR)
options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/analytics",
                                        "https://www.googleapis.com/auth/analytics.edit",
                                        "https://www.googleapis.com/auth/analytics.readonly"))
# options(googleAnalyticsR.webapp.client_id = "YOUR ID")
# options(googleAnalyticsR.webapp.client_secret = "YOUR SECRET")
app <- shinyApp(
  ui = fluidPage(
    googleAuthUI("login"),
    authDropdownUI("auth_menu"),
    dateRangeInput("date_seg", "Date Range", start = Sys.Date() - 30, width = "50%"),
    textInput("label_filter", "Label filter"),
    actionButton("get_seg", "Submit"),
    hr(),
    h2("Results"),
    h3(textOutput("total")),
    dataTableOutput("segment_table")
  ),
  server = function(input, output, session){
    token <- callModule(googleAuth, "login")
    
    ga_accounts <- reactive({
      validate(
        need(token(), "Authenticate")
      )
      
      with_shiny(ga_account_list, shiny_access_token = token())
    })
    selected_id <- callModule(authDropdown, "auth_menu", ga.table = ga_accounts)
    segment_data <- eventReactive(input$get_seg, {
      
      viewId <- selected_id()
      dates <- input$date_seg
      user_parameter_shiny <- input$label_filter
      df <- dim_filter("eventAction","REGEXP","risp|tel")
      df2 <- dim_filter("eventLabel","REGEXP",user_parameter_shiny)
      fc2 <- filter_clause_ga4(list(df, df2), operator = "AND")
      my_filter_clause <- filter_clause_ga4(list(df2))
      with_shiny(google_analytics_4,
                 shiny_access_token = token(),
                 viewId = viewId,
                 date_range = c(dates[1], dates[2]),
                 metrics = c("totalEvents"),
                 dimensions = c("eventLabel","eventAction"),
                 dim_filters = fc2,
                 anti_sample = TRUE,
                 anti_sample_batches = 5
      )
      
    })
    output$segment_table <- renderDataTable({
      
      segment_data()
      
    })
    output$total <- renderText({ paste("Sum of totalEvents:", sum(segment_data()$totalEvents))})
  })

runApp(app, launch.browser=T, port=1221)