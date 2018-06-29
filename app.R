#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)

cardUI <- function(id){
  ns <- NS(id)

  uiOutput(ns("content"))
}

card <- function(input, output, session, content){

  ns <- session$ns

  Modal <- if(exists("modal", where = content)){
    actionLink(ns("modal"), icon("fullscreen", lib = "glyphicon"))
  } else{
    NULL
  }

  observeEvent(input$modal, {
    showModal(modalDialog(
      title = content["title"],
      content["modal"],
      easyClose = TRUE,
      footer = NULL,
      size = 'l'
    ))
  })

  output$content <- renderUI({
    box(
      content["content"],
      title = content["title"],
      footer = fluidRow(column(2, Modal),
                        column(10, align = "right",
                               paste("Source:", content["source"]))),
      status = "warning",
      solidHeader = FALSE,
      width = 12,
      collapsible = TRUE,
      collapsed = FALSE
    )
  })
}



# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Old Faithful Geyser Data"),
  dashboardSidebar(
  ),
  dashboardBody(
    fluidRow(
      column(
        width = 6,
        cardUI("cats"),
        cardUI("dogs")
      )

    )


  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  Options <- reactiveValues(bins = 12)

  observe({
    req(input$bins)
    Options$bins <- input$bins
  })

  output$plot1 <- output$plot2 <- renderPlot({
    hist(faithful[, 2], breaks = Options$bins, col = 'darkgray', border = 'white')
  })

  output$options <- renderUI({
    box(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = Options$bins),
      width = 12,
      title = "Options",
      status = "warning",
      solidHeader = TRUE,
      collapsible = TRUE
    )
  })

  Plot <- list(
    title = "Cats!",
    content = plotOutput("plot1"),
    source = "Batman",
    modal = fluidPage(
      fluidRow(
        column(width = 4,
               uiOutput("options")),
        column(width = 8,
               plotOutput("plot2"))
      ),
      fluidRow(
        actionButton("modalz", "All Options"),
        tags$b("Hey Stoopid")
      )
    )
  )

  observeEvent(input$modalz, {
    showModal(modalDialog(
      title = "All your options",
      "There are no good options.",
      easyClose = TRUE,
      footer = NULL,
      size = 'l'
    ))
  })

  callModule(card, "cats", Plot)

  Plot2 <- list(
    title = "Dogs!",
    content = renderPlot({
      hist(faithful[, 2], breaks = 10, col = 'darkgray', border = 'white')
    }),
    source = "Home Mortgage Disclosure Act"
  )

  callModule(card, "dogs", Plot2)

  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}

# Run the application
shinyApp(ui = ui, server = server)

