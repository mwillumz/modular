#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard); library(leaflet)

Map <- readRDS("map.rda")
Map <- Map[is.na(Map@data$B19113_001E) == FALSE,]

cardUI <- function(id){
  ns <- NS(id)

  uiOutput(ns("content"))

}

card <- function(input, output, session, content){

  ns <- session$ns

  #reactive values
  values <- reactiveValues(income = 50000)

  #Shorthand for map data
  Content <- reactive({content[["content"]]})

  #Create color palatte
  pal <- isolate({
    colorNumeric("Greens", range(Content()@data$B19113_001E, na.rm = TRUE))
  })

  #Render inital maps
  output$maps <- output$maps2 <- renderLeaflet({
    leaflet(Content()) %>%
      addTiles() %>%
      fitBounds(-124.848974, 24.396308, -66.885444, 49.384358) %>%
      addCircles(radius = ~radius * 50, stroke = FALSE,
                 fillColor = ~pal(B19113_001E), fillOpacity = 0.5)
  })

  #main output
  output$content <- renderUI({
    box(
      leafletOutput(ns("maps")),
      title = content["title"],
      footer = fluidRow(column(2, actionLink(ns("modal"), icon("fullscreen", lib = "glyphicon"))),
                        column(10, align = "right",
                               paste("Source:", content["source"]))),
      status = "warning",
      solidHeader = FALSE,
      width = 12a,
      collapsible = TRUE,
      collapsed = FALSE
    )
  })

  output$options <- renderUI({
    req(values$income)
    sliderInput("income",
              "Income Cutoff:",
              min = min(Map@data$B19113_001E, na.rm = TRUE),
              max = max(Map@data$B19113_001E, na.rm = TRUE),
              value = values$income)
  })

  values$income <- input$income

  #modal output
  output$modal <- renderUI({
    modalDialog(
      title = content["title"],
      fluidPage(
        verticalLayout(
          leafletOutput(ns("maps2")),
          uiOutput(ns("options"))
        )
      ),
      easyClose = TRUE,
      footer = NULL,
      size = 'l'
    )

  })

  #modal observer
  observeEvent(input$modal, {
    showModal(
      uiOutput(ns("modal"))
    )
  })

  # observe({
  #   req(input$income)
  #   values$income <- input$income
  # })

  #leaflet proxy
  observeEvent(input$income, {
    Map <- Content()
    MapT <- Map[Map@data$B19113_001E <= input$income,]

    leafletProxy(ns("maps2"), data = MapT) %>%
      clearShapes() %>%
      addCircles(radius = ~radius * 50, stroke = FALSE,
                 fillColor = ~pal(B19113_001E), fillOpacity = 0.5)
  })
}



# Define UI for application that draws a histogram
ui <- dashboardPage(

  dashboardHeader(title = "Old Faithful Geyser Data"),
  dashboardSidebar(
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
    ),
    fluidRow(
      column(
        width = 6,
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

  Plot <- list(
    title = "Cats!",
    content = renderPlot({
      hist(faithful[, 2], breaks = Options$bins, col = 'darkgray', border = 'white')
    }),
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

  # callModule(card, "cats", Plot)
  # observeEvent(input$income, {
  #   pal <- colorNumeric("Greens", range(Map@data$B19113_001E, na.rm = TRUE))
  #   MapT <- Map[Map@data$B19113_001E <= input$income,]
  #
  #   leafletProxy("map") %>%
  #     clearShapes() %>%
  #     addCircles(data = MapT, radius = ~radius * 50, stroke = FALSE,
  #                fillColor = ~pal(B19113_001E), fillOpacity = 0.5)
  # })

  # Options <- renderUI({
  #   tagList(
  #     sliderInput("income",
  #                 "Income Cutoff:",
  #                 min = min(Map@data$B19113_001E, na.rm = TRUE),
  #                 max = max(Map@data$B19113_001E, na.rm = TRUE),
  #                 value = 60000),
  #     sliderInput("binz2",
  #                 "Number of bins:",
  #                 min = 1,
  #                 max = 50,
  #                 value = 5)
  #   )
  #
  # })

  thing <- list(
    title = "County Classification",
    content = Map,
    source = "American Community Survey"
  )

  callModule(card, "dogs", thing)
}

# Run the application
shinyApp(ui = ui, server = server)

