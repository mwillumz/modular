
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
      width = 12,
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
