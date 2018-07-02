library(shinydashboard); library(leaflet)
library(rintrojs)

#remove this
library(shinyBS)

Map <- readRDS("map.rda")
Map <- Map[is.na(Map@data$B19113_001E) == FALSE,]

Min <- min(Map@data$B19113_001E, na.rm = TRUE)
Max <- max(Map@data$B19113_001E, na.rm = TRUE)
# source("modules.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Dash"),
  dashboardSidebar(
    sidebarMenu(
        menuItem("County Classification",
                 introBox(
                   sliderInput("income", "Median Family Income:", min = Min, max = Max, value = 60000,
                               pre = "$"),
                   data.step = 2,
                   data.intro = "WTF?"
                 ),
                 actionButton("intro", "intro")
        )
    )
   ),
  dashboardBody(
    fluidPage(
      introjsUI(),

    fluidRow(
      column(
        width = 6,
        box(leafletOutput("map"),
            title = "County Classification",
            footer = fluidRow(
              column(2, actionLink("modal", icon("fullscreen", lib = "glyphicon"))),
              column(10, align = "right", paste("Source:", "American Community Survey"))),
            width = 12),

        introBox(
          textOutput("bounds"),
          data.step = 1,
          data.intro = "Classify this shit."
        )


      )
    )
    ),
    bsModal("modalExample", "County Classification", "modal", size = "large",
            verticalLayout(
              leafletOutput("map2")
              )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  observeEvent(input$intro, introjs(session))

  output$bounds <- renderPrint({
    input$map_bounds
  })

  #Create color palatte
  pal <- reactive({
    req(input$income)
    Center <- input$income
    Thing <- range(Map@data$B19113_001E, na.rm = TRUE) %>%
      abs()
    Thing <- abs(Thing - Center) %>%
      max()

    colorNumeric("PuOr", c(Center - Thing, Center + Thing))
  })

  output$map <- output$map2 <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      fitBounds(-124.848974, 24.396308, -66.885444, 49.384358) %>%
      addCircles(data = Map, radius = ~radius * 50, stroke = FALSE,
                 fillColor = ~isolate(pal())(B19113_001E), fillOpacity = 0.5)
  })

  #leaflet proxy
  observeEvent(input$income, {
    leafletProxy("map") %>%
      clearShapes() %>%
      addCircles(data = Map, radius = ~radius * 50, stroke = FALSE,
                 fillColor = ~pal()(B19113_001E), fillOpacity = 0.5,
                 label = ~name)

    leafletProxy("map2") %>%
      clearShapes() %>%
      addCircles(data = Map, radius = ~radius * 50, stroke = FALSE,
                 fillColor = ~pal()(B19113_001E), fillOpacity = 0.5,
                 label = ~name)
  })

  observeEvent(c(input$map_zoom, input$map_center), {
    leafletProxy("map2") %>%
      setView(input$map_center$lng, input$map_center$lat,
              input$map_zoom)
  })

  observeEvent(c(input$map2_zoom, input$map2_center), {
    leafletProxy("map") %>%
      setView(input$map2_center$lng, input$map2_center$lat,
              input$map2_zoom)
  })

}

# Run the application
shinyApp(ui = ui, server = server)

