library(shiny); library(shinythemes)
library(dplyr); library(leaflet)

source("modules.R")

# Define UI for application that draws a histogram
ui <- navbarPage("Modular",
                 theme = shinytheme("simplex"),
                 inverse = TRUE,
                 id = "navbar",
                 tabPanel("one",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("data",
                                          "Data",
                                          choices = list(cats = c("one" = "cats_one", "two" = "cats_two"),
                                                         dogs = c("one" = "dogs_one", "three" = "dogs_three")))
                            ),
                            # Show a plot of the generated distribution
                            mainPanel(
                              fluidRow(
                                tabsetPanel(
                                  type = "pill",
                                  tabPanel("Dogs"),
                                  tabPanel("Cats",
                                           leafletOutput("dog")),
                                  tabPanel("Bats",
                                    tagList(
                                      cardUI("card1"), cardUI("card2"), cardUI("card3")
                                    )

                                  )
                                )

                              )
                            )
                          )
                 ),
                 tabPanel("two"),
                 tabPanel("cats")

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  output$dog <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addFullscreenControl()
  })

  Values1 <- reactiveValues(bins = 5)
  callModule(card, "card1",
             list(title = "Home Ownership Rate",
                  footer = "Source: American Community Survey.",
                  plot = renderPlot({
                    x    <- faithful[, 2]
                    bins <- seq(min(x), max(x), length.out = Values1$bins + 1)
                    hist(x, breaks = bins, col = 'darkgray', border = 'white')
                  }),
                  params = Values1)
  )

  Values2 <- reactiveValues(bins = 1)
  What <- callModule(card, "card2",
             list(title = "Loser Rate",
                  footer = "Source: American Community Survey.",
                  plot = renderPlot({
                    x    <- faithful[, 2]
                    bins <- seq(min(x), max(x), length.out = Values2$bins + 1)
                    hist(x, breaks = bins, col = 'darkgray', border = 'white')
                  }),
                  params = Values2)
  )

  Values3 <- reactiveValues(bins = 25)
  callModule(card, "card3",
             list(title = "Loser Rate x 2",
                  footer = "Source: American Community Survey.",
                  plot = renderPlot({
                    x    <- faithful[, 2]
                    bins <- seq(min(x), max(x), length.out = Values3$bins + 1)
                    hist(x, breaks = bins, col = 'darkgray', border = 'white')
                  }),
                  # controls = renderUI({
                  #   ns <- session$ns
                  #
                  #   sliderInput(ns("bins"),
                  #               "Number of bins:",
                  #               min = 1,
                  #               max = 50,
                  #               value = Values3$bins)
                  #
                  # }),
                  params = Values3)
  )
}

# Run the application
shinyApp(ui = ui, server = server, enableBookmarking = "url")

