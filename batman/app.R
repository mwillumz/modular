library(shiny)
library(leaflet)
library(shinythemes)
library(rintrojs)
source("modules.R")

lmiUI <- function(id){
  ns <- NS(id)

  tagList(
    introjsUI(),
    actionButton(ns("help"), "help"),
    uiOutput(ns("controls"))
  )
}

lmi <- function(input, output, session, title = "LMI", cats = TRUE, dogs = NULL){

  ns <- session$ns

  One <- if(!is.null(cats)){
    introBox(
      sliderInput(ns("cats"), "Cats:", min = 1, max = 10, value = cats),
      data.step = 1,
      data.intro = "This is where you slide things.",
      data.hint = "you know bro"
    )
  } else NULL

  Two <- if(!is.null(dogs)){
    textInput(ns('dogs'), "Dogs:", value = dogs)
  } else NULL

  output$controls <- renderUI({
    div(class="panel panel-info",
        div(class="panel-heading",
            title
        ),
        div(class="panel-body",
            tagList(
              One,
              Two
            )
        )
    )
  })

  observeEvent(input$help,
               introjs(session))

  input
}

# Define UI for application that draws a histogram
ui <- navbarPage(
  theme = shinytheme("flatly"),

  title = "Old Faithful Geyser Data",
  # Sidebar with a slider input for number of bins
  tabPanel(
    title = "Cats",
    fluidRow(
      column(width = 3,
             lmiUI("one"),
             lmiUI("two")
      ),
      column(width = 9,
             plotOutput("distPlot"),
             div(class="col-sm-6 col-md-4",
                 div(class = "thumbnail",
                     img(src="https://www.sideshowtoy.com/wp-content/uploads/2017/11/dc-comics-justice-league-batman-statue-prime1-studio-feature-903246-1.jpg", alt="Dogs",
                         div(class = "caption",
                             h3("Thumbnail label"),
                             p('Fanciful words about batman.'),
                             p(a(href = "#",
                                 class = "btn btn-primary",
                                 role = "button",
                                 "Button")))
                     )

                 )
             ),
             div(class="col-sm-6 col-md-4",
                 div(class = "thumbnail",
                     "Bags"))
      )
    ),
    fluidRow(
      div(class="col-sm-4 col-md-4",
          div(class="panel panel-default",
              div(class="panel-heading",
                  "LMI"
              ),
              a(href="http://www.cnn.com",
                target = "_blank",
                div(class="panel-body",
                    style="max-height: 300px;overflow-y: scroll;",
                    plotOutput("plot2",
                               height = "200px"),

                    div(class = "caption",
                        h4("Plot Label"),
                        p('Fanciful words about batman.'))
                )
              )

          )
      ),
      div(class="col-sm-4 col-md-4",
          div(class="panel panel-default",
              div(class="panel-heading",
                  strong("LMI Monitor")
              ),
              a(href="http://www.cnn.com",
                target = "_blank",
                style = 'text-decoration: none; color:inherit',
                div(class="panel-body",
                    style="max-height: 300px;overflow-y: scroll;",
                    plotOutput("plot3",
                               height = "200px"),

                    div(class = "caption",
                        p('Fanciful words about batman. Fanciful words about batman. Fanciful words about batman.Fanciful words about batman.'))
                )),
                div(class='panel-footer',
                    em("Updated May 23, 2016"))

          )
      )
    ),

    fluidRow(
      div(class="alert alert-success",
          role="alert",
          "Look out!!!")
    ),
    fluidRow(
      hcthangUI("line"), hcthangUI("bar"), hcthangUI("scatter")
      ),
    fluidRow(hcthangUI("column"),
      hcthangUI("spline")

    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  n <- 5

  set.seed(123)

  colors <- c("#d35400", "#2980b9", "#2ecc71", "#f1c40f", "#2c3e50", "#7f8c8d")
  colors2 <- c("#000004", "#3B0F70", "#8C2981", "#DE4968", "#FE9F6D", "#FCFDBF")

  df <- data.frame(x = seq_len(n) - 1) %>%
    mutate(
      y = 10 + x + 10 * sin(x),
      y = round(y, 1),
      z = (x*y) - median(x*y),
      e = 10 * abs(rnorm(length(x))) + 2,
      e = round(e, 1),
      low = y - e,
      high = y + e,
      value = y,
      name = sample(fruit[str_length(fruit) <= 5], size = n),
      color = rep(colors, length.out = n),
      segmentColor = rep(colors2, length.out = n)
    )

  df2 <- data.frame(x = seq_len(5) - 1) %>%
    mutate(
      y = 100 + x + 10 * sin(x),
      y = round(y, 1),
      value = y,
      name = sample(fruit[str_length(fruit) <= 5], size = n),
      color = rep(colors, length.out = n),
      segmentColor = rep(colors2, length.out = n)
    )

  callModule(hcthang, "line", df2, "line")
  callModule(hcthang, "bar", df, "line")
  callModule(hcthang, "scatter", df, "scatter")
  callModule(hcthang, "column", df, "column")
  callModule(hcthang, "spline", df, "spline")

  Values <- reactiveValues()

  Values$lmi <- callModule(lmi, "one", cats = 7)

  callModule(lmi, "two", title = "Comparison Area", cats = 3, dogs = "batman")



  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles()
  })

  output$distPlot <- output$plot2 <- output$plot3 <- renderPlot({
     req(Values$lmi$cats)
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2]
      bins <- seq(min(x), max(x), length.out = Values$lmi$cats + 1)

      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application
shinyApp(ui = ui, server = server)

