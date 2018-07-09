library(shinydashboard)
library(rintrojs)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Dash"),
  dashboardSidebar(
    sidebarMenu(
      )
  ),
  dashboardBody(

    #   <div class="panel-body">
    #   Panel content
    # </div>
    #   <div class="panel-footer">Panel footer</div>
    #   </div>
  )
)

server <- function(input, output, session) {}

# Run the application
shinyApp(ui = ui, server = server)

