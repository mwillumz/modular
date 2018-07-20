library(dplyr)
library(stringr)
library(purrr)
library(highcharter)



# hcs <- c("line", "spline",  "area", "areaspline",
#          "column", "bar", "waterfall" , "funnel", "pyramid",
#          "pie" , "treemap", "scatter", "bubble",
#          "arearange", "areasplinerange", "columnrange", "errorbar",
#          "polygon", "polarline", "polarcolumn", "polarcolumnrange",
#          "coloredarea", "coloredline")  %>%
#   map(create_hc)

hcthangUI <- function(id){
  ns <- NS(id)

  div(class="col-sm-4 col-md-4",
      div(class="panel panel-default",
          div(class="panel-heading",
              "LMI"
          ),
          a(href="http://www.cnn.com",
            target = "_blank",
            div(class="panel-body",
                style="max-height: 500px;overflow-y: scroll;",
                highchartOutput(ns("chart")),
                uiOutput(ns("select"))
            )
          )

      )
  )



}

hcthang <- function(input, output, server, data, type){

  create_hc <- function(t) {

    # dont_rm_high_and_low <- c("arearange", "areasplinerange",
    #                           "columnrange", "errorbar")

    # is_polar <- str_detect(t, "polar")

    # t <- str_replace(t, "polar", "")

    # if(!t %in% dont_rm_high_and_low) data <- data %>% select(-e, -low, -high)


    highchart() %>%
      hc_title(text = t,
               style = list(fontSize = "15px")) %>%
      hc_chart(type = t) %>%
      hc_xAxis(categories = data$name) %>%
      hc_add_series(data, name = "Fruit Consumption", showInLegend = FALSE)

  }

  output$chart <- renderHighchart({
    create_hc(type)
  })

  output$select <- renderUI({
    if(type %in% c('line', 'bar', 'column')){
      selectInput("cats", "cats", c("cats", "dogs"))
    } else NULL
  })

}
