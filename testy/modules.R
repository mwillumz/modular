shitModal <- function (id, title, footer, trigger, ...){

  bsTag <- tags$div(class = "modal sbs-modal fade",
                    id = id, tabindex = "-1", `data-sbs-trigger` = trigger,
                    tags$div(class = "modal-dialog modal-lg", tags$div(class = "modal-content",
                                                                       tags$div(class = "modal-header", tags$button(type = "button",
                                                                                                                    class = "close", `data-dismiss` = "modal", tags$span(HTML("&times;"))),
                                                                                tags$h4(class = "modal-title", title)),
                                                                       tags$div(class = "modal-body", list(...)),
                                                                       tags$div(class = "modal-footer", footer))))

  shinyBSDep <- htmltools::htmlDependency("shinyBS", packageVersion("shinyBS"), src = c("href" = "sbs"), script = "shinyBS.js", stylesheet = "shinyBS.css")
  htmltools::attachDependencies(bsTag, shinyBSDep)
}

controlsUI <- function(id){
  ns <- NS(id)

  uiOutput(ns("controls"))
}

controls <- function(input, output, session){
  ns <- session$ns

  output$controls <- renderUI({
    sliderInput(ns("bins"),
                "Number of bins:",
                min = 1,
                max = 50,
                value = 25)
  })

  return(input)
}

cardUI <- function(id){
  ns <- NS(id)

  tagList(
    shitModal(ns("modalExample"),
              title = textOutput(ns("mtitle")),
              footer = textOutput(ns("mfooter")),
              trigger = ns("modal"),
              tagList(controlsUI(ns("dog")),
                      uiOutput(ns("mheadline")))
    ),

    column(width = 6,
           div(class="panel panel-default",
               div(class="panel-heading",
                   fluidRow(
                     column(width = 10, strong(
                       textOutput(ns("title"))
                     )),
                     column(width = 2, align = "right",
                            actionLink(ns("modal"), icon("fullscreen", lib = "glyphicon")))
                   )
               ),
               div(class = "panel-body",
                   uiOutput(ns("headline"))
               ),
               div(class = "panel-footer",
                   align = "right",
                   textOutput(ns("footer")))
           )
    )
  )
}

card <- function(input, output, session, content){

  ns <- session$ns

  #return input
  Thang <- callModule(controls, "dog")

  observeEvent(Thang, {
    content[['params']]$bins <- Thang$bins
  })

  output$controls <- content[["controls"]]
  output$title <- output$mtitle <- renderText({content[["title"]]})
  output$distPlot <- output$distPlot2 <- content[["plot"]]
  output$headline <- renderUI({
    plotOutput(ns('distPlot'))
  })

  output$mheadline <- renderUI({
    verticalLayout(
      uiOutput(ns("controls")),
      plotOutput(ns("distPlot2"))
    )
  })

  output$footer <- output$mfooter <- renderText({content[["footer"]]})
}
