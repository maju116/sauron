library(shiny)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("cyborg"),

                  titlePanel("Sauron XAI App"),

                  sidebarLayout(
                    mainPanel(
                      #nothing returned for now
                      #imageOutput('explanation_plot')
                      img(src = 'draft_plot.png')
                    ),
                    div(class = "ui grid",
                        sidebarPanel(
                          fluidRow(
                            uiOutput('init_screen'),
                            uiOutput('explain_specifics')
                          )
                        )
                    )
                  ))
)
