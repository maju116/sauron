library(shiny)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("cyborg"),

                  titlePanel("Sauron XAI App"),
                  uiOutput("main_screen")
                  )
)
