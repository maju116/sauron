library(shiny)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("cyborg"),
                  titlePanel(title= h3(HTML(" <div style='width:85%;'>
                  <div style='float:right;width:0%;'>  <img src='sauron_sticker.png', height='150px', width='150px' </img>   </div>
                  <div style='float:none;'> Exemplary XAI app
                  <br>
                  powered by <span style='color:red', 'bold'> SAURON </span>
                  </div>
                  </div>")),
                  windowTitle=("Sauron")),
                  uiOutput("main_screen")
                  )
)
