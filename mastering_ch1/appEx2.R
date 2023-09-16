#reducing duplication with reative expressions
#to avoid dataset <-get(..  appearing twice
library(shiny)
server <- function (input,output,
session){
  output$product <- renderText({
    (input$x )*(input$y)
  })
}
ui <- fluidPage(
  sliderInput("x", label = "If x is", min = 1, max = 50 , value =30),
  sliderInput("y", label = "If y is", min = 1, max = 50 , value =5),
  "then x times y is ",
  textOutput("product")
)
shinyApp(ui,server)