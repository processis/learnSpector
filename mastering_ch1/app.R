#reducing duplication with reative expressions
#to avoid dataset <-get(..  appearing twice
library(shiny)
server <- function (input,output,
session){
  #create a reactive expression
  product <- reactive ({
    input$x * input$y
  })
  output$product <- renderText({
    #product <- (input$x )*(input$y)  replaced by reactive exp
    product()
  })
  output$product_plus5 <- renderText({
    #product <- (input$x )*(input$y)
    product() + 5
  })
  output$product_plus10 <- renderText({
    #product <- (input$x )*(input$y)
    product() + 10
  })
}
ui <- fluidPage(
  sliderInput("x", label = "If x is", min = 1, max = 50 , value =30),
  sliderInput("y", label = "and y is", min = 1, max = 50 , value =5),
  "then x times y is ",
  textOutput("product"),
  "and, (x * y) + 5 is ",
  textOutput("product_plus5"),
  "and (x * y) + 10 is",
  textOutput("product_plus10")
)
shinyApp(ui,server)