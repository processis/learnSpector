#reducing duplication with reative expressions
#to avoid dataset <-get(..  appearing twice
library(shiny)
server <- function (input,output,
session){
  #create a reactive expression
  dataset <- reactive({
    get(input$dataset,"package:datasets")
  })
  #
  output$summary <- renderPrint({
    #dataset <- get(input$dataset,"package:datasets")   replaced by reactive expression
    summary(dataset())
  })
  output$table <- renderTable({
    #dataset <- get(input$dataset,"package:datasets")   replaced by reactive
    dataset()
  })
}
ui <- fluidPage(
  selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
  verbatimTextOutput("summary"),
  tableOutput("table")
)
shinyApp(ui,server)