library(shiny)
server <- function (input,output,
session){
  output$summary <- renderPrint({
    dataset <- get(input$dataset,"package:datasets")
    summary(dataset)
  })
  output$table <- renderTable({
    dataset <- get(input$dataset,"package:datasets")
    dataset
  })
}
ui <- fluidPage(
  selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
  verbatimTextOutput("summary"),
  tableOutput("table")
)
shinyApp(ui,server)