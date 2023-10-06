server <- function (input,output,
session){
  output$files<-renderTable(input$upload)

}
ui <- fluidPage(
  fileInput("upload" , NULL , buttonLabel = "uploading ...", multiple = TRUE),
  tableOutput("files")
)
shinyApp(ui,server)