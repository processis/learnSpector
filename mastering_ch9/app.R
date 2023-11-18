#Mastering ch 9 case study p150
server <- function (input,output,
session){
  # upload -------
  raw <- reactive({
    req(input$file)
    delim <- if(input$delim == "") NULL else input$delim
    vroom::vroom(input$file$datapath, delim = delim, skip = input$skip)
  })
  output$preview1 <- renderTable(head(raw(), input$rows))
#clean --------
  tidied <- reactive({
    out <- raw()
    if (input$snake){
      name(out) <- janitor::make_clean_names(names(out))
    }
    if (input$empty){
      out <- janitor::remove_empty(out,"cols")
    }
    if (input$constant){
      out <-janitor::remove_constant(out)
    }
    out
  })
  output$preview2 <- renderTable(head(tidied(), input$rows))
  # download ---------
  output$download <- downloadHandler(
    filename = function(){
      paste0(tools::file_path_sans_ext(input$file$name), ".tsv")
    },
    content = function(file) {
      vroom:vroom_write(tidied(),file)
    }
  )
}
# upload and parsing file
ui_upload <- sidebarLayout(
  sidebarPanel(
    fileInput("file", "Data", buttonLabel = "Upload..."),
    textInput("delim", "Delimiter(leave balnk to guess)", ""),
    numericInput("skip", "Rows to skip", 0, min = 0),
    numericInput("rows","ows to preview", 10, min = 1)
  ),
  mainPanel(
    h3("Cleaner data"),
    tableOutput("preview2")
  )
)
#clean file
ui_clean <- sidebarLayout(
  sidebarPanel(
    checkboxInput("snake","Rename col to snake case?"),
    checkboxInput("constant", "Remove constant columns?"),
    checkboxInput("empty" , "Remove empty cols?")
  ),
  mainPanel(
    h3("Clener data"),
    tableOutput("preview2")
  )
)
#downlad file
ui_download <- fluidRow(
  column(width = 12 , downloadButton("download" , class = "btn-block"))
)
ui <- fluidPage(
  ui_upload,
  ui_clean,
  ui_download
)
shinyApp(ui,server)