# 定义UI
ui <- fluidPage(
  titlePanel("上传数据并进行分析"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "上传CSV文件", accept = c(".csv")),
      tags$hr(),
      checkboxInput("header", "文件包含表头", TRUE),
      radioButtons("sep", "分隔符", choices = c(逗号 = ",", 分号 = ";", 制表符 = "\t"), selected = ","),
      radioButtons("quote", "引号", choices = c(无 = "", 双引号 = '"', 单引号 = "'"), selected = '"')
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("数据预览", DTOutput("preview")),
        tabPanel("摘要统计", verbatimTextOutput("summary")),
        tabPanel("数据结构", verbatimTextOutput("structure"))
      )
    )
  )
)

# 定义服务器逻辑
server <- function(input, output) {
  
  # 读取上传的数据
  data <- reactive({
    req(input$file)
    inFile <- input$file
    df <- read.csv(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
    return(df)
  })
  
  # 显示数据预览
  output$preview <- renderDT({
    datatable(data())
  })
  
  # 显示摘要统计信息
  output$summary <- renderPrint({
    summary(data())
  })
  
  # 显示数据结构
  output$structure <- renderPrint({
    str(data())
  })
}

# 运行应用程序
shinyApp(ui = ui, server = server)