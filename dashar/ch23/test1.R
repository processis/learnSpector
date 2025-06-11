# 加载必要的库
library(shiny)
library(DT)

# 定义UI
ui <- fluidPage(
  titlePanel("数据上传与分析"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "上传CSV文件", accept = c(".csv")),
      tags$hr(),
      checkboxInput("header", "第一行是表头", TRUE),
      radioButtons("sep", "分隔符",
                   choices = c(逗号 = ",", 分号 = ";", 制表符 = "\t"),
                   selected = ","),
      radioButtons("quote", "引号",
                   choices = c(无 = "", 双引号 = '"', 单引号 = "'"),
                   selected = '"')
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("数据与分析",
                 h3("上传的数据"),
                 DTOutput("data_table"),
                 h3("基础分析"),
                 verbatimTextOutput("summary")
        )
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
  
  # 显示数据表
  output$data_table <- renderDT({
    datatable(data())
  })
  
  # 显示基础分析结果
  output$summary <- renderPrint({
    df <- data()
    summary(df)
  })
}

# 运行Shiny应用
shinyApp(ui = ui, server = server)