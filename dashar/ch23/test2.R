library(shiny)
library(ggplot2)
library(readxl)
library(DT)
#library(flextable)
library(kableExtra)
library(pagedown)

# 定义 UI 
ui <- fluidPage(
  titlePanel("数据分析与可视化"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "上传数据文件",
                accept = c(".csv", ".xlsx")),
      
      selectInput("chart_type", "选择图表类型",
                  choices = c("柱状图", "散点图")),
      
      downloadButton("download_report", "下载报告")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("数据预览",
                 DT::dataTableOutput("data_preview")),
        
        tabPanel("统计摘要",
                 verbatimTextOutput("summary")),
        
        tabPanel("可视化",
                 plotOutput("chart")),
        
        tabPanel("回归分析",
                 verbatimTextOutput("regression_result"))
      )
    )
  )
)

# 定义 Server 逻辑 
server <- function(input, output) {
  # 读取上传的文件 
  data <- reactive({
    req(input$file)
    if (input$file$dataptr == 0) {
      return(NULL)
    }
    ext <- tools::file_ext(input$file$name)
    if (ext == "csv") {
      read.csv(input$file$datapath, header = TRUE)
    } else if (ext == "xlsx") {
      read_excel(input$file$datapath)
    }
  })
  
  # 数据预览 
  output$data_preview <- DT::renderDataTable({
    data()
  })
  
  # 统计摘要 
  output$summary <- renderPrint({
    if (is.null(data())) return()
    summary(data())
  })
  
  # 可视化图表 
  output$chart <- renderPlot({
    if (is.null(data())) return()
    
    x_var <- names(data())[1]
    y_var <- names(data())[2]
    
    if (input$chart_type == "柱状图") {
      ggplot(data(), aes_string(x = x_var)) +
        geom_bar() +
        theme_minimal()
    } else {
      ggplot(data(), aes_string(x = x_var, y = y_var)) +
        geom_point() +
        theme_minimal()
    }
  })
  
  # 回归分析 
  output$regression_result <- renderPrint({
    if (is.null(data())) return()
    
    x_var <- names(data())[1]
    y_var <- names(data())[2]
    
    model <- lm(as.formula(paste(y_var, "~", x_var)), data = data())
    summary(model)
  })
  
  # 下载报告 
  output$download_report <- downloadHandler(
    filename = function() {
      paste("分析报告_", Sys.Date(), ".pdf", sep = "")
    },
    
    content = function(file) {
      if (is.null(data())) return()
      
      # 创建报告内容 
      report <- rmd_to_html(
        input = NULL,
        template = NULL,
        output_file = file,
        params = list(
          data = data(),
          summary = summary(data()),
          chart = print(
            ggplot(data(), aes_string(x = names(data())[1], y = names(data())[2])) +
              geom_point() +
              theme_minimal()
          ),
          regression = summary(lm(as.formula(paste(names(data())[2], "~", names(data())[1])), data = data()))
        )
      )
    }
  )
}

# 运行应用 
shinyApp(ui, server)