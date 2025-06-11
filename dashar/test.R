library(shiny)
library(datasets)
library(DT)
library(ggplot2)
library(reshape2)  # 用于数据重塑
library(corrplot)  # 用于绘制相关性热力图
library(glmnet)  # 用于岭回归分析
library(e1071)
library(pls)
library(earth)

ui <- shinyUI(fluidPage(
  titlePanel("Column Plot"),
  tabsetPanel(
    tabPanel("Upload File",
             titlePanel("Uploading Files"),
             sidebarLayout(
               
               
               sidebarPanel(
                 fileInput('file1', 'Choose CSV File',
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                 
                 # added interface for uploading data from
                 # http://shiny.rstudio.com/gallery/file-upload.html
                 tags$br(),
                 checkboxInput('header', 'Header', TRUE),
                 radioButtons("sep", "Separator",
                              choices = c(Comma = ",",
                                          Semicolon = ";",
                                          Tab = "\t"),
                              selected = ","),
                 radioButtons("quote", "Quote",
                              choices = c(None = "",
                                          "Double Quote" = '"',
                                          "Single Quote" = "'"),
                              selected = '"'),
                 
                 
                 radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                              inline = TRUE),
                 downloadButton('downloadReport')
                 
                 
                 
                 
                 
               ),
               mainPanel(
                 verbatimTextOutput("summar"),
                 tableOutput('contents')
                 
                 
               )
             )
    ),
    
    
    
    #散点图
    tabPanel("First Type",
             pageWithSidebar(
               headerPanel('Scatter plot'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput("PLOTxvar", "CHOOSE X", choices = NULL),
                 selectInput("PLOTyvar", "CHOOSEY", choices = NULL),
                 actionButton("plot", "PLOT scatterplot")
               ),
               mainPanel(
                 plotOutput("scatterplot"),
                 verbatimTextOutput("regression_equation")  # 显示回归方程
               )
             )
    ),
    
    #直方图
    
    tabPanel("second Type",
             pageWithSidebar(
               headerPanel('histogram '),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 #selectInput('xcol2', 'X Variable', ""),
                 #selectInput('ycol2', 'Y Variable', "", selected = "")
                 
                 #numericInput("n","NUmber of observations:",value=100),
                 #submitButton("Update Plot")
                 
                 
                 #fileInput("file", "上传CSV文件", accept = c(".csv")),
                 uiOutput("column_selector2")
                 
                 
                 
                 
                 
               ),
               mainPanel(
                 plotOutput('Myhistogram')
               )
             )
    ),
    
    
    
    tabPanel("third Type",
             pageWithSidebar(
               headerPanel('select data'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 uiOutput("column_selector3"),
                 uiOutput("filter_selector3")
                 
               ),
               mainPanel(
                 DTOutput("filtered_table")
               )
             )
    ),
    
    
    
    
    
    
    
    
    #回归分析
    tabPanel("fifth Type",
             pageWithSidebar(
               headerPanel('regression analysis'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 #selectInput('xcol5', 'X Variable', ""),
                 
                 #selectInput('ycol5', 'Y Variable', "", selected = "")
                 actionButton("choice", "Define Regression Variables"),
                 selectInput("independent", "Independent Variables:", choices = NULL, multiple = T),
                 
               ),
               mainPanel(
                 #actionButton("choice", "Define Regression Variables"),
                 #selectInput("independent", "Independent Variables:", choices = NULL, multiple = T),
                 
                 uiOutput("dependent1"),
                 #tableOutput("Table_selected.col"),
                 verbatimTextOutput("regTab")
               )
             )
    ),
    
    
    
    
    
    #热力图 相关性分析
    
    tabPanel("sixth Type",
             pageWithSidebar(
               headerPanel('Correlation Analysis'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 # selectInput('xcol6', 'X Variable', ""),
                 # selectInput('ycol6', 'Y Variable', "", selected = ""),
                 # actionButton("choice", "incorporate external information"),
                 #selectInput("columns", "Select Columns", choices = NULL), # no choices before uploading 
                 uiOutput("variable_select"),  # 动态生成变量选择器
                 actionButton("COLanalyze", "CONDUCT correlation ")
                 
               ),
               mainPanel(
                 h4("correlation matrix"),
                 tableOutput("correlation_matrix"),  # 显示相关性矩阵
                 h4("THERMAL MAP"),
                 plotOutput("heatmap")  # 显示热力图
                 
                 #selectInput("columns", "Select Columns", choices = NULL), # no choices before uploading 
                 
               )
             )
    ),
    
    
    
    #岭回归分析
    tabPanel("senventh Type",
             pageWithSidebar(
               headerPanel(' ridge regression '),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 #selectInput('xcol5', 'X Variable', ""),
                 
                 #selectInput('ycol5', 'Y Variable', "", selected = "")
                 uiOutput("response_selector"),  # 选择响应变量
                 uiOutput("predictor_selector"), # 选择预测变量
                 sliderInput("lambda", "(lambda)", min = 0, max = 10, value = 1, step = 0.1),
                 actionButton("run", "ridge regression")
               ),
               mainPanel(
                 #actionButton("choice", "Define Regression Variables"),
                 #selectInput("independent", "Independent Variables:", choices = NULL, multiple = T),
                 
                 verbatimTextOutput("summary"),  # 显示岭回归结果
                 plotOutput("coef_plot")        # 绘制系数图
               )
             )
    ),
    
    
    
    
    #SVM
    
    #SVM预测
    tabPanel("eighth Type",
             pageWithSidebar(
               headerPanel('SVM'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 #selectInput('xcol5', 'X Variable', ""),
                 
                 #selectInput('ycol5', 'Y Variable', "", selected = "")
                 actionButton("SVMpredict", "PREDICT"),
                 selectInput("x_axis8", "CHOOSE X", choices = NULL), # 选择X轴变量
                 selectInput("y_axis8", "CHOOSE Y", choices = NULL)  # 选择Y轴变量
               ),
               mainPanel(
                 #actionButton("choice", "Define Regression Variables"),
                 #selectInput("independent", "Independent Variables:", choices = NULL, multiple = T),
                 
                 
                 verbatimTextOutput("SVMprediction"),# 显示预测结果
                 plotOutput("SVMplot") # 显示绘图
               )
             )
    ),
    
    #偏最小二乘回归分析
    
    tabPanel("ninth Type",
             pageWithSidebar(
               headerPanel('Partial Least Squares '),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 #selectInput('xcol5', 'X Variable', ""),
                 
                 #selectInput('ycol5', 'Y Variable', "", selected = "")
                 selectInput("plsresponse", "CHOOSE X", choices = NULL),
                 selectInput("plspredictors", "CHOOSE Y", choices = NULL, multiple = TRUE),
                 actionButton("runpls", "RUN PLS")
               ),
               mainPanel(
                 #actionButton("choice", "Define Regression Variables"),
                 #selectInput("independent", "Independent Variables:", choices = NULL, multiple = T),
                 
                 
                 verbatimTextOutput("plssummary"),
                 plotOutput("plsplot")
               )
             )
    ),
    
    
    #多元自适应回归样条分析 (MARS)
    
    
    tabPanel("tenth Type",
             pageWithSidebar(
               headerPanel('MARS'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 #selectInput('xcol5', 'X Variable', ""),
                 
                 #selectInput('ycol5', 'Y Variable', "", selected = "")
                 selectInput("marsresponse", "CHOOSE X", choices = NULL),
                 selectInput("marspredictors", "CHOOSE Y", choices = NULL, multiple = TRUE),
                 actionButton("marsrun", "RUNMARS")
               ),
               mainPanel(
                 #actionButton("choice", "Define Regression Variables"),
                 #selectInput("independent", "Independent Variables:", choices = NULL, multiple = T),
                 
                 
                 verbatimTextOutput("marssummary"),
                 plotOutput("marsplot")
               )
             )
    ),
    
    
    #RESM分析
    
    tabPanel("eleventh Type",
             pageWithSidebar(
               headerPanel('RESM'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 #selectInput('xcol5', 'X Variable', ""),
                 
                 #selectInput('ycol5', 'Y Variable', "", selected = "")
                 selectInput("RESMxvar", "CHOOSE X", choices = NULL),
                 selectInput("RESMyvar", "CHOOSE Y", choices = NULL),
                 actionButton("RESManalyze", "RUN RMSE")
               ),
               mainPanel(
                 #actionButton("choice", "Define Regression Variables"),
                 #selectInput("independent", "Independent Variables:", choices = NULL, multiple = T),
                 
                 
                 plotOutput("RESMplot"),
                 verbatimTextOutput("RESMsummary")
               )
             )
    )
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  )
)
)









server <- shinyServer(function(input, output, session) {
  # added "session" because updateSelectInput requires it
  
  #req(input$file1)
  
  data <- eventReactive(input$choice, { 
    req(input$file1) ## ?req #  require that the input is available
    
    inFile <- input$file1
    
    # tested with a following dataset: write.csv(mtcars, "mtcars.csv")
    # and                              write.csv(iris, "iris.csv")
    df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                   quote = input$quote)
    
    vars <- names(df)
    # Update select input immediately after clicking on the action button. 
    updateSelectInput(session, "columns","Select Columns", choices = vars)
    
    df
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    # Update inputs (you could create an observer with both updateSel...)
    # You can also constraint your choices. If you wanted select only numeric
    # variables you could set "choices = sapply(df, is.numeric)"
    # It depends on what do you want to do later on.
    
    #xia la kuang xuan xiang
    #yu UI dui ying
    
    updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                      choices = names(df), selected = names(df)[2])
    
    
    updateSelectInput(session, inputId = 'xcol2', label = 'X Variable',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycol2', label = 'Y Variable',
                      choices = names(df), selected = names(df)[2])
    
    
    updateSelectInput(session, inputId = 'xcol3', label = 'X Variable',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycol3', label = 'Y Variable',
                      choices = names(df), selected = names(df)[2])
    
    #    updateSelectInput(session, inputId = 'ycol4', label = 'Y Variable',
    #                      choices = names(df), selected = names(df)[2])
    
    updateSelectInput(session, inputId = 'xcol5', label = 'X Variable',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycol5', label = 'Y Variable',
                      choices = names(df), selected = names(df)[2])
    
    
    updateSelectInput(session, inputId = 'xcol6', label = 'X Variable',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycol6', label = 'Y Variable',
                      choices = names(df), selected = names(df)[2])
    
    
    
    return(df)
  })
  
  output$contents <- renderTable({
    data()
    
  })
  
  output$summar <- renderPrint({
    req(input$file1)
    summary(data())
  })
  
  
  
  #dowmload  PDF
  
  regFormula <- reactive({
    as.formula(paste('mpg ~', input$x))
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('report.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
  
  
  
  
  
  
  #散点图
  
  # 更新变量选择
  observeEvent(data(), {
    updateSelectInput(session, "PLOTxvar", choices = names(data()))
    updateSelectInput(session, "PLOTyvar", choices = names(data()))
  })
  
  # 计算回归方程
  regression_model <- reactive({
    req(input$PLOTxvar, input$PLOTyvar)
    lm(as.formula(paste(input$PLOTyvar, "~", input$PLOTxvar)), data = data())
  })
  
  # 绘制散点图和回归线
  output$scatterplot <- renderPlot({
    req(input$PLOTxvar, input$PLOTyvar)
    ggplot(data(), aes_string(x = input$PLOTxvar, y = input$PLOTyvar)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      theme_minimal()
  })
  
  # 显示回归方程
  output$regression_equation <- renderPrint({
    req(regression_model())
    model <- regression_model()
    intercept <- coef(model)[1]
    slope <- coef(model)[2]
    cat("回归方程:\n")
    cat(paste(input$PLOTyvar, "=", round(slope, 2), "*", input$PLOTxvar, "+", round(intercept, 2))
    )})
  
  
  #生成 直方图
  # 动态生成列选择器
  output$column_selector2 <- renderUI({
    req(data())
    selectInput("column", "选择要绘制直方图的列", choices = names(data()))
  })
  
  
  
  output$Myhistogram <- renderPlot({
    # for a histogram: remove the second variable (it has to be numeric as well):
    #x    <- data()[, c(input$xcol, input$ycol)]
    #bins <- nrow(data())
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
    # Correct way:
    #x    <- data()[, input$xcol]
    #y    <- data()[, input$ycol]
    #x    <- data()[, c(input$xcol, input$ycol)]
    #bins <- nrow(data())
    #hist(x,  col = 'darkgray', border = 'white')
    #hist(y,  col = 'darkgray', border = 'white')
    
    
    # I Since you have two inputs I decided to make a scatterplot
    #x <- data()[, c(input$xcol, input$ycol)]
    #plot(x)
    #hist(x)
    
    #data<-rnorm(input$n)
    #ggplot(data.frame(x=data),aes(x=x))+geom_histogram()
    
    req(input$column)
    ggplot(data(), aes_string(x = input$column)) +
      geom_histogram(binwidth = 3, fill = "blue", color = "black") +
      labs(title = paste("直方图 -", input$column), x = input$column, y = "频率")
    
    
  })
  
  
  
  
  
  output$Mycol <- renderPlot({
    # for a histogram: remove the second variable (it has to be numeric as well):
    #x    <- data()[, c(input$xcol, input$ycol)]
    #bins <- nrow(data())
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
    # Correct way:
    #x    <- data()[, input$xcol]
    #bins <- nrow(data())
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
    
    # I Since you have two inputs I decided to make a scatterplot
    x <- data()[, c(input$xcol, input$ycol)]
    plot(x)
    
  })
  
  ################
  
  # 动态生成列选择器
  output$column_selector3 <- renderUI({
    req(data())
    selectInput("column", "选择要筛选的列", choices = names(data()))
  })
  
  # 动态生成筛选条件选择器
  output$filter_selector3 <- renderUI({
    req(input$column)
    column_data <- data()[[input$column]]
    if (is.numeric(column_data)) {
      sliderInput("filter", "选择数值范围", min = min(column_data), max = max(column_data), value = c(min(column_data), max(column_data)))
    } else {
      selectInput("filter", "选择类别", choices = unique(column_data), multiple = TRUE)
    }
  })
  
  # 根据筛选条件显示部分数据表
  output$filtered_table <- renderDT({
    req(input$column, input$filter)
    filtered_data <- data()
    column_data <- filtered_data[[input$column]]
    if (is.numeric(column_data)) {
      filtered_data <- filtered_data[column_data >= input$filter[1] & column_data <= input$filter[2], ]
    } else {
      filtered_data <- filtered_data[column_data %in% input$filter, ]
    }
    datatable(filtered_data)
  })
  
  
  
  
  #  output$Mylm<-renderPrint(
  #    {
  #      x    <- data()[, input$xcol]
  #      y<-data()[, input$ycol]
  
  
  #      model<-lm(y~x)
  #      summary(model())
  #   }
  #  )
  
  #回归分析
  
  info <- eventReactive(input$choice, {
    req(data())
    f <- data()
    f
  })
  
  observeEvent(input$choice, {  ## to update only when you click on the actionButton
    req(info())
    updateSelectInput(session,"independent", "Please Select independent Variable(s):", choices = names(info()) )
  })
  
  
  # output$Table_selected.col <- renderTable({
  #   input$choice
  #   req(info(),input$columns)
  #   f = info()
  #   f = subset(f, select = input$columns) #subsetting takes place here
  #   head(f)
  # })
  
  output$dependent1 = renderUI({
    req(data(),input$independent)
    radioButtons("dependent1", "Select a dependent Variable:",choices=names(data())[!names(data()) %in% input$independent])
  })
  
  ###  need to build your formuila correctly; It will work with multiple independent variables
  ###  model <- reactive({lm(reformulate(input$IndVar, input$DepVar), data = RegData)})
  
  runRegression <- reactive({
    req(data(),input$independent,input$dependent1)
    lm(reformulate(input$independent, input$dependent1), data=data())
    # multinom(reformulate(input$independent, input$dependent1), data=mydf())  ### mulitnomial from nnet package
  })
  
  output$regTab = renderPrint({
    req(runRegression())
    if(!is.null(input$independent)){
      summary(runRegression())
    } else {
      print(data.frame(Warning="Please select Model Parameters."))
    }
  })
  
  
  
  
  
  #热力图
  
  
  
  # 动态生成变量选择器
  output$variable_select <- renderUI({
    req(data())
    numeric_vars <- names(data())[sapply(data(), is.numeric)]  # 仅选择数值型变量
    checkboxGroupInput("selected_vars", "选择变量进行相关性分析", choices = numeric_vars)
  })
  
  # 计算相关性矩阵
  correlation_matrix <- reactive({
    req(input$selected_vars)
    selected_data <- data()[, input$selected_vars, drop = FALSE]
    cor(selected_data, use = "complete.obs")  # 计算相关性矩阵
  })
  
  # 显示相关性矩阵
  output$correlation_matrix <- renderTable({
    req(correlation_matrix())
    correlation_matrix()
  }, rownames = TRUE)
  
  # 绘制热力图
  output$heatmap <- renderPlot({
    req(correlation_matrix())
    melted_cormat <- melt(correlation_matrix())  # 将相关性矩阵转换为长格式
    ggplot(melted_cormat, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "", y = "", fill = "Correlation")
  })
  
  
  #岭回归方法
  
  # 动态生成响应变量选择器
  output$response_selector <- renderUI({
    req(data())
    selectInput("response", "选择响应变量", choices = names(data()))
  })
  
  # 动态生成预测变量选择器
  output$predictor_selector <- renderUI({
    req(data())
    selectInput("predictors", "选择预测变量", choices = names(data()), multiple = TRUE)
  })
  
  # 运行岭回归
  ridge_model <- eventReactive(input$run, {
    req(input$response, input$predictors)
    response <- data()[[input$response]]
    predictors <- as.matrix(data()[, input$predictors])
    
    # 岭回归
    glmnet(predictors, response, alpha = 0, lambda = input$lambda)
  })
  
  # 显示岭回归结果
  output$summary <- renderPrint({
    req(ridge_model())
    print(ridge_model())
  })
  
  # 绘制岭回归系数图
  output$coef_plot <- renderPlot({
    req(ridge_model())
    coef_values <- coef(ridge_model())
    coef_df <- data.frame(
      Predictor = rownames(coef_values),
      Coefficient = as.numeric(coef_values)
    )
    
    ggplot(coef_df, aes(x = Predictor, y = Coefficient)) +
      geom_bar(stat = "identity", fill = "blue") +
      theme_minimal() +
      labs(title = "岭回归系数图", x = "预测变量", y = "系数值")
  })
  
  
  
  
  # 动态更新X轴和Y轴变量选择
  observe({
    df <- data()
    updateSelectInput(session, "x_axis8", choices = colnames(df))
    updateSelectInput(session, "y_axis8", choices = colnames(df))
  })
  
  # 进行SVM预测
  observeEvent(input$SVMpredict, {
    df <- data()
    
    # 假设最后一列是标签，其余列是特征
    features <- df[, -ncol(df)]
    labels <- df[, ncol(df)]
    
    # 训练SVM模型
    svm_model <- svm(features, labels, type = "C-classification")
    
    # 进行预测
    SVMpredictions <- predict(svm_model, features)
    
    # 将预测结果添加到数据中
    df$SVMPrediction <- SVMpredictions
    
    # 显示预测结果
    output$SVMprediction <- renderPrint({
      table(SVMPrediction = df$SVMPrediction, Actual = df[, ncol(df) - 1])
    })
    
    # 绘制图形
    output$plot <- renderPlot({
      ggplot(df, aes(x = .data[[input$x_axis8]], y = .data[[input$y_axis8]], color = Prediction)) +
        geom_point(size = 3) +
        labs(title = "SVM预测结果", x = input$x_axis8, y = input$y_axis8) +
        theme_minimal()
    })
  })
  
  
  #偏最小二乘回归分析
  
  # 更新因变量和自变量的选择
  observeEvent(data(), {
    updateSelectInput(session, "plsresponse", choices = names(data()))
    updateSelectInput(session, "plspredictors", choices = names(data()))
  })
  
  # 运行PLS回归
  pls_model <- eventReactive(input$runpls, {
    req(input$plsresponse, input$plspredictors)
    formula <- as.formula(paste(input$plsresponse, "~", paste(input$plspredictors, collapse = "+")))
    plsr(formula, data = data(), validation = "CV")
  })
  
  # 显示回归结果
  output$plssummary <- renderPrint({
    req(pls_model())
    summary(pls_model())
  })
  
  # 绘制回归结果图
  output$plsplot <- renderPlot({
    req(pls_model())
    plot(pls_model())
  })
  
  
  #多元自适应回归样条分析 (MARS)
  
  # 更新因变量和自变量的选择
  observeEvent(data(), {
    updateSelectInput(session, "marsresponse", choices = names(data()))
    updateSelectInput(session, "marspredictors", choices = names(data()))
  })
  
  # 运行MARS分析
  mars_model <- eventReactive(input$marsrun, {
    req(input$marsresponse, input$marspredictors)
    formula <- as.formula(paste(input$marsresponse, "~", paste(input$marspredictors, collapse = "+")))
    earth(formula, data = data())
  })
  
  # 显示回归结果
  output$marssummary <- renderPrint({
    req(mars_model())
    summary(mars_model())
  })
  
  # 绘制回归结果图
  output$marsplot <- renderPlot({
    req(mars_model())
    plot(mars_model())
  })
  
  
  #RESM分析
  
  # 更新变量选择
  observeEvent(data(), {
    updateSelectInput(session, "RESMxvar", choices = names(data()))
    updateSelectInput(session, "RESMyvar", choices = names(data()))
  })
  
  # 进行RESM分析（线性回归）
  resm_model <- eventReactive(input$RESManalyze, {
    req(input$RESMxvar, input$RESMyvar)
    lm(as.formula(paste(input$RESMyvar, "~", input$RESMxvar)), data = data())
  })
  
  # 绘制图表
  output$RESMplot <- renderPlot({
    req(resm_model())
    ggplot(data(), aes_string(x = input$RESMxvar, y = input$RESMyvar)) +
      geom_point() +
      geom_smooth(method = "lm", col = "red") +
      ggtitle("线性回归拟合图")
  })
  
  # 显示模型摘要
  output$RESMsummary <- renderPrint({
    req(resm_model())
    summary(resm_model())
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
})






# 运行Shiny应用程序
shinyApp(ui = ui, server = server)