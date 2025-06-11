library(nnet)

ui = navbarPage(tabPanel("Regression Analysis",
                         dataTableOutput('mytable'),
                         sidebarLayout(
                           sidebarPanel(width=3, fileInput("file1", "Please choose a CSV file",
                                                           multiple = T,
                                                           accept = c("text/csv",
                                                                      "text/comma-separated-values,text/plain",
                                                                      ".csv")),
                                        tags$hr(),
                                        checkboxInput("header", "Header", TRUE),
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
                                        tags$hr(),
                                        radioButtons("disp", "Display",
                                                     choices = c(Head = "head",
                                                                 All = "all"),
                                                     selected = "head")
                                        
                           ),
                           
                           mainPanel(
                             tableOutput("contents"),
                             actionButton("choice", "Define Regression Variables"),
                             selectInput("independent", "Independent Variables:", choices = NULL, multiple = T),
                             uiOutput("dependent1"),
                             #tableOutput("Table_selected.col"),
                             verbatimTextOutput("regTab")
                           )
                         ),
                         
))
