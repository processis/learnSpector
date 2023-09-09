#36.3.3 A Dashboard with shinydashboard
# this file should be called app.R
#library(shinydashboard)
# general code (not part of server or ui function)
my_seed <- 1865
set.seed(my_seed)
N <- 150
# user interface ui
ui <- dashboardPage(
  dashboardHeader(title = "ShinyDashBoard"),
  dashboardSidebar(
    title = "Choose ...",
    numericInput('N', 'Number of data points:', N),
    numericInput('my_seed', 'seed:', my_seed),
    sliderInput("bins", "Number of bins:",
                min = 1, max = 50, value = 30)
  ),
  dashboardBody(
    fluidRow(
      valueBoxOutput("box1"),
      valueBoxOutput("box2")
    ),
    plotOutput("p1", height = 250)
  )
)
# server function
server <- function(input, output) {
  d <- reactive({
    set.seed(input$my_seed)
    rnorm(input$N)
  })
  output$p1 <- renderPlot({
    x <- d()
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = 'deepskyblue3', border = 'gray')
  })
  output$box1 <- renderValueBox({
    valueBox(
      value
      = formatC(mean(d()), digits = 2, format = "f"),
      subtitle = "mean", icon
      = icon("globe"),
      color
      = "light-blue")
  })
  output$box2 <- renderValueBox({
    valueBox(
      value
      = formatC(sd(d()), digits = 2, format = "f"),
      subtitle = "standard deviation", icon
      = icon("table"),
      color
      = "light-blue")
  })
}
# load the app
shinyApp(ui, server)