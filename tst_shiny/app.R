server <- function(input, output){
  output$distPlot <- renderPlot({
    hist(rnorm(input$nbr_obs), col = 'khaki3', border = 'white',
         breaks = input$breaks,
         main = input$title,
         xlab = "random observations")
  })
  output$nbr_txt <- renderText({
    paste("You have selected", input$nbr_obs, "observations.")
  })
}

ui <- fluidPage(
  titlePanel("Our random simulator"), 
  sidebarLayout(
    sidebarPanel(
      sliderInput("nbr_obs" , "Number of observations:",
             min = 10, max = 500, value = 100),
      sliderInput("breaks" , "Number of bins:",
             min =4 , max = 50, value =10),
      textInput("title", "Title of the plot",
            value = "title goes here")
  ),
  mainPanel(plotOutput("distPlot"),
            h4("Conclusion"),
            p("small sample..."),
            "Note that we can provide text, but not <b>html code</b> directly.",
            textOutput("nbr_txt")
            )
  )
)
#
shinyApp(ui = ui, server = server)
