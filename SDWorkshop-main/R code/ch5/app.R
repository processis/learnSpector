library(shiny)
library(aimsir17)
library(ggplot2)
library(glue)
library(dplyr)
library(tidyr)

# Load the model function
source("Model.R")

ui <- fluidPage(
  titlePanel("Simulating the SIR Model"),
  sliderInput("con", "aEffective.Contact.Rate:",
              min = 2, max = 20, value = 6
  ),
  
  
  plotOutput("sim_output")
)

server <- function(input, output, session){
  message("\nStarting the server...")
  output$sim_output <- renderPlot({
    message("\nreacting to input control change...")
    sim <- run_sir(contacts = input$con
                    ) 
    sim <- sim %>%
      select(time,I,R) %>%
      pivot_longer(cols = -time,
                   names_to = "Variable",
                   values_to = "Value")
    
    ggplot(sim,aes(x=time,y=Value,colour=Variable))+
      geom_point()+geom_line()+
      facet_wrap(~Variable,nrow = 2,scales="free")
    
  })
}

shinyApp(ui, server)
