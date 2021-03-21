library(shiny)
library(tidyverse)
library(readr)
library(shinyWidgets)
ui <- fluidPage(
  titlePanel("Varikliniu transporto priemoniu atsarginiu daliu
             ir pagalbiniu reikmenu mazmenine prekyba"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = "imones_kodas", label = "Imones kodas", 
                     choices = NULL, selected = NULL)
    ),
    mainPanel(tabsetPanel(
      tabPanel("grafikas", plotOutput("plot")),
      tabPanel("lentele", tableOutput("table"))
    )
    )
  ),
  setBackgroundColor(
    color = c("#F7FBFF","khaki")
  )
)

server <- function(input, output, session) {
  data <- read_csv("../data/lab_sodra.csv")
  data <- data %>%
    filter(ecoActCode == 453200)
  updateSelectizeInput(session, "imones_kodas", choices = data$code,
                       server = TRUE)
  
  output$table <- renderTable(
    data %>%
      filter(code == input$imones_kodas), digits = 0
  )
  
  output$plot <- renderPlot(
    data %>%
      filter(code == input$imones_kodas) %>%
      ggplot(aes(x = month, y = avgWage, colour = name)) +
      geom_line() + scale_color_manual(values = "gold") + theme_light() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.line = element_line(size = 0.5), 
            axis.ticks = element_line(colour = "black", size = 1))
  )
}

shinyApp(ui = ui, server = server)
