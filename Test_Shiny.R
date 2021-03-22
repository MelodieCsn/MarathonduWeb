ui <- fluidPage(
  
  # App title ----
  titlePanel("Prix repas par % bio"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  
  output$distPlot <- renderPlot({
    ggplot(data=bdd20_dedup, aes(x=bio_fact, y=cmp)) + 
      geom_bar(stat = "summary",fill="#DC4405")
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

