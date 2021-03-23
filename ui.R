dashboardPage(
  
    #Header
    dashboardHeader(title = "Un plus Bio"),
    #Sidebar
    dashboardSidebar(
      sidebarMenu(
        menuItem("Stat", tabName = "Stat"),
        menuItem("Carte interactive", tabName = "Carte interactive"),
        menuItem("Questionnaire", tabName = "Questionnaire")
      )
    ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Stat",
              # Input: selectInput ----
              
              selectInput("nbrepas", "Nombres de repas : ", 
                          choices= c("- 500", "500-3000", "3000-10000", "+ 10000", "Total")
              ),
              fluidRow( 
                
                box(
                  plotOutput(outputId = "distPlot")),
                box(
                  plotOutput(outputId = "distPlotProdLoc"))
              ),
              
              fluidRow(
                
                    splitLayout(style = "border: 1px solid silver:", 
                                plotOutput(outputId = "Pie1"),
                                plotOutput(outputId = "Pie2"),
                                plotOutput(outputId = "Pie3")
                    )
                
              
              )
            )
  
  
            )
      )
)
  
 
  