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
                        checkboxGroupInput("loliouhisto", "Type de graphe désiré ",c("Histogramme" = "histo","graphe lolipop" = "loli"), 
                                           selected = c("graphe lolipop" = "loli")),
                        
                        box(
                            plotlyOutput(outputId = "plotbioprix")),
                        box(
                            plotlyOutput(outputId = "plotbioloc"))
                    ),
                    
                    fluidRow(
                               box(plotlyOutput(outputId = "linePriceBio")),
                               box(plotlyOutput(outputId = "Stacked"))
                    )
                    
                    
            ),
            
            tabItem(tabName = "Carte interactive"
                    
            ),
            
            tabItem(tabName = "Questionnaire"
                    
            )
        )
    )
)


