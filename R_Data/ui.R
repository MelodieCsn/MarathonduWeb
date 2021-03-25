dashboardPage(
    
    #Header
    dashboardHeader(title = "Un plus Bio"),
    #Sidebar
    dashboardSidebar(
        sidebarMenu(
            menuItem("Stat", tabName = "Stat"),
            menuItem("Carte", tabName = "Carte"),
            menuItem("test", tabName = "test")
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "Stat",
                    # Input: selectInput ----
                    
                    selectInput("nbrepas", "Nombres de repas : ", 
                                choices= c("- 500", "500-3000", "3000-10000", "+ 10000", "Total")
                    ),
                    radioButtons("loliouhisto", "Type de graphe désiré ",c("Histogramme" = "histo","graphe lolipop" = "loli"), 
                                 selected = c("graphe lolipop" = "loli")),
                    fluidRow( 
                        box(
                            plotlyOutput(outputId = "plotbioprix")),
                        box(
                            plotlyOutput(outputId = "plotbioloc"))
                    ),
                    
                    fluidRow(
                        box(plotOutput(outputId = "PrixRegAlim"), width = "70%"),
                        box(plotOutput(outputId = "PrixBioLoc"), width = "30%")
                        ),
                        
                    
                    fluidRow(
                               box(plotOutput(outputId = "linePriceBio")),
                               box(plotlyOutput(outputId = "Stacked"))
                    ),
                    fluidRow(
                        
                        column(4,plotOutput(outputId = "venn"))
                    )
                    
                    
            ),
            
            tabItem(tabName = "Carte",
                    #titlePanel("Répartition géographique"),
                        # Sidebar panel for inputs ----
                    radioButtons("projection", "Affichage : ",
                                choices= c("Part du bio (en %)" = "bio" ,
                                            "Part de produits Locaux (en%)" = "loc",
                                            "Prix moyen par repas" = "prix" ,
                                            "% Au moins un menu végétarien hebdomadaire" = "vege")),
                    leafletOutput(outputId = "mymap"), withSpinner(tableOutput('mymap'), type=4)
            ),
            tabItem(tabName="test",
                    p("COUCOU")
            )
                        
        )
            
    )
)



