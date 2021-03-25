dashboardPage(
    
    #Header
    dashboardHeader(title = "Un plus Bio"),
    #Sidebar
    dashboardSidebar(
        sidebarMenu(
            menuItem("Statistiques", tabName = "Stat", icon = icon("chart-bar")),
            menuItem("Carte interactive", tabName = "Carte", icon = icon("map-marked-alt"))
            
        )
    ),
    dashboardBody(
        tags$style(type = "text/css", "#mymap {height: calc(100vh - 80px) !important;}"),
        tags$style(type = "text/css", "#PrixBioLoc {height: calc(75vh) !important;}"),
        tags$style(type = "text/css", "#PrixRegAlim {height: calc(60vh) !important;}"),
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
                        box(plotOutput(outputId = "PrixRegAlim"), width = "70%", collapsible = TRUE),
                        box(plotOutput(outputId = "PrixBioLoc"), width = "30%", collapsible = TRUE)
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
                            
                    leafletOutput(outputId = "mymap")
            ),
            tabItem(tabName="test",
                    p("COUCOU")
            )
                        
        )
            
    )
)



