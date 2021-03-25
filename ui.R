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
                    column(3),
                    column(6, 
                           box(status = "info", width = "30%",
                               selectInput("nbrepas", "Nombres de repas : ", 
                                           choices= c("- 500", "500-3000", "3000-10000", "+ 10000", "Total")
                              ),
                               
                               radioButtons("loliouhisto", "Type de graphe désiré ",c("Histogramme" = "histo","graphe lolipop" = "loli"), 
                                            selected = c("graphe lolipop" = "loli"))
                           )
                           ),
                    
                    column(3),
                    
                    
                    fluidRow( 
                        box(title = "Pourcentage de produit bio
                            en fonction du prix moyen d'un repas ", 
                            status = "warning", solidHeader = TRUE,
                            collapsible = TRUE,
                            plotlyOutput(outputId = "plotbioprix")),
                        
                        box(title = "Pourcentage de produit bio
                            en fonction du prix moyen d'un repas ", 
                            status = "success", solidHeader = TRUE,
                            collapsible = TRUE,
                            plotlyOutput(outputId = "plotbioloc"))
                    ),
                    
                    fluidRow(
                        box(title = "Prix des repas en fonction du régime alimentaire ", 
                            background = "purple", solidHeader = TRUE,
                            plotOutput(outputId = "PrixRegAlim"), width = "70%"),
                        
                        box(title = "Proportion de repas bio en fonction de la proportion de produits locaux et du coût d'un repas ", 
                            background = "yellow", solidHeader = TRUE,
                            plotOutput(outputId = "PrixBioLoc"), width = "30%")
                        ),
                        
                    
                    fluidRow(
                               box(title = "Évolution du prix moyen d'un repas et la part
                                   en pourcentage de produits bio, en fonction des années", 
                                   status = "primary", solidHeader = TRUE,
                                   plotOutput(outputId = "linePriceBio")),
                               
                               box(title = "Qualité de la viande en fonction du régime alimentaire", 
                                   status = "success", solidHeader = TRUE,
                                   plotlyOutput(outputId = "Stacked"))
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

