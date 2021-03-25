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
    tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #e45f2e;
                                }
                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #e45f2ed;
                                }
                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #e45f2e;
                                }
                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #e45f2e;
                                }
                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #e45f2e;
                                }
                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #e45f2e;
                                color: #000000;
                                }
                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #e45f2e;
                                }
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #114c2d;
                                }
                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #FFFFF7;
                                }
                                '))),
    tags$style(type = "text/css", "#mymap {height: calc(100vh - 80px) !important;}"),
    tags$style(type = "text/css", "#PrixBioLoc {height: calc(75vh) !important;}"),
    tags$style(type = "text/css", "#PrixRegAlim {height: calc(60vh) !important;}"),
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
                    plotOutput(outputId = "PrixRegAlim"), width = "70%",
                    background = "purple", solidHeader = TRUE, collapsible = TRUE),
                box(title = "Proportion de repas bio en fonction de la proportion de produits locaux et du coût d'un repas ",
                    background = "yellow", solidHeader = TRUE,
                    collapsible = TRUE, width = "30%",
                    plotOutput(outputId = "PrixBioLoc"))
              ),


              fluidRow(
                box(plotOutput(title = "Évolution du prix moyen d'un repas et la part
                                   en pourcentage de produits bio, en fonction des années",
                               status = "primary", solidHeader = TRUE,
                               outputId = "linePriceBio")),
                box(plotlyOutput(outputId = "Stacked"),
                    title = "Qualité de la viande en fonction du régime alimentaire",
                    status = "success", solidHeader = TRUE,
                )
              ),

              fluidRow(
                column(4),
                column(4,box(title = "Diagramme de Venn",
                             background = "maroon", solidHeader = TRUE,
                             plotOutput(outputId = "venn"))
                ),
                column(4)
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