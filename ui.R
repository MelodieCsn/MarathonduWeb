dashboardPage(

  #Header
  dashboardHeader(title = ""),
    #img(src='UPB-LOGO-ROUGE-CADRE.png', align = "center", height=200, width =200)),
  
  # tags$li(class = "dropdown",
  #         tags$style(".main-header {max-height: 100px}"),
  #         tags$style(".main-header .logo {height: 100px}")
  # ),
  # 
  # tags$li(div(
  #   img(src = 'UPB-LOGO-ROUGE-CADRE.png',
  #       height = "100px",  width ="100px"),
  #   style = "padding-top:auto; padding-right:100px;"),
  #   class = "dropdown")
  # ),
  
  
  #Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Statistiques", tabName = "Stat", icon = icon("chart-bar")),
      menuItem("Carte interactive", tabName = "Carte", icon = icon("map-marked-alt"))
      #img(src='UPB-LOGO-ROUGE.png', align = "center", height=200, width =200)
    )
  ),
  dashboardBody(
    
    
    
    
    tags$head(tags$style(HTML('
                                
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #d83e00;
                                background-size: contain;
                                background-repeat: no-repeat;
                                background-image: url("UPB-LOGO-ROUGE-CADRE.png");
                                background-position: 100% 100%;
                                height: 150px;
                                }
                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-image: url("UPB-LOGO-ROUGE.png");
                                background-color: #d83e00;
                                }
                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #d83e00;
                                margin-bottom: 50px
                                }
                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #FFFFF7;
                                color: #d83e00;
                                border-right:10px solid #d83e00;
                                }
                                .skin-blue .main-header .navbar .sidebar-toggle {
                                display:none;
                                
                                }
                                /* active selected tab in the sidebarmenu */
                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a,
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #d83e00;
                                color: #FFFFF7;
                                font-size: 20px;
                                }
                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color:  #FFFFF7;
                                color: #FFFFF7;
                                color: #d83e00;
                                font-size: 20px;
                                }
                                #sidebarItemExpanded {
                                padding-top: 115px;
                                }
                                #sidebarCollapsed {
                                width : 240px;
                                }
                                /* remove left bar on active item */
                                .skin-blue .sidebar-menu > li > a,
                                .skin-blue .sidebar-menu > li.active > a {
                                border: none
                                }
                                .btn.dropdown-toggle.btn-primary{
                                background-color : #613e92;
                                }
                                input:checked{
                                color : #613e92 !important;
                                }
                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #FFFFF7;
                                margin-left : 0px
                                margin-top : 0px
                                padding: 15px
                                }
                                /*jaune*/
                                .box.box-solid.box-warning {
                                border-color: #fde03c;
                                }
                                .box.box-solid.box-warning > .box-header {
                                background-color: #fde03c;
                                }
                                /*vert*/
                                .box.box-solid.box-success {
                                border-color: #007d57;
                                }
                                .box.box-solid.box-success > .box-header {
                                background-color: #007d57;
                                }
                                /*violet*/
                                .box.box-solid.box-primary {
                                border-color: #613e92;
                                }
                                
                                .box.box-primary {
                                border-top-color : #613e92;
                                }
                                
                                .box.box-solid.box-primary > .box-header {
                                background-color: #613e92;
                                }
                                /*rouge*/
                                .box.box-solid.box-danger {
                                border-color: #e95a32;
                                }
                                .box.box-solid.box-danger > .box-header {
                                background-color: #e95a32;
                                }
                                /*bleu*/
                                .box.box-solid.box-info {
                                border-color: #d0ebfc;
                                }
                                .box.box-solid.box-info > .box-header {
                                background-color: #d0ebfc;
                                }
                                
                                
                                '))),
    tags$style(type = "text/css", "#mymap {height: calc(100vh - 80px) !important;}"),
    tags$style(type = "text/css", "#PrixBioLoc {height: calc(75vh) !important;}"),
    tags$style(type = "text/css", "#PrixRegAlim {height: calc(60vh) !important;}"),
    tabItems(
      tabItem(tabName = "Stat",
              # Input: selectInput ----
              column(12,
              column(2,box(status = "primary", width = "30%",
                            pickerInput("nbrepas", "Nombres de repas : ", 
                                        choices= c("- 500", "500-3000", "3000-10000", "+ 10000", "Total"),
                                        options = list(style = "btn-primary")),
                            
                            prettyRadioButtons("loliouhisto", "Type de graphe désiré",c("Histogramme" = "histo","Loliplot" = "loli"), 
                                               selected = c("graphe lolipop" = "loli"),icon = icon("check"), 
                                               bigger = TRUE,
                                               status = "info",
                                               animation = "jelly")
              )),
              column(1),
              column(8,h2("Comment évolue la part de bio dans vos repas... Quel en est l'impact sur le budget...", align = "center", 
                          style="font-family:gangster grotesk"),
                     h1("...Dans les cantines de même taille que la vôtre !*", align="center", style="font-family:gangster grotesk")),
              column(1)
              ),

              fluidRow(
                box(title = "Prix moyen d'un repas en fonction du pourcentage de produit bio
                              ",
                    status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    plotlyOutput(outputId = "plotbioprix"),
                    h3("Il n'y a pas d'augmentation nette du prix du repas avec l'augmentation de la part de produit bio.",
                       style="font-family:gangster grotesk",align="center")
                    ),

                box(title = "Pourcentage de produit bio
                            en fonction du prix moyen d'un repas ",
                    status = "success", solidHeader = TRUE,
                    collapsible = TRUE,
                    plotlyOutput(outputId = "plotbioloc"),
                    h3("Les structures qui servent le plus de produits bio servent aussi le plus de produits locaux.",
                       style="font-family:gangster grotesk", align="center"))
              ),

              fluidRow(
                box(title = "Prix des repas en fonction du régime alimentaire ",
                    plotOutput(outputId = "PrixRegAlim"), width = "70%",
                    status = "danger", solidHeader = TRUE, collapsible = TRUE,
                    h3("Servir des plats végétariens est un levier pour maîtriser son budget.", style="font-family:gangster grotesk",align="center"),
                    h3(" La distribution des structures qui ne proposent pas de plats végétariens tend vers les prix les plus élevés",
                      style="font-family:gangster grotesk; font-style:italic;",align="center")
                    ),
                
                
                box(title = "Proportion de repas bio en fonction de la proportion de produits locaux et du coût d'un repas ",
                    status = "primary", solidHeader = TRUE,
                    collapsible = TRUE, width = "30%", height = "60%",
                    plotOutput(outputId = "PrixBioLoc"),
                    h3("Augmenter la part de bio et de local dans l'assiette ne conduit pas forcément à une augmentation du prix.",style="font-family:gangster grotesk", align="center"),
                    h3("Chaque bulle représente une structure: plus elle est petite, plus le prix par repas est faible.",style="font-family:gangster grotesk;font-style:italic;",align="center")
                    )
              ),


              fluidRow(
                box(title = "Évolution du prix moyen d'un repas et la part
                                   en pourcentage de produits bio, en fonction des années",
                    status = "primary", solidHeader = TRUE,
                    plotOutput(outputId = "linePriceBio"),
                    h3("La part de bio servie dans les cantines suivies depuis 2018 a augmenté, pas le prix !",
                       style="font-family:gangster grotesk", align="center")
                    
                    ),
                box(plotlyOutput(outputId = "Stacked"),
                    title = "Qualité de la viande en fonction du régime alimentaire",
                    status = "success", solidHeader = TRUE,
                    h3("Les structures qui proposent un repas végétarien sont aussi celles qui proposent le plus de viande bio !" ,
                       style="font-family:gangster grotesk", align="center")
                )
              ),

              fluidRow(
                column(2),
                column(8, align="center", offset=4, 
                       box(title = "Diagramme de Venn",
                           status = "danger", solidHeader = TRUE,
                           plotOutput(outputId = "venn"),
                           h3("Intersection entre le fait de proposer plus de 20% de bio, au moins un repas végétarien et avoir un prix inférieur à 2,50€",
                              style="font-family:gangster grotesk", align="center")
                      )
                ),
                       
               column(2,
                      img(src='UPB POMME4.png',style="position:relative ;bottom:500px;left:50px", height=300, width =300)
              
              )
      
            ),
            h3("*Les données qui ont permis la réalisation de ces analyses statistiques proviennent des réponses aux questionnaires de Observatoire National de la Restauration Collective bio et durable des années 2018, 2019 et 2020",
               style="font-family:gangster grotesk; font-style:italic;",align="center")
      ),

      tabItem(tabName = "Carte",
              #titlePanel("Répartition géographique"),
              # Sidebar panel for inputs ----
              h2("Explorez la carte dynamique à la rencontre des cantines suivies par l'association !", style="font-family:gangster grotesk", align="center"),
              h2("Visualisez les indicateurs mesurés projetés à l'échelle du département.",style="font-family:gangster grotesk", align="center"),
              prettyRadioButtons("projection", "Affichage : ",
                                 choices= c("Part du bio (en %)" = "bio" ,
                                            "Part de produits Locaux (en%)" = "loc",
                                            "Prix moyen par repas" = "prix" ,
                                            "% Au moins un menu végétarien hebdomadaire" = "vege"),
                                 icon = icon("check"), 
                                 bigger = TRUE,
                                 status = "info",
                                 animation = "jelly"),
              leafletOutput(outputId = "mymap")
      ),
      tabItem(tabName="test",
              p("COUCOU")
      )
    )
  )
)