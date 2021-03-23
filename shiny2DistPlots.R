ui <- fluidPage(
    navbarPage(
        "UnPlusBio",
        tabPanel("Stats",
        # App title ----
            titlePanel("Prix repas par % bio"),
            
            # Sidebar layout with input and output definitions ----
            sidebarLayout(
                
                # Sidebar panel for inputs ----
                sidebarPanel(
                    
                    # Input: selectInput ----
                    selectInput("nbrepas", "Nombres de repas : ", 
                                choices= c("- 500", "500-3000", "3000-10000", "+ 10000", "Total"))
                    
                ),
                
                # Main panel for displaying outputs ----
                mainPanel(
                    
                    # Output: Histogram ----
                    
                    plotOutput(outputId = "distPlot"),
                    plotOutput(outputId = "distPlotProdLoc"),
                    fluidRow(
                        splitLayout(style = "border: 1px solid silver:", cellWidths = c(300,300,300),
                                    plotOutput(outputId = "Pie1"),
                                    plotOutput(outputId = "Pie2"),
                                    plotOutput(outputId = "Pie3")
                        )
                    )
                )
            )
        ),
        tabPanel("Carte interractive", "Ceci est une carte interractive"
        ),
        tabPanel("Questionnaire", "Questionnaire à l'intention des élus"
        )
    )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
    df_subset <- reactive({
        a <- subset(bdd20_dedup, bdd20_dedup$tot_rep_fact == input$nbrepas)
        return(a)
    })
    output$distPlot <- renderPlot({
        ggplot(data=df_subset(), aes(x=bio_fact, y=cmp)) + 
            geom_bar(stat = "summary",fill="#582c83")+
            theme_classic(base_size = 20)+
            geom_hline(yintercept = 3, linetype = "dashed")+
            xlab("% de bio")+
            ylab("Coûts denrées moyen par repas")
    })
    
    
    output$distPlotProdLoc <- renderPlot({
        ggplot(data=df_subset(), aes(x=bio_fact, y=loc)) + 
            geom_bar(stat = "summary",fill="#99e3ff")+
            theme_classic(base_size = 20)+
            geom_hline(yintercept = 100, linetype = "dashed")+
            xlab("% de bio")+
            ylab("% de produits locaux")
    })
    
    output$Pie1 <- renderPlot({
        pie3D(x=dfnovege$freq,start=2, labels=dfnovege$category, col=myPalette, theta=3.14/2, main = "Menus non végétariens")
        
    })
    
    output$Pie2 <- renderPlot({
        pie3D(x=dfvegehebdo$freq, start=sqrt(2),labels=dfvegehebdo$category, col=myPalette, theta=3.14/2, main = "Menus végétariens hebdomadaires")
        
    })
    
    output$Pie3 <- renderPlot({
        pie3D(x=dfvegequot$freq, start=sqrt(2),labels=dfvegequot$category, col=myPalette, theta=3.14/2, main = "Menus végétariens quotidiens")
        
    })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

