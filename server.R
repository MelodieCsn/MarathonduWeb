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
    pie3D(x=dfnovege$freq,start=2, labels=dfnovege$category, col=myPalette, theta=3.14/2, 
          main = "Menus non végétariens",
          radius = 0.8,
          height = 0.025,
          labelcex = 0.85)
    
  })
  
  output$Pie2 <- renderPlot({
    pie3D(x=dfvegehebdo$freq, start=sqrt(2),labels=dfvegehebdo$category, col=myPalette, theta=3.14/2, 
          main = "Menus végétariens hebdomadaires",
          radius = 0.8,
          height = 0.025,
          labelcex = 0.85
          )
    
  })
  
  output$Pie3 <- renderPlot({
    pie3D(x=dfvegequot$freq, start=sqrt(2),labels=dfvegequot$category, col=myPalette, theta=3.14/2, 
          main = "Menus végétariens quotidiens",
          radius = 0.8,
          height = 0.025,
          labelcex = 0.85
          )
    
    
  })
}

