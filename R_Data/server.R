# Define server logic required to draw a histogram ----
server <- function(input, output) {
    df_subset <- reactive({
        a <- subset(bdd20_dedup, bdd20_dedup$tot_rep_fact == input$nbrepas)
        return(a)
    })
    df_join_subset2020 <- reactive({
        a <- subset(dfjoin, dfjoin$tot_rep_fact2020 == input$nbrepas)
        return(a)
    })
    df_join_subset2019 <- reactive({
        a <- subset(dfjoin, dfjoin$tot_rep_fact2019 == input$nbrepas)
        return(a)
    })
    df_join_subset2018 <- reactive({
        a <- subset(dfjoin, dfjoin$tot_rep_fact2018 == input$nbrepas)
        return(a)
    })
    
    
    output$distPlot <- renderPlotly({
        ggplotly(
            ggplot(data=df_subset(), aes(x=bio_fact, y=cmp)) + 
                geom_bar(stat = "summary",fill="#582c83")+
                theme_classic(base_size = 20)+
                geom_hline(yintercept = 3, linetype = "dashed")+
                xlab("% de bio")+
                ylab("Coûts denrées moyen par repas")
        )
        
    })
    
    
    output$distPlotProdLoc <- renderPlotly({
        ggplotly(
            ggplot(data=df_subset(), aes(x=bio_fact, y=loc)) + 
                geom_bar(stat = "summary",fill="#99e3ff")+
                theme_classic(base_size = 20)+
                geom_hline(yintercept = 100, linetype = "dashed")+
                xlab("% de bio")+
                ylab("% de produits locaux")
        )
        
    })
    
    output$Pie1 <- renderPlot({
        ggplot(dfnovege, aes(y=points, x=team)) + 
            geom_bar(position='stack', stat='identity')
        
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
    
    
    
    output$linePriceBio <- renderPlot({
        
        js2018 <- df_join_subset2018()
        js2019 <- df_join_subset2019()
        js2020 <- df_join_subset2020()
        annee <- c("2018", "2019", "2020")
        biorate <- c(mean(js2018$bio2018),mean(js2019$bio2019),mean(js2020$bio2020))
        price <- c(mean(js2018$cmp2018),mean(js2019$cmp2019),mean(js2020$cmp2020))
        locrate <- c(mean(js2018$loc2018),mean(js2019$loc2019),mean(js2020$loc2020))
        dfjoinannee <- data.frame(annee,biorate,price)
        dfjoinannee$category <- as.factor(dfjoinannee$annee)
        #print(dfjoinannee)
        
        ggplot(dfjoinannee, aes(x=annee)) +
            
            geom_line( aes(y=biorate,group=1), size=3, color=temperatureColor) + 
            geom_line( aes(y=price*10 ,group=1), size=3, color=priceColor) +
            geom_point(y=biorate, size=3)+
            geom_point(y=price*10, size=3)+
            geom_hline(yintercept = 40, linetype = "dashed")+
            
            scale_y_continuous(
                
                # Features of the first axis
                name = "Pourcentage de bio",
                
                # Add a second axis and specify its features
                sec.axis = sec_axis(~./10, name="Prix du repas")
            ) + 
            
            theme_ipsum() +
            
            theme(
                axis.title.y = element_text(color = temperatureColor, size=13),
                axis.title.y.right = element_text(color = priceColor, size=13)
            ) +
            
            ggtitle("Evolution du prix et de la proportion de bio entre 2018 et 2020")
        
    })
}

