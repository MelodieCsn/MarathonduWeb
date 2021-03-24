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
    
    df_stack_subset <- reactive({
        a <- subset(dfjoin, dfjoin$tot_rep_fact2020 == input$nbrepas)
        return(a)
    })
    
    output$plotbioprix <- renderPlotly({
        
        p1 <- ggplotly(
                ggplot(df_subset(), aes(x=bio_fact, y=cmp)) +
                geom_segment( aes(x=bio_fact, xend=bio_fact, y=0, yend=cmp)) +
                geom_point( size=5, color="red", fill=alpha("red", 0.5), alpha=0.7, shape=21, stroke=1) +
                theme_bw()
        )
        if(is.null(input$loliouhisto)){ 
            p1 
        }
        else if(input$loliouhisto== "histo") {
            ggplotly(
                ggplot(data=df_subset(), aes(x=bio_fact, y=cmp)) + 
                    geom_bar(stat = "summary",fill="#582c83")+
                    theme_classic(base_size = 20)+
                    geom_hline(yintercept = 5, linetype = "dashed")+
                    xlab("Pourcentage de bio")+
                    ylab("Coût moyen par repas")
            )
        }    
        else if(input$loliouhisto== "loli"){
        
            p1
        }
        
    })
    
    output$plotbioloc <- renderPlotly({
        
        p2 <- ggplotly(
            ggplot(df_subset(), aes(x=bio_fact, y=loc)) +
                geom_segment( aes(x=bio_fact, xend=bio_fact, y=0, yend=loc)) +
                geom_point( size=5, color="red", fill=alpha("red", 0.5), alpha=0.7, shape=21, stroke=1) +
                theme_bw()
        )
        if(is.null(input$loliouhisto)){ 
            p2 
        }
        else if(input$loliouhisto== "histo") {
            ggplotly(
                ggplot(data=df_subset(), aes(x=bio_fact, y=loc)) + 
                    geom_bar(stat = "summary",fill="#582c83")+
                    theme_classic(base_size = 20)+
                    geom_hline(yintercept = 70, linetype = "dashed")+
                    xlab("Pourcentage de bio")+
                    ylab("Pourcentage de local")
            )
        }    
        else if(input$loliouhisto== "loli"){
            
            p2
        }
        
    })
    
    
    output$Stacked <- renderPlotly({
        
        vegehebdo = df_subset()[df_subset()$freq_veg == 2,]
        vegehebdo$typeviande = NA
        vegehebdo = vegehebdo[complete.cases(vegehebdo$freq_vege), ]
        vegehebdo = vegehebdo[complete.cases(vegehebdo$via_bio), ]
        vegehebdo$typeviande[vegehebdo$via_bio == 1] <- "viande bio"
        vegehebdo$typeviande[vegehebdo$via_bio == 0] <- "viande non bio"
        
        vegehebdo$typeviande = as.factor(vegehebdo$typeviande)
        dfvegehebdo = count(vegehebdo, 'typeviande')
        dfvegehebdo$freqvege = "Hebdomadaire"
        
        vegequot = df_subset()[df_subset()$freq_veg == 3,]
        vegequot = vegequot[complete.cases(vegequot$freq_vege), ]
        vegequot = vegequot[complete.cases(vegequot$via_bio), ]
        vegequot$typeviande = NA
        vegequot$typeviande[vegequot$via_bio == 1] <- "viande bio"
        vegequot$typeviande[vegequot$via_bio == 0] <- "viande non bio"
        
        vegequot$typeviande = as.factor(vegequot$typeviande)
        dfvegequot = count(vegequot, 'typeviande')
        dfvegequot$freqvege = "Quotidien"
        
        novege = df_subset()[df_subset()$menuvege == 2,]
        novege = novege[complete.cases(novege$menuvege), ]
        novege = novege[complete.cases(novege$via_bio), ]
        novege$typeviande = NA
        novege$typeviande[novege$via_bio == 1] <- "viande bio"
        novege$typeviande[novege$via_bio == 0] <- "viande non bio"
        
        novege$typeviande = as.factor(novege$typeviande)
        dfnovege = count(novege, 'typeviande')
        dfnovege$freqvege = "Non végétarien"
        
        df = dplyr::bind_rows(dfnovege, dfvegehebdo)
        dfstackedbar = dplyr::bind_rows(df, dfvegequot)
        
        ggplot(dfstackedbar, aes(fill = typeviande,y=freq, x=freqvege)) + 
            geom_bar(position='stack', stat='identity')+
            scale_fill_manual('Position', values=c('coral2', 'coral4'))
        
    })
    
    
    
    output$linePriceBio <- renderPlotly({
        
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
            
            theme(
                axis.title.y = element_text(color = temperatureColor, size=13),
                axis.title.y.right = element_text(color = priceColor, size=13)
            ) +
            
            ggtitle("Evolution du prix et de la proportion de bio entre 2018 et 2020")
        
    })
}

