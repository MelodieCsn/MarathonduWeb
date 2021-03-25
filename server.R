# Define server logic required to draw a histogram ----
server <- function(input, output) {
    df_subset <- reactive({
        if(input$nbrepas=="Total"){a <- bdd20_dedup}
        else{a <- subset(bdd20_dedup, bdd20_dedup$tot_rep_fact == input$nbrepas)}
        return(a)
    })
    df_join_subset2020 <- reactive({
        if(input$nbrepas=="Total"){a <- dfjoin}
        else{a <- subset(dfjoin, dfjoin$tot_rep_fact2020 == input$nbrepas)}
        return(a)
    })
    df_join_subset2019 <- reactive({
        if(input$nbrepas=="Total"){a <- dfjoin}
        else{a <- subset(dfjoin, dfjoin$tot_rep_fact2019 == input$nbrepas)}
        return(a)
    })
    df_join_subset2018 <- reactive({
        if(input$nbrepas=="Total"){a <- dfjoin}
        else{a <- subset(dfjoin, dfjoin$tot_rep_fact2018 == input$nbrepas)}
        return(a)
    })

    df_stack_subset <- reactive({
        if(input$nbrepas=="Total"){a <- dfjoin}
        else{a <- subset(dfjoin, dfjoin$tot_rep_fact2020 == input$nbrepas)}
        return(a)
    })

    output$plotbioprix <- renderPlotly({
        fun_mean <- function(x){return(round(data.frame(y=mean(x),label=mean(x,na.rm=T))
          ,digit=1))}
        p1 <- ggplotly(
          ggplot(df_subset(), aes(x=bio_fact, y=cmp)) +
            geom_segment( aes(x=bio_fact, xend=bio_fact, y=0, yend=cmp)) +
            geom_point( size=5, color="#DC4405", fill=alpha("orange", 0.5), alpha=0.8, shape=21, stroke=1) +
            theme_bw()+
            xlab("Pourcentage de produits bio")+
            ylab("Prix moyen d'un repas")
        )
        if(is.null(input$loliouhisto)){
            p1
        }
        else if(input$loliouhisto== "histo") {
            ggplotly(
              ggplot(data=df_subset(), aes(x=bio_fact, y=cmp)) +
                geom_bar(stat = "summary",fill="#DC4405")+
                theme_classic(base_size = 20)+
                geom_hline(yintercept = 5, linetype = "dashed")+
                xlab("% de bio")+
                ylab("Prix moyen d'un repas")+
                #geom_text(label =y,size = 3, position = position_stack(vjust = 0.5))+
                #stat_summary(aes(label=..y..), fun.data=fun_mean, geom="text",
                #size=4,vjust = -10, color ="white")+
                stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5)
            )
        }
        else if(input$loliouhisto== "loli"){

            p1
        }

    })

    output$plotbioloc <- renderPlotly({
        fun_mean <- function(x){return(round(data.frame(y=mean(x),label=mean(x,na.rm=T))
          ,digit=1))}
        p2 <- ggplotly(
          ggplot(df_subset(), aes(x=bio_fact, y=loc)) +
            geom_segment( aes(x=bio_fact, xend=bio_fact, y=0, yend=loc)) +
            geom_point( size=5, color="#582c83", fill=alpha("purple", 0.5), alpha=0.7, shape=21, stroke=1) +
            theme_bw()+
            xlab("Pourcentage de produits bio")+
            ylab("Pourcentage de produits locaux")
        )
        if(is.null(input$loliouhisto)){
            p2
        }
        else if(input$loliouhisto== "histo") {
            ggplotly(
              ggplot(data=df_subset(), aes(x=bio_fact, y=loc)) +
                geom_bar(stat = "summary",fill="#582c83")+
                theme_classic(base_size = 20)+
                geom_hline(yintercept = 100, linetype = "dashed")+
                xlab("% de bio")+
                ylab("% de produits locaux")+
                #stat_summary(aes(label=..y..), fun.data=fun_mean, geom="text",
                #size=4,vjust = 5, color ="white")+
                stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5)
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

        vegequot <- df_subset()[df_subset()$freq_veg == 3,]
        vegequot <- vegequot[complete.cases(vegequot$freq_vege), ]
        vegequot <- vegequot[complete.cases(vegequot$via_bio), ]
        vegequot$typeviande <- NA
        vegequot$typeviande[vegequot$via_bio == 1] <- "viande bio"
        vegequot$typeviande[vegequot$via_bio == 0] <- "viande non bio"

        vegequot$typeviande <- as.factor(vegequot$typeviande)
        dfvegequot <- count(vegequot, 'typeviande')
        dfvegequot$freqvege <- "Quotidien"

        novege <- df_subset()[df_subset()$menuvege == 2,]
        novege <- novege[complete.cases(novege$menuvege), ]
        novege <- novege[complete.cases(novege$via_bio), ]
        novege$typeviande <- NA
        novege$typeviande[novege$via_bio == 1] <- "viande bio"
        novege$typeviande[novege$via_bio == 0] <- "viande non bio"

        novege$typeviande <- as.factor(novege$typeviande)
        dfnovege <- count(novege, 'typeviande')
        dfnovege$freqvege <- "Non végétarien"

        df <- dplyr::bind_rows(dfnovege, dfvegehebdo)
        dfstackedbar <- dplyr::bind_rows(df, dfvegequot)
        ggplotly(
          ggplot(dfstackedbar, aes(fill = typeviande,y=freq, x=freqvege)) +
            geom_bar(position='stack', stat='identity')+
            scale_fill_manual('Qualité de la Viande', values=c("#DC4405",'#582c83'))+
            xlab("Type de menu")+
            ylab("Effectif")+
            theme_bw()
        )


    })


    output$linePriceBio <- renderPlot({

        js2018 <- df_join_subset2018()
        js2019 <- df_join_subset2019()
        js2020 <- df_join_subset2020()
        annee <- c("2018", "2019", "2020")
        biorate <- c(mean(js2018$bio2018),mean(js2019$bio2019),mean(js2020$bio2020))
        price <- c(mean(js2018$cmp2018),mean(js2019$cmp2019),mean(js2020$cmp2020))
        dfjoinannee <- data.frame(annee,biorate,price)
        dfjoinannee$category <- as.factor(dfjoinannee$annee)
        print(dfjoinannee)

        ggplot(dfjoinannee, aes(x=annee)) +

          geom_line( aes(y=biorate,group=1), size=2, color="#582c83") +
          geom_line( aes(y=price*30 ,group=1), size=2, color="#DC4405") +
          geom_point(y=biorate, size=3)+
          geom_point(y=price*30, size=3)+
          #geom_hline(yintercept = 33, linetype = "dashed")+
          xlab("Année") +
          scale_y_continuous(
            # Features of the first axis
            name = "Pourcentage de bio",
            # Add a second axis and specify its features
            sec.axis = sec_axis(~./30, name="Prix du repas")) +
          theme_ipsum() +
          theme(
            axis.title.y = element_text(angle = 0, color = "#582c83", size=20),
            axis.title.y.right = element_text(angle = 0,color = "#DC4405", size=20),
            axis.text.x = element_text(size=18,color="black"),
            axis.title.x = element_text(size=18,color="black"))

        # angle possible


    })

    # PARTIE MOULIKA --------------------------------------------------------------------------------------------------------------------------

    #prix régime alimentaire
    output$PrixRegAlim <- renderPlot({
        bdd_2020_menu <- read.xlsx("bdd_observatoire_2020.xlsx")%>%
          select(c("freq_vege","cmp"))

        bdd_2020_menu$freq_vege[is.na(bdd_2020_menu$freq_vege)] <- 0

        # on eneleve les NA de la colonne cmp
        bdd_2020_menu = bdd_2020_menu[complete.cases(bdd_2020_menu$cmp), ]
        trash <- with(bdd_2020_menu, which(freq_vege==3, arr.ind=TRUE))
        bdd_2020_menu<- bdd_2020_menu[-trash, ]


        # string format
        bdd_2020_menu = bdd_2020_menu %>%
          mutate(freq_vege= replace(freq_vege, freq_vege == 2, "Végétarien hébdomadaire"))

        bdd_2020_menu = bdd_2020_menu %>%
          mutate(freq_vege= replace(freq_vege, freq_vege == 1, "Végétarien Quotidiens"))

        bdd_2020_menu = bdd_2020_menu %>%
          mutate(freq_vege= replace(freq_vege, freq_vege == 0, "Non Végétarien"))



        #plot
        ggplot(bdd_2020_menu , aes(x = cmp, y = freq_vege, fill = freq_vege)) +
          geom_density_ridges() +
          theme_ridges() +
          theme(legend.position = "none", axis.title.x = element_text(size=18,color="black"),axis.title.y = element_text(size=18,color="black"))+
          xlab("Prix") +
          ylab("Fréquence des menus végétariens")+
          scale_fill_cyclical(name = "Fill colors",
                              values = c("#582c83", "#007644", "#DC4405"))



    })

    output$PrixBioLoc <- renderPlot({

        bdd_2020_loc<- read.xlsx("bdd_observatoire_2020.xlsx")%>%
          select(c("loc","cmp","bio"))

        # on eneleve les NA de la colonne cmp
        bdd_2020_loc = bdd_2020_loc[complete.cases(bdd_2020_loc$cmp), ]
        bdd_2020_loc = bdd_2020_loc[complete.cases(bdd_2020_loc$loc), ]
        bdd_2020_loc = bdd_2020_loc[complete.cases(bdd_2020_loc$bio), ]

        #plot
        ggplot( bdd_2020_loc, aes(x = loc  ,y=bio, size=cmp, fill=bio)) +
          geom_point(alpha=0.5, shape=21, color="black") +
          scale_size(range = c(5,15), name="Coût d'un repas") +
          scale_fill_viridis(discrete=FALSE, guide=FALSE, option="A") +
          theme(plot.title = element_text(hjust = 0.7, size = 8))+
          theme_ipsum()+
          theme(legend.position="bottom") +
          xlab("Proportion de poduits locaux (en %)") +
          ylab("Proportion de repas bio (en %)")+
          theme(axis.title.x = element_text(size=18,color="black"),axis.title.y = element_text(size=18,color="black"))

    })

    output$venn <- renderPlot({
        grid.newpage()
        draw.triple.venn(area1 = venn_cmp,
                         area2 = venn_vege_hebdo,
                         area3 = venn_bio,
                         n12 = venn_vege_cmp,
                         n23 = venn_vege_bio,
                         n13 = venn_bio_cmp,
                         n123 = les_trois,
                         fill = c("#582c83", "#007644", "#DC4405"),
                         category = c("Prix < 2.5€", "Au moins 1 repas végé hebdo", "bio +20%"))

    })

    # CARTE MANUE ----------------------------------------------------------------------------------------------------------------------------
    selectedData <- reactive({
        if(input$projection=="bio") return(bdd20_bio)
        if(input$projection=="loc") return(bdd20_loc)
        if(input$projection=="prix")return(bdd20_price)
        if(input$projection=="vege") return(bdd20_vege)

    })

    # Definition legende
    legende <- c("Déléguée", "Directe", "Les deux", "Mixte", "NC")

    # Definition palette
    pal_gestion <- colorFactor(as.character(wes_palette("Darjeeling1")[c(1,2,3,4,5)]),
                               participants_map$`MODE.GESTION.RESTAURATION.COLLECTIVE.(Gestion.directe,.concédée,.mixte.ou.les.deux)`)

    pal <- reactive({
        if(input$projection=="bio") return (colorNumeric(palette = brewer.pal(n = 9, "Greens")[2:9],domain = bdd20_bio$mean))
        if(input$projection=="loc") return (colorNumeric(palette = brewer.pal(n = 9, "Blues")[2:9],domain = bdd20_loc$mean))
        if(input$projection=="prix") return (colorNumeric(palette = brewer.pal(n = 9, "Reds")[2:9],domain = bdd20_price$mean))
        if(input$projection=="vege") return (colorNumeric(palette = brewer.pal(n = 9, "Purples")[2:9],domain = bdd20_vege$mean))
    })

    # Definition de la carte
    output$mymap <- renderLeaflet({
        pal <- pal()
        leaflet()%>%
          setView(lng = 3.1074, lat = 45.7825, zoom = 5)%>%
          addProviderTiles(providers$Stamen.TonerLite)%>%
          addPolygons(data = department,
                      weight = 1,
                      smoothFactor = 0.5,
                      color = "white",
                      fillOpacity = 0.8,
                      fillColor = pal(selectedData()$mean),
                      highlight = highlightOptions(weight = 5, color = "black"),
                      popup = bdd20_summary$label)%>%
          addCircleMarkers(data = participants_map, lat = ~lat, lng = ~long, opacity = 0.7, radius = ~taille_num,
                           color = ~pal_gestion(participants_map$`MODE.GESTION.RESTAURATION.COLLECTIVE.(Gestion.directe,.concédée,.mixte.ou.les.deux)`),
                           label = participants_map$`TYPE.ETABLISSEMENT.(Cuisine.centrale.d'une.collectivité,.écoles,.collèges,.lycées,.Ehpad,.etc.)`
          )%>%
          addLegend("bottomright", pal = pal_gestion, values = participants_map$`MODE.GESTION.RESTAURATION.COLLECTIVE.(Gestion.directe,.concédée,.mixte.ou.les.deux)`,
                    title = "Mode de Gestion",
                    opacity = 1,
                    labFormat = function(type, cuts, p) {paste0(legende)
                    })%>%
          addLegend("topright", pal = pal, values = selectedData()$mean,
                    title = "Projection départements")
    })


    outputOptions(output, "plotbioprix", priority = 1)
    outputOptions(output, "plotbioloc", priority = 1)
    outputOptions(output, "Stacked", priority = 1)
    outputOptions(output, "linePriceBio", priority = 1)
    outputOptions(output, "PrixRegAlim", priority = 1)
    outputOptions(output, "PrixBioLoc", priority = 1)
    outputOptions(output, "venn", priority = 1)
    outputOptions(output, "mymap", suspendWhenHidden = FALSE, priority = 10)
}


