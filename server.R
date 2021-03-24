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
    fun_mean <- function(x){return(round(data.frame(y=mean(x),label=mean(x,na.rm=T))
                                         ,digit=1))}
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
        geom_point( size=5, color="red", fill=alpha("red", 0.5), alpha=0.7, shape=21, stroke=1) +
        theme_bw()
    )
    if(is.null(input$loliouhisto)){ 
      p2 
    }
    else if(input$loliouhisto== "histo") {
      ggplotly(
        ggplot(data=df_subset(), aes(x=bio_fact, y=loc)) + 
          geom_bar(stat = "summary",fill="#DC4405")+
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
      labs(title = 'Prix des repas en fonction du régime alimentaire')+
      
      theme_ridges() + 
      theme(legend.position = "none")+
      xlab("Prix") +
      ylab("Régime alimentaire")
    
    
  })
  
  output$PrixBioLoc <- renderPlot({
    
    bdd_2020_loc<- read.xlsx("bdd_observatoire_2020.xlsx")%>%
      select(c("loc","cmp","bio"))
    
    # on eneleve les NA de la colonne cmp
    bdd_2020_loc = bdd_2020_loc[complete.cases(bdd_2020_loc$cmp), ]
    bdd_2020_loc = bdd_2020_loc[complete.cases(bdd_2020_loc$loc), ]
    bdd_2020_loc = bdd_2020_loc[complete.cases(bdd_2020_loc$bio), ]
    #trash <- with(bdd_2020_menu, which(freq_vege==3, arr.ind=TRUE))
    #bdd_2020_menu<- bdd_2020_menu[-trash, ]
    
    
    #plot
    
    ggplot( bdd_2020_loc, aes(x = loc  ,y=bio, size=cmp, fill=bio)) +
    geom_point(alpha=0.5, shape=21, color="black") +
      ggtitle("Proportion de repas bio en fonction de la proportion de produits locaux
          et du coût d'un repas") +
      scale_size(range = c(.1, 7), name="Coût d'un repas") +
      scale_fill_viridis(discrete=FALSE, guide=FALSE, option="A") +
      theme(plot.title = element_text(hjust = 0.7, size = 8))+
    theme_ipsum()+
      theme(legend.position="bottom") +
      xlab("Proportion de poduits locaux (en %)") +
      ylab("Proportion de repas bio (en %)")
    
    
  })
  
  
}
