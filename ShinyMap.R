library(shiny)
library(dplyr)
library(RColorBrewer)

bdd20_summary <- bdd20_summary[order(match(bdd20_summary$code_insee, department$code_insee)),]


bdd20_bio <- bdd20_summary%>%
  select(c("code.département","mean_bio_anonyme"))
bdd20_bio$mean = bdd20_bio$mean_bio_anonyme

bdd20_loc <- bdd20_summary%>%
  select(c("code.département","mean_local_anonyme"))
bdd20_loc$mean = bdd20_loc$mean_local_anonyme

bdd20_price <- bdd20_summary%>%
  select(c("code.département","mean_price_anonyme"))
bdd20_price$mean = bdd20_price$mean_price_anonyme

bdd20_vege <- bdd20_summary%>%
  select(c("code.département","mean_vege_anonyme"))
bdd20_vege$mean = bdd20_vege$mean_vege_anonyme


# Define UI 
ui <- fluidPage(
  
  # App title ----
  titlePanel("Répartition géographique"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      radioButtons("projection", "Affichage : ", 
                   choices= c("Part du bio (en %)" = "bio" ,
                              "Part de produits Locaux (en%)" = "loc",
                              "Prix moyen par repas" = "prix" ,
                              "% Au moins un menu végétarien hebdomadaire" = "vege")),
  
  
),
leafletOutput(outputId = "mymap")
))


server <- function(input, output, session) {
  
  
  selectedData <- reactive({
    if(input$projection=="bio") return(bdd20_bio)
    if(input$projection=="loc") return(bdd20_loc)
    if(input$projection=="prix")return(bdd20_price)
    if(input$projection=="vege") return(bdd20_vege)
    
  })
  
  
   # Definition palette
  pal_gestion <- colorFactor(as.character(wes_palette("Darjeeling1")[c(1,2,3,4,5)]), 
                             liste_participants$`MODE.GESTION.RESTAURATION.COLLECTIVE.(Gestion.directe,.concédée,.mixte.ou.les.deux)`)
  
  pal <- reactive({
    if(input$projection=="bio") return (colorNumeric(palette = brewer.pal(n = 9, "Greens")[3:9],domain = bdd20_bio$mean))
    if(input$projection=="loc") return (colorNumeric(palette = brewer.pal(n = 9, "Blues")[3:9],domain = bdd20_loc$mean))
    if(input$projection=="prix") return (colorNumeric(palette = brewer.pal(n = 9, "Reds")[3:9],domain = bdd20_price$mean))
    if(input$projection=="vege") return (colorNumeric(palette = brewer.pal(n = 9, "Purples")[3:9],domain = bdd20_vege$mean))
  })
  
  # Definition de la carte
  output$mymap <- renderLeaflet({
    pal <- pal()
    leaflet()%>%
      setView(lng = 3.1074, lat = 45.7825, zoom = 6)%>%
      addProviderTiles(providers$Stamen.TonerLite)%>%
      addPolygons(data = department,
                  weight = 1,
                  smoothFactor = 0.5,
                  color = "white",
                  fillOpacity = 0.8,
                  fillColor = pal(selectedData()$mean),
                  label = bdd20_summary$N)%>%
      addCircleMarkers(data = liste_participants, lat = ~lat, lng = ~long, opacity = 0.7, radius = ~taille_num,
                       color = ~pal_gestion(liste_participants$`MODE.GESTION.RESTAURATION.COLLECTIVE.(Gestion.directe,.concédée,.mixte.ou.les.deux)`)
      )%>%
      addLegend("bottomright", pal = pal_gestion, values = liste_participants$`MODE.GESTION.RESTAURATION.COLLECTIVE.(Gestion.directe,.concédée,.mixte.ou.les.deux)`,
                title = "Mode de Gestion",
                opacity = 1)
  })
}

shinyApp(ui, server)

    
  