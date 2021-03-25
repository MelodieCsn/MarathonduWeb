library(shiny)
library(dplyr)
library(leaflet)
library(rgdal)
library(RColorBrewer)
library(wesanderson)
library(tidyr)
library(rlist)
library(openxlsx)

# Chargement des données
department <- readOGR(dsn="./Map",layer = "departements-20170102")
liste_participants <- read.xlsx("./Out/liste_participants_coordinates.xlsx")
bdd20_summary <- read.csv("./Out/bdd20_summary.csv")

getSize <- function(repas){
  a=0
  if(repas== "Moins de 500 repas/jour"){
    a = 2
  }
  if(repas== "Entre 500 et 3000 repas/jour"){
    a= 4
  }
  if(repas== "Entre 3000 et 10 000 repas/jour"){
    a= 6
  }
  if(repas== "Plus de 10 000 repas/jour") {
    a= 8
  }
  return(a)
  
}


liste_participants$taille_num <- lapply(liste_participants$`TAILLE.COLLECTIVITÉS.(en.nombre.de.repas/jour)`, getSize)

getInsee <- function(elt){
  if(nchar(elt) == 1){
    return(paste0("0",elt))
  }
  if(elt == "69"){
    return("69M")
  }
  else{
    return(elt)
  }
}

bdd20_summary$code_insee <- lapply(bdd20_summary$code.département, getInsee)

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
  
  # Definition legende
  legende <- c("Déléguée", "Directe", "Les deux", "Mixte", "NC")
  
   # Definition palette
  pal_gestion <- colorFactor(as.character(wes_palette("Darjeeling1")[c(1,2,3,4,5)]), 
                             liste_participants$`MODE.GESTION.RESTAURATION.COLLECTIVE.(Gestion.directe,.concédée,.mixte.ou.les.deux)`)
  
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
      addCircleMarkers(data = liste_participants, lat = ~lat, lng = ~long, opacity = 0.7, radius = ~taille_num,
                       color = ~pal_gestion(liste_participants$`MODE.GESTION.RESTAURATION.COLLECTIVE.(Gestion.directe,.concédée,.mixte.ou.les.deux)`),
                       label = liste_participants$`TYPE.ETABLISSEMENT.(Cuisine.centrale.d'une.collectivité,.écoles,.collèges,.lycées,.Ehpad,.etc.)`
      )%>%
      addLegend("bottomright", pal = pal_gestion, values = liste_participants$`MODE.GESTION.RESTAURATION.COLLECTIVE.(Gestion.directe,.concédée,.mixte.ou.les.deux)`,
                title = "Mode de Gestion",
                opacity = 1,
                labFormat = function(type, cuts, p) {paste0(legende)
                })%>%
      addLegend("topright", pal = pal, values = selectedData()$mean,
                title = "Projection départements")
  })
}

shinyApp(ui, server)

    
  