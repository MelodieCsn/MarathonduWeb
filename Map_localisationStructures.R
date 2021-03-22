library(leaflet)
library(dbplyr)
library(tidyverse)
library(wesanderson)
library("openxlsx")

# Chargement des données

liste_participants <- read.xlsx("R_Data/LISTE PARTICIPANTS OBSERVATOIRE.xlsx", rowNames = TRUE)

# Enlever les accents dans le nom des communes

Unaccent <- function(text) {
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII")
  text <- gsub("['`^~\"]", "", text)
  return(text)
}

liste_participants$COMMUNE = lapply(liste_participants$COMMUNE, Unaccent)


# Récupérer longitude et lattitude des communes

getLat <- function(code) {
  df = ComByName(code)
  a = df[1,'lat']
  return(a)
}

getLong <- function(code) {
  df = ComByName(code)
  a = df[1,'long']
  return(a)
}


liste_participants$lat = as.numeric(lapply(liste_participants$COMMUNE, getLat))

liste_participants$long = as.numeric(lapply(liste_participants$COMMUNE, getLong))




## Création de la map

pal = colorFactor(as.character(wes_palette("Darjeeling1")[c(1,2,3,4,5)]), 
                  liste_participants$`TAILLE.COLLECTIVITÉS.(en.nombre.de.repas/jour)`)

m <- leaflet()%>%addProviderTiles(providers$Stamen.TonerLite)%>%setView(lng = 3.1074, lat = 45.7825, zoom = 6)%>%
  addCircleMarkers(data = liste_participants, lat = ~lat, lng = ~long,opacity = 1, radius = ~3, color = ~pal(liste_participants$'TAILLE.COLLECTIVITÉS.(en.nombre.de.repas/jour)'))%>%
  addLegend("bottomright", pal = pal, values = liste_participants$`TAILLE.COLLECTIVITÉS.(en.nombre.de.repas/jour)`,
            title = "Taille des collectivités en repas/jours",
            opacity = 1
  )
m

