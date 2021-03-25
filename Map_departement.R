library(leaflet)
library(rgdal)
library("openxlsx")
library(dplyr)
library(rlist)
library(tidyr)
library(naniar)
library(rgeoapi)
library(wesanderson)

#Préparations données structures
# Chargement des données

liste_participants <- read.xlsx("R_Data/LISTE PARTICIPANTS OBSERVATOIRE.xlsx", rowNames = TRUE)


# Récupérer longitude et lattitude des communes

get_commune_data <- function(row) {
  results <- tryCatch({
    ComByName(row$COMMUNE)
  }, warning = function(w){
    tryCatch({
      ComByPostal(row$CODE.POSTAL)
    }, warning = function(w){
      ComByCode(row$CODE.POSTAL)
    })
  })
  
  return(results[1,])
}

for (i in seq_len(nrow(liste_participants))) {
  liste_participants[i, "lat"] <- get_commune_data(liste_participants[i,])$lat
  liste_participants[i, "long"] <- get_commune_data(liste_participants[i,])$long
}

liste_participants$lat <- as.numeric(liste_participants$lat)
liste_participants$long <- as.numeric(liste_participants$long)


getSize <- function(repas){
  if(repas== "Moins de 500 repas/jour"){
    return(2)
  }
  if(repas== "Entre 500 et 3000 repas/jour"){
    return(4)
  }
  if(repas== "Entre 3000 et 10 000 repas/jour"){
    return(6)
  }
  if(repas== "Plus de 10 000 repas/jour") {
    return(8)
  }
  
}

liste_participants$taille_num <- lapply(liste_participants$`TAILLE.COLLECTIVITÉS.(en.nombre.de.repas/jour)`, getSize)


# Préparation donées departements
department <- readOGR(dsn="./Map",layer = "departements-20170102")

bdd20_departement <- read.xlsx("R_Data/bdd_observatoire_2020_departements.xlsx")%>%
  select(c("id","freq_vege","menuvege","bio","loc","code.département","cmp")) %>%
  group_by(code.département)#%>% 
  #dplyr::summarise(N = n())

for (i in seq_len(nrow(bdd20_departement))) {
  bdd20_departement[i,"hedbo"] <- 0
  
  bdd20_departement$freq_vege[is.na(bdd20_departement$freq_vege)] <- 0
  
  if (bdd20_departement[i, "freq_vege"] == 1 || bdd20_departement[i, "freq_vege"] == 2) {
    bdd20_departement[i,"hedbo"] <- 1
  }
}

bdd20_summary = bdd20_departement%>% dplyr::summarise(N = n(),mean_bio = mean(bio, na.rm = TRUE), mean_local =mean(loc, na.rm = TRUE), mean_price = mean(cmp, na.rm= TRUE), mean_vege = mean(hedbo, na.rm = TRUE))

# On verifie que tt les codes de bdd sont dans le Shapefile

is.element(bdd20_summary$code.département, department$code_insee)
#Les departements inféreieurs à 10 dans bdd sont noté 1,2,3 ... alors qu'on a 01,02 dans le shapefile

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
bdd20_summary <- bdd20_summary[!(bdd20_summary$code.département =="Belgique"),]

#Add missing department

`%notin%` <- Negate(`%in%`)
missing_dep <- list()


for(elt in department$code_insee){
  if(elt %notin% bdd20_summary$code_insee){
    #missing_dep <- append(missing_dep,elt)
    bdd20_summary[nrow(bdd20_summary) + 1, 'code.département'] <- elt
  }
  
}

bdd20_summary$code_insee <- lapply(bdd20_summary$code.département, getInsee)



bdd20_summary2 <- rbind(bdd20_summary,missing_dep)
#Ajout d'une colonne qui indique sio on a assez de structures pour représenter la statistique sur la map

IsMoreThanOne <-function(n){
  if (n == 1 | is.na(n)){
    return(0)
  }
  else{ 
    return(1)
    }
}

bdd20_summary$MoreThanOne <- lapply(bdd20_summary$N, IsMoreThanOne)

bdd20_summary$mean_bio_anonyme = bdd20_summary$mean_bio * as.numeric(bdd20_summary$MoreThanOne)
bdd20_summary%>%replace_with_na(replace = list(mean_bio_anonyme=0))

bdd20_summary$mean_local_anonyme = bdd20_summary$mean_local * as.numeric(bdd20_summary$MoreThanOne)
bdd20_summary%>%replace_with_na(replace = list(mean_local_anonyme=0))

bdd20_summary$mean_price_anonyme = bdd20_summary$mean_price * as.numeric(bdd20_summary$MoreThanOne)
bdd20_summary%>%replace_with_na(replace = list(mean_price_anonyme=0))

bdd20_summary$mean_vege_anonyme = bdd20_summary$mean_vege * as.numeric(bdd20_summary$MoreThanOne)
bdd20_summary%>%replace_with_na(replace = list(mean_vege_anonyme=0))


# On s assure que les departements soient dans le même ordre que le Shapefile
bdd20_summary <- bdd20_summary[order(match(bdd20_summary$code_insee, department$code_insee)),]


# Carte

pal_gestion <- colorFactor(as.character(wes_palette("Darjeeling1")[c(1,2,3,4,5)]), 
                           liste_participants$`MODE.GESTION.RESTAURATION.COLLECTIVE.(Gestion.directe,.concédée,.mixte.ou.les.deux)`)

pal_ano <- colorNumeric(
  palette = "Greens",
  domain = bdd20_summary$mean_bio_anonyme)


test_ano <- leaflet()%>%
  setView(lng = 3.1074, lat = 45.7825, zoom = 6)%>%
  addProviderTiles(providers$Stamen.TonerLite)%>%
  addPolygons(data = department,
              weight = 1,
              smoothFactor = 0.5,
              color = "white",
              fillOpacity = 0.8,
              fillColor = pal_ano(bdd20_summary$mean_bio_anonyme),
              label = bdd20_summary$N)%>%
  addCircleMarkers(data = liste_participants, lat = ~lat, lng = ~long, opacity = 0.7, radius = ~taille_num,
                   color = ~pal_gestion(liste_participants$`MODE.GESTION.RESTAURATION.COLLECTIVE.(Gestion.directe,.concédée,.mixte.ou.les.deux)`)
                   )%>%
  addLegend("bottomright", pal = pal_gestion, values = liste_participants$`MODE.GESTION.RESTAURATION.COLLECTIVE.(Gestion.directe,.concédée,.mixte.ou.les.deux)`,
            title = "Mode de Gestion",
            opacity = 1)
test_ano  

