library(leaflet)
library(rgdal)
library("openxlsx")
library(dplyr)
library(rlist)
library(tidyr)
library(naniar)

department <- readOGR(dsn="./Map",layer = "departements-20170102")

bdd20_departement <- read.xlsx("R_Data/bdd_observatoire_2020_departements.xlsx")%>%
  select(c("id","freq_vege","menuvege","bio","loc","code.département")) %>%
  group_by(code.département)#%>% 
  #dplyr::summarise(N = n())

bdd20_summary = bdd20_departement%>% dplyr::summarise(N = n(),mean_bio = mean(bio, na.rm = TRUE))

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
# On s assure que les departements soient dans le même ordre que le Shapefile
bdd20_summary <- bdd20_summary[order(match(bdd20_summary$code_insee, department$code_insee)),]


# Carte

pal <- colorNumeric(
  palette = "Blues",
  domain = bdd20_summary$mean_bio)

pal_ano <- colorNumeric(
  palette = "Greens",
  domain = bdd20_summary$mean_bio_anonyme)


mm<- leaflet(data=department)%>%addTiles()%>%
  addPolygons(fill = "green", label = ~nom)
mm

test <- leaflet()%>%
  addProviderTiles(providers$Stamen.TonerLite)%>%
  addPolygons(data = department,
              weight = 1,
              smoothFactor = 0.5,
              color = "white",
              fillOpacity = 0.8,
              fillColor = pal(bdd20_summary$mean_bio))
test  


test_ano <- leaflet()%>%
  setView(lng = 3.1074, lat = 45.7825, zoom = 6)%>%
  addProviderTiles(providers$Stamen.TonerLite)%>%
  addPolygons(data = department,
              weight = 1,
              smoothFactor = 0.5,
              color = "white",
              fillOpacity = 0.8,
              fillColor = pal_ano(bdd20_summary$mean_bio_anonyme),
              label = bdd20_summary$N)
test_ano  

