
library(rgeoapi)
library(openxlsx)


liste_participants <- read.xlsx("R_Data/LISTE PARTICIPANTS OBSERVATOIRE.xlsx", rowNames = TRUE)

Unaccent <- function(text) {
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII")
  text <- gsub("['`^~\"]", "", text)
  return(text)
}

liste_participants$COMMUNE = lapply(liste_participants$COMMUNE, Unaccent)


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


