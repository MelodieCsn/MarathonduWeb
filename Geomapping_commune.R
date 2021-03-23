library(rgeoapi)
library(openxlsx)


liste_participants <- read.xlsx("R_Data/LISTE PARTICIPANTS OBSERVATOIRE.xlsx", rowNames = TRUE)

liste_communes <- subset(liste_participants, select = c("CODE.POSTAL", "COMMUNE"))

get_commune_data <- function(row) {
  results <- tryCatch({
    ComByName(row$COMMUNE)
  }, warning = function(){
    tryCatch({
      ComByPostal(row$CODE.POSTAL)
    }, warning = function(){
      ComByCode(row$CODE.POSTAL)
    })
  })

  return(results[1,])
}

liste_communes
for (i in seq_len(nrow(liste_communes))) {
  liste_communes[i, "lat"] <- get_commune_data(liste_communes[i,])$lat
  liste_communes[i, "long"] <- get_commune_data(liste_communes[i,])$long
}
