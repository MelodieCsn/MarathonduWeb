library("openxlsx")
library("shiny")
library("dplyr")
library("ggplot2")
library("tidyr")
library("stats")
library("plyr")
library("plotrix")
library("shinydashboard")
library("plotly")
library("hrbrthemes")
library("viridis")
library("RColorBrewer")
library("ggridges")
library("VennDiagram")
library("shiny")
library("dplyr")
library("leaflet")
library("rgdal")
library("RColorBrewer")
library("wesanderson")
library("tidyr")
library("rlist")
library("openxlsx")

# Chargement des données


department <- readOGR(dsn="./Map",layer = "departements-20170102")

participants_map <- read.xlsx("./Out/liste_participants_coordinates.xlsx")
bdd20_summary <- read.xlsx("./Out/bdd20_summary.xlsx")

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


participants_map$taille_num <- lapply(participants_map$`TAILLE.COLLECTIVITÉS.(en.nombre.de.repas/jour)`, getSize)

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


myPalette <- brewer.pal(2,"Dark2") 

liste_participants <- read.xlsx("LISTE PARTICIPANTS OBSERVATOIRE.xlsx", rowNames = TRUE)
bdd_2020 <- read.xlsx("bdd_observatoire_2020.xlsx")
bdd_2019 <- read.xlsx("bdd_observatoire_2019.xlsx")
bdd_2018 <- read.xlsx("bdd_observatoire_2018.xlsx")


bdd_2020$bio_fact = cut(bdd_2020$bio, breaks=c(0,10, 20,30, 40,50, 60,70, 80 ,90,Inf), labels = c("0-10","10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90","90-100"))
bdd_2020$tot_rep_fact = cut(bdd_2020$tot_rep, breaks = c(0,500,3000,10000,Inf), labels= c("- 500", "500-3000", "3000-10000", "+ 10000"))


bdd_2019$bio_fact = cut(bdd_2019$bio, breaks=c(0,10, 20,30, 40,50, 60,70, 80 ,90,Inf), labels = c("0-10","10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90","90-100"))
bdd_2019$tot_rep_fact = cut(bdd_2019$tot_rep, breaks = c(0,500,3000,10000,Inf), labels= c("- 500", "500-3000", "3000-10000", "+ 10000"))

bdd_2018$bio_fact = cut(bdd_2018$bio, breaks=c(0,10, 20,30, 40,50, 60,70, 80 ,90,Inf), labels = c("0-10","10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90","90-100"))
bdd_2018$tot_rep_fact = cut(bdd_2018$tot_rep, breaks = c(0,500,3000,10000,Inf), labels= c("- 500", "500-3000", "3000-10000", "+ 10000"))

bdd20_dedup =bdd_2020[, !duplicated(colnames(bdd_2020))]
bdd20_dedup = bdd20_dedup[complete.cases(bdd20_dedup$bio_fact), ]

# On enlève les NA des colonnes de bio et de prix 
bdd_2020_bioclean = bdd_2020[complete.cases(bdd_2020$bio), ]
bdd_2020_bioclean = bdd_2020_bioclean[complete.cases(bdd_2020_bioclean$cmp), ]
bdd_2020_bioclean = bdd_2020_bioclean %>% select(bio, loc, cmp,id,tot_rep_fact)
bdd_2020_bioclean = bdd_2020_bioclean %>% dplyr::rename(bio2020 = bio, loc2020 = loc, cmp2020=cmp,tot_rep_fact2020 = tot_rep_fact)

bdd_2019_bioclean = bdd_2019[complete.cases(bdd_2019$bio), ]
bdd_2019_bioclean = bdd_2019_bioclean[complete.cases(bdd_2019_bioclean$cmp), ]
bdd_2019_bioclean = bdd_2019_bioclean %>% select(bio, loc, cmp,id,tot_rep_fact)
bdd_2019_bioclean = bdd_2019_bioclean %>% dplyr::rename(bio2019 = bio, loc20219 = loc, cmp2019=cmp, tot_rep_fact2019=tot_rep_fact)

dfjoin = inner_join(bdd_2019_bioclean, bdd_2020_bioclean, by = "id")

bdd_2018_bioclean = bdd_2018[complete.cases(bdd_2018$bio), ]
bdd_2018_bioclean = bdd_2018_bioclean[complete.cases(bdd_2018_bioclean$cmp), ]
bdd_2018_bioclean = bdd_2018_bioclean %>% select(bio, loc, cmp,id,tot_rep_fact)
bdd_2018_bioclean = bdd_2018_bioclean %>% dplyr::rename(bio2018 = bio, loc20218 = loc, cmp2018=cmp, tot_rep_fact2018=tot_rep_fact)


dfjoin = inner_join(dfjoin, bdd_2018_bioclean, by = "id")


# Partie ou on garde tous les participants 
annee <- c("2018", "2019", "2020")
biorate <- c(mean(bdd_2018_bioclean$bio),mean(bdd_2019_bioclean$bio),mean(bdd_2020_bioclean$bio))
price <- c(mean(bdd_2018_bioclean$cmp),mean(bdd_2019_bioclean$cmp),mean(bdd_2020_bioclean$cmp))
locrate <- c(mean(bdd_2018_bioclean$loc),mean(bdd_2019_bioclean$loc),mean(bdd_2020_bioclean$loc))
dfbioannee = data.frame(annee,biorate,price)
dfbioannee$category = as.factor(dfbioannee$annee)


#pie3D(x=dfnovege$freq, labels=dfnovege$category, col=myPalette, theta=3.14/2)
#pie3D(x=dfvegehebdo$freq, labels=dfvegehebdo$category, col=myPalette, theta=3.14/2)
#pie3D(x=dfvegequot$freq, labels=dfvegequot$category, col=myPalette, theta=3.14/2)


bdd_vendiagram <- bdd_2020%>%
  select(c("id","freq_vege","menuvege","gasp_auc","cmp","bio"))

for (i in seq_len(nrow(bdd_vendiagram))) {
  bdd_vendiagram[i,"bool_vege_hebdo"] <- 0
  
  bdd_vendiagram$freq_vege[is.na(bdd_vendiagram$freq_vege)] <- 0
  
  if (bdd_vendiagram[i, "freq_vege"] == 1 || bdd_vendiagram[i, "freq_vege"] == 2) {
    bdd_vendiagram[i,"bool_vege_hebdo"] <- 1
  }
}

venn_cmp <- length(bdd_vendiagram[bdd_vendiagram$cmp<2.5 ,1])
venn_vege_hebdo <- length(bdd_vendiagram[bdd_vendiagram$bool_vege_hebdo==1 ,1])
venn_bio <- length(bdd_vendiagram[bdd_vendiagram$bio>20 ,1])


bdd_vendiagram$cmp[is.na(bdd_vendiagram$cmp)] <- 5
bdd_vendiagram$bool_vege_hebdo[is.na(bdd_vendiagram$bool_vege_hebdo)] <- 0
bdd_vendiagram$bio[is.na(bdd_vendiagram$bio)] <- 0

venn_vege_cmp <- 0
for (i in seq_len(nrow(bdd_vendiagram))) {
  if (bdd_vendiagram[i,"cmp"]<2. && bdd_vendiagram[i,"bool_vege_hebdo"] == 1) {
    venn_vege_cmp <- venn_vege_cmp + 1
  }
}

venn_bio_cmp <- 0
for (i in seq_len(nrow(bdd_vendiagram))) {
  if (bdd_vendiagram[i,"bio"]>20 && bdd_vendiagram[i,"cmp"]<2.5) {
    venn_bio_cmp <- venn_bio_cmp + 1
  }
}

venn_vege_bio <- 0
for (i in seq_len(nrow(bdd_vendiagram))) {
  if (bdd_vendiagram[i,"bio"]>20 && bdd_vendiagram[i,"bool_vege_hebdo"] == 1) {
    venn_vege_bio <- venn_vege_bio + 1
  }
}


les_trois <- 0
for (i in seq_len(nrow(bdd_vendiagram))) {
  if (bdd_vendiagram[i,"bio"]>20 && bdd_vendiagram[i,"bool_vege_hebdo"] == 1 && bdd_vendiagram[i,"cmp"]<2.5) {
    les_trois <- les_trois + 1
  }
}
