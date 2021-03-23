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


library(RColorBrewer)
myPalette <- brewer.pal(2,"Dark2") 

liste_participants <- read.xlsx("LISTE PARTICIPANTS OBSERVATOIRE.xlsx", rowNames = TRUE)
bdd_2020 <- read.xlsx("bdd_observatoire_2020.xlsx")
bdd_2019 <- read.xlsx("bdd_observatoire_2019.xlsx")
bdd_2018 <- read.xlsx("bdd_observatoire_2018.xlsx")


bdd_2020$bio_fact = cut(bdd_2020$bio, breaks=c(0, 20, 40, 60, 80,Inf), labels = c("0-20","20-40","40-60","60-80","80+"))
bdd_2020$tot_rep_fact = cut(bdd_2020$tot_rep, breaks = c(0,500,3000,10000,Inf), labels= c("- 500", "500-3000", "3000-10000", "+ 10000"))


bdd_2019$bio_fact = cut(bdd_2019$bio, breaks=c(0, 20, 40, 60, 80,Inf), labels = c("0-20","20-40","40-60","60-80","80+"))
bdd_2019$tot_rep_fact = cut(bdd_2019$tot_rep, breaks = c(0,500,3000,10000,Inf), labels= c("- 500", "500-3000", "3000-10000", "+ 10000"))

bdd_2018$bio_fact = cut(bdd_2018$bio, breaks=c(0, 20, 40, 60, 80,Inf), labels = c("0-20","20-40","40-60","60-80","80+"))
bdd_2018$tot_rep_fact = cut(bdd_2018$tot_rep, breaks = c(0,500,3000,10000,Inf), labels= c("- 500", "500-3000", "3000-10000", "+ 10000"))



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

#Partie ou on garde que les participants fidèles depuis 2018
# annee <- c("2018", "2019", "2020")
# biorate <- c(mean(df_join_subset2018$bio2018),mean(df_join_subset2019$bio2019),mean(df_join_subset2020$bio2020))
# price <- c(mean(df_join_subset2018$cmp2018),mean(df_join_subset2019$cmp2019),mean(df_join_subset2020$cmp2020))
# locrate <- c(mean(df_join_subset2018$loc2018),mean(df_join_subset2019$loc2019),mean(df_join_subset2020$loc2020))
# dfjoinannee <- data.frame(annee,biorate,price)
# dfjoinannee$category <- as.factor(dfjoinannee$annee)


# création du df des cantines non végétariennes
novege =bdd_2020[, !duplicated(colnames(bdd_2020))]
novege = novege[complete.cases(novege$menuvege), ]
novege = novege[complete.cases(novege$via_bio), ]
novege = novege[novege$menuvege == 2,]

# ajout de deux catégorie à l'intérieur des non végétariens, ceux qui mangent de la viande bio et ceus qui n'en mangent pas
novege$category = NA
novege$category[novege$via_bio == 1] <- "viande bio"
novege$category[novege$via_bio == 0] <- "viande non bio"

# création du df pour le pie chart
novege$category = as.factor(novege$category)
dfnovege = count(novege, 'category')

# Création du df des cantines végé hébdomadaires
vegehebdo = bdd_2020[, !duplicated(colnames(bdd_2020))]
vegehebdo = vegehebdo[complete.cases(vegehebdo$freq_vege), ]
vegehebdo = vegehebdo[complete.cases(vegehebdo$via_bio), ]
vegehebdo = vegehebdo[vegehebdo$freq_veg == 2,]

# ajout de deux catégorie à l'intérieur des non végétariens, ceux qui mangent de la viande bio et ceux qui n'en mangent pas
vegehebdo$category = NA
vegehebdo$category[vegehebdo$via_bio == 1] <- "viande bio"
vegehebdo$category[vegehebdo$via_bio == 0] <- "viande non bio"

# création du df pour le pie chart
vegehebdo$category = as.factor(vegehebdo$category)
dfvegehebdo = count(vegehebdo, 'category')

# Création du df des cantines végé hébdomadaires
vegequot = bdd_2020[, !duplicated(colnames(bdd_2020))]
vegequot = vegequot[complete.cases(vegequot$freq_vege), ]
vegequot = vegequot[complete.cases(vegequot$via_bio), ]
vegequot = vegequot[vegequot$freq_veg == 3,]

# ajout de deux catégorie à l'intérieur des non végétariens, ceux qui mangent de la viande bio et ceux qui n'en mangent pas
vegequot$category = NA
vegequot$category[vegequot$via_bio == 1] <- "Viande bio"
vegequot$category[vegequot$via_bio == 0] <- "Viande non bio"

# création du df pour le pie chart
vegequot$category = as.factor(vegequot$category)
dfvegequot = count(vegequot, 'category')


# les éléments qui sont viabo = oui et menuvege = non

pie3D(x=dfnovege$freq, labels=dfnovege$category, col=myPalette, theta=3.14/2)
pie3D(x=dfvegehebdo$freq, labels=dfvegehebdo$category, col=myPalette, theta=3.14/2)
pie3D(x=dfvegequot$freq, labels=dfvegequot$category, col=myPalette, theta=3.14/2)


#Y-a-t-il une relation entre le pourcentage de produits bio et le % de produits locaux ? Si oui, quelle est-elle ?

bdd20_dedup =bdd_2020[, !duplicated(colnames(bdd_2020))]
bdd20_dedup = bdd20_dedup[complete.cases(bdd20_dedup$bio_fact), ]

ggplot(data=bdd20_dedup, aes(x=bio_fact, y=loc)) + 
  geom_bar(stat = "summary", fill="#DC4405") +
  theme_classic(base_size = 20)+
  geom_hline(yintercept = 100, linetype = "dashed")+
  xlab("% de bio")+
  ylab("% de produits locaux")

temperatureColor <- "#69b3a2"
priceColor <- rgb(0.2, 0.6, 0.9, 1)

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
  
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = temperatureColor, size=13),
    axis.title.y.right = element_text(color = priceColor, size=13)
  ) +
  
  ggtitle("Evolution du prix et de la proportion de bio entre 2018 et 2020 (pour les collectivités présentes depuis 2018)")

