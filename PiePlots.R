
library("openxlsx")
library("shiny")
library("dplyr")
library("ggplot2")
library("tidyr")
library("stats")
library("plyr")

liste_participants <- read.xlsx("R_Data/LISTE PARTICIPANTS OBSERVATOIRE.xlsx", rowNames = TRUE)
bdd_2020 <- read.xlsx("R_Data/bdd_observatoire_2020.xlsx")

str(bdd_2020$bio)
bdd_2020$bio_fact = cut(bdd_2020$bio, breaks=c(0, 20, 40, 60, 80,Inf), labels = c("0-20","20-40","40-60","60-80","80+"))
bdd_2020$tot_rep_fact = cut(bdd_2020$tot_rep, breaks = c(0,500,3000,10000,Inf), labels= c("- 500", "500-3000", "3000-10000", "+ 10000"))
str(bdd_2020$bio_fact)
bdd_2020$bio_fact
str(bdd_2020$cmp)



# création du df des cantines non végétariennes
novege =bdd_2020[, !duplicated(colnames(bdd_2020))]
novege = novege[complete.cases(novege$menuvege), ]
novege = novege[complete.cases(novege$via_bio), ]
novege = novege[novege$menuvege == 2,]

# ajout de deux catégorie à l'intérieur des non végétariens, ceux qui mangent de la viande bio et ceus qui n'en mangent pas
novege$category = NA
novege$category[novege$via_bio == 1] <- "vbio"
novege$category[novege$via_bio == 0] <- "vnobio"

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
vegehebdo$category[vegehebdo$via_bio == 1] <- "vbio"
vegehebdo$category[vegehebdo$via_bio == 0] <- "vnobio"

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
vegequot$category[vegequot$via_bio == 1] <- "vbio"
vegequot$category[vegequot$via_bio == 0] <- "vnobio"

# création du df pour le pie chart
vegequot$category = as.factor(vegequot$category)
dfvegequot = count(vegequot, 'category')


# les éléments qui sont viabo = oui et menuvege = non

pie(x=dfnovege$freq, labels=dfnovege$category)
pie(x=dfvegehebdo$freq, labels=dfvegehebdo$category)
pie(x=dfvegequot$freq, labels=dfvegequot$category)

#Y-a-t-il une relation entre le pourcentage de produits bio et le % de produits locaux ? Si oui, quelle est-elle ?

bdd20_dedup =bdd_2020[, !duplicated(colnames(bdd_2020))]
bdd20_dedup = bdd20_dedup[complete.cases(bdd20_dedup$bio_fact), ]

ggplot(data=bdd20_dedup, aes(x=bio_fact, y=loc)) + 
  geom_bar(stat = "summary", fill="#DC4405") +
  theme_classic(base_size = 20)+
  geom_hline(yintercept = 100, linetype = "dashed")+
  xlab("% de bio")+
  ylab("% de produits locaux")
