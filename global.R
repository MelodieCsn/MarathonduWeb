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
library("ggridges")
library("ggplot2")
library("ggplot2")
library("dplyr")
library("hrbrthemes")
library("viridis")

library(RColorBrewer)
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

#Partie ou on garde que les participants fidèles depuis 2018
# annee <- c("2018", "2019", "2020")
# biorate <- c(mean(df_join_subset2018$bio2018),mean(df_join_subset2019$bio2019),mean(df_join_subset2020$bio2020))
# price <- c(mean(df_join_subset2018$cmp2018),mean(df_join_subset2019$cmp2019),mean(df_join_subset2020$cmp2020))
# locrate <- c(mean(df_join_subset2018$loc2018),mean(df_join_subset2019$loc2019),mean(df_join_subset2020$loc2020))
# dfjoinannee <- data.frame(annee,biorate,price)
# dfjoinannee$category <- as.factor(dfjoinannee$annee)



#pie3D(x=dfnovege$freq, labels=dfnovege$category, col=myPalette, theta=3.14/2)
#pie3D(x=dfvegehebdo$freq, labels=dfvegehebdo$category, col=myPalette, theta=3.14/2)
#pie3D(x=dfvegequot$freq, labels=dfvegequot$category, col=myPalette, theta=3.14/2)


#Y-a-t-il une relation entre le pourcentage de produits bio et le % de produits locaux ? Si oui, quelle est-elle ?



ggplot(data=bdd20_dedup, aes(x=bio_fact, y=loc)) + 
  geom_bar(stat = "summary", fill="#DC4405") +
  theme_classic(base_size = 20)+
  geom_hline(yintercept = 100, linetype = "dashed")+
  xlab("% de bio")+
  ylab("% de produits locaux")

temperatureColor <- "#69b3a2"
priceColor <- rgb(0.2, 0.6, 0.9, 1)