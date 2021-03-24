
library("openxlsx")
library("shiny")
library("dplyr")
library("ggplot2")
library("tidyr")
library("stats")
library("plyr")
library("plotrix")
library("patchwork") # To display 2 charts together
library("hrbrthemes")

library(RColorBrewer)
myPalette <- brewer.pal(2,"Dark2") 

liste_participants <- read.xlsx("R_Data/LISTE PARTICIPANTS OBSERVATOIRE.xlsx", rowNames = TRUE)
bdd_2020 <- read.xlsx("R_Data/bdd_observatoire_2020.xlsx")
bdd_2019 <- read.xlsx("R_Data/bdd_observatoire_2019.xlsx")
bdd_2018 <- read.xlsx("R_Data/bdd_observatoire_2018.xlsx")

bdd_2020$bio_fact = cut(bdd_2020$bio, breaks=c(0, 20,40, 60, 80,Inf), labels = c("0-20","20-40","40-60","60-80","80+"))
bdd_2020$tot_rep_fact = cut(bdd_2020$tot_rep, breaks = c(0,500,3000,10000,Inf), labels= c("- 500", "500-3000", "3000-10000", "+ 10000"))


bdd_2019$bio_fact = cut(bdd_2019$bio, breaks=c(0, 20,40, 60, 80,Inf), labels = c("0-20","20-40","40-60","60-80","80+"))
bdd_2019$tot_rep_fact = cut(bdd_2019$tot_rep, breaks = c(0,500,3000,10000,Inf), labels= c("- 500", "500-3000", "3000-10000", "+ 10000"))

bdd_2018$bio_fact = cut(bdd_2018$bio, breaks=c(0, 20,40, 60, 80,Inf), labels = c("0-20","20-40","40-60","60-80","80+"))
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
annee <- c("2018", "2019", "2020")
biorate <- c(mean(dfjoin$bio2018),mean(dfjoin$bio2019),mean(dfjoin$bio2020))
price <- c(mean(dfjoin$cmp2018),mean(dfjoin$cmp2019),mean(dfjoin$cmp2020))
locrate <- c(mean(dfjoin$loc2018),mean(dfjoin$loc2019),mean(dfjoin$loc2020))
dfjoinannee = data.frame(annee,biorate,price)
dfjoinannee$category = as.factor(dfjoinannee$annee)


# création du df des cantines non végétariennes
novege =bdd_2020[, !duplicated(colnames(bdd_2020))]
novege = novege[complete.cases(novege$menuvege), ]
novege = novege[complete.cases(novege$via_bio), ]
novege = novege[novege$menuvege == 2,]

# ajout de deux catégorie à l'intérieur des non végétariens, ceux qui mangent de la viande bio et ceus qui n'en mangent pas
novege$typeviande = NA
novege$typeviande[novege$via_bio == 1] <- "viande bio"
novege$typeviande[novege$via_bio == 0] <- "viande non bio"

# création du df pour le pie chart
novege$typeviande = as.factor(novege$typeviande)
dfnovege = count(novege, 'typeviande')
dfnovege$proportion = NA
dfnovege$proportion[1] = 100*(dfnovege$freq[1]/(dfnovege$freq[1]+dfnovege$freq[2]))
dfnovege$proportion[2] = 100*(dfnovege$freq[2]/(dfnovege$freq[1]+dfnovege$freq[2]))
dfnovege$freqvege = "Non végétarien"


# Création du df des cantines végé hébdomadaires
vegehebdo = bdd_2020[, !duplicated(colnames(bdd_2020))]
vegehebdo = vegehebdo[complete.cases(vegehebdo$freq_vege), ]
vegehebdo = vegehebdo[complete.cases(vegehebdo$via_bio), ]
vegehebdo = vegehebdo[vegehebdo$freq_veg == 2,]

# ajout de deux catégorie à l'intérieur des non végétariens, ceux qui mangent de la viande bio et ceux qui n'en mangent pas
vegehebdo$typeviande = NA
vegehebdo$typeviande[vegehebdo$via_bio == 1] <- "viande bio"
vegehebdo$typeviande[vegehebdo$via_bio == 0] <- "viande non bio"

# création du df pour le pie chart
vegehebdo$typeviande = as.factor(vegehebdo$typeviande)
dfvegehebdo = count(vegehebdo, 'typeviande')
dfvegehebdo$proportion = NA
dfvegehebdo$proportion[1] = 100*(dfvegehebdo$freq[1]/dfvegehebdo$freq[1]+dfvegehebdo$freq[2])
dfvegehebdo$proportion[2] = 100*(dfvegehebdo$freq[2]/dfvegehebdo$freq[1]+dfvegehebdo$freq[2])
dfvegehebdo$freqvege = "Hebdomadaire"

# Création du df des cantines végé hébdomadaires
vegequot = bdd_2020[, !duplicated(colnames(bdd_2020))]
vegequot = vegequot[complete.cases(vegequot$freq_vege), ]
vegequot = vegequot[complete.cases(vegequot$via_bio), ]
vegequot = vegequot[vegequot$freq_veg == 3,]

# ajout de deux catégorie à l'intérieur des non végétariens, ceux qui mangent de la viande bio et ceux qui n'en mangent pas
vegequot$typeviande = NA
vegequot$typeviande[vegequot$via_bio == 1] <- "viande bio"
vegequot$typeviande[vegequot$via_bio == 0] <- "viande non bio"



# création du df pour le pie chart
vegequot$typeviande = as.factor(vegequot$typeviande)
dfvegequot = count(vegequot, 'typeviande')
dfvegequot$proportion = NA
dfvegequot$proportion[1] = 100*(dfvegequot$freq[1]/dfvegequot$freq[1]+dfvegequot$freq[2])
dfvegequot$proportion[2] = 100*(dfvegequot$freq[2]/dfvegequot$freq[1]+dfvegequot$freq[2])
dfvegequot$freqvege = "Quotidien"

df = dplyr::bind_rows(dfnovege, dfvegehebdo)
dfstackedbar = dplyr::bind_rows(df, dfvegequot)




ggplot(dfstackedbar, aes(fill = typeviande,y=freq, x=freqvege)) + 
  geom_bar(position='stack', stat='identity')+
  scale_fill_manual('Position', values=c('coral2', 'coral4')) +
  geom_text(label =dfstackedbar$proportion )

# les éléments qui sont viabo = oui et menuvege = non

pie3D(x=dfnovege$freq,start=2*sqrt(2), labels=dfnovege$category, col=myPalette, theta=3.14/2, main = "Menus non végétariens")
pie3D(x=dfvegehebdo$freq, start=sqrt(2),labels=dfvegehebdo$category, col=myPalette, theta=3.14/2, main = "Menus végétariens hebdomadaires")
pie3D(x=dfvegequot$freq, start=sqrt(2),labels=dfvegequot$category, col=myPalette, theta=3.14/2, main = "Menus végétariens quotidiens")


#Y-a-t-il une relation entre le pourcentage de produits bio et le % de produits locaux ? Si oui, quelle est-elle ?

bdd20_dedup =bdd_2020[, !duplicated(colnames(bdd_2020))]
bdd20_dedup = bdd20_dedup[complete.cases(bdd20_dedup$bio_fact), ]
# POUR LES CNO
ggplot(data=bdd20_dedup, aes(x=bio_fact, y=loc)) + 
  geom_bar(stat = "summary", fill="#DC4405") +
  theme_classic(base_size = 20)+
  geom_hline(yintercept = 100, linetype = "dashed")+
  xlab("% de bio")+
  ylab("% de produits locaux")


bdd20_vegeprix <- read.xlsx("R_Data/bdd_observatoire_2020.xlsx")%>%
  select(c("id","freq_vege","menuvege","cmp"))

bdd20_vegeprix$freq_vege[is.na(bdd20_vegeprix$freq_vege)] <- 0

bdd20_vegeprix = filter(bdd20_vegeprix, freq_vege != 3)
bdd20_vegeprix[bdd20_vegeprix$freq_vege == 0 , ]$freq_vege <- "Non végétarien"
bdd20_vegeprix[bdd20_vegeprix$freq_vege == 1 ,]$freq_vege <- "Végétarien hebdomadaire"
bdd20_vegeprix[bdd20_vegeprix$freq_vege == 2 ,]$freq_vege <- "Végétarien quotidien"



ggplot(data=bdd20_vegeprix, aes(x=freq_vege, y=cmp)) + 
  geom_bar(stat = "summary",fill="#DC4405")+
  theme_classic(base_size = 20)+
  geom_hline(yintercept = 2.7, linetype = "dashed")+
  xlab("Type de repas servi")+
  ylab("Prix moyen d'un repas (euros)")+
  stat_summary(aes(label=..y..), fun.data=fun_mean, geom="text",
               size=5,vjust = 2, color ="white")

fun_mean <- function(x){return(round(data.frame(y=mean(x),label=mean(x,na.rm=T))
                                     ,digit=1))}

ggplot(data=bdd20_dedup, aes(x=bio_fact, y=cmp)) + 
  geom_bar(stat = "summary",fill="#582c83")+
  theme_classic(base_size = 20)+
  geom_hline(yintercept = 3, linetype = "dashed")+
  xlab("Part de bio dans les repas (%)")+
  ylab("Prix moyen d'un repas")+
  stat_summary(aes(label=..y..), fun.data=fun_mean, geom="text",
             size=5,vjust = 2, color ="white")

ymax = max(bdd20_dedup$cmp)

ggplot(bdd20_dedup, aes(x=bio_fact, y=cmp)) +
  geom_segment( aes(x=bio_fact, xend=bio_fact, y=0, yend = cmp)) +
  geom_point( size=6, color="red", fill=alpha("red", 0.5), alpha=0.7, shape=21, stroke=1) +
  theme_bw()


temperatureColor <- "#69b3a2"
priceColor <- rgb(0.2, 0.6, 0.9, 1)

ggplot(dfbioannee, aes(x=annee)) +
  
  geom_line( aes(y=biorate,group=1), size=2, color=temperatureColor) + 
  geom_line( aes(y=price*15 ,group=1), size=2, color=priceColor) +
  geom_point(y=biorate, size=3)+
  geom_point(y=price*15, size=3)+
  geom_hline(yintercept = 33, linetype = "dashed")+
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Pourcentage de bio",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./15, name="Prix du repas")
    
  ) + 
  
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = temperatureColor, size=13),
    axis.title.y.right = element_text(color = priceColor, size=13)
  ) +
  
  ggtitle("Evolution du prix et de la proportion de bio entre 2018 et 2020 (pour toutes les collectivités)")

#-------------------------------------------------------------------------------------------

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


ggplot(dfstackedbar, aes(fill = typeviande,y=freq, x=freqvege)) + 
  geom_bar(position='stack', stat='identity')+
  scale_fill_manual('Position', values=c('coral2', 'coral4'))
  



