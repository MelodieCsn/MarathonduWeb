library("openxlsx")
library("shiny")
library("dplyr")
library("ggplot2")
library("tidyr")
library("stats")

liste_participants <- read.xlsx("R_Data/LISTE PARTICIPANTS OBSERVATOIRE.xlsx", rowNames = TRUE)
bdd_2020 <- read.xlsx("R_Data/bdd_observatoire_2020.xlsx")

str(bdd_2020$bio)

bdd_2020$bio_fact = cut(bdd_2020$bio, breaks=c(0, 20, 40, 60, 80,Inf), labels = c("0-20","20-40","40-60","60-80","80+"))
bdd_2020$tot_rep_fact = cut(bdd_2020$tot_rep, breaks = c(0,500,3000,10000,Inf), labels= c("- 500", "500-3000", "3000-10000", "+ 10000"))
str(bdd_2020$bio_fact)
bdd_2020$bio_fact
str(bdd_2020$cmp)
str(v)
# Test barplot
novege = bdd_2020
novege = novege[complete.cases(novege$menuvege), ]
novege = novege[complete.cases(novege$via_bio), ]
novege = bdd_2020[bdd_2020$menuvege == 2,]

vbionovege = novege[novege$via_bio == 0,]
vbionovege = vbionovege[complete.cases(vbionovege$via_bio), ]

vnobionovege = novege[novege$via_bio == 1,]
vnobionovege = vnobionovege[complete.cases(vnobionovege$via_bio), ]

novege$category = NA
novege$category[novege$via_bio == 1] <- "vbionovege"
novege$category[novege$via_bio == 0] <- "vnobionovege"


# les éléments qui sont viabo = oui et menuvege = non

ggplot(novege, aes(x=novege, y=c(nrow(vbionovege),nrow(vnobionovege)), fill=category)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

ggplot(data=bdd20_dedup, aes(x=bio_fact, y=cmp)) + 
  geom_bar(stat = "summary", fill="#DC4405") +
  theme_classic(base_size = 20)+
  geom_hline(yintercept = 3, linetype = "dashed")+
  xlab("% de bio")+
  ylab("Coûts denrées moyen par repas")

