library("openxlsx")
library("shiny")
library("dplyr")
library("ggplot2")


liste_participants <- read.xlsx("R_Data/LISTE PARTICIPANTS OBSERVATOIRE.xlsx", rowNames = TRUE)
bdd_2020 <- read.xlsx("R_Data/bdd_observatoire_2020.xlsx")

str(bdd_2020$bio)

bdd_2020$bio_fact = cut(bdd_2020$bio, breaks=c(0, 20, 40, 60, 80,Inf), labels = c("0-20","20-40","40-60","60-80","80+"))
bdd_2020$tot_rep_fact = cut(bdd_2020$tot_rep, breaks = c(0,500,3000,10000,Inf), labels= c("- 500", "500-3000", "3000-10000", "+ 10000"))
str(bdd_2020$bio_fact)
bdd_2020$bio_fact
str(bdd_2020$cmp)

# Test barplot


bdd20_dedup =bdd_2020[, !duplicated(colnames(bdd_2020))]

ggplot(data=bdd20_dedup, aes(x=bio_fact, y=cmp)) + 
  geom_bar(stat = "summary", fill="#DC4405") +
  theme_classic(base_size = 20)+
  geom_hline(yintercept = 3, linetype = "dashed")+
  xlab("% de bio")+
  ylab("Coûts denrées moyen par repas")

