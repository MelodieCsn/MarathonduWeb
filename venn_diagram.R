library(dplyr)
library(VennDiagram)
library(openxlsx)

bdd20_departement <- read.xlsx("R_Data/bdd_observatoire_2020.xlsx")%>%
  select(c("id","freq_vege","menuvege","gasp_auc","cmp","bio"))

for (i in seq_len(nrow(bdd20_departement))) {
  bdd20_departement[i,"bool_vege_hebdo"] <- 0

  bdd20_departement$freq_vege[is.na(bdd20_departement$freq_vege)] <- 0

  if (bdd20_departement[i, "freq_vege"] == 1 || bdd20_departement[i, "freq_vege"] == 2) {
    bdd20_departement[i,"bool_vege_hebdo"] <- 1
  }
}

cmp <- length(bdd20_departement[bdd20_departement$cmp<2.5 ,1])
vege_hebdo <- length(bdd20_departement[bdd20_departement$bool_vege_hebdo==1 ,1])
bio <- length(bdd20_departement[bdd20_departement$bio>20 ,1])
cmp
vege_hebdo
bio

bdd20_departement$cmp[is.na(bdd20_departement$cmp)] <- 5
bdd20_departement$bool_vege_hebdo[is.na(bdd20_departement$bool_vege_hebdo)] <- 0
bdd20_departement$bio[is.na(bdd20_departement$bio)] <- 0

vege_cmp <- 0
for (i in seq_len(nrow(bdd20_departement))) {
  if (bdd20_departement[i,"cmp"]<2. && bdd20_departement[i,"bool_vege_hebdo"] == 1) {
    vege_cmp <- vege_cmp + 1
  }
}
vege_cmp

bio_cmp <- 0
for (i in seq_len(nrow(bdd20_departement))) {
  if (bdd20_departement[i,"bio"]>20 && bdd20_departement[i,"cmp"]<2.5) {
    bio_cmp <- bio_cmp + 1
  }
}
bio_cmp

vege_bio <- 0
for (i in seq_len(nrow(bdd20_departement))) {
  if (bdd20_departement[i,"bio"]>20 && bdd20_departement[i,"bool_vege_hebdo"] == 1) {
    vege_bio <- vege_bio + 1
  }
}
vege_bio

les_trois <- 0
for (i in seq_len(nrow(bdd20_departement))) {
  if (bdd20_departement[i,"bio"]>20 && bdd20_departement[i,"bool_vege_hebdo"] == 1 && bdd20_departement[i,"cmp"]<2.5) {
    les_trois <- les_trois + 1
  }
}
les_trois

grid.newpage()
draw.triple.venn(area1 = cmp,
                 area2 = vege_hebdo,
                 area3 = bio,
                 n12 = vege_cmp,
                 n23 = vege_bio,
                 n13 = bio_cmp,
                 n123 = les_trois,
                 fill = c("yellow", "blue", "red"),
                 category = c("Prix < 2.5€", "Au moins 1 repas végé hebdo", "bio +20%"))
