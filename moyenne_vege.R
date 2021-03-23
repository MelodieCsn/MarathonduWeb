bdd20_departement <- read.xlsx("R_Data/bdd_observatoire_2020.xlsx")%>%
  select(c("id","freq_vege","menuvege","code.département")) %>%
  group_by(code.département)

for (i in seq_len(nrow(bdd20_departement))) {
  bdd20_departement[i,"hedbo"] <- 0

  bdd20_departement$freq_vege[is.na(bdd20_departement$freq_vege)] <- 0

  if (bdd20_departement[i, "freq_vege"] == 1 || bdd20_departement[i, "freq_vege"] == 2) {
    bdd20_departement[i,"hedbo"] <- 1
  }
}



bdd20_summary <- bdd20_departement %>% dplyr::summarise(N = n(),mean_vege = mean(hedbo, na.rm = TRUE))