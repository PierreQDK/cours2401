library(readxl)


# data <- read.csv("/Users/pierrequintindekercadio/Desktop/cours2401/elus-conseillers-municipaux-cm.csv")

file_path <- "/Users/pierrequintindekercadio/Desktop/cours2401/elus-conseillers-municipaux-cm.csv"

# Mise en page de la base de données
data_exercice <- read.table(
  file_path,
  header = TRUE,
  sep = ";",
  fill = TRUE,
  quote = ""
)

View(data_exercice)



# Question 2


df_nantes <- data_exercice[data_exercice$Libellé.de.la.commune == "Nantes", ]
# la virgule est pour les colonnes, la on dit partout, si on, avait mis 1 alors on aurait dit qu'on voulait la colonne1

df_Faverelles <- data_exercice[data_exercice$Libellé.de.la.commune == "Faverelles", ]

df_loire_Atlantique <- data_exercice[data_exercice$Libellé.du.département == "Loire-Atlantique", ]

df_Gers <- data_exercice[data_exercice$Libellé.du.département == "Gers", ]



# Question 3

compter_nombre_d_elus <- function(df) {
  # $ crée une ligne en plus

  df$triplet <- paste(df$Nom.de.l.élu, df$Prénom.de.l.élu, df$Date.de.naissance)

  # on vérifie si le triplet est unique

  unique_triplets <- df[!duplicated(df$triplet), ]

  # on compte unique de triplet

  count <- nrow(unique_triplets)

  return(count)
}

compter_nombre_d_elus(df_nantes)



# Question 4


compter_nombre_d_adjoint <- function(df) {
  count <- grepl("adjoint", df$Libellé.de.la.fonction, ignore.case = TRUE )  |>
  sum()
  return(count)
}

compter_nombre_d_adjoint(df_nantes)





#Question 5 
trouver_personne_la_plus_vieille <- function(data_exercice) {
  

df$Date.de.naissance <- as.Date(data_exercice$Date.de.naissance, format = "%Y-%m-%d")
date_plus_ancienne <- min(data_exercice$Date.de.naissancena.rm = TRUE)

return(date_plus_ancienne)

}

trouver_personne_la_plus_vieille(df_nantes)






trouver_personne_la_plus_vieille <- function(df) {
  
  df <- df[order(df$Date.de.naissance)]
  
  df$Date.de.naissance <- as.Date(df$Date.de.naissance, format = "%Y-%m-%d")
  date_plus_ancienne <- min(df$Date.de.naissancena.rm = TRUE)
  
  return(date_plus_ancienne)
  
}

trouver_personne_la_plus_vieille(df_nantes)



# Correction quetsion 3 
library(tidyverse)

Compter_eclu <- function(data_exercice) {
  schema <- c("Code.du.département", "Libellé.du.département", "Code.de.la.collectivité.à.statut.particulier", 
                "Libellé.de.la.collectivité.à.statut.particulier", "Code.de.la.commune", 
                "Libellé.de.la.commune", "Nom.de.l.élu", "Prénom.de.l.élu", 
                "Code.sexe", "Date.de.naissance", "Code.de.la.catégorie.socio.professionnelle", 
                "Libellé.de.la.catégorie.socio.professionnelle", "Date.de.début.du.mandat", 
                "Libellé.de.la.fonction", "Date.de.début.de.la.fonction", "Code.nationalité"
  )

  stopifnot(identical(colnames(data_exercice),  schema))
  
  data_exercice  |>
    select(Nom.de.l.élu, Prénom.de.l.élu, Date.de.naissance)  |> 
    distinct()  |>
    nrow()
}

sapply(list(df_nantes), Compter_eclu)


# colnames(data_exercice) |> dput() ==> faire ca pour afficher toutes les colonnes

library(stringr)

#COrrection question 4
Compter_eclu2 <- function(data_exercice) {
  schema <- c("Code.du.département", "Libellé.du.département", "Code.de.la.collectivité.à.statut.particulier", 
              "Libellé.de.la.collectivité.à.statut.particulier", "Code.de.la.commune", 
              "Libellé.de.la.commune", "Nom.de.l.élu", "Prénom.de.l.élu", 
              "Code.sexe", "Date.de.naissance", "Code.de.la.catégorie.socio.professionnelle", 
              "Libellé.de.la.catégorie.socio.professionnelle", "Date.de.début.du.mandat", 
              "Libellé.de.la.fonction", "Date.de.début.de.la.fonction", "Code.nationalité"
  )
  
  stopifnot(identical(colnames(data_exercice),  schema))
  
  data_exercice  |>
    sum(str_detect(data_exercice$Libellé.de.la.fonction, "adjoint" ))
 
}

sapply(list(df_nantes), Compter_eclu2)



# correection 5

data_exercice <- as_tibble(data_exercice)

#install.packages("lubridate")
library(lubridate)
trouver_elu_vieux <- function(data_exercice) {
  schema <- c("Code.du.département", "Libellé.du.département", "Code.de.la.collectivité.à.statut.particulier", 
              "Libellé.de.la.collectivité.à.statut.particulier", "Code.de.la.commune", 
              "Libellé.de.la.commune", "Nom.de.l.élu", "Prénom.de.l.élu", 
              "Code.sexe", "Date.de.naissance", "Code.de.la.catégorie.socio.professionnelle", 
              "Libellé.de.la.catégorie.socio.professionnelle", "Date.de.début.du.mandat", 
              "Libellé.de.la.fonction", "Date.de.début.de.la.fonction", "Code.nationalité"
  )
  
  stopifnot(identical(colnames(data_exercice),  schema))
  
  data_exercice |>
    mutate(Date.de.naissance = dmy(Date.de.naissance)) |>
    slice(which.min(Date.de.naissance)) |>
    select(Nom.de.l.élu, Prénom.de.l.élu, Date.de.naissance)
  
  
}

trouver_elu_vieux(df_nantes)

# Pour ranger en ligne 
# purrr::map_df(list(df_Faverelles, df_loire_Atlantique), f = trouver_elu_vieux)|>
