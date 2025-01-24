
library(readxl)


#data <- read.csv("/Users/pierrequintindekercadio/Desktop/cours2401/elus-conseillers-municipaux-cm.csv")

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


df_nantes <- data_exercice[data_exercice$Libellé.de.la.commune == "Nantes",]
# la virgule est pour les colonnes, la on dit partout, si on, avait mis 1 alors on aurait dit qu'on voulait la colonne1 

df_Faverelles <- data_exercice[data_exercice$Libellé.de.la.commune == "Faverelles",]

df_loire_Atlantique <- data_exercice[data_exercice$Libellé.du.département == "Loire-Atlantique",]

df_Gers <- data_exercice[data_exercice$Libellé.du.département == "Gers",]



# Question 3 

compter_nombre_d_elus <- function(df){
  
  df$triplet <- paste(df$Nom.de.l.élu, df$Prénom.de.l.élu, df$Date.de.naissance)
  
  # on vérifie si le triplet est unique 
  
  unique_triplets <- df[!duplicated(df$triplet),]
  
  # on compte unique de triplet 
  
  count = nrow(unique_triplets)
  
  return(count)

}

compter_nombre_d_elus(df_nantes)  



# Question 4 



















































  
  
  
  
