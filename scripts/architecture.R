if(!("here" %in% installed.packages())){ # Vérifie si le package est déja installé
  install.packages("here") # L'installe si besoin
}

library(here) # Charge la bibliothèque


dir.create(here("scripts"))

print(paste(here("scripts"), " est créé"))


dir.create(here("data"))
  dir.create(here("data", "raw")) # Crée des dossiers à l'intérieur de \data
  dir.create(here("data", "processed"))
  
  dir.create(here("results"))
  dir.create(here("results", "figure"))
  dir.create(here("results", "tables"))
  
  dir.create(here("docs"))

print("Architecture à jour")