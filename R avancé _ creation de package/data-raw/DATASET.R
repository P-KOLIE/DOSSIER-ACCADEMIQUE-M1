## code to prepare `iris` dataset goes here

# Source du jeu de données
url_jeu_de_donnees <- 'https://www.kaggle.com/datasets/himanshunakrani/iris-dataset'

# Charger les données (déjà téléchargées manuellement)
iris <- read.csv("data-raw/iris.csv")

# Sauvegarder dans le package
usethis::use_data(iris, overwrite = TRUE)
