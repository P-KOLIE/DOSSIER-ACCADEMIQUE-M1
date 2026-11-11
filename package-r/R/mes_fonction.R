#'Statistiques descriptives d'une variable quantitative
#'
#'
#'Calcule les principales statistiques descriptives (moyenne, médiane,écart-type, minimum, maximum) pour une variable numérique d'un data.frame.
#' @param data Un data.frame contenant les données.
#' @param variable Le nom ou numero(position de la colonne) de la variable à analyser
#'
#'
#' @returns Un data.frame contenant les statistiques descriptives.
#'
#'
#' @importFrom stats median sd
#'
#'
#' @export
#'
#'
#' @examples
#' stats_var(iris, "Sepal.Length")
#' stats_var(iris, 1)

stats_var <- function(data, variable){

  x <- as.numeric(data[[variable]])

  data.frame(
    variable = names(data)[variable],
    moyenne = mean(x, na.rm = TRUE),
    mediane = median(x, na.rm = TRUE),
    ecart_type = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE)
  )
}



#' Nuage de points entre deux variables quantitatives
#'
#'
#'Produit un graphique représentant la relation entre deux variables quantitatives, accompagné d'une droite de régression linéaire.
#'
#' @param data Un data.frame contenant les données.
#' @param x Le nom de la variable en abscisse.
#' @param y Le nom de la variable en ordonnée.
#'
#'
#' @returns Un objet ggplot2 représentant le graphique.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth labs
#' @importFrom rlang .data
#'
#' @export
#' @examples
#' plot_quanti_quanti(iris, "Sepal.Length", "Petal.Length")

plot_quanti_quanti <- function(data, x, y){

  ggplot(data, aes(x = .data[[x]], y = .data[[y]])) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color="red") +
    labs(
      x = x,
      y = y,
      title = paste("Relation entre", x, "et", y)
    )
}


#' Filtrer les observations selon l'espèce
#'
#' Cette fonction retourne les lignes d'un jeu de données
#' correspondant à une espèce donnée de la variable Species.
#'
#' @param data Un data.frame contenant une colonne Species
#' @param Species Nom de l'espèce à conserver "setosa", "versicolor" ou "virginica"
#'
#' @return Un data.frame filtré selon l'espèce choisie
#'
#' @export
#'
#' @examples
#' filtre_species(iris, "setosa")
#' filtre_species(iris, "virginica")
filtre_species <- function(data, species_name) {
  subset(data, Species == species_name)
}
