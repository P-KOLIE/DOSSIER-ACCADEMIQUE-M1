test_that("stats_var calcule les statistiques correctement", {
  test_data <- data.frame(
    valeurs = c(10, 20, 30, 40, 50)
  )

  res <- stats_var(test_data, "valeurs")

  expect_s3_class(res, "data.frame")
  expect_equal(res$moyenne, 30)
  expect_equal(res$min, 10)
  expect_equal(res$max, 50)
  expect_equal(nrow(res), 1)
})



test_that("plot_quanti_quanti génère un graphique ggplot valide", {
  test_data <- data.frame(
    x = rnorm(10),
    y = rnorm(10)
  )

  res <- plot_quanti_quanti(test_data, "x", "y")

  expect_s3_class(res, "ggplot")
  expect_equal(res$labels$x, "x")
  expect_equal(res$labels$y, "y")
})



test_that("filtre_species renvoie un tableau vide si l'espèce n'existe pas", {
  test_data <- data.frame(
    Species = c("setosa", "virginica")
  )

  res <- filtre_species(test_data, "inconnue")

  expect_equal(nrow(res), 0)
})
