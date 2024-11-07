#' Calcula correlación y asociaciones ponderadas en la EPH
#'
#' Esta función calcula diferentes estadísticas de correlación y asociación ponderadas
#' en el contexto de un diseño de encuesta. Acepta métodos como chi-cuadrado, V de Cramer,
#' correlación de Pearson y R².
#'
#' @param data El dataset que contiene las variables a analizar.
#' @param var1 Primer variable a analizar.
#' @param var2 Segunda variable a analizar.
#' @param peso El nombre de la columna que contiene los pesos.
#' @param method El método estadístico a usar: "chisquare", "cramer", "pearson", "r2".
#' @return Una o varias tablas con los resultados estadísticos.
#' @export
calculate_cor <- function(data, var1, var2, peso = "PONDERA", method = "chisquare") {

  # Definir el diseño de encuesta ponderado con srvyr
  diseño <- as_survey_design(data, weights = !!sym(peso))

  # Detectar tipo de variables (numéricas o categóricas)
  tipo_var1 <- ifelse(is.numeric(data[[var1]]), "numérica", "categórica")
  tipo_var2 <- ifelse(is.numeric(data[[var2]]), "numérica", "categórica")

  # Inicializar lista para almacenar resultados
  resultados <- list()

  # Seleccionar método de cálculo
  if (method == "chisquare" && tipo_var1 == "categórica") {
    # Test de chi-cuadrado ponderado
    chi_test <- svychisq(as.formula(paste0("~", var1, "+", var2)), diseño)
    resultados$Chi_Cuadrado <- tibble(
      Estadístico = "Chi-cuadrado",
      Valor = chi_test$statistic,
      p_valor = chi_test$p.value
    )

  } else if (method == "cramer" && tipo_var1 == "categórica") {
    # Chi-cuadrado ponderado y V de Cramer
    chi_test <- svychisq(as.formula(paste0("~", var1, "+", var2)), diseño)
    chi_square <- chi_test$statistic
    n_efectivo <- sum(weights(diseño))  # Tamaño efectivo ponderado
    tabla <- svytable(as.formula(paste0("~", var1, "+", var2)), diseño)
    min_dim <- min(nrow(tabla) - 1, ncol(tabla) - 1)
    v_cramer <- sqrt((chi_square / n_efectivo) / min_dim)
    resultados$Cramer <- tibble(
      Estadístico = "V de Cramer",
      Valor = v_cramer,
      p_valor = NA
    )

  } else if (method == "pearson" && tipo_var1 == "numérica") {
    # Correlación de Pearson no ponderada usando cor.test para obtener estadísticas detalladas
    test_pearson <- cor.test(data[[var1]], data[[var2]], method = "pearson")
    resultados$Pearson <- tibble(
      Estadístico = "R de Pearson",
      Valor = test_pearson$estimate,
      p_valor = test_pearson$p.value,
      Conf.Lower = test_pearson$conf.int[1],
      Conf.Upper = test_pearson$conf.int[2],
      t_estadistico = test_pearson$statistic,
      gl = test_pearson$parameter
    )

  } else if (method == "r2" && tipo_var1 == "numérica") {
    # Usar lm para obtener la regresión completa
    modelo <- lm(as.formula(paste(var2, "~", var1)), data = data)
    resumen <- summary(modelo)

    # Extraer los valores necesarios
    r_cuadrado <- resumen$r.squared
    r_cuadrado_ajustado <- resumen$adj.r.squared
    f_estadistico <- resumen$fstatistic[1]
    f_df1 <- resumen$fstatistic[2]
    f_df2 <- resumen$fstatistic[3]
    f_p_value <- pf(f_estadistico, f_df1, f_df2, lower.tail = FALSE)

    # Tablas para resultados de la regresión
    resultados$Estadisticos_Generales <- tibble(
      R2 = r_cuadrado,
      R2_Ajustado = r_cuadrado_ajustado,
      F_Estadistico = f_estadistico,
      F_p_value = f_p_value,
      F_df1 = f_df1,
      F_df2 = f_df2
    )

    resultados$Coeficientes <- as_tibble(resumen$coefficients, rownames = "Variable")
    resultados$Residuales <- tibble(Residuales = residuals(modelo))

  } else {
    stop("Método no válido o tipo de variables incompatible con el método seleccionado.")
  }

  return(resultados)
}

