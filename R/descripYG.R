#' Análisis descriptivo para una variable dependiente con/sin variable independiente
#'
#' Esta función realiza un análisis descriptivo univariado o agrupado según la presencia de una variable independiente.
#' Incluye estadísticos como media, desviación estándar, curtosis y asimetría, y visualizaciones como histogramas, boxplots y densidades.
#'
#' @param dataset data.frame que contiene los datos.
#' @param vd Variable dependiente (numérica).
#' @param vi Variable independiente (opcional, categórica o continua).
#'
#' @return Un data.frame con estadísticas descriptivas. También imprime gráficos.
#' @export
#' @importFrom magrittr "%>%"
#' @importFrom dplyr group_by summarise rename n across tibble
#' @importFrom tidyr drop_na
#' @importFrom tidyselect all_of
#' @importFrom stats IQR median quantile sd mean min max
#' @importFrom ggplot2 ggplot aes aes_string geom_histogram geom_boxplot
#'             geom_density theme_minimal theme_classic xlab ylab labs
#' @importFrom ggridges geom_density_ridges2
#' @importFrom moments kurtosis skewness
#' @importFrom rlang sym
#' @importFrom patchwork wrap_plots plot_layout
#'
#' @examples
#' data(iris)
#' descripYG(iris, vd = Sepal.Length)
#' descripYG(iris, vd = Sepal.Length, vi = Species)

descripYG <- function(dataset, vd, vi = NULL) {

  vd_name <- as.character(substitute(vd))
  vi_name <- if (!missing(vi)) as.character(substitute(vi)) else NULL

  if (!vd_name %in% names(dataset)) stop(paste("La variable dependiente", vd_name, "no existe."))
  if (!is.null(vi_name) && !vi_name %in% names(dataset)) stop(paste("La variable independiente", vi_name, "no existe."))

  vd <- dataset[[vd_name]]
  vi <- if (!is.null(vi_name)) dataset[[vi_name]] else NULL
  dataset <- dataset %>% tidyr::drop_na(tidyselect::all_of(c(vd_name, vi_name)))

  if (is.null(vi)) {
    IQR.dy <- IQR(vd)
    info.dy <- dplyr::tibble(
      n = length(vd),
      promedio = mean(vd),
      mediana = median(vd),
      desv.estd = sd(vd),
      curtosis = moments::kurtosis(vd),
      asimetria = moments::skewness(vd),
      CV = sd(vd)/abs(mean(vd)),
      Min = min(vd),
      Max = max(vd),
      P25 = quantile(vd, 0.25),
      P75 = quantile(vd, 0.75),
      IQR = IQR.dy,
      bmin = quantile(vd, 0.25) - 1.5 * IQR.dy,
      bmax = quantile(vd, 0.75) + 1.5 * IQR.dy
    )

    barras <- trunc(3.322 * log10(length(vd)) + 1)
    g1 <- ggplot2::ggplot(dataset, ggplot2::aes(x = !!rlang::sym(vd_name))) +
      ggplot2::geom_histogram(color = 'white', fill = 'steelblue', bins = barras) +
      ggplot2::theme_classic()

    g2 <- ggplot2::ggplot(dataset, ggplot2::aes(x = !!rlang::sym(vd_name))) +
      ggplot2::geom_boxplot(fill = "lightgreen", width = 0.05)

    p <- patchwork::wrap_plots(g1, g2, ncol = 1, heights = c(3, 1))
    print(p)
    return(as.data.frame(info.dy))

  } else {
    info.grupo <- dataset %>%
      dplyr::group_by(dplyr::across(tidyselect::all_of(vi_name))) %>%
      dplyr::summarise(
        ni = dplyr::n(),
        Promedio = mean(!!rlang::sym(vd_name)),
        Mediana = median(!!rlang::sym(vd_name)),
        Desv.Estd = sd(!!rlang::sym(vd_name)),
        Curtosis = moments::kurtosis(!!rlang::sym(vd_name)),
        Asimetria = moments::skewness(!!rlang::sym(vd_name)),
        CV = sd(!!rlang::sym(vd_name))/abs(mean(!!rlang::sym(vd_name))),
        Min = min(!!rlang::sym(vd_name)),
        Max = max(!!rlang::sym(vd_name)),
        P25 = quantile(!!rlang::sym(vd_name), 0.25),
        P75 = quantile(!!rlang::sym(vd_name), 0.75),
        IQR = IQR(!!rlang::sym(vd_name)),
        .groups = "drop"
      ) %>%
      dplyr::rename(Grupo = tidyselect::all_of(vi_name))

    bd1 <- ggplot2::ggplot(dataset, ggplot2::aes(x = !!rlang::sym(vd_name), y = as.factor(!!rlang::sym(vi_name)))) +
      ggridges::geom_density_ridges2() +
      ggplot2::theme_minimal()

    bp <- ggplot2::ggplot(dataset, ggplot2::aes(x = factor(!!rlang::sym(vi_name)), y = !!rlang::sym(vd_name))) +
      ggplot2::geom_boxplot(color = 'darkslategray', fill = 'steelblue') +
      ggplot2::theme_classic()

    print(bd1)
    print(bp)
    return(as.data.frame(info.grupo))
  }
}

