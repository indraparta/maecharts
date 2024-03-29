mae_categorical <- list(

  "blue"   = "#0e6ca6",
  "red"    = "#cc1219",
  "purple" = "#5f4b8b",
  "grey"   = "#a9a9a9",
  "grey"   = "#a9a9a9",
  "grey"   = "#a9a9a9"

)

mae_sequential <- list(

  "dark_blue6" = "#ecf7fd",
  "dark_blue5" = "#7db2d2",
  "dark_blue4" = "#468FBC",
  "dark_blue3" = "#0E6CA6",
  "dark_blue2" = "#0C4173",
  "dark_blue1" = "#091540"

)

mae_contrast <- list(

  "mae_blue1"  = "#0E6CA6",
  "mae_blue2"  = "#0E6CA699",
  "mae_blue3"  = "#0E6CA64C",
  "mae_red1"   = "#CC1219",
  "mae_red2"   = "#CC121999",
  "mae_red3"   = "#CC12194C"

)

ORTRTA_categorical <- list(

  "ORTRTA_dark_blue" = "#1e1447",
  "ORTRTA_purple"    = "#822c90",
  "ORTRTA_pink"      = "#ee4980",
  "ORTRTA_orange"    = "#f19620",
  "ORTRTA_yellow"    = "#f2e403"

)

ORTRTA_sequential <- list(

  "ORTRTA_orange"  = "#FCEAD2",
  "ORTRTA_orange2" = "#F3AB4C",
  "ORTRTA_orange3" = "#F6C079",
  "ORTRTA_orange4" = "#F3AB4C",
  "ORTRTA_orange5" = "#FCEAD2",
  "grey"           = "#a9a9a9"

)

ORTRTA_contrast <- list(

  "ORTRTA_dark_blue" = "#1e1447",
  "ORTRTA_blue"      = "#1154a0",
  "ORTRTA_teal"      = "#2cbec5",
  "ORTRTA_purple"    = "#822c90",
  "ORTRTA_pink"      = "#ee4980",
  "ORTRTA_orange"    = "#f19620"

)

mae_pal <- function(
  num_highlight_colours = 2,
  direction = TRUE,
  type = "categorical",
  mae = T
) {

  function(n) {

    if (n > 6) warning("mae Colour Palette only has 6 colours.")

    if (type == "categorical" & mae == T){
      colours <- mae_categorical
    } else if (type == "sequential" & mae == T){
      colours <- mae_sequential
    } else if (type == "contrast" & mae == T){
      colours <- mae_contrast
    } else if (type == "categorical" & mae != T){
      colours <- ORTRTA_categorical
    } else if (type == "sequential" & mae != T){
      colours <- ORTRTA_sequential
    } else if (type == "contrast" & mae != T){
      colours <- ORTRTA_contrast
    }

    if (num_highlight_colours == 1 & type == "categorical") {
      colours[c(2, 3)] <- "#a9a9a9"
      colour_list <- colours[1:n]
    } else if (num_highlight_colours == 2 & type == "categorical") {
      colours[c(3)] <- "#a9a9a9"
      colour_list <- colours[1:n]
    } else {
      colour_list <- colours[1:n]
    }

    if (n == 2 & type == "contrast") {
      colour_list <- colours[c(1, 4)]
    } else if (n == 3 & type == "contrast") {
      colour_list <- colours[c(1, 3, 4)]
    } else if (n == 4 & type == "contrast") {
      colour_list <- colours[c(1, 2, 4, 5)]
    } else if (n == 5 & type == "contrast") {
      colour_list <- colours[c(1, 2, 3, 4, 5)]
    }

    if (n == 2 & type == "sequential") {
      colour_list <- colours[c(1, 6)]
    } else if (n == 3 & type == "sequential") {
      colour_list <- colours[c(1, 4, 6)]
    } else if (n == 4 & type == "sequential") {
      colour_list <- colours[c(1, 2, 4, 6)]
    } else if (n == 5 & type == "sequential") {
      colour_list <- colours[c(1, 2, 3, 4, 6)]
    }

    colour_list <- unname(unlist(colour_list))
    if (direction == TRUE) colour_list else rev(colour_list)
  }
}



#' MAE colour scales for charting
#'
#' This argument adds the MAE colour palette to ggplot charts. The MAE colour palette is designed to be perceived by viewers with common forms of colour blindness.
#'
#' @usage
#' scale_colour_mae(
#'   num_highlight_colours = 2.
#'   direction = T,
#'   type      = "categorical"
#' )
#'
#' @param num_highlight_colours
#' The number of colours to highlight in your chart (categorical data only). The maximum is 3 with additional colours defaulting to grey.
#' @param direction
#' Use argument direction = F to reverse the colour palette.
#' @param type
#' The type of data being charted. Three options are available: "categorical", "sequential" and "contrast".
#' @param ...
#' Other arguments passed on to discrete_scale(), continuous_scale(), or binned_scale to control name, limits, breaks, labels and so forth.
#'
#' @import magrittr
#'
#' @return
#' @export
#'
#' @examples
#'\dontrun{
#' #Use categorical for distinct categories
#'
#' iris %>%
#'   ggplot() +
#'   geom_point(aes(x = Sepal.Width, y = Sepal.Length, colour = Species)) +
#'   scale_colour_mae(type = "categorical")
#'
#' #Use sequential for continuous data
#'
#' diamonds %>%
#'   ggplot() +
#'   geom_point(aes(x = carat, y =  price, colour = clarity)) +
#'   scale_colour_mae(type = "sequential")
#'
#' #Use contrast for data with two distinct categories. Contrast supports 6 shades of two contrasting colours
#'
#' Titanic %>%
#'   as_tibble() %>%
#'   group_by(Class, Survived, Sex) %>%
#'   summarise(n = sum(n)) %>%
#'   ungroup() %>%
#'   mutate(legend = paste(Survived, Sex),
#'          legend = factor(legend, levels = c("No Female", "Yes Female", "No Male", "Yes Male"), labels = c("No ", "Yes ", "No", "Yes"))) %>%
#'   ggplot() +
#'   geom_col(aes(x = Class, y = n, fill = legend)) +
#'   facet_wrap(~Sex) +
#'   scale_fill_mae(type = "contrast", direction = F) +
#'   theme_mae() +
#'   labs(title = "Survivors of the titanic", subtitle = "By sex and passenger class",
#'        x = "", y = "")
#'}



scale_colour_mae <- function(
  num_highlight_colours = 2,
  direction = TRUE,
  type = "categorical",
  mae = TRUE,
  ...
) {
  ggplot2::discrete_scale(
    "colour", "mae",
    mae_pal(num_highlight_colours, direction, type, mae),
    ...
  )
}
