create_footer <- function (source_name, logo_image_path) {
  #Make the footer
  footer <- grid::grobTree(grid::textGrob(source_name,
                                          x = 0.004, hjust = 0, gp = grid::gpar(fontsize = 26, lineheight = .5, fontfamily = "VIC")),
                           grid::rasterGrob(png::readPNG(logo_image_path), x = 1 - 0.04, y = 0.5))
  return(footer)

}


create_line <- function() {

  line <- grid::grobTree(grid::linesGrob(x = grid::unit(c(0, 1), "npc"), y = grid::unit(0.3, "npc"), gp = grid::gpar(col = 'lightgrey')))

  return(line)

}

#'  Align MAE ggplot chart
#'
#' Running this function will save your plot with the correct guidelines for publication.
#' It will left align your title, subtitle and source, add the MAE blocks at the bottom right.
#' This function is useful for loading plots in rmarkdowns with the DH watermark
#'
#' @param x The name of the plot you want to export to rsconnect
#' @param source_name The text you want to place in the bottom left hand side of your side of your chart add "Source: to the start of your string to specify a source"
#' @param logo_image_path File path for the logo image you want to use in the right hand side of your chart,
#'  which needs to be a PNG file - defaults to DH logo image that sits within the data folder of your package
#' @return (Invisibly) an updated ggplot object.
#'
#'
#' @importFrom patchwork plot_layout
#'
#' @keywords left_align_plot
#' @examples
#' myplot <- ggplot(iris) +
#'              geom_point(aes(x = Sepal.Width, y = Sepal.Length))
#'
#'
#' left_align_plot(x = myplot,
#'          source_name = "The source for my data",
#'          logo_image_path = "path_to_logo.png
#' )
#'
#' @export

left_align_plot <- function(x,
                           source_name,
                           logo_image_path = file.path(system.file("extdata", package = 'maecharts'),"dh-logo.png")) {


  #error if character limit exceeded
  if(length(source_name) > 154)
    stop(paste0("Error: source_name exceeds character limit (130)"))

  #create footer
  footer <- create_footer(source_name, logo_image_path)

  #add line
  line <- create_line()

  #Draw your left-aligned grid
  plot_grid <- x / line / footer + patchwork::plot_layout(nrow = 3, heights = c(1, 0.0045, 0.0045))

  ## Return (invisibly) a copy of the graph. Can be assigned to a
  ## variable or silently ignored.
  return(plot_grid)
}


