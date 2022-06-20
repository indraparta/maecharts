

save_plot <- function (plot_grid, width, height, x) {
  grid::grid.draw(plot_grid)
  #save it
  ggplot2::ggsave(filename = x,
                  plot=plot_grid, width=(width/72), height=(height/72),  bg="white")
}

#Left align text
left_align <- function(x, pieces){
  grob <- ggplot2::ggplotGrob(x)
  n <- length(pieces)
  grob$layout$l[grob$layout$name %in% pieces] <- 2
  return(grob)
}

create_footer <- function (source_name, logo_image_path) {
  #Make the footer
  footer <- grid::grobTree(grid::linesGrob(x = grid::unit(c(0, 1), "npc"), y = grid::unit(1.1, "npc")),
                           grid::textGrob(source_name,
                                          x = 0.004, hjust = 0, gp = grid::gpar(fontsize=26)),
                           grid::rasterGrob(png::readPNG(logo_image_path), x = 0.944))
  return(footer)

}

#' Arrange alignment and save MAE ggplot chart
#'
#' Running this function will save your plot with the correct guidelines for publication.
#' It will left align your title, subtitle and source, add the MAE blocks at the bottom right.
#' This function will save your plot as a pin on rsconnect which will then be automatically included in the chart_upload_table rmd where you can easily download your chart outside of the VM.
#' @param x The name of the plot you want to export to rsconnect
#' @param source_name The text you want to come after the text 'Source:' in the bottom left hand side of your side
#' @param save_file_path the name and location of the chart you want to save
#' @param width_pixels Width in pixels that you want to save your chart to - defaults to 640
#' @param height_pixels Height in pixels that you want to save your chart to - defaults to 450
#' @param logo_image_path File path for the logo image you want to use in the right hand side of your chart,
#'  which needs to be a PNG file - defaults to DH logo image that sits within the data folder of your package
#' @return (Invisibly) an updated ggplot object.

#' @keywords mae_save_local
#' @examples
#' myplot <- ggplot(iris) +
#'              geom_point(aes(x = Sepal.Width, y = Sepal.Length))
#'
#'
#' mae_save_local(x = myplot,
#'          source = "The source for my data",
#'          save_file_path = "rsconnect_plot",
#'          width_pixels = 640,
#'          height_pixels = 450,
#'          logo_image_path = "path_to_logo.png
#' )
#'
#' @export
mae_save_local <- function(x,
                     source_name,
                     save_file_path,
                     width_pixels = 640,
                     height_pixels = 450,
                     logo_image_path = file.path(system.file("extdata", package = 'maecharts'),"dh-logo.png")) {

  footer <- create_footer(source_name, logo_image_path)

  #Draw your left-aligned grid
  plot_left_aligned <- left_align(x, c("subtitle", "title", "caption"))
  plot_grid <- cowplot::plot_grid(plot_left_aligned, footer,
                                  ncol = 1, nrow = 2,
                                  rel_heights = c(1, 0.045 / (height_pixels / 450)))

  ## Return (invisibly) a copy of the graph. Can be assigned to a
  ## variable or silently ignored.
  save_plot(plot_grid, height = height_pixels, width = width_pixels, x = save_file_path)
}


