

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

create_footer <- function (source_name, logo_image_path, rel_ratio) {
  #Make the footer
  footer <- grid::grobTree(grid::linesGrob(x = grid::unit(c(0, 1), "npc"), y = grid::unit(1.1, "npc")),
                           grid::textGrob(source_name,
                                          x = 0.004, hjust = 0, gp = grid::gpar(fontsize=26, lineheight = .5)),
                           grid::rasterGrob(png::readPNG(logo_image_path), x = 1 - (rel_ratio * 0.056)))
  return(footer)

}

#' Arrange alignment and save MAE ggplot chart
#'
#' Running this function will save your plot with the correct guidelines for publication.
#' It will left align your title, subtitle and source, add the MAE blocks at the bottom right.
#' This function will save your plot as a pin on rsconnect which will then be automatically included in the chart_upload_table rmd where you can easily download your chart outside of the VM.
#' @param x The name of the plot you want to export to rsconnect
#' @param source_name The text you want to place in the bottom left hand side of your side of your chart add "Source: to the start of your string to specify a source"
#' @param save_file_path The name and location of the chart you want to save
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

  #this variable sets the relative height of the source so that it automatically wraps if the text is too long
  rel_ratio = floor((nchar(source_name) / width_pixels) / 0.196) + 1
  source_name <- str_wrap(source_name, width = width_pixels *  0.196 * (1 - ((rel_ratio - 1) * 0.122)))

  #error if character limit exceeded
  if(rel_ratio > 2)
    stop(paste0("Error: source_name exceeds character limit (", floor(pixel_width *  0.196 * (1 - ((2 - 1) * 0.122)) * 2), ")"))

  #create footer
  footer <- create_footer(source_name, logo_image_path, rel_ratio)

  #Draw your left-aligned grid
  plot_left_aligned <- left_align(x, c("subtitle", "title", "caption"))
  plot_grid <- cowplot::plot_grid(plot_left_aligned, footer,
                                  ncol = 1, nrow = 2,
                                  rel_heights = c(1, 0.045 * rel_ratio / (height_pixels / 450)))



  ## Return (invisibly) a copy of the graph. Can be assigned to a
  ## variable or silently ignored.
  pins::pin_write(pins::board_rsconnect(),
                  x = plot_grid,
                  name = paste("mae", plot_name, width_pixels, height_pixels, sep = "-"))
}


