

save_plot <- function (plot_grid, width, height, x) {
  #save it
  ggplot2::ggsave(filename = x,
                  plot=plot_grid, width=(width/72), height=(height/72),  bg="white")
}


create_footer <- function (source_name, logo_image_path) {
  #Make the footer
  footer <- grid::grobTree(grid::textGrob(source_name,
                                          x = 0.004, hjust = 0, gp = grid::gpar(fontsize = 26, lineheight = .5, fontfamily = "VIC")),
                           grid::rasterGrob(png::readPNG(logo_image_path), x = 1 - 0.07, y = 0.5))
  return(footer)

}


create_line <- function() {

  line <- grid::grobTree(grid::linesGrob(x = grid::unit(c(0, 1), "npc"), y = grid::unit(0.3, "npc"), gp = grid::gpar(col = 'lightgrey')))

  return(line)

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
#'
#'
#' @importFrom patchwork plot_layout
#'
#' @keywords mae_save_local
#' @examples
# myplot <- ggplot(iris) +
#              geom_point(aes(x = Sepal.Width, y = Sepal.Length))
#
#
# mae_save_local(x = myplot,
#          source_name = "The source for my data",
#          save_file_path = "rsconnect_plot.png",
#          width_pixels = 640,
#          height_pixels = 450
# )
#'
#' @export

mae_save_local <- function(x,
                           source_name,
                           save_file_path,
                           width_pixels = 640,
                           height_pixels = 450,
                           logo_image_path = file.path(system.file("extdata", package = 'maecharts'),"dh-logo.png")) {


  #error if character limit exceeded
  if(length(source_name) > 154)
    stop(paste0("Error: source_name exceeds character limit (130)"))

  #create footer
  footer <- create_footer(source_name, logo_image_path)

  #add line
  line <- create_line()

  #Draw your left-aligned grid
  plot_grid <- x / line / footer + patchwork::plot_layout(nrow = 3, heights = c(1, 0.03, 0.03))

  ## Return (invisibly) a copy of the graph. Can be assigned to a
  ## variable or silently ignored.
  save_plot(plot_grid, height = height_pixels, width = width_pixels, x = save_file_path)
}


