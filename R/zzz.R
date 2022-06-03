.onLoad <- function(libname, pkgname) {

  library(showtext)

  showtext::font_add("VIC",
           regular = file.path(system.file("data", package = 'maecharts'),"VIC-Regular.ttf"),
           bold  = file.path(system.file("data", package = 'maecharts'),"VIC-Bold.ttf"))

  showtext::showtext_auto()

}
