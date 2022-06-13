.onLoad <- function(libname, pkgname) {

  sysfonts::font_add("VIC",
           regular = file.path(system.file("extdata", package = 'maecharts'),"VIC-Regular.ttf"),
           bold  = file.path(system.file("extdata", package = 'maecharts'),"VIC-Bold.ttf"))

  showtext::showtext_auto()

}
