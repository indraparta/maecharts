.onLoad <- function(libname, pkgname) {

  sysfonts::font_add("VIC",
           regular = file.path(system.file("extdata", package = 'maecharts'),"VIC-Regular.ttf"),
           bold  = file.path(system.file("extdata", package = 'maecharts'),"VIC-Bold.ttf"))

  showtext::showtext_auto()

  .mae_blue <- "#0e6ca6"
  .mae_red <- "#cc1219"
  .mae_purple <-"#5f4b8b"
  .mae_grey <- "#a9a9a9"
  .mae_darkblue <- "#091540"
}
