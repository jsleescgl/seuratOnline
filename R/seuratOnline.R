#' @export
seuratOnline <- function(){
  appDir <- system.file('shiny', package = "seuratOnline")
  shiny::runApp(appDir, display.mode = "normal")
}
