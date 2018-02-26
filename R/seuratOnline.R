#' @export
seuratOnline <- function(port){
  if(is.null(port))
    port = 1234
  appDir <- system.file('shiny', package = "seuratOnline")
  shiny::runApp(appDir,host = getOption('shiny.host', '0.0.0.0'),port = port,launch.browser = FALSE)
}
