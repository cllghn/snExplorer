#' @title Application Launcher
#'
#' @param app name of application.
#' @param use_browser logical.
#'
#' @importFrom shiny runApp
#'
#' @examples
#' \dontrun{
#'
#'
#' launch_shiny_app()
#' }
#'
#' @export
launch_shiny_app <- function(app = "shiny-snExplorer",
                             use_browser = TRUE) {
  dir <- system.file(app,
    package = "snExplorer"
  )

  runApp(
    appDir = dir,
    launch.browser = use_browser
  )
}
