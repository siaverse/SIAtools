#' @importFrom fs path_file path_ext_remove
#' @importFrom cli cli_warn
#' @importFrom rlang try_fetch
#' @importFrom shiny runApp
#'
preview_current_module <- function() {
  rlang::check_installed("rstudioapi", "to get the active file with the module definition.")

  if (!is_rstudio_available() || !is_rs_api_fun_available("documentPath")) {
    cli_abort("RStudio is not running or the function is not available.")
  }

  active_file_path <- rstudioapi::documentPath()

  module_id <- path_ext_remove(path_file(active_file_path))

  mod <- try_fetch(preview_module(module_id = module_id),
    error = function(cnd) {
      cli_abort(
        "There was an error in previewing the module. Inspect the error below:",
        parent = cnd
      )
    }
  )

  runApp(mod)
}
