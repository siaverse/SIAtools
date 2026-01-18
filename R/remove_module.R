#' Remove a module
#'
#' Removes the given module from the SIA Module Manifest and deletes the
#' respective `.R` file.
#'
#' @param module_id *character*, name of the module to remove (including the
#'   prefix). If `NULL` (the default), all modules discovered by [get_modules()]
#'   are listed and you are asked to pick one.
#' @param proj *character*, a path to the project. Defaults to [current
#'   project][curr_proj()].
#'
#' @return No return value. Called for the side effect.
#' @export
#'
#' @examples
#' \dontrun{
#' remove_module()
#' }
#'
#'
#' @family module_management
#'
#' @importFrom yaml read_yaml write_yaml
#' @importFrom cli cli_alert_success
#' @importFrom fs file_delete path path_rel
#'
remove_module <- function(module_id = NULL, proj = curr_proj()) {
  curr_mod <- pick_module(module_id = module_id, reason = "remove", proj = proj)
  yaml_path <- get_yaml_path(proj = proj)

  manifest <- read_yaml(yaml_path)

  manifest[curr_mod$id] <- NULL
  manifest|> write_yaml(yaml_path)
  cli_alert_success("Module {.field {curr_mod$id}} was removed from the manifest.")

  mod_path <- path(proj, "R", curr_mod$id, ext = "R")
  file_delete(path_rel(mod_path, start = proj))
  cli_alert_success(
    "Module's source file at {.file {path_rel(mod_path, start = proj)}} was deleted."
  )
}
