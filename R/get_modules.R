#' Get the SIA Modules Manifest for the currently developed package
#'
#' Returns a list with all modules for the current package as described in its
#' SIA Modules Manifest, which resides at `/inst/sia/modules.yml` and is
#' generated with [add_module()] calls. Can be formatted as a `tibble` using the
#' respective [print method][print.sm_manifest].
#'
#' @param proj *character*, a path to the project. Defaults to [current
#'   project][curr_proj()].
#'
#' @return A SIA Modules Manifest of class `sm_manifest`. Inherits from a
#'   `list`.
#'
#' @importFrom cli cli_alert_danger cli_alert_warning cli_alert_info
#' @importFrom yaml read_yaml
#' @importFrom fs file_exists path_rel
#' @importFrom desc desc_get_or_fail
#'
#' @export
#'
#' @family module_management
#'
#' @examples
#' \dontrun{
#' get_modules()
#' }
#'
get_modules <- function(proj = curr_proj()) {
  if (!is_siamodule_package(proj = proj)) {
    cli_alert_warning(
      c(
        "This package doesn't declare that it contains any SIA modules (there is no line reading ",
        "{.field {names(description_field)}: {description_field}} in the {.file DESCRIPTION})."
      )
    )
    cli_alert_info("Trying to obtain and read SIA Modules Manifest nevertheless...")
  }

  yaml_path <- get_yaml_path(proj = proj)

  if (!file_exists(yaml_path)) {
    cli_alert_danger(
      "There is no SIA Modules Manifest file at {.file {path_rel(yaml_path, start = proj)}}"
    )
    return(invisible(NULL))
  }

  yaml_content <- read_yaml(yaml_path)

  if (is.null(yaml_content)) {
    cli_alert_danger(
      "The SIA Modules Manifest file at {.file {path_rel(yaml_path, start = proj)}} is blank."
    )

    return(invisible(NULL))
  }

  if (!is.list(yaml_content)) {
    cli_alert_danger(
      "The SIA Modules Manifest file at {.file {path_rel(yaml_path, start = proj)}} is corrupt (not a list)."
    )

    return(invisible(NULL))
  }

  # append class for S3 methods (print)
  structure(yaml_content, class = c("sm_manifest", "list"))
}


#' Print a SIA Modules Manifest
#'
#' Prints out the SIA Modules Manifest obtained through [get_modules()]. By
#' default, a plain YAML content is returned, but you can also get a formatted
#' output in a `tibble`, which is suitable for packages with large number of SIA
#' modules.
#'
#' @param x *sm_manifest* object, i.e., an output of [get_modules()].
#' @param ... Not used at the moment.
#' @param as_tibble *logical*, print the manifest as a `tibble`? Defaults to
#'   `FALSE`.
#'
#' @return Called for side effects by default. Returns a `tibble` if `as_tibble`
#' argument is set to `TRUE`.
#'
#' @export
#'
#' @family helpers
#'
#' @importFrom yaml as.yaml
#' @importFrom rlang check_installed .data
#'
print.sm_manifest <- function(x, ..., as_tibble = FALSE) {
  if (as_tibble) {
    check_installed(c("tibble", "tidyr"),
      reason = "to print the SIA Module Manifest as a tibble. Use `as_tibble = FALSE` in `get_modules()` to print a plain YAML."
    )

    out <- x|>
      unclass()|>
      tibble::enframe("module_id", "desc")|>
      tidyr::unnest_wider("desc")|>
      tidyr::unnest_wider("binding", names_sep = "_")

    print(out)
  } else {
    cat(as.yaml(x), "\n", sep = "")
  }
}
