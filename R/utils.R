# this field and value is what SIA looks for
description_field <- c("Config/ShinyItemAnalysis/module" = "true")


#' Get a current project path
#'
#' This is a thin wrapper around `usethis::proj_get()` that silences any messages.
#'
#' @return The path to the current project.
#' @export
#'
#' @importFrom usethis proj_get
#' @family helpers
#'
#' @examples
#' \dontrun{
#' curr_proj()
#' }
#'
curr_proj <- function() {
  with_quiet_usethis(proj_get())
}


#' @importFrom desc desc_get_field
#' @importFrom fs file_exists
#' @importFrom rlang try_fetch
#' @importFrom cli cli_abort
#'
get_package_name <- function(proj = curr_proj()) {
  if (!file_exists("DESCRIPTION")) {
    cli_abort(
      c("There is no {.file DESCRIPTION} file in {.file {curr_proj()}}. This signifies that there is no package.
      {.pkg SIAtools} needs to operate on a package.",
        ">" = "Create a new package prepared for SIA modules with {.fn SIAtools::create_module_project}
      or...",
        ">" = "set your working directory to your existing package's root in case you want to equip it
      with a SIA module."
      )
    )
  }
  # this aborts if not a package (starting in 2023, only packages has DESCRIPTION)
  pkg_name <- try_fetch(desc_get_field("Package", default = NULL),
    error = function(cnd) {
      cli_abort(
        "There was an error in reading the package name. The {.file DESCRIPTION} is likely blank,
        corrupted or does not contain the {.field Package} field.",
        parent = cnd
      )
    }
  )
  invisible(pkg_name)
}

check_is_package <- get_package_name


#' Check if a package is identified as containing SIA modules
#'
#' @param proj *character*, a path to the project. Defaults to [current
#'   project][curr_proj()].
#'
#' @return Side effects.
#' @keywords internal
#'
#' @importFrom desc desc_set
#'
check_description <- function(proj = curr_proj()) {
  check_is_package()

  siamodule_field <- get_siamodule_description_field(proj = proj)

  if (isFALSE(siamodule_field)) {
    cli_alert_info("Adding {.field {names(description_field)}: {description_field}} to {.file DESCRIPTION}.")
    do.call(desc_set, as.list(description_field))

    return(
      invisible(
        do.call(desc_set, as.list(description_field))
      )
    )
  }

  if (siamodule_field != "true") {
    cli_alert_warning(
      c(
        "There is {.field {names(description_field)}} field in your {.file DESCRIPTION}, ",
        "but it is set to {.val {siamodule_field}}, repairing to {.val {description_field}}."
      )
    )

    invisible(
      do.call(desc_set, as.list(description_field))
    )
  }
}


#' @importFrom desc desc_get_field
get_siamodule_description_field <- function(proj = curr_proj()) {
  desc_get_field(
    names(description_field),
    default = FALSE,
    file = proj
  )
}

is_siamodule_package <- function(proj = curr_proj()) {
  siamodule_field <- get_siamodule_description_field(proj = proj)
  siamodule_field == "true"
}


SIAtools_file <- function(...) {
  system.file(..., package = "SIAtools", mustWork = TRUE)
}


with_quiet_usethis <- function(expr) {
  orig <- options(usethis.quiet = TRUE)
  on.exit(options(orig))

  force(expr)
}

# a standard yaml path to consult
get_yaml_path <- function(proj = curr_proj()) {
  paste0(proj, "/inst/sia/modules.yml")
}

#' @importFrom cli cli_abort col_green
#' @importFrom purrr map_chr
pick_module <- function(module_id = NULL, reason, proj = curr_proj()) {
  mods <- get_modules(proj = proj)

  if (is.null(mods)) {
    cli_abort("No modules available to {reason}.")
  }

  mods_titles <- mods|> map_chr("title")

  if (!is.null(module_id)) {
    if (!is.character(module_id)) {
      cli_abort("{.arg module_id} must be of class {.cls character}.")
    }
    if (length(module_id) != 1L) {
      cli_abort("You cannot preview multiple modules at once.")
    }
    if (!module_id %in% names(mods)) {
      cli_abort("There is no module called {.val {module_id}} in the manifest.")
    }
    # if legal module_id, subset it from all mods
    mods <- mods[module_id]
  }

  # if there is only one module (or one was picked by name), don't ask for anything
  if (length(mods) == 1L) {
    curr_mod <- mods
  } else {
    # or if no module_id was given, let the user pick interactively
    picked_mod <- menu(
      paste0(col_green(names(mods_titles)), ": ", mods_titles),
      title = paste0("Pick the module you want to ", reason, ":")
    )
    curr_mod <- mods[picked_mod]
  }

  # add an id directly to the mod specs, along with the title, bindings etc..
  curr_mod[[1L]][["id"]] <- names(curr_mod)

  curr_mod[[1L]]
}


#' Show RStudio Keyboard Shortcuts
#'
#' Shows a popup window with RStudio keyboard shortcuts. Applicable only in
#' RStudio and in interactive `R` session.
#'
#' You can quickly reach out solicited addin function by typing it in the
#' `Filter...` box in the very top of the window. Then double click at the blank
#' space just next to the addin function name and press down desired key or key
#' combination. Apply the changes and from now on, just call the addin function
#' with one keystroke. For more details, navigate to [RStudio
#' documentation](https://support.posit.co/hc/en-us/articles/206382178-Customizing-Keyboard-Shortcuts-in-the-RStudio-IDE).
#'
#' @return No return value. Called for side effect.
#'
#' @family helpers
#'
#' @importFrom rlang check_installed
#' @importFrom cli cli_abort
#'
#' @examples
#' if (interactive()) {
#' edit_rstudio_shortcuts()
#' }
#'
#' @export
edit_rstudio_shortcuts <- function() {
  check_installed("rstudioapi", "to open RStudio keyboard shortcuts window.")

  if (!is_rstudio_available() || !is_rs_api_fun_available("executeCommand")) {
    cli_abort("RStudio is not running or the function is not available.")
  }

  invisible(rstudioapi::executeCommand("modifyKeyboardShortcuts"))
}


# to mock in tests
is_rstudio_available <- function(...) {
  rstudioapi::isAvailable(...)
}

is_rs_api_fun_available <- function(...) {
  rstudioapi::hasFun(...)
}

rs_api_documentSaveAll <- function(...) {
  rstudioapi::documentSaveAll(...)
}


#' Open SIA Modules Manifest for Editing
#'
#' @param proj *character*, a path to the project. Defaults to [current
#'   project][curr_proj()].
#'
#' @return *character*, a path to SIA Module Manifest (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' open_sm_manifest()
#' }
#'
#' @importFrom usethis edit_file
#'
#' @family helpers
open_sm_manifest <- function(proj = curr_proj()) {

yaml_path <- get_yaml_path(proj = proj)
with_quiet_usethis(edit_file(yaml_path))

invisible(yaml_path)
}


#' List SIA module packages available on the official repository
#'
#' @param repo *character*, a URL to the repository. Defaults to the official
#'  SIA repository.
#'
#' @keywords internal
#'
#' @importFrom utils available.packages
#'
sm_list_available <- function(repo = default_repo()) {
  sm_field <- names(description_field)
  pkgs <- available.packages(repos = repo, fields = sm_field)
  pkgs <- pkgs[, sm_field]
  pkgs <- pkgs[!is.na(pkgs)]
  pkgs <- pkgs[pkgs == "true"]
  names(pkgs)
}

default_repo <- function() {
  getOption("sia.modules_repo", "https://applstat.github.io/SIArepo/")
}
