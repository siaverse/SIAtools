
#' Add a new SIA module to your package
#'
#' This is the workhorse of `{SIAtools}` package. The function checks if the
#' package is properly configured for SIA modules and provides immediate fixes
#' as needed. `add_module()` automatically puts a correct entry in SIA Modules
#' Manifest of your package (which is created if not already present), and
#' prepares `.R` file with the code template. Both files are automatically
#' opened for you by default.
#'
#' @param name *character*, a name for the new SIA module.
#' @param title *character*, new module's title. You can leave the default
#'   `NULL` and set manually in the manifest later on.
#' @param category *character*, new module's category. The category dictates the
#'   tab within the `{ShinyItemAnalysis}` app to which the module should be
#'   appended. You can leave the default `NULL` and set manually in the manifest
#'   later on. Check the available categories using [list_categories()].
#' @param open Whether to open the manifest and module's source for interactive
#'   editing. Defaults to `TRUE`.
#' @param prefix *character*, a prefix to denote SIA module. It's highly
#'   recommended to stick with the default `"sm_"` (standing for **S**IA
#'   **M**odule).
#' @param proj *character*, a path to the project. Defaults to [current
#'   project][curr_proj()].
#'
#' @return No return value. Called for the side effects.
#' @export
#'
#' @importFrom cli cli_abort cli_alert_info cli_abort cli_alert
#' @importFrom usethis proj_path
#' @importFrom fs dir_exists dir_create dir_exists path_dir file_create
#'   path_file path_rel
#' @importFrom yaml read_yaml write_yaml
#' @importFrom usethis edit_file proj_get
#' @importFrom rlang arg_match is_interactive
#'
#' @family module_management
#'
#' @examples
#' \dontrun{
#' # add the module called "test" and edit the details later on in the YAML
#' add_module("test")
#'
#' # specify the title and category at creation time
#' add_module("test", title = "Test module", category = "Validity")
#' }
add_module <- function(name = "new_module", title = NULL, category = NULL,
                       open = TRUE, prefix = "sm_", proj = curr_proj()) {
  # check (and repair) the DESCRIPTION field
  check_description(proj = proj)

  # get modules in a list, also check for proj being a package
  mod_list <- get_modules(proj = proj)

  # this name is THE identifier throughout its whole lifetime
  prefixed_name <- paste0(prefix, name)

  # assert new name
  if (prefixed_name %in% names(mod_list)) {
    cli_abort("Module {.val {prefixed_name}} is already present, please pick a different one.")
  }

  yaml_path <- get_yaml_path(proj = proj)

  # if YAML is NULL for any reason
  if (is.null(mod_list)) {
    yaml_dir <- path_dir(yaml_path)

    # create dir for the YAML if not already present
    if (!dir_exists(yaml_dir)) {
      dir_create(yaml_dir)
    }

    cli_alert_info("Creating a new one.")
    file_create(yaml_path)
  }

  # make a temp title if not provided in args
  if (is.null(title)) {
    on.exit(cli_alert("Edit the title of your module in the YAML."), add = TRUE)
    title <- "CHANGE THIS TO YOUR MODULE'S TITLE"
  }

  if (is.null(category)) {
    # if no category provided in args, make a string of available ones
    on.exit(cli_alert("Chose a category from the options listed in the YAML."), add = TRUE)
    category <- paste0(list_categories(), collapse = " OR ")
  } else {
    category <- arg_match(category, list_categories())
  }

  # propose a new module record
  new_mod <- list(
    prefixed_name = list(
      title = title,
      category = category,
      binding = list(
        ui = paste0(prefixed_name, "_ui"),
        server = paste0(prefixed_name, "_server")
      )
    )
  )

  # rename new mod (we couldn't assign a name at list() call a step before)
  names(new_mod) <- prefixed_name

  # just concatenate with the existing YAML content
  new_yaml_content <- c(mod_list, new_mod)

  # write out the new YAML
  new_yaml_content|> write_yaml(yaml_path)

  # overwrite open arg if not interactive
  open <- if (!is_interactive()) FALSE else open

  # open for manual inspection and edits
  with_quiet_usethis(edit_file(yaml_path, open = open))
  cli_alert("{.file {path_rel(yaml_path, start = proj)}} YAML file with the SIA Modules Manifest has been {ifelse(open, \"opened\", \"created\")}. Please inspect and edit.")

  # use mod template with server and UI functions matching those in YAML
  target_file <- paste0("R/", prefixed_name, ".R")
  make_mod_skeleton(prefixed_name = prefixed_name, target_file = target_file, open = open)
  cli_alert("{.file {target_file}} file with your new module has been {ifelse(open, \"opened\", \"created\")}. You can write your code here.")
}

#' @importFrom usethis use_template
#'
make_mod_skeleton <- function(prefixed_name, target_file, open) {
  with_quiet_usethis(
    use_template("module_skeleton.R",
      save_as = target_file,
      data = list(prefixed_name = prefixed_name),
      open = open, package = "SIAtools"
    )
  )
}
