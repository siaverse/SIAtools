#' Preview a module
#'
#' Previews a SIA module in a standalone development environment. See the
#' details below.
#'
#' The function takes module's function bindings and puts (or evaluates) them
#' inside a bare bone [shiny::shinyApp()]. By default, a customized head tag is
#' injected in order to mimic the "environment" of full `{ShinyItemAnalysis}`
#' app. See [sia_head_tag()] for more details. Besides, a `onSessionEnded` hook
#' is set to call [shiny::stopApp()] after the client disconnects, so the
#' "process" is automatically quit after you close the preview in your browser
#' or RStudio viewer.
#'
#' In order to use the function bindings, `preview_module()` attempts to
#' [load][pkgload::load_all] your package without the actual installation by
#' default.
#' **Note that you have to install the package as usual for
#' `{ShinyItemAnalysis}` to detect your modules.**
#'
#' ## Using objects from the  `{ShinyItemAnalysis}` app
#'
#' Note that this "emulated" preview environment is really meant to test the
#' basic UI layout and functionality and is not able to receive any object from
#' `{ShinyItemAnalysis}` app. However, you can pass any object like
#' `{ShinyItemAnalysis}` does to `server_imports` argument manually. For further
#' details and examples, please refer to `vignette("imports", "SIAtools")`
#' vignette.
#'
#' @param module_id *character*, name of the module to preview (including the
#'   prefix). If `NULL` (the default), all modules discovered by [get_modules()]
#'   are listed and you are asked to pick one.
#' @param ui_imports *list*, UI objects exported from the `{ShinyItemAnalysis}`
#'   app. *Not used at the moment*.
#' @param server_imports *list*, reactive objects exported from the
#'   `{ShinyItemAnalysis}` app. See the Details.
#' @param ui_elements elements to include in `fluidPage`, preferably packed in
#'   `tagList()`.
#' @param save_and_document *logical*, whether to [save
#'   all][rstudioapi::documentSaveAll] unsaved files (only available in RStudio)
#'   and [document][roxygen2::roxygenise] the package. Defaults to `TRUE`. Note
#'   that documenting the package is necessary if you use any functions from
#'   external packages (to produce the `NAMESPACE`).
#' @param load *logical*, whether to [load][pkgload::load_all] your package
#'   before running the module preview. Defaults to `TRUE`. Note that you have
#'   to load the package by yourself or install it in the usual way if you set
#'   this to `FALSE`.
#' @param proj *character*, a path to the project. Defaults to [current
#'   project][curr_proj()].
#' @inheritDotParams shiny::shinyApp -ui -server
#'
#' @return Shiny app object of class `shiny.appobj`.
#'
#' @examples
#' if (interactive()) {
#'   preview_module()
#' }
#'
#' @importFrom shiny shinyApp fluidPage
#' @importFrom usethis proj_get
#' @importFrom utils menu
#' @importFrom cli cli_abort
#' @importFrom purrr map_chr
#' @importFrom shiny stopApp h1 p tagList strong
#' @importFrom rlang try_fetch check_installed
#'
#' @export
#' @family module_management
#'
preview_module <- function(module_id = NULL,
                           ui_imports = NULL,
                           server_imports = NULL,
                           ui_elements = sia_head_tag(),
                           save_and_document = TRUE,
                           load = TRUE,
                           proj = curr_proj(),
                           ...) {
  curr_mod <- pick_module(module_id = module_id, reason = "preview", proj = proj)

  if (save_and_document) {
    save_and_document_fun(proj = proj)
  }

  pkg_name <- get_package_name(proj = proj) # aborts if not a package

  # this differs from the mechanism implemented
  # in SIA because we cannot library() any package that is not installed
  # load_all() is needed for this to work as it attempts to register the NS as
  # usual, so we can use asNamespace
  if (load) {
    check_installed("pkgload",
      reason = "to load the package to use its functions without proper installation."
    )
    try_fetch(
      pkgload::load_all(
        path = proj, export_all = FALSE,
        helpers = FALSE, attach_testthat = FALSE
      ),
      error = function(cnd) {
        cli_abort(
          "There was an error in loading your package:",
          parent = cnd
        )
      }
    )
  }

  cur_mod_ns <- try_fetch(asNamespace(pkg_name),
    error = function(cnd) {
      cli_abort(
        c("Cannot get the package's namespace.",
          " " = "Maybe you called `{.help [preview_module](SIAtools::preview_module)}` with {.arg load = FALSE}.",
          ">" = "Turn the loading on, load the package by your own. If you face any other difficulty, set {.arg load = FALSE} and install the package normally."
        ),
        parent = cnd
      )
    }
  )

  # test for displayable content and add a hint that the module is empty, if so
  mod_ui <- do.call(
    cur_mod_ns[[curr_mod$binding$ui]],
    list(id = curr_mod$id, ui_imports)
  )

  if (!nzchar(as.character(mod_ui))) {
    mod_ui <- tagList(
      tagList(
        h1("This module seems empty"),
        p("Please edit the UI part of the module, i.e., the", strong(curr_mod$binding$ui), "function.")
      ),
      mod_ui
    )
  }

  # this is just a simple shiny app skeleton to test the module in

  ui <- fluidPage(
    ui_elements,
    mod_ui
  )

  server <- function(input, output, session) {
    # close the session as the app's window closes
    session$onSessionEnded(function(x) {
      stopApp()
    })

    # call the server function
    do.call(
      cur_mod_ns[[curr_mod$binding$server]],
      list(id = curr_mod$id, server_imports)
    )
  }

  # run the app using the parts defined above
  shinyApp(ui, server, ...)
}


#' Save all opened files and document the package in development
#'
#' @param proj *character*, a path to the project. Defaults to [current
#'   project][curr_proj()].
#'
#' @return No return value. Called for side effects.
#' @keywords internal
#'
#' @importFrom rlang try_fetch
#' @importFrom rlang check_installed
#'
save_and_document_fun <- function(proj = curr_proj()) {
  check_installed(c("rstudioapi", "roxygen2"),
    reason = "to automatically save all files and document the package."
  )

  if (is_rstudio_available() && is_rs_api_fun_available("documentSaveAll")) {
    rs_api_documentSaveAll()
  }


  try_fetch(
    roxygen2::roxygenise(package.dir = proj),
    error = function(cnd) {
      cli_abort(
        "There was an error in documenting your package:",
        parent = cnd
      )
    }
  )
}


#' Core HTML head tag from ShinyItemAnalysis
#'
#' This function is intended to be used for SIA module preview only, which is
#' done by default in [preview_module()]. It provides a HTML head tag with a
#' math rendering facility provided by the
#' \ifelse{html}{\eqn{\KaTeX}}{\emph{KaTeX}} library, and some CSS rules to
#' format tables. *If printed in the console, expect nothing to be shown.*
#'
#' @return HTML head tag of class `shiny.tag`.
#' @export
#'
#' @family helpers
#'
#' @importFrom shiny tags HTML
#'
sia_head_tag <- function() {
  tags$head(
    # KaTeX tags based on the current version available at katex.org
    # sourced from internal .rds, which is made using the script in data-raw/
    # rerun before every release of SIAtools (can use make in terminal)
    katex_head_tags,
    # rerender math in the whole document on any shiny output update, except plotly
    tags$script(
      HTML(
        "$(document).on('shiny:value', (event) => {
        setTimeout(() => {renderMathInElement(event.target, {
          ignoredClasses: [\"plotly\"]
        });}, 0);
      });"
      )
    ),
    # prevent word wrapping inside tables, add scrollbar in case of overflow
    tags$style(
      HTML(
        ".shiny-html-output:has(> .table) {
        overflow-x: auto;
        white-space: nowrap;
      }"
      )
    )
  )
}
