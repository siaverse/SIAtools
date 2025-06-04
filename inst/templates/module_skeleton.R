
# Module documentation ----------------------------------------------------

# This is the user-facing documentation.

#' Title of the module
#'
#' Here goes the description.
#'
#' @author
#' Here goes the author(s) information.
#'
#' @name {{prefixed_name}}
#' @family SIAmodules
#'
NULL


# Module definition -------------------------------------------------------

## UI part ----------------------------------------------------------------

#' `{{prefixed_name}}` module (internal documentation)
#'
#' This is the internal documentation of your module that is not included in the
#' help index of the package. For a [user-facing help page][{{prefixed_name}}],
#' please edit the documentation above.
#'
#' If your module uses any external packages, such as ggplot2,
#' **you have to declare that** with the `@importFrom` tag and include
#' the package in the DESCRIPTION. See
#' <https://r-pkgs.org/dependencies-in-practice.html> for more details.
#'
#' You can preview your module using
#' `SIAtools::preview_module("{{prefixed_name}}")`.
#'
#' See `vignette("getting_started", "SIAtools")` vignette for further details.
#'
#' @param id *character*, the ID assigned by ShinyItemAnalysis. **Do not set any
#'   default value!**
#' @param imports *list* of reactive objects exported by ShinyItemAnalysis. See
#'   `vignette("imports", "SIAtools")` for more details on how to use objects
#'   from the ShinyItemAnalysis app.
#' @param ... additional parameters (not used at the moment).
#'
#' @keywords internal
#' @rdname {{prefixed_name}}_internal
#'
#' @import shiny
#'
{{prefixed_name}}_ui <- function(id, imports = NULL, ...) {
  ns <- NS(id) # shorthand for NS(id, <inputId>)
  # Any `inputId` and `outputId` of {shiny} UI elements MUST be "wrapped" in
  # `ns()` call! Use `SIAtools::lint_ns()` to check for possible omissions.

  tagList(
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    # YOUR CODE FOR THE UI GOES HERE.
    # Please edit only the part inside the dashed lines.

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  )
}

## Server part ------------------------------------------------------------

#' @rdname {{prefixed_name}}_internal
#'
{{prefixed_name}}_server <- function(id, imports = NULL, ...) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    # YOUR CODE FOR THE SERVER LOGIC GOES HERE
    # Please edit only the part inside the dashed lines.

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  })
}
