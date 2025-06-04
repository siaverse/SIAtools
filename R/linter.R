#' Require usage of `ns()` in `inputId` and `outputId` arguments of UI functions
#' in `{shiny}` modules
#'
#' A custom *linter* to be used by `{lintr}` package. Checks the functions in a
#' module's UI part for any missing `ns()` calls. These are often omitted when
#' working with the plain `{shiny}` or SIA modules. More details follows below.
#'
#' ## How to use this linter
#'
#' The easiest way is to call [lint_ns()] which is essentially a wrapper around:
#'
#' ```
#' lintr::lint_package(linters = module_namespace_linter())
#' ```
#'
#' Both calls use our linter for the whole package. However, note that *only*
#' `module_namespace_linter` is considered. Using this custom linter with the
#' native ones is somewhat complicated, but not impossible. To the best of our
#' knowledge, the only place where the `{lintr}` documentation mentions the
#' actual usage of external linters, is in
#' [linters_with_tags()][lintr::linters_with_tags] help page. According to that,
#' you can pass the following call to `linters` argument in any supported
#' `lintr::lint_*` function:
#'
#' ```
#' lintr::linters_with_tags(
#'  tags = NULL, packages = c("lintr", "SIAtools")
#' )
#' ```
#'
#' That should select all linters available in both packages.
#'
#' It is also possible to set up a configuration file that enables you to
#' shorten calls to `{lintr}` functions, use RStudio Addins to lint an active
#' file, or even apply linters during continuous integration workflows, e.g., in
#' GitHub Actions or in Travis. To opt for that, create `.lintr` file at your
#' package's root and fill in the following line:
#'
#' ```
#' linters: linters_with_tags(tags = NULL, packages = "SIAtools")
#' ```
#'
#' Then, you can use the provided addins or call `lintr::lint_package()` to get
#' your modules checked.
#'
#' ## What the linter does
#'
#' By default, the linter looks for any `inputId` or `outputId` arguments of
#' `{shiny}`'s UI functions (such as [numericInput][shiny::numericInput] or
#' [plotOutput][shiny::plotOutput], respectively), and tests if the values
#' assigned to the arguments are all "namespaced", i.e., wrapped in `ns()`
#' function. This is crucial for inputs and outputs in the UI portion of a
#' module to match their counterparts in the server logic chunk.
#'
#' Only `{shiny}` UI calls that are inside of a [tagList][shiny::tagList] in a
#' function ("lambda" shorthand, `\( )`, applies as well) are inspected. This is
#' because we don't want to cause false alarms for any "ordinary" `{shiny}` apps
#' that aren't modules. All UI portions of modules are usually defined as
#' functions, and all input/output UI functions are inside a
#' [tagList][shiny::tagList], so we opted for the this strategy to minimalize
#' false positive matches outside `{shiny}` modules.
#'
#' We look for any `inputId` or `outputId` arguments that are named as such. On
#' top of that, the `ns()` omission is detected even if you call the function
#' without named arguments that would be evaluated as input or output IDs.
#' However, if you use partial matching (`numericInput(inp = "input")`), the
#' actual input won't get linted, even though it should, as it is eventually
#' evaluated as `inputId`. The same applies for arguments defined outside the
#' call and passed as a variable, e.g., `inp <- "input"; numericInput(inputId =
#' inp)`. That is tricky to catch in a static code analysis, which is employed
#' in this linter.
#'
#'
#' @param io_funs *character*, `{shiny}` input/output UI functions to check.
#'   Defaults to [default_shiny_io_functions], which covers all native ones and
#'   several others from `{plotly}` or `{DT}`. The functions must include the
#'   namespace, i.e., `shiny::textInput`.
#' @param io_args *character*, arguments of UI functions to check. `inputId` and
#'   `outputId` by default. These are checked even if unnamed. Named arguments
#'   that partially match are ignored and discouraged.
#' @param ns_funs *character*, function names that are considered valid in order
#'   to "namespace" inputs' or outputs' IDs. Defaults to both `ns` and `NS`
#'   (although we recommend to stick with the former, which is predefined in the
#'   module template).
#'
#' @return A `linter` closure. To be used by `{lintr}` only. See the first
#'   example below.
#'
#' @seealso [linters][lintr::linters] for a complete list of linters available
#'   in lintr.
#'
#' @family linters
#'
#' @export
#'
#' @importFrom rlang check_installed arg_match
#' @importFrom cli cli_abort
#'
#' @examplesIf requireNamespace("lintr", quietly = TRUE)
#' # will produce lints
#' lintr::lint(
#'   text =
#'     "module_ui <- function(id, imports, ...) {
#'       tagList(
#'         numericInput(inputId = \"input_id_without_ns\", ...)
#'       )
#'     }",
#'   linter = module_namespace_linter()
#' )
#'
#' # is OK
#' lintr::lint(
#'   text =
#'     "module_ui <- function(id, imports, ...) {
#'       tagList(
#'         numericInput(inputId = ns(\"input_id_with_ns\"), ...)
#'       )
#'     }",
#'   linter = module_namespace_linter()
#' )
#'
module_namespace_linter <- function(io_funs = default_shiny_io_functions,
                                    io_args = c("inputId", "outputId"),
                                    ns_funs = c("ns", "NS")) {
  check_installed(c("lintr", "xml2", "glue"),
    reason = "to use namespace linter for `{shiny}` modules."
  )

  if (length(io_funs) == 0L) {
    cli_abort("No functions provided in {.arg io_funs} to check for.")
  }
  if (length(io_args) == 0L) {
    cli_abort("No arguments provided in {.arg io_args} to check for.")
  }
  if (length(ns_funs) == 0L) {
    cli_abort("No functions provided in {.arg ns_funs} for linter to expect as correct.")
  }

  funs_without_namespace <- io_funs[!grepl("::", io_funs)]

  if (length(funs_without_namespace) != 0L) {
    cli_abort("{funs_without_namespace} function{?s} are provided without namespace.")
  }

  io_funs <- sub(".*::", "", io_funs)

  io_funs <- xp_text_or_chain(io_funs)
  io_args <- xp_text_or_chain(io_args)
  ns_funs <- xp_text_or_chain(ns_funs)


  # match tagList in a function, search in the descendants of its parent (i.e.,
  # contents of tagList)
  in_module <-
    "(//FUNCTION | //OP-LAMBDA)
         /parent::expr
         /descendant::SYMBOL_FUNCTION_CALL[text() = 'tagList']
         /parent::expr
         /parent::expr"


  xpath_named <-
    glue::glue(
      "{in_module}
         /descendant::SYMBOL_FUNCTION_CALL[ {io_funs} ]
         /parent::expr
         /following-sibling::expr[
             preceding-sibling::*[2][self::SYMBOL_SUB[ {io_args} ]]
               and
             preceding-sibling::*[1][self::EQ_SUB]
               and
             not(
               self::expr[1][expr[1][SYMBOL_FUNCTION_CALL[ {ns_funs} ]]]
             )
           ]"
    )

  # look only for io_funs that doesn't have any named io_args then the inputId is
  # the first unnamed arg and we check for it ignore io_funs that already have
  # ns() as the first unnamed argument so we don't get false positives in the
  # following args
  xpath_unnamed <-
    glue::glue(
      "{in_module}
         /descendant::SYMBOL_FUNCTION_CALL[
           ( {io_funs} )
              and
            not(parent::expr/following-sibling::SYMBOL_SUB[ {io_args} ])
              and
           not(parent::expr/following-sibling::expr[1][expr[1][SYMBOL_FUNCTION_CALL[ {ns_funs} ]]])
         ]
         /parent::expr
         /following-sibling::expr[2][
           not(
             preceding-sibling::*[1][self::EQ_SUB]
           )
             and
           not(
             self::expr[1][expr[1][SYMBOL_FUNCTION_CALL[ {ns_funs} ]]]
           )
         ][1]"
    )

  # this is only for the one case that is missed by previous xpath - unnamed first arg
  # (because the xpath above ignores any io_fun that has ns() as first arg)
  xpath_unnamed_first <-
    glue::glue(
      "{in_module}
         /descendant::SYMBOL_FUNCTION_CALL[
           ( {io_funs} )
              and
            not(parent::expr/following-sibling::SYMBOL_SUB[ {io_args} ])
         ]
         /parent::expr
         /following-sibling::expr[1][
           not(
             preceding-sibling::*[1][self::EQ_SUB]
           )
             and
           not(
             self::expr[1][expr[1][SYMBOL_FUNCTION_CALL[ {ns_funs} ]]]
           )
         ][1]"
    )

  xpath <- paste(xpath_named, xpath_unnamed, xpath_unnamed_first, sep = "\n|\n")

  lintr::Linter(function(source_expression) {
    if (!lintr::is_lint_level(source_expression, "file")) {
      return(list())
    }

    xml <- source_expression$full_xml_parsed_content
    bad_args <- xml2::xml_find_all(xml, xpath)

    lintr::xml_nodes_to_lints(
      bad_args,
      source_expression = source_expression,
      lint_message = "`inputId` and `outputId` arguments must be wrapped in `ns()` inside shiny modules.",
      type = "warning"
    )
  })
}


xp_text_or_chain <- function(text) {
  paste0("text() = ", paste0("'", text, "'"), collapse = " or ")
}


#' Default `{shiny}` input/output UI functions consulted by
#' `module_namespace_linter()`
#'
#' A character vector of function names whose calls are inspected for `ns()`
#' omission. Name of each element denotes the original package. Please refer to
#' [module_namespace_linter()] for more details.
#'
#' Following functions are covered:
#' `r format_for_docs(default_shiny_io_functions)`
#'
#' @export
#'
#' @family helpers
#'
default_shiny_io_functions <- c(
  "shiny::actionButton",
  "shiny::actionLink",
  "shiny::checkboxGroupInput",
  "shiny::checkboxInput",
  "shiny::dateInput",
  "shiny::dateRangeInput",
  "shiny::fileInput",
  "shiny::numericInput",
  "shiny::passwordInput",
  "shiny::radioButtons",
  "shiny::selectInput",
  "shiny::sliderInput",
  "shiny::textAreaInput",
  "shiny::textInput",
  "shiny::varSelectInput",
  "shiny::dataTableOutput",
  "shiny::tableOutput",
  "shiny::uiOutput",
  "shiny::htmlOutput",
  "shiny::verbatimTextOutput",
  "shiny::imageOutput",
  "shiny::textOutput",
  "shiny::plotOutput",
  "plotly::plotlyOutput",
  "DT::DTOutput"
)

format_for_docs <- function(shiny_funs) {
  paste0("- `", shiny_funs, "`", collapse = "\n")
}

#' Check the `{shiny}` module UI functions for `ns()` omission
#'
#' This is a simple wrapper of [lintr::lint_package()] call using only the
#' [module_namespace_linter()] from `{SIAtools}`. See the
#' [linter documentation][module_namespace_linter()] for more details.
#'
#' @param path *character*, path to the package root directory. Default is the
#'   current project’s directory.
#'
#' @inheritDotParams lintr::lint_package -path
#'
#' @return An object of class c("lints", "list"), each element of which is a "list" object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' lint_ns()
#' }
#'
#' @family linters
#'
#' @importFrom rlang check_installed
#'
lint_ns <- function(path = curr_proj(), ...) {
  check_installed("lintr", reason = "to use namespace linter for `{shiny}` modules.")

  lintr::lint_package(path = path, linters = module_namespace_linter(), ...)
}
