test_that("no lints on no code", {
  lintr::expect_lint(" ", NULL, module_namespace_linter())
})

test_that("linter fails if io functions does not have namespace", {
  expect_error(
    lintr::expect_lint(
      " ", NULL,
      module_namespace_linter(io_funs = c("shiny:textInput", "numericInput"))
    ),
    regexp = "are provided without namespace"
  )
})

test_that("linter fails if args are empty", {
  expect_error(
    lintr::expect_lint(
      " ", NULL,
      module_namespace_linter(io_funs = NULL)
    ),
    regexp = "No functions provided"
  )
  expect_error(
    lintr::expect_lint(
      " ", NULL,
      module_namespace_linter(io_args = NULL)
    ),
    regexp = "No arguments provided"
  )
  expect_error(
    lintr::expect_lint(
      " ", NULL,
      module_namespace_linter(ns_funs = NULL)
    ),
    regexp = "for linter to expect as correct."
  )
})

test_that("no lints outside shiny modules", {
  lintr::expect_lint(
    "ui <- fluidPage(
      numericInput(\"input\", \"label\", 1)
     )",
    NULL,
    module_namespace_linter()
  )

  lintr::expect_lint(
    "ui <- fluidPage(
      tagList(
        numericInput(\"input\", \"label\", 1)
      )
     )",
    NULL,
    module_namespace_linter()
  )

  lintr::expect_lint(
    "numericInput(\"input\", \"label\", 1)",
    NULL,
    module_namespace_linter()
  )
})

test_that("no lints in correct shiny module code", {
  lintr::expect_lint(
    "module_ui <- function(id, imports, ...) {
      tagList(
        numericInput(ns(\"input\"), \"label\", 1)
      )
     }",
    NULL,
    module_namespace_linter()
  )

  lintr::expect_lint(
    "module_ui <- function(id, imports, ...) {
      tagList(
        numericInput(inputId = ns(\"input\"), \"label\", 1)
      )
     }",
    NULL,
    module_namespace_linter()
  )

  lintr::expect_lint(
    "module_ui <- function(id, imports, ...) {
      tagList(
        numericInput(inputId = ns(\"input\"), label = \"label\", 1)
      )
     }",
    NULL,
    module_namespace_linter()
  )

  lintr::expect_lint(
    "module_ui <- function(id, imports, ...) {
      tagList(
        numericInput(\"label\", 1, inputId = ns(\"input\"))
      )
     }",
    NULL,
    module_namespace_linter()
  )

  lintr::expect_lint(
    "module_ui <- function(id, imports, ...) {
      tagList(
        numericInput(label = \"label\", ns(\"input\"), \"random_arg\",  value = 1)
      )
     }",
    NULL,
    module_namespace_linter()
  )

  lintr::expect_lint(
    "module_ui <- function(id, imports, ...) {
      tagList(
        numericInput(label = \"label\", value = 1, ns(\"input\"), \"random_arg\", ...)
      )
     }",
    NULL,
    module_namespace_linter()
  )
})


test_that("wrong unnamed inputId at second position is linted", {
  lintr::expect_lint(
    "module_ui <- function(id, imports, ...) {
      tagList(
        numericInput(label = \"label\", \"random_arg\", value = 1)
      )
     }",
    "arguments must be wrapped",
    module_namespace_linter()
  )

  lintr::expect_lint(
    "module_ui <- function(id, imports, ...) {
      tagList(
        numericInput(label = \"label\", \"random_arg\", ns(\"input\"), value = 1)
      )
     }",
    "arguments must be wrapped",
    module_namespace_linter()
  )
})

test_that("wrong named inputId at any position is linted", {
  lintr::expect_lint(
    "module_ui <- function(id, imports, ...) {
      tagList(
        numericInput(label = \"label\", inputId = \"random_arg\", value = 1)
      )
     }",
    "arguments must be wrapped",
    module_namespace_linter()
  )

  lintr::expect_lint(
    "module_ui <- function(id, imports, ...) {
      tagList(
        numericInput(label = \"label\", value = 1,  inputId = \"random_arg\")
      )
     }",
    "arguments must be wrapped",
    module_namespace_linter()
  )

  lintr::expect_lint(
    "module_ui <- function(id, imports, ...) {
      tagList(
        numericInput( inputId = \"random_arg\", label = \"label\", value = 1)
      )
     }",
    "arguments must be wrapped",
    module_namespace_linter()
  )

  # for plotOutput
  lintr::expect_lint(
    "module_ui <- function(id, imports, ...) {
      tagList(
        plotOutput(label = \"label\", outputId = \"random_arg\", value = 1)
      )
     }",
    "arguments must be wrapped",
    module_namespace_linter()
  )

  lintr::expect_lint(
    "module_ui <- function(id, imports, ...) {
      tagList(
        plotOutput(label = \"label\", value = 1,  outputId = \"random_arg\")
      )
     }",
    "arguments must be wrapped",
    module_namespace_linter()
  )

  lintr::expect_lint(
    "module_ui <- function(id, imports, ...) {
      tagList(
        plotOutput( outputId = \"random_arg\", label = \"label\", value = 1)
      )
     }",
    "arguments must be wrapped",
    module_namespace_linter()
  )
})


test_that("wrong ns function gets linted", {
  lintr::expect_lint(
    "module_ui <- function(id, imports, ...) {
      tagList(
        plotOutput( outputId = nss(\"random_arg\"), label = \"label\", value = 1)
      )
     }",
    "arguments must be wrapped",
    module_namespace_linter()
  )

  lintr::expect_lint(
    "module_ui <- function(id, imports, ...) {
      tagList(
        plotOutput( wrong_fun(\"random_arg\"), label = \"label\", value = 1)
      )
     }",
    "arguments must be wrapped",
    module_namespace_linter()
  )
  lintr::expect_lint(
    "module_ui <- function(id, imports, ...) {
      tagList(
        plotOutput(label = \"label\", wrong_fun(\"random_arg\"), value = 1)
      )
     }",
    "arguments must be wrapped",
    module_namespace_linter()
  )
})

test_that("NS function does'n get linted", {
  lintr::expect_lint(
    "module_ui <- function(id, imports, ...) {
      tagList(
        plotOutput( outputId = NS(\"random_arg\"), label = \"label\", value = 1)
      )
     }",
    NULL,
    module_namespace_linter()
  )
})
