test_that("produces a shiny.appobj", {
  pkg <- local_create_project(type = "ordinary_package")
  add_module("test")

  local_mocked_bindings(
    is_rstudio_available = function(...) TRUE,
    is_rs_api_fun_available = function(...) TRUE,
    rs_api_documentSaveAll = function(...) NULL
  )

  expect_s3_class(preview_module(), "shiny.appobj")

  # unload the test package for further test independence
  pkgload::unload(get_package_name(pkg))
})

test_that("produces a shiny.appobj with manual roxygenization", {
  pkg <- local_create_project(type = "ordinary_package")
  add_module("test")

  local_mocked_bindings(
    is_rstudio_available = function(...) TRUE,
    is_rs_api_fun_available = function(...) TRUE,
    rs_api_documentSaveAll = function(...) NULL
  )

  roxygen2::roxygenise()

  expect_s3_class(preview_module(save_and_document = FALSE), "shiny.appobj")
  pkgload::unload(get_package_name(pkg))
})

test_that("fails without load", {
  pkg <- local_create_project(type = "ordinary_package")
  add_module("test")

  local_mocked_bindings(
    is_rstudio_available = function(...) TRUE,
    is_rs_api_fun_available = function(...) TRUE,
    rs_api_documentSaveAll = function(...) NULL
  )

  expect_error(
    preview_module(save_and_document = FALSE, load = FALSE),
    regexp = "Cannot get the package's namespace")
})
