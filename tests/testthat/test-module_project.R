test_that("project has DESCRIPTION", {
  pkg <- local_create_project(type = "module")

  expect_true(fs::file_exists("DESCRIPTION"))
})


test_that("project has NAMESPACE", {
  pkg <- local_create_project(type = "module")

  expect_true(fs::file_exists("NAMESPACE"))
})


test_that("project has package documentation", {
  pkg <- local_create_project(type = "module")

  pkg_name <- fs::path_file(pkg) # get the last portion of the path
  package_docs <- fs::path(pkg, "R", paste0(pkg_name, "-package.R"))

  expect_true(fs::file_exists(package_docs))
})


test_that("project declares SIA modules field in DESCRIPTION", {
  pkg <- local_create_project(type = "module")


  expect_true(is_siamodule_package(pkg))
})

test_that("project has .Rprofile copied", {
  pkg <- local_create_project(type = "module")

  expect_true(fs::file_exists(".Rprofile"))
})


# no RStudio --------------------------------------------------------------

test_that("project is activated outside of RStudio", {
  local_create_project(type = "module")

  local_mocked_bindings(
    is_rstudio_available = function(...) FALSE,
    is_rs_api_fun_available = function(...) FALSE
  )

  expect_s3_class(curr_proj(), c("fs_path", "character"))
})
