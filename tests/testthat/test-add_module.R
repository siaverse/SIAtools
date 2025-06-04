test_that("adding module to non-package project without DESCRIPTION fails", {
  pkg <- local_create_project(type = "project")

  expect_error(
    add_module(name = "test"),
    regexp = "There is no 'DESCRIPTION' file in"
  )
})

test_that("adding module to non-package project with invalid DESCRIPTION fails", {
  pkg <- local_create_project(type = "project")

  fs::file_create("DESCRIPTION")

  expect_error(add_module(name = "test"),
    regexp = "There was an error in reading the package name."
  )
})

test_that("adding module to ordinary package fixes the DESCRIPTION", {
  pkg <- local_create_project(type = "ordinary_package")

  expect_false(is_siamodule_package(pkg))

  expect_message(
    add_module(name = "test"),
    regexp = paste0("Adding ", names(description_field))
  )

  expect_true(is_siamodule_package(pkg))
})

test_that("adding module to module package is silent about DESCRIPTION", {
  pkg <- local_create_project(type = "module")

  expect_snapshot(add_module(name = "test"))
})

test_that("adding module in ordinary package project creates the manifest", {
  pkg <- local_create_project(type = "ordinary_package")

  add_module(name = "test")
  yaml_path <- get_yaml_path(pkg)

  expect_true(fs::file_exists(yaml_path))
})

test_that("adding module produces its .R file", {
  pkg <- local_create_project(type = "ordinary_package")

  add_module("test")

  expect_true(fs::file_exists("R/sm_test.R"))
})


test_that("adding module produces file with the expected content", {
  pkg <- local_create_project(type = "ordinary_package")

  add_module("test")

  expect_snapshot_file("R/sm_test.R", cran = TRUE)
})


test_that("subsequent adding of modules in ordinary package produces expected messages", {
  pkg <- local_create_project(type = "ordinary_package")

  expect_snapshot(add_module(name = "test1"))
  expect_snapshot(add_module(name = "test2"))
})

test_that("subsequent adding of modules in module package produces expected messages", {
  pkg <- local_create_project(type = "module")

  expect_snapshot(add_module(name = "test1"))
  expect_snapshot(add_module(name = "test2"))
})


test_that("adding module with already used name fails", {
  pkg <- local_create_project(type = "module")

  add_module(name = "test")

  expect_error(add_module(name = "test"),
    regexp = "Module \"sm_test\" is already present"
  )
})


test_that("adding a pre-specified module is silent about title and category editing", {
  pkg <- local_create_project(type = "module")

  expect_snapshot(add_module(name = "category_given", category = "Modules"))
  expect_snapshot(add_module(name = "title_given", title = "Test module"))
  expect_snapshot(add_module(name = "both_given", title = "Test module", category = "Modules"))

})

test_that("adding a pre-specified module with wrong category fails", {
  pkg <- local_create_project(type = "module")

  expect_error(add_module(name = "test", category = "Wrong category"))

})
