test_that("NULL is returned in case of zero modules", {
  pkg <- local_create_project(type = "ordinary_package")

  get_modules()|>
    expect_null()|>
    expect_invisible()

  get_modules()|>
    expect_message(
      "This package doesn't declare that it contains any SIA modules"
    )|>
    expect_message(
      "Trying to obtain and read SIA Modules Manifest nevertheless"
    )|>
    expect_message("There is no SIA Modules Manifest")
})

test_that("manifest is of class sm_manifest and list", {
  pkg <- local_create_project(type = "ordinary_package")

  add_module("test")
  mods <- get_modules()

  expect_s3_class(mods, "sm_manifest")
  expect_s3_class(mods, "list")
})

test_that("manifest is of correct structure and with expected values", {
  pkg <- local_create_project(type = "ordinary_package")

  add_module("test1", title = "Test 1", category = "Validity")
  add_module("test2", title = "Test 2", category = "Reliability")
  mods <- get_modules()

  expect_named(mods)

  expect_true(mods$sm_test1$title == "Test 1")
  expect_true(mods$sm_test2$title == "Test 2")

  expect_true(mods$sm_test1$category == "Validity")
  expect_true(mods$sm_test2$category == "Reliability")

  expect_true(mods$sm_test1$binding$ui == "sm_test1_ui")
  expect_true(mods$sm_test2$binding$ui == "sm_test2_ui")

  expect_true(mods$sm_test1$binding$server == "sm_test1_server")
  expect_true(mods$sm_test2$binding$server == "sm_test2_server")
})


test_that("edgecases are handled with correct messages", {
  pkg <- local_create_project(type = "ordinary_package")
  yaml_path <- get_yaml_path(pkg)

  # create blank manifest
  fs::dir_create(fs::path_dir(yaml_path))
  fs::file_create(yaml_path)

  expect_message(get_modules(), regexp = "is blank")

  writeLines("random string", yaml_path)
  expect_message(get_modules(), regexp = "is corrupt")

  # in all cases, should return NULL invisibly
  get_modules()|>
    expect_null()|>
    expect_invisible()
})


test_that("manifest prints out correctly", {
  pkg <- local_create_project(type = "ordinary_package")
  add_module("test")

  expect_snapshot(get_modules())

  # tibble format
  get_modules()|>
    print(as_tibble = TRUE)|>
    expect_snapshot_value(style = "deparse")
})
