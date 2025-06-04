local_create_project <- function(type, dir = fs::file_temp(), env = parent.frame()) {
  withr::local_options(
    list(usethis.quiet = TRUE)
  )

  orig_project <- usethis:::proj_get_()
  orig_wd <- getwd()

  withr::defer(
    {
      cli::cli_alert_success(
        "Deleting temporary project at {.path {dir}}..."
      )
      fs::dir_delete(dir)
    },
    envir = env
  )


  switch(type,
    "module" = create_module_project(dir),
    "project" = usethis::create_project(dir),
    "ordinary_package" = usethis::create_package(dir)
  )


  withr::defer(usethis::proj_set(orig_project, force = TRUE), envir = env)
  usethis::proj_set(dir)

  withr::defer(
    {
      cli::cli_alert_success(
        "Restoring original working directory to {.path {orig_wd}}..."
      )
      setwd(orig_wd)
    },
    envir = env
  )

  setwd(usethis::proj_get())
  invisible(usethis::proj_get())
}
