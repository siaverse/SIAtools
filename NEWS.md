# SIAtools 0.1.4

## Minor changes

- `{SIAtools}` does no longer reexport `%>%` operator from magrittr package. The native pipe operator `|>` in used instead, internally.

# SIAtools 0.1.3

## Bugfixes

- `create_module_project()` now correctly sets working directory to the newly
created module project directory even outside of RStudio if `open = TRUE`


# SIAtools 0.1.2

## Minor changes

- `preview_module()` now injects an informative message into the UI if no content was provided,
so you immediately know that you are looking at a blank module and nothing is going to happen.
- Some of the error messages were made clearer.

## Bugfixes

- Nonexistent vignette removed from the welcome message displayed upon SIA module project creation.


# SIAtools 0.1.1

## Bugfixes

- `preview_module()` does not crash anymore when run outside of RStudio

# SIAtools 0.1.0

- initial release
