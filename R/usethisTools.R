# These functions come directly from usethis

# template -----------------
find_template <- function(template_name, package = "picard") {
  rlang::check_installed(package)
  path <- tryCatch(
    fs::path_package(
      package = package, "templates", template_name
    ),
    error = function(e) ""
  )
  if (identical(path, "")) {
    usethis::ui_stop("Could not find template {ui_value(template_name)} \\\n      in package {ui_value(package)}.")
  }
  path
}

render_template <- function(template, data = list(), package = "picard") {
  template_path <- find_template(template, package = package)
  strsplit(whisker::whisker.render(read_utf8(template_path),
                                   data), "\n")[[1]]
}
#
# # Read/write -----------------------
read_utf8 <- function(path, n = -1L) {
  base::readLines(path, n = n, encoding = "UTF-8", warn = FALSE)
}

write_utf8 <- function(path, lines, append = FALSE, line_ending = NULL) {

  file_mode <- if (append) "ab" else "wb"
  con <- file(path, open = file_mode, encoding = "utf-8")
  withr::defer(close(con))

  line_ending <- platform_line_ending()

  # convert embedded newlines
  lines <- gsub("\r?\n", line_ending, lines)
  base::writeLines(enc2utf8(lines), con, sep = line_ending, useBytes = TRUE)

  invisible(TRUE)
}
#
# lines ----------------
platform_line_ending <- function() {
  if (.Platform$OS.type == "windows")
    "\r\n"
  else "\n"
}
#
#  # Checkers ----------------
# .rlang_check_is_string <- function(x,
#                                    allow_empty,
#                                    allow_na,
#                                    allow_null) {
#   if (rlang::is_string(x)) {
#     if (allow_empty || !is_string(x, "")) {
#       return(TRUE)
#     }
#   }
#
#   if (allow_null && is_null(x)) {
#     return(TRUE)
#   }
#
#   if (allow_na && (identical(x, NA) || identical(x, na_chr))) {
#     return(TRUE)
#   }
#
#   FALSE
# }
#
# check_name <- function(x,
#                        ...,
#                        allow_null = FALSE,
#                        arg = caller_arg(x),
#                        call = caller_env()) {
#   if (!missing(x)) {
#     is_string <- .rlang_check_is_string(
#       x,
#       allow_empty = FALSE,
#       allow_na = FALSE,
#       allow_null = allow_null
#     )
#     if (is_string) {
#       return(invisible(NULL))
#     }
#   }
#
#   stop_input_type(
#     x,
#     "a valid name",
#     ...,
#     allow_na = FALSE,
#     allow_null = allow_null,
#     arg = arg,
#     call = call
#   )
# }
#
#
#
# check_character <- function(x,
#                             ...,
#                             allow_null = FALSE,
#                             arg = caller_arg(x),
#                             call = caller_env()) {
#   if (!missing(x)) {
#     if (is_character(x)) {
#       return(invisible(NULL))
#     }
#     if (allow_null && is_null(x)) {
#       return(invisible(NULL))
#     }
#   }
#
#   stop_input_type(
#     x,
#     "a character vector",
#     ...,
#     allow_na = FALSE,
#     allow_null = allow_null,
#     arg = arg,
#     call = call
#   )
# }
