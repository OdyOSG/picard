read_utf8 <- function(path, n = -1L) {
  base::readLines(path, n = n, encoding = "UTF-8", warn = FALSE)
}

write_utf8 <- function(path, lines, append = FALSE, line_ending = NULL) {
  check_name(path)
  check_character(lines)

  file_mode <- if (append) "ab" else "wb"
  con <- file(path, open = file_mode, encoding = "utf-8")
  withr::defer(close(con))

  if (is.null(line_ending)) {
    if (is_in_proj(path)) {              # path is in active project
      line_ending <- proj_line_ending()
    } else if (possibly_in_proj(path)) { # path is some other project
      line_ending <-
        with_project(proj_find(path), proj_line_ending(), quiet = TRUE)
    } else {
      line_ending <- platform_line_ending()
    }
  }

  # convert embedded newlines
  lines <- gsub("\r?\n", line_ending, lines)
  base::writeLines(enc2utf8(lines), con, sep = line_ending, useBytes = TRUE)

  invisible(TRUE)
}
