
#' @keywords internal
.norm2nastr <- function(value) {
  if (.is.empty(value)) {
    "N/A"
  } else {
    as.character(value)
  }
}

#' @keywords internal
.is.empty <- function(value) {
  if (length(value) == 0 || is.null(value) || is.na(value)) {
    TRUE
  } else {
    str <- as.character(value)
    if (nchar(str) == 0) {
      TRUE
    } else {
      FALSE
    }
  }
}

#' @keywords internal
.localized2str <- function(item, locale) {
  for (msg in item) {
    if (msg$locale == locale) {
      return(msg$text)
    }
  }
  return("")
}
