
#' @keywords internal
.na2str <- function(value) {
  if (is.na(value)) {
    "N/A"
  } else {
    as.character(value)
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
