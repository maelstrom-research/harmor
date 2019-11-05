#' Apply the dictionary to a tibble
#'
#' Apply the dictionary described in a Opal Excel format as attributes of the tibble's columns.
#'
#' @param tibble Tibble to be decorated.
#' @param variables A data frame with one row per variable (column name) and then one column per property/attribute.
#' @param categories A data frame with one row per category (columns variable and name) and then column per property/attribute.
#'
#' @keywords internal
applyDictionary <- function(tibble, variables, categories = NULL) {
  names <- names(tibble)
  applyAttribute <- function(attrs, name, value) {
    rval <- attrs
    if (is.null(rval)) {
      rval <- list()
      rval[[name]] <- value
    } else {
      rval[[name]] <- value
    }
    rval
  }
  localizedValue <- function(name, value) {
    nameWithLocale <- strsplit(name, ":")[[1]]
    val <- value
    if (length(nameWithLocale)>1) {
      val <- paste0("(", nameWithLocale[2], ") ", val)
    }
    val
  }
  # go through variable descriptions
  for (i in 1:length(variables)) {
    var <- variables[i,]
    # do we have a variable and a column with same name
    if (var$name %in% names) {
      # make column attributes from variable description
      for (n in names(var)) {
        attrs <- attributes(tibble[[var$name]])
        if (startsWith(n, "label")) {
          attrs <- applyAttribute(attrs, "label", localizedValue(n, var[[n]]))
        } else if (startsWith(n, "description")) {
          attrs <- applyAttribute(attrs, "description", localizedValue(n, var[[n]]))
        } else if (n != "name") {
          attrs <- applyAttribute(attrs, n, var[[n]])
        }
        attributes(tibble[[var$name]]) <- attrs
      }
      # look for categories
      if (!is.null(categories)) {
        varcats <- categories[categories$variable == var$name,]
        if (nrow(varcats)>0) {
          labels <- varcats$name
          for (n in names(var)) {
            if (startsWith(n, "label")) {
              names(labels) <- localizedValue(n, varcats[[n]])
            }
          }
          attributes(tibble[[var$name]])$labels <- labels
          #clazz <- class(tibble[[var$name]])
          #if (is.null(clazz)) {
          #  clazz <- "haven_labelled"
          #} else {
          #  clazz <- append(clazz, "haven_labelled")
          #}
          class(tibble[[var$name]]) <- "haven_labelled"
        }
      }
    }
  }
  tibble
}
