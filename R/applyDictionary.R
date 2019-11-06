#' Apply the dictionary to a tibble
#'
#' Apply the dictionary described in a Opal Excel format as attributes of the tibble's columns.
#'
#' @param tibble Tibble to be decorated.
#' @param variables A data frame with one row per variable (column name) and then one column per property/attribute.
#' @param categories A data frame with one row per category (columns variable and name) and then column per property/attribute.
#'
#' @export
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
        } else if (n == "unit") {
          attrs <- applyAttribute(attrs, "opal.unit", var[[n]])
        } else if (n == "referencedEntityType") {
          attrs <- applyAttribute(attrs, "opal.referenced_entity_type", var[[n]])
        } else if (n == "mimeType") {
          attrs <- applyAttribute(attrs, "opal.mime_type", var[[n]])
        } else if (n == "occurrenceGroupe") {
          attrs <- applyAttribute(attrs, "opal.occurrence_group", var[[n]])
        } else if (n == "repeatable") {
          attrs <- applyAttribute(attrs, "opal.repeatable", var[[n]])
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
          missings <- list()
          for (n in names(varcats)) {
            if (startsWith(n, "label")) { # note: multilang labels not supported
              if (is.null(names(labels))) {
                names(labels) <- localizedValue(n, varcats[[n]])
              } else {
                warning("Multilang labels are not supported")
              }
            } else if (n == "missing") {
              missings <- as.logical(varcats[[n]])
            }
          }
          attributes(tibble[[var$name]])$labels <- labels
          if (any(missings)) {
            attributes(tibble[[var$name]])$na_values <- labels[missings]
          }
          clazz <- class(tibble[[var$name]])
          if (is.null(clazz)) {
            clazz <- "haven_labelled"
          } else {
            clazz <- append(clazz, "haven_labelled")
          }
          class(tibble[[var$name]]) <- clazz #"haven_labelled"
        }
      }
    }
  }
  tibble
}
