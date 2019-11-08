
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

#' @keywords internal
.splitAttributeKey <- function(key) {
  str <- strsplit(key, ":")[[1]]
  namespace <- NULL
  name <- NULL
  loc <- NULL
  if (length(str)>2 && str[2] == "") {
    namespace <- str[1]
    name <- str[3]
    if(length(str) == 4) {
      loc <- str[4]
    }
  } else {
    name <- str[1]
    if(length(str) == 2) {
      loc <- str[2]
    }
  }
  rval <- list()
  if (!is.null(namespace)) {
    rval$namespace <- namespace
  }
  rval$name <- name
  if (!is.null(loc)) {
    rval$locale <- loc
  }
  rval
}

#' Create a new progress instance with default settings.
#' @import progress
#' @keywords internal
.newProgress <- function(format = "  :what [:bar] :percent /:elapsed", clear = getOption("opal.progress.clear", FALSE), total, width = 100) {
  progress::progress_bar$new(format = format, clear = clear, total = total, width = width)
}

#' Output the progress status if option "datashield.progress" is allows to.
#' @keywords internal
.tickProgress <- function(progress, tokens = list()) {
  if (getOption("opal.progress", TRUE)) progress$tick(tokens = tokens)
}

#' @keywords internal
.toJSONVariables <- function(table = NULL, variables, categories = NULL, pretty = FALSE) {
  varCols <- names(variables)
  vars <- variables
  # filter variables by table of interest
  if ("table" %in% varCols && !is.null(table)) {
    vars <- variables[variables$table == table,]
  }

  is.empty <- function(x) {
    is.null(x) || is.na(x)
  }

  varArray <- list()
  for (i in 1:nrow(vars)) {
    var <- vars[i,]
    varObj <- list(name=jsonlite::unbox(var$name))
    if ("valueType" %in% varCols && !is.empty(var$valueType)) {
      varObj$valueType <- jsonlite::unbox(var$valueType)
    } else {
      varObj$valueType <- jsonlite::unbox("text")
    }
    if ("entityType" %in% varCols && !is.empty(var$entityType)) {
      varObj$entityType <- jsonlite::unbox(var$entityType)
    } else {
      varObj$entityType <- jsonlite::unbox("Participant")
    }
    if ("unit" %in% varCols && !is.empty(var$unit)) {
      varObj$unit <- jsonlite::unbox(var$unit)
    }
    if ("mimeType" %in% varCols && !is.empty(var$mimeType)) {
      varObj$mimeType <- jsonlite::unbox(var$mimeType)
    }
    if ("referencedEntityType" %in% varCols && !is.empty(var$referencedEntityType)) {
      varObj$referencedEntityType <- jsonlite::unbox(var$referencedEntityType)
    }
    if ("repeatable" %in% varCols && !is.empty(var$repeatable)) {
      varObj$isRepeatable <- jsonlite::unbox(as.logical(var$repeatable))
    } else {
      varObj$isRepeatable <- jsonlite::unbox(FALSE)
    }
    if ("occurrenceGroup" %in% varCols && !is.empty(var$occurrenceGroup)) {
      varObj$occurrenceGroup <- jsonlite::unbox(var$occurrenceGroup)
    }
    if ("index" %in% varCols && !is.empty(var$index)) {
      varObj$isRepeatable <- jsonlite::unbox(as.numeric(var$index))
    }
    attrs <- list()
    j <- 1
    for (col in varCols) {
      if (!(col %in% c("table", "name", "valueType", "entityType", "unit", "mimeType", "referencedEntityType", "repeatable", "occurrenceGroup", "index"))
          && !is.empty(var[[col]])) {
        attr <- .splitAttributeKey(col)
        attr$value <- jsonlite::unbox(var[[col]])
        attrs[[j]] <- attr
        j <- j + 1
      }
    }
    varObj$attributes <- attrs

    if (!is.null(categories)) {
      catCols <- names(categories)
      cats <- categories[categories$variable == var$name,]
      # filter categories by table of interest
      if ("table" %in% catCols && !is.null(table)) {
        cats <- cats[cats$table == table,]
      }
      if (nrow(cats)>0) {
        catArray <- list()
        for (k in 1:nrow(cats)) {
          cat <- cats[k,]
          catObj <- list(name = jsonlite::unbox(as.character(cat$name)))
          if ("missing" %in% names(cats) && !is.empty(cat$missing)) {
            catObj$isMissing <- jsonlite::unbox(as.logical(cat$missing))
          } else {
            catObj$isMissing <- jsonlite::unbox(FALSE)
          }
          attrs <- list()
          j <- 1
          for (col in catCols) {
            if (!(col %in% c("table", "variable", "name", "missing")) && !is.empty(cat[[col]])) {
              attr <- .splitAttributeKey(col)
              attr$value <- jsonlite::unbox(cat[[col]])
              attrs[[j]] <- attr
              j <- j + 1
            }
          }
          catObj$attributes <- attrs

          catArray[[k]] <- catObj
        }
        varObj$categories <- catArray
      }
    }

    varArray[[i]] <- varObj
  }
  jsonlite::toJSON(varArray, pretty = pretty, auto_unbox = TRUE)
}
