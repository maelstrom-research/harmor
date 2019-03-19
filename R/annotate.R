#' Set variable annotation with a taxonomy term
#'
#' Apply or remove an annotation from a set of variables.
#'
#' @param tibble Tibble to be annotated
#' @param variables A character vector of variable names to be annotated. If NULL or empty, all
#' the columns of the tibble will be annotated.
#' @param taxonomy The taxonomy to which the vocabulary belongs. If NULL, the annotation is a simple
#' attribute (i.e. without a taxonomy reference).
#' @param vocabulary The vocabulary to which the term belongs.
#' @param term The term to apply. If NULL, the annotation will be deleted.
#' @return The annotated tibble
#'
#' @export
annotate <- function(tibble, variables = NULL, taxonomy = "Mlstr_area", vocabulary, term) {
  if (!("tbl" %in% class(tibble))) {
    stop("The tibble parameter must be a tibble.")
  }
  attrstr <- paste0(taxonomy, "::", vocabulary)
  if (is.null(taxonomy)) {
    attrstr <- vocabulary
  }
  vars <- variables
  if (is.null(vars) || length(vars) == 0) {
    vars <- colnames(tibble)
  }
  for (var in vars) {
    if (var %in% colnames(tibble)) {
      if (is.null(term)) {
        message("Removing ", attrstr, " from ", var, " ...")
      } else {
        message("Applying ", attrstr, "=", term, " to ", var, " ...")
      }

      if (is.null(attributes(tibble[[var]]))) {
        attr <- list()
        attr[[attrstr]] <- term
        attributes(tibble[[var]]) <- attr
      } else {
        attributes(tibble[[var]])[[attrstr]] <- term
      }
    } else {
      warning("Not a valid variable name: ", var)
    }
  }
  tibble
}
