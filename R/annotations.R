#' List the annotations
#'
#' List the annotations of each of the variables.
#'
#' @param tibble Tibble to be annotated
#' @param variables A character vector of variable names to be inspected. If NULL or empty, all
#' the columns of the tibble will be inspected.
#' @return A data frame in long format (one row per annotation).
#'
#' @export
annotations <- function(tibble, variables = NULL) {
  if (!("tbl" %in% class(tibble))) {
    stop("The tibble parameter must be a tibble.")
  }
  vars <- variables
  if (is.null(vars)) {
    vars <- colnames(tibble)
  }

  variable <- c()
  taxonomy <- c()
  vocabulary <- c()
  term <- c()

  for (var in vars) {
    if (var %in% colnames(tibble)) {
      attrs <- attributes(tibble[[var]])
      if (!is.null(attrs)) {
        for (n in names(attrs)) {
          if (grepl("::", n)) {
            tokens <- unlist(strsplit(n, "::"))
            variable <- append(variable, var)
            taxonomy <- append(taxonomy, tokens[1])
            vocabulary <- append(vocabulary, tokens[2])
            term <- append(term, attrs[[n]])

          }
        }
      }
    }
  }

  data.frame(variable, taxonomy, vocabulary, term)
}
