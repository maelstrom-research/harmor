#' Get a Opal table as a tibble
#'
#' Shortcut function to assign a Opal table to a tibble in the R server-side session
#' and then retrieve it into the R client-side session. Requires to have the permission to
#' see the individual values of the table and to perform R assignments.
#'
#' @param opal Opal connection object.
#' @param project Project name where the table is located.
#' @param table Table name from which the tibble should be extracted.
#' @param variables List of variable names or Javascript expression that selects the variables of a table (ignored if value does not refere to a table). See javascript documentation: http://wiki.obiba.org/display/OPALDOC/Variable+Methods
#'
#' @export
getOpalTable <- function(opal, project, table, variables = NULL) {
  message("Assigning ", project, ".", table, " ...")
  opal.assign.table.tibble(opal, symbol = ".D", value = paste0(project, ".", table), variables = variables)
  message("Retrieving ", project, ".", table, " ...")
  rval <- opal.execute(opal, ".D")
  opal.symbol_rm(opal, ".D")
  rval
}
