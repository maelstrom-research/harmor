#' Save the tibble as a Opal table
#'
#' Assign the tibble from the R client-side session to the R session-side session with the provided
#' symbol name and import it into the Opal project.
#'
#' @param opal Opal connection object.
#' @param tibble The tibble object to be imported.
#' @param project Project name where the table will be located.
#' @param table Destination table name.
#' @param overwrite If the destination table already exists, it will be replaced (deleted and then
#' imported). Otherwise the table will be updated (data dictionaries merge may conflict). Default is TRUE.
#' @param force If the destination already exists, stop with an informative message if this flag is FALSE (default).
#' @param identifiers Name of the identifiers mapping to use when assigning entities to Opal.
#' @param policy Identifiers policy: 'required' (each identifiers must be mapped prior importation (default)), 'ignore' (ignore unknown identifiers) and 'generate' (generate a system identifier for each unknown identifier).
#' @param id.name The name of the column representing the entity identifiers. Default is 'id'.
#' @param type Entity type (what the data are about). Default is 'Participant'
#' @return An invisible logical indicating whether the destination table exists.
#' @export
saveOpalTable <- function(opal, tibble, project, table, overwrite = TRUE, force = FALSE, identifiers=NULL, policy='required', id.name='id', type='Participant') {
  if (!("tbl" %in% class(tibble))) {
    stop("The tibble parameter must be a tibble.")
  }
  message("Checking ", project, " project ...")
  if (table %in% opal.datasource(opal, project)$table) {
    if (overwrite) {
      if (!force) {
        stop("Destination table needs to be deleted. Use 'force' parameter to proceed.")
      }
      message("Deleting ", table, " from ", project, " ...")
      opal.delete(opal, "datasource", project, "table", table)
    } else {
      if (!force) {
        stop("Destination table will be updated. There could be data dictionary conflicts. Use 'force' parameter to proceed.")
      }
      message("Merging with ", table, " from ", project, " ...")
    }
  }

  message("Assigning ", table, " ...")
  opal.assign.data(opal, table, tibble)

  message("Importing ", table, " into ", project, " ...")
  opal.symbol_import(opal, table, project = project, identifiers = identifiers, policy = policy, id.name = id.name, type = type)

  tryCatch(opal.symbol_rm(opal, table))
  invisible(table %in% opal.datasource(opal, project)$table)
}
