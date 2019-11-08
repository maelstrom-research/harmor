#' Save the tibble as a Opal table
#'
#' Assign the tibble from the R client-side session to the R session-side session with the provided
#' symbol name and import it into the Opal project. If variables dictionary is provided, it will be applied after
#' the import.
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
#' @param variables A data frame with one row per variable (column name) and then one column per property/attribute (Opal Excel format).
#' @param categories A data frame with one row per category (columns variable and name) and then column per property/attribute (Opal Excel format). If there are
#' no categories, this parameter is optional.
#' @return An invisible logical indicating whether the destination table exists.
#' @export
#' @import jsonlite
saveOpalTable <- function(opal, tibble, project, table, overwrite = TRUE, force = FALSE, identifiers=NULL, policy='required', id.name='id', type='Participant', variables = NULL, categories = NULL) {
  if (!("tbl" %in% class(tibble))) {
    stop("The tibble parameter must be a tibble.")
  }
  steps <- 7
  if (!is.null(variables)) {
    steps <- steps + 1
  }
  pb <- .newProgress(total = steps)
  .tickProgress(pb, tokens = list(what = paste0("Checking ", project, " project")))
  if (table %in% opal.datasource(opal, project)$table) {
    if (overwrite) {
      if (!force) {
        stop("Destination table needs to be deleted. Use 'force' parameter to proceed.")
      }
      .tickProgress(pb, tokens = list(what = paste0("Deleting ", table, " from ", project)))
      opal.delete(opal, "datasource", project, "table", table)
    } else {
      if (!force) {
        stop("Destination table will be updated. There could be data dictionary conflicts. Use 'force' parameter to proceed.")
      }
      .tickProgress(pb, tokens = list(what = paste0("Merging with ", table, " from ", project)))
    }
  } else {
    .tickProgress(pb, tokens = list(what = paste0("Creating table ", table, " in ", project)))
  }

  .tickProgress(pb, tokens = list(what = paste0("Saving in R data file")))
  file <- tempfile(fileext = ".rda")
  save(tibble, file = file)

  .tickProgress(pb, tokens = list(what = paste0("Uploading R data file")))
  opal.file_upload(opal, file, "/tmp")
  filename <- basename(file)
  unlink(file)
  opal.file_write(opal, paste0("/tmp/", filename))
  opal.file_rm(opal, paste0("/tmp/", filename))

  .tickProgress(pb, tokens = list(what = paste0("Loading R data file")))
  opal.execute(opal, paste0("load(file='", filename, "')"))
  opal.execute(opal, paste0("unlink('", filename, "')"))
  opal.execute(opal, paste0("assign('", table, "', tibble)"))
  opal.execute(opal, paste0("rm(tibble)"))

  .tickProgress(pb, tokens = list(what = paste0("Importing ", table, " into ", project)))
  opal.symbol_import(opal, table, project = project, identifiers = identifiers, policy = policy, id.name = id.name, type = type)

  # update dictionary
  if (!is.null(variables)) {
    .tickProgress(pb, tokens = list(what = paste0("Updating ", table, " dictionary")))
    body <- .toJSONVariables(table=table, variables = variables, categories = categories)
    opal.post(opal, "datasource", project, "table", table, "variables", contentType = "application/json", body = body)
  }

  tryCatch(opal.symbol_rm(opal, table))
  opal.execute(opal, "gc()")
  rval <- table %in% opal.datasource(opal, project)$table
  .tickProgress(pb, tokens = list(what = "Save completed"))
  invisible(rval)
}
