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
#' @param missings Include the missing values (default is TRUE).
#'
#' @export
getOpalTable <- function(opal, project, table, variables = NULL, missings = TRUE) {
  pb <- .newProgress(total = 5)
  .tickProgress(pb, tokens = list(what = paste0("Assigning ", project, ".", table)))
  opal.assign.table.tibble(opal, symbol = ".D", value = paste0(project, ".", table), variables = variables, missings = missings)

  .tickProgress(pb, tokens = list(what = paste0("Saving in R data file")))
  opal.assign.script(opal, ".file", quote(tempfile(tmpdir = getwd(), fileext = '.rda')))
  file <- opal.execute(opal, ".file")
  filename <- basename(file)
  opal.execute(opal, paste0("save(.D, file=.file)"))
  opal.symbol_rm(opal, ".D")
  opal.execute(opal, "gc()")

  .tickProgress(pb, tokens = list(what = paste0("Downloading R data file")))
  opalfile <- paste0("/home/", opal$username, "/", filename)
  opal.file_read(opal, filename, opalfile)
  opal.execute(opal, paste0("unlink(.file)"))
  opal.file_download(opal, opalfile)
  opal.file_rm(opal, opalfile)

  .tickProgress(pb, tokens = list(what = paste0("Loading R data file")))
  env <- new.env()
  load(filename, envir = env)
  unlink(filename)
  rval <- get(".D", envir = env)
  .tickProgress(pb, tokens = list(what = "Data loaded"))
  rval
}
