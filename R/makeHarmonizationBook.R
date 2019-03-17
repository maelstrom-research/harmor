#' Make a R markdown book
#'
#' Get the variables from a Opal table and dump a book of R markdown templates,
#' ready for the including derivation code chunks and harmonization status.
#'
#' @param opal Opal connection object.
#' @param project Project name where the table is located.
#' @param table Table name from which the data dictionary should be extracted.
#' @param locale The locale in which the labels should be extracted (default is "en").
#' @param taxonomy Taxonomy name to group variables by vocabularies and terms.
#' @param vocabularies Vocabularies that shall be included, default is all (NULL value).
#' @param outDir Output folder where the R markdown templates should be produced (default is
#' '_harmobook' folder in the current working directory).
#' @param outFile Name of the output file.
#'
#' @import opalr
#' @export
makeHarmonizationBook <- function(opal, project, table, taxonomy = "Mlstr_area", vocabularies = NULL, locale = "en", outDir = file.path(getwd(), "_harmobook"), outFile = "04-domains.Rmd") {
  variables <- opal.variables(opal, datasource = project, table = table, locale = locale)
  taxo <- opal.taxonomy(opal, taxonomy)
  voc.filter <- function(voc) {
    if (is.null(vocabularies)) {
      TRUE
    } else {
      voc$name %in% vocabularies
    }
  }
  .makeHarmonizationBook(variables, taxonomy = taxo, vocabularies.filter = voc.filter, locale = locale, outDir = outDir, outFile = outFile)
}

#' @keywords internal
.makeHarmonizationBook <- function(variables, taxonomy, vocabularies.filter, locale = "en", outDir = file.path(getwd(), "_harmobook"), outFile = "04-domains.Rmd") {
  message(paste0("Outputing R markdown files in: ", outDir, " ..."))

  templates <- list()
  templates[["vocabulary"]] <- .getTemplate("vocabulary", outDir)
  templates[["term"]] <- .getTemplate("term", outDir)
  templates[["variable"]] <- .getTemplate("variable", outDir)
  templates[["categories"]] <- .getTemplate("categories", outDir)
  templates[["category"]] <- .getTemplate("category", outDir)

  if (!dir.exists(outDir)) {
    dir.create(outDir)
  }
  conn <- file(file.path(outDir, outFile), "w+")
  sink(conn)

  prefix <- paste0(taxonomy$name, ".")
  varTermMap <- list()
  for (i in 1:nrow(variables)) {
    variable <- variables[i,]
    name <- as.character(variable[["name"]])
    for (col in colnames(variable)) {
      if (startsWith(col, prefix)) {
        term <- as.character(variable[[col]])
        if (!is.na(term)) {
          if (is.null(varTermMap[[name]])) {
            varTermMap[[name]] <- list()
          }
          varTermMap[[name]] <- append(varTermMap[[name]], term)
        }
      }
    }
  }

  #cat(paste0("# ", taxonomy$name, "\n\n"))
  for (vocabulary in taxonomy$vocabularies) {
    if (vocabularies.filter(vocabulary)) {
      colname <- paste0(taxonomy$name, ".", vocabulary$name)
      vocVars <- variables[!is.na(variables[[colname]]),]
      if (!is.null(vocVars) && nrow(vocVars)>0) {
        message(paste0(vocabulary$name, " ..."))
        cat(.makeVocabulary(templates, vocabulary, vocVars, colname, locale))
      }
    }
  }

  sink()
  flush(conn)
  close(conn)
}

#' @keywords internal
.makeCategory <- function(templates, name, label, missing) {
  miss <- ""
  if (.na2str(missing) == "T") {
    miss <- "*"
  }
  outCat <- gsub("\\{\\{name\\}\\}", name, templates$category)
  outCat <- gsub("\\{\\{label\\}\\}", .na2str(label), outCat)
  outCat <- gsub("\\{\\{missing\\}\\}", miss, outCat)
  paste(outCat, collapse = "\n")
}

#' @keywords internal
.makeCategories <- function(templates, names, labels, missings) {
  catName <- unlist(strsplit(names, "\\|"))
  catLabel <- unlist(strsplit(labels, "\\|"))
  catMissing <- unlist(strsplit(missings, "\\|"))
  outCats <- c()
  for (i in 1:length(catName)) {
    outCat <- .makeCategory(templates, catName[i], catLabel[i], catMissing[i])
    outCats <- append(outCats, outCat)
  }
  outCats <- paste(outCats, collapse = "\n")
  outCats <- gsub("\\{\\{category\\}\\}", outCats, templates$categories)
  paste(outCats, collapse = "\n")
}

#' @keywords internal
.makeVariable <- function(templates, name, label, description, valueType, unit, categories, categories.label, categories.missing) {
  outVar <- gsub("\\{\\{name\\}\\}", name, templates$variable)
  outVar <- gsub("\\{\\{label\\}\\}", .na2str(label), outVar)
  outVar <- gsub("\\{\\{description\\}\\}", .na2str(description), outVar)
  outVar <- gsub("\\{\\{valueType\\}\\}", .na2str(valueType), outVar)
  outVar <- gsub("\\{\\{unit\\}\\}", .na2str(unit), outVar)

  if (!is.na(categories)) {
    outCats <- .makeCategories(templates, categories, categories.label, categories.missing)
    outVar <- gsub("\\{\\{categories\\}\\}", outCats, outVar)
  } else {
    outVar <- gsub("\\{\\{categories\\}\\}", "", outVar)
  }
  paste(outVar, collapse = "\n")
}

#' @keywords internal
.makeVariables <- function(templates, vars) {
  name <- as.character(vars[["name"]])
  label <- as.character(vars[["label"]])
  description <- as.character(vars[["description"]])
  valueType <- as.character(vars[["valueType"]])
  unit <- as.character(vars[["unit"]])
  categories <- as.character(vars[["categories"]])
  categories.label <- as.character(vars[["categories.label"]])
  categories.missing <- as.character(vars[["categories.missing"]])
  outVars <- c()
  for (i in 1:nrow(vars)) {
    outVar <- .makeVariable(templates, name[i], label[i], description[i], valueType[i], unit[i], categories[i], categories.label[i], categories.missing[i])
    outVars <- append(outVars, outVar)
  }
  paste(outVars, collapse = "\n")
}

#' @keywords internal
.makeTerm <- function(templates, term, termVars, locale) {
  outTerm <- gsub("\\{\\{name\\}\\}", term$name, templates$term)
  outTerm <- gsub("\\{\\{label\\}\\}", .localized2str(term$title, locale), outTerm)
  outTerm <- gsub("\\{\\{description\\}\\}", .localized2str(term$description, locale), outTerm)
  outVars <- .makeVariables(templates, termVars)
  outTerm <- gsub("\\{\\{variables\\}\\}", outVars, outTerm)
  paste(outTerm, collapse = "\n")
}

#' @keywords internal
.makeVocabulary <- function(templates, vocabulary, vocVars, vocCol, locale) {
  outVoc <- gsub("\\{\\{name\\}\\}", vocabulary$name, templates$vocabulary)
  outVoc <- gsub("\\{\\{label\\}\\}", .localized2str(vocabulary$title, locale), outVoc)
  outVoc <- gsub("\\{\\{description\\}\\}", .localized2str(vocabulary$description, locale), outVoc)

  outTerms <- c()
  for (term in vocabulary$terms) {
    termVars <- vocVars[vocVars[[vocCol]] == term$name,]
    if (!is.null(termVars) && nrow(termVars)>0) {
      #message(paste0("  ", term$name, " ..."))
      outTerm <- .makeTerm(templates, term, termVars, locale)
      outTerms <- append(outTerms, outTerm)
    }
  }
  outVoc <- gsub("\\{\\{terms\\}\\}", paste(outTerms, collapse = "\n"), outVoc)
  paste(outVoc, collapse = "\n")
}

#' Get item (vocabulary, term, variable, categories, category) template
#'
#' Look for R markdown template file from provided directory (from _templates sub-directory) or from
#' the package default ones.
#'
#' @keywords internal
.getTemplate <- function(type, dir) {
  tmplDir <- file.path(dir, "_templates")
  if (dir.exists(tmplDir)) {
    tmplFile <- file.path(tmplDir, paste0(type, ".Rmd"))
    if (file.exists(tmplFile)) {
      message(paste0("Using ", type, " template: ", tmplFile, " ..."))
      return(readLines(tmplFile))
    }
  }
  .getPackageTemplate(type)
}

#' Read the package template
#' @keywords internal
.getPackageTemplate <- function(type) {
  templatePath <- system.file("templates", paste0(type, ".Rmd"), package = "harmor")
  message(paste0("Using ", type, " template: ", templatePath, " ..."))
  readLines(templatePath)
}
