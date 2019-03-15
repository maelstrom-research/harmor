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
#' @param outDir Output folder where the R markdown templates should be produced (default is
#' '_harmobook' folder in the current working directory).
#' @param outFile Name of the output file.
#'
#' @import opalr
#' @export
makeHarmonizationBook <- function(opal, project, table, taxonomy = "Mlstr_area", locale = "en", outDir = file.path(getwd(), "_harmobook"), outFile = "04-domains.Rmd") {
  variables <- opal.variables(opal, datasource = project, table = table, locale = locale)
  taxo <- opal.taxonomy(opal, taxonomy)
  .makeHarmonizationBook(variables, taxonomy = taxo, locale = locale, outDir = outDir, outFile = outFile)
}

#' @keywords internal
.makeHarmonizationBook <- function(variables, taxonomy, locale = "en", outDir = file.path(getwd(), "_harmobook"), outFile = "04-domains.Rmd") {
  message(paste0("Outputing R markdown files in: ", outDir, " ..."))

  vocabularyTemplate <- .getPackageTemplate("vocabulary")
  termTemplate <- .getPackageTemplate("term")
  variableTemplate <- .getPackageTemplate("variable")
  categoriesTemplate <- .getPackageTemplate("categories")
  categoryTemplate <- .getPackageTemplate("category")

  if (!dir.exists(outDir)) {
    dir.create(outDir)
  }
  conn <- file(file.path(outDir, outFile), "w+")
  sink(conn)

  .na2str <- function(value) {
    if (is.na(value)) {
      "N/A"
    } else {
      as.character(value)
    }
  }

  .item2str <- function(item) {
    for (msg in item) {
      if (msg$locale == locale) {
        return(msg$text)
      }
    }
    return("")
  }

  .makeCategory <- function(name, label, missing) {
    outCat <- gsub("\\{\\{name\\}\\}", name, categoryTemplate)
    outCat <- gsub("\\{\\{label\\}\\}", .na2str(label), outCat)
    outCat <- gsub("\\{\\{missing\\}\\}", .na2str(missing), outCat)
    paste(outCat, collapse = "\n")
  }

  .makeCategories <- function(names, labels, missings) {
    catName <- unlist(strsplit(names, "\\|"))
    catLabel <- unlist(strsplit(labels, "\\|"))
    catMissing <- unlist(strsplit(missings, "\\|"))
    outCats <- c()
    for (i in 1:length(catName)) {
      outCat <- .makeCategory(catName[i], catLabel[i], catMissing[i])
      outCats <- append(outCats, outCat)
    }
    outCats <- paste(outCats, collapse = "\n")
    outCats <- gsub("\\{\\{category\\}\\}", outCats, categoriesTemplate)
    paste(outCats, collapse = "\n")
  }

  .makeVariable <- function(name, label, description, valueType, unit, categories, categories.label, categories.missing) {
    outVar <- gsub("\\{\\{name\\}\\}", name, variableTemplate)
    outVar <- gsub("\\{\\{label\\}\\}", .na2str(label), outVar)
    outVar <- gsub("\\{\\{description\\}\\}", .na2str(description), outVar)
    outVar <- gsub("\\{\\{valueType\\}\\}", .na2str(valueType), outVar)
    outVar <- gsub("\\{\\{unit\\}\\}", .na2str(unit), outVar)

    if (!is.na(categories)) {
      outCats <- .makeCategories(categories, categories.label, categories.missing)
      outVar <- gsub("\\{\\{categories\\}\\}", outCats, outVar)
    } else {
      outVar <- gsub("\\{\\{categories\\}\\}", "", outVar)
    }
    paste(outVar, collapse = "\n")
  }

  .makeVariables <- function(vars) {
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
      outVar <- .makeVariable(name[i], label[i], description[i], valueType[i], unit[i], categories[i], categories.label[i], categories.missing[i])
      outVars <- append(outVars, outVar)
    }
    paste(outVars, collapse = "\n")
  }

  .makeTerm <- function(term, termVars) {
    outTerm <- gsub("\\{\\{name\\}\\}", term$name, termTemplate)
    outTerm <- gsub("\\{\\{label\\}\\}", .item2str(term$title), outTerm)
    outTerm <- gsub("\\{\\{description\\}\\}", .item2str(term$description), outTerm)
    outVars <- .makeVariables(termVars)
    outTerm <- gsub("\\{\\{variables\\}\\}", outVars, outTerm)
    paste(outTerm, collapse = "\n")
  }

  .makeVocabulary <- function(vocabulary, vocVars) {
    outVoc <- gsub("\\{\\{name\\}\\}", vocabulary$name, vocabularyTemplate)
    outVoc <- gsub("\\{\\{label\\}\\}", .item2str(vocabulary$title), outVoc)
    outVoc <- gsub("\\{\\{description\\}\\}", .item2str(vocabulary$description), outVoc)

    outTerms <- c()
    for (term in vocabulary$terms) {
      termVars <- vocVars[vocVars[[colname]] == term$name,]
      if (!is.null(termVars) && nrow(termVars)>0) {
        message(paste0("  ", term$name, " ..."))
        outTerm <- .makeTerm(term, termVars)
        outTerms <- append(outTerms, outTerm)
      }
    }
    outVoc <- gsub("\\{\\{terms\\}\\}", paste(outTerms, collapse = "\n"), outVoc)
    paste(outVoc, collapse = "\n")
  }

  #cat(paste0("# ", taxonomy$name, "\n\n"))
  for (vocabulary in taxonomy$vocabularies) {
    colname <- paste0(taxonomy$name, ".", vocabulary$name)
    vocVars <- variables[!is.na(variables[[colname]]),]
    if (!is.null(vocVars) && nrow(vocVars)>0) {
      message(paste0(vocabulary$name, " ..."))
      cat(.makeVocabulary(vocabulary, vocVars))
    }
  }

  sink()
  flush(conn)
  close(conn)
}

#' Read the package template
#' @keywords internal
.getPackageTemplate <- function(type) {
  templatePath <- system.file("templates", paste0(type, ".Rmd"), package = "harmor")
  message(paste0("Using ", type, " template: ", templatePath, " ..."))
  readLines(templatePath)
}
