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
#' @param outDir Output folder where the R markdown files will be produced (default is
#' '_harmobook' folder in the current working directory). Also used to find R markdown templates (in the
#' '_templates' subfolder); if non is found the 'harmor' package default ones will be used.
#' @param outFiles Names of the output files, idenitified by "domains" or "index".
#'
#' @import opalr
#' @export
makeHarmonizationBook <- function(opal, project, table, taxonomy = "Mlstr_area", vocabularies = NULL, locale = "en",
                                  outDir = file.path(getwd(), "_harmobook"), outFiles = list(domains="04-domains.Rmd", index="06-index.Rmd")) {
  variables <- opal.variables(opal, datasource = project, table = table, locale = locale)
  taxo <- opal.taxonomy(opal, taxonomy)
  voc.filter <- function(voc) {
    if (is.null(vocabularies)) {
      TRUE
    } else {
      voc %in% vocabularies
    }
  }
  .makeHarmonizationBook(variables, taxonomy = taxo, vocabularies.filter = voc.filter, locale = locale, outDir = outDir, outFiles = outFiles)
}

#' @keywords internal
.makeHarmonizationBook <- function(variables, taxonomy, vocabularies.filter, locale = "en",
                                   outDir = file.path(getwd(), "_harmobook"), outFiles = list(domains="04-domains.Rmd", index="06-index.Rmd")) {
  message(paste0("Outputing R markdown files in: ", outDir, " ..."))

  templates <- list()
  templates[["vocabulary"]] <- .getTemplate("vocabulary", outDir)
  templates[["term"]] <- .getTemplate("term", outDir)
  templates[["variable"]] <- .getTemplate("variable", outDir)
  templates[["variable-ref"]] <- .getTemplate("variable-ref", outDir)
  templates[["categories"]] <- .getTemplate("categories", outDir)
  templates[["category"]] <- .getTemplate("category", outDir)
  templates[["index"]] <- .getTemplate("index", outDir)
  templates[["variable-index"]] <- .getTemplate("variable-index", outDir)

  if (!dir.exists(outDir)) {
    dir.create(outDir)
  }

  prefix <- paste0(taxonomy$name, ".")

  if (!is.null(outFiles$domains)) {

    conn <- file(file.path(outDir, outFiles$domains), "w+")
    sink(conn)

    # for each variable get the annotation terms grouped by vocabulary
    varTermMap <- list()
    for (i in 1:nrow(variables)) {
      variable <- variables[i,]
      name <- as.character(variable[["name"]])
      varIndexMap <- list()
      for (col in colnames(variable)) {
        if (startsWith(col, prefix)) {
          vocabulary <- gsub(prefix, "", col)
          if (vocabularies.filter(vocabulary)) {
            term <- as.character(variable[[col]])
            if (!.is.empty(term)) {
              if (is.null(varIndexMap[[vocabulary]])) {
                varIndexMap[[vocabulary]] <- list()
              }
              varIndexMap[[vocabulary]] <- append(varIndexMap[[vocabulary]], term)
            }
          }
        }
      }
      varTermMap[[name]] <- varIndexMap
    }

    for (vocabulary in taxonomy$vocabularies) {
      if (vocabularies.filter(vocabulary$name)) {
        colname <- paste0(taxonomy$name, ".", vocabulary$name)
        vocVars <- variables[!is.na(variables[[colname]]),]
        if (!is.null(vocVars) && nrow(vocVars)>0) {
          message(paste0(vocabulary$name, " ..."))
          cat(.makeVocabulary(templates, taxonomy, vocabulary, vocVars, colname, locale, varTermMap))
        }
      }
    }

    sink()
    flush(conn)
    close(conn)

  }

  if (!is.null(outFiles$index)) {

    conn <- file(file.path(outDir, outFiles$index), "w+")
    sink(conn)

    outRefs <- c()
    for (i in 1:nrow(variables)) {
      variable <- variables[i,]
      name <- as.character(variable[["name"]])
      label <- as.character(variable[["label"]])
      varIndexMap <- list()
      varIndexMap[["label"]] <- label
      for (col in colnames(variable)) {
        if (startsWith(col, prefix)) {
          vocabulary <- gsub(prefix, "", col)
          if (vocabularies.filter(vocabulary)) {
            term <- as.character(variable[[col]])
            if (!.is.empty(term)) {
              varIndexMap[["vocabularies"]][[vocabulary]] <- term
            }
          }
        }
      }
      if (!is.null(varIndexMap[["vocabularies"]])) {

        termLinks <- c()
        for (vocabulary in names(varIndexMap[["vocabularies"]])) {
          links <- c()
          if (vocabularies.filter(vocabulary)) {
            links <- unlist(lapply(varIndexMap[["vocabularies"]][[vocabulary]], function(t) {
              paste0("[", .getTermLabel(taxonomy, vocabulary, t, locale), "](#", t, ")")
            }))
          }
          termLinks <- append(termLinks, links)
        }
        if (length(termLinks)>0) {
          outRef <- gsub("\\{\\{name\\}\\}", paste0("**[", name, "](#", name,")**"), templates[["variable-index"]])
          outRef <- gsub("\\{\\{label\\}\\}", label, outRef)
          outRef <- gsub("\\{\\{links\\}\\}", paste(termLinks, collapse = ", "), outRef)
          outRefs <- append(outRefs, paste(outRef, collapse = "\n"))
          outRefs <- append(outRefs, ("\n\n"))
        }
      }
    }
    cat(paste(gsub("\\{\\{variable-index\\}\\}", paste(outRefs, collapse = "\n"), templates$index), collapse = "\n"))


    sink()
    flush(conn)
    close(conn)

  }
}

#' @keywords internal
.getTermLabel <- function(taxonomy, vocabularyName, termName, locale) {
  for (voc in taxonomy$vocabularies) {
    if (voc$name == vocabularyName) {
      for (term in voc$terms) {
        if (term$name == termName) {
          title <- .localized2str(term$title, locale)
          if (length(title) == 0) {
            title <- termName
          }
          return(title)
        }
      }
    }
    termName
  }
}

#' @keywords internal
.makeCategory <- function(templates, name, label, missing) {
  miss <- ifelse(missing == "T", "TRUE", "FALSE")
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
.makeVariable <- function(templates, variable) {
  name <- as.character(variable[["name"]])
  label <- as.character(variable[["label"]])
  description <- as.character(variable[["description"]])
  valueType <- as.character(variable[["valueType"]])
  unit <- as.character(variable[["unit"]])
  categories <- as.character(variable[["categories"]])
  categories.label <- as.character(variable[["categories.label"]])
  categories.missing <- as.character(variable[["categories.missing"]])

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

  harmo.status <- as.character(variable[["Mlstr_harmo.status"]])
  harmo.comment <- as.character(variable[["Mlstr_harmo.comment"]])

  if (.is.empty(harmo.status)) {
    harmo.status <- "complete|undetermined|impossible"
  }
  outVar <- gsub("\\{\\{Mlstr_harmo.status\\}\\}", harmo.status, outVar)
  outVar <- gsub("\\{\\{Mlstr_harmo.comment\\}\\}", .na2str(harmo.comment), outVar)

  paste(outVar, collapse = "\n")
}

#' @keywords internal
.makeVariableRef <- function(templates, variable) {
  name <- as.character(variable[["name"]])
  label <- as.character(variable[["label"]])
  outVar <- gsub("\\{\\{name\\}\\}", name, templates[["variable-ref"]])
  outVar <- gsub("\\{\\{label\\}\\}", .na2str(label), outVar)
  paste(outVar, collapse = "\n")
}

#' @keywords internal
.makeVariables <- function(templates, taxonomy, vocabulary, term, vars, locale, varTermMap) {
  outVars <- c()
  for (i in 1:nrow(vars)) {
    var <- vars[i,]
    name <- as.character(var$name)
    vocabularies <- names(varTermMap[[name]])
    outVar <- ""
    #message(paste0("  ", var$name, " ", paste0(vocabularies, collapse = "|")))
    if (vocabularies[1] == vocabulary$name) {
      outVar <- .makeVariable(templates, var)
    } else {
      outVar <- .makeVariableRef(templates, var)
      vocName <- vocabularies[1]
      termName <- varTermMap[[name]][[1]]
      termLabel <- .getTermLabel(taxonomy, vocName, termName, locale)
      outVar <- gsub("\\{\\{term.name\\}\\}", termName, outVar)
      outVar <- gsub("\\{\\{term.label\\}\\}", termLabel, outVar)
    }
    outVars <- append(outVars, "\n")
    outVars <- append(outVars, outVar)
  }
  paste(outVars, collapse = "\n")
}

#' @keywords internal
.makeTerm <- function(templates, taxonomy, vocabulary, term, termVars, locale, varTermMap) {
  outTerm <- gsub("\\{\\{name\\}\\}", term$name, templates$term)
  outTerm <- gsub("\\{\\{label\\}\\}", .localized2str(term$title, locale), outTerm)
  outTerm <- gsub("\\{\\{description\\}\\}", .localized2str(term$description, locale), outTerm)
  outVars <- .makeVariables(templates, taxonomy, vocabulary, term, termVars, locale, varTermMap)
  outTerm <- gsub("\\{\\{variables\\}\\}", outVars, outTerm)
  paste(outTerm, collapse = "\n")
}

#' @keywords internal
.makeVocabulary <- function(templates, taxonomy, vocabulary, vocVars, vocCol, locale, varTermMap) {
  outVoc <- gsub("\\{\\{name\\}\\}", vocabulary$name, templates$vocabulary)
  outVoc <- gsub("\\{\\{label\\}\\}", .localized2str(vocabulary$title, locale), outVoc)
  outVoc <- gsub("\\{\\{description\\}\\}", .localized2str(vocabulary$description, locale), outVoc)

  outTerms <- c()
  for (term in vocabulary$terms) {
    termVars <- vocVars[vocVars[[vocCol]] == term$name,]
    if (!is.null(termVars) && nrow(termVars)>0) {
      #message(paste0("  ", term$name, " ..."))
      outTerm <- .makeTerm(templates, taxonomy, vocabulary, term, termVars, locale, varTermMap)
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
