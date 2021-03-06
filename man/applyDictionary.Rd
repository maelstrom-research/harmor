% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/applyDictionary.R
\name{applyDictionary}
\alias{applyDictionary}
\title{Apply the dictionary to a tibble}
\usage{
applyDictionary(tibble, variables, categories = NULL)
}
\arguments{
\item{tibble}{Tibble to be decorated.}

\item{variables}{A data frame with one row per variable (column name) and then one column per property/attribute.}

\item{categories}{A data frame with one row per category (columns variable and name) and then column per property/attribute.}
}
\description{
Apply the dictionary described in a Opal Excel format as attributes of the tibble's columns.
}
