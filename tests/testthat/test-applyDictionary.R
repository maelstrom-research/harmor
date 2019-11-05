.makeTestDataset <- function() {
  data <- tibble::as_tibble(mtcars)
  data$id <- as.character(1:nrow(data))
  variables <- tibble::tribble(
    ~name, ~valueType, ~`label:en`,  ~`description:en`, ~`Namespace::Name`, ~unit,
    "mpg", "decimal", "Mpg label",  "Mpg description", "Value1", "years",
    "cyl", "decimal", "Cyl label",  "Cyl description", "Value2", "kg/m2",
    "disp", "decimal", "Disp label", "Disp description", NA, NA
  )
  categories <- tibble::tribble(
    ~variable, ~name, ~missing, ~`label:en`, ~`label:fr`,
    "cyl", "4", 0, "Four", "Quatre",
    "cyl", "6", 0, "Six", "Six",
    "cyl", "8", 1, "Height", "Huit"
  )
  list(data = data, variables = variables, categories = categories)
}

test_that("dico applied to tibble", {
  dataset <- .makeTestDataset()
  data <- dataset$data
  variables <- dataset$variables
  categories <- dataset$categories

  data <- applyDictionary(data, variables, categories)
  attrs <- attributes(data$cyl)
  expect_equal(attrs$label, "(en) Cyl label")
  expect_equal(attrs$description, "(en) Cyl description")
  expect_equal(attrs$`Namespace::Name`, "Value2")
  expect_equal(attrs$unit, "kg/m2")
  expect_equal(class(data$cyl), c("haven_labelled"))
  expect_false(is.null(attrs$labels))
  expect_equal(length(attrs$labels), 3)
  expect_equal(attrs$labels, c("(en) Four"="4", "(en) Six"="6", "(en) Height"="8"))
})

test_that("dico to JSON", {
  dataset <- .makeTestDataset()
  json <- .toJSONVariables(variables = dataset$variables, categories = dataset$categories)
  expect_equal(as.character(json), '[{"name":"mpg","valueType":"decimal","entityType":"Participant","unit":"years","isRepeatable":false,"attributes":[{"name":"label","locale":"en","value":"Mpg label"},{"name":"description","locale":"en","value":"Mpg description"},{"namespace":"Namespace","name":"Name","value":"Value1"}]},{"name":"cyl","valueType":"decimal","entityType":"Participant","unit":"kg/m2","isRepeatable":false,"attributes":[{"name":"label","locale":"en","value":"Cyl label"},{"name":"description","locale":"en","value":"Cyl description"},{"namespace":"Namespace","name":"Name","value":"Value2"}],"categories":[{"name":"4","isMissing":false,"attributes":[{"name":"label","locale":"en","value":"Four"},{"name":"label","locale":"fr","value":"Quatre"}]},{"name":"6","isMissing":false,"attributes":[{"name":"label","locale":"en","value":"Six"},{"name":"label","locale":"fr","value":"Six"}]},{"name":"8","isMissing":true,"attributes":[{"name":"label","locale":"en","value":"Height"},{"name":"label","locale":"fr","value":"Huit"}]}]},{"name":"disp","valueType":"decimal","entityType":"Participant","isRepeatable":false,"attributes":[{"name":"label","locale":"en","value":"Disp label"},{"name":"description","locale":"en","value":"Disp description"}]}]')
})
