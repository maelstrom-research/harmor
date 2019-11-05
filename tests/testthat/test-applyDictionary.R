test_that("dico", {
  data <- tibble::as_tibble(mtcars)
  variables <- tibble::tribble(
    ~name, ~`label:en`,  ~`description:en`, ~`Namespace::Name`, ~unit,
    "mpg", "Mpg label",  "Mpg description", "Value1", "years",
    "cyl", "Cyl label",  "Cyl description", "Value2", "kg/m2"
  )
  categories <- tibble::tribble(
    ~variable, ~name, ~missing, ~`label:en`,
    "cyl", "1", 0, "One",
    "cyl", "2", 0, "Two",
    "cyl", "3", 0, "Three",
    "cyl", "4", 1, "Four"
  )
  data <- applyDictionary(data, variables, categories)
  attrs <- attributes(data$cyl)
  expect_equal(attrs$label, "(en) Cyl label")
  expect_equal(attrs$description, "(en) Cyl description")
  expect_equal(attrs$`Namespace::Name`, "Value2")
  expect_equal(attrs$unit, "kg/m2")
  expect_equal(class(data$cyl), c("haven_labelled"))
  expect_false(is.null(attrs$labels))
  expect_equal(length(attrs$labels), 4)
  expect_equal(attrs$labels, c("(en) One"="1", "(en) Two"="2", "(en) Three"="3", "(en) Four"="4"))
  #data$id <- as.character(1:32)
  #data
  #print(attrs)
  #o <- opal.login()
  #saveOpalTable(o, data, "test", "data", force = TRUE)
  #opal.logout(o)
})
