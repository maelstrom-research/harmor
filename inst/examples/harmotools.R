# dependencies
devtools::install_github("obiba/opalr", dependencies = TRUE)

library(opalr)
library(harmor)

o <- opal.login("administrator", "password", "https://opal-demo.obiba.org")

# get a opal table as a tibble in client R session
cnsim <- getOpalTable(o, "datashield", "CNSIM1")

# update annotations
annotations(cnsim)
cnsim <- annotate(cnsim, c("LAB_HDL", "LAB_GLUC_ADJUSTED", "LAB_TRIG"), vocabulary = "Laboratory_measures", term = "Biochemistry")
cnsim <- annotate(cnsim, "GENDER", vocabulary = "Sociodemographic_economic_characteristics", term = "Sex")
annotations(cnsim)

# save the tibble in the project
saveOpalTable(o, cnsim, "datashield", "CNSIM", overwrite = TRUE, force = TRUE)

# get a harmonized dataset and apply harmo status annotations
cnsim1 <- getOpalTable(o, "datashield", "CNSIM1")
annotations(cnsim1)
cnsim1 <- annotateHarmoStatus(cnsim1, status = "undetermined")
annotations(cnsim1)

# save the tibble
saveOpalTable(o, cnsim1, "datashield", "CNSIM1", overwrite = TRUE, force = TRUE)

# data dictionary can also be retrieved directly (see one column per vocabulary)
cnsim1_vars <- opal.variables(o, "datashield", "CNSIM1")

# apply annotations directly
annot <- data.frame(variable=c("LAB_HDL", "LAB_GLUC_ADJUSTED", "LAB_TRIG"),
                    taxonomy=rep("Mlstr_harmo", 3), vocabulary=rep("status", 3), term=rep("complete", 3))
annot
opal.annotate(o, "datashield", "CNSIM1", annot)
opal.annotations(o, "datashield", "CNSIM1")

# remove or update annotations directly
annot <- data.frame(variable=c("LAB_HDL", "LAB_TRIG"),
                    taxonomy=rep("Mlstr_harmo", 2), vocabulary=rep("status", 2), term=c("impossible", NA))
annot
opal.annotate(o, "datashield", "CNSIM1", annot)
opal.annotations(o, "datashield", "CNSIM1")

# reminder: taxonomy details can be retrieved from opal
opal.taxonomies(o)
opal.vocabularies(o, taxonomy = "Mlstr_area")
opal.terms(o, taxonomy = "Mlstr_area", vocabulary = "Sociodemographic_economic_characteristics")

opal.logout(o)
