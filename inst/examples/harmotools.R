# dependencies
devtools::install_github("obiba/opalr", dependencies = TRUE)

library(opalr)
library(harmor)

o <- opal.login("administrator", "password", "https://opal-demo.obiba.org")

# get a opal table as a tibble in client R session
cnsim1 <- getOpalTable(o, "datashield", "CNSIM1")

# update annotations
annotations(cnsim1)
cnsim1 <- annotate(cnsim1, c("LAB_HDL", "LAB_GLUC_ADJUSTED", "LAB_TRIG"), vocabulary = "Laboratory_measures", term = "Biochemistry")
cnsim1 <- annotate(cnsim1, "GENDER", vocabulary = "Sociodemographic_economic_characteristics", term = "Sex")
annotations(cnsim1)

# save the new tibble into a project
saveOpalTable(o, cnsim1, "datashield", "CNSIM4", overwrite = TRUE, force = TRUE)

# get the saved table and verifies the annotations
cnsim4 <- getOpalTable(o, "datashield", "CNSIM4")
annotations(cnsim4)

# data dictionary can also be retrieved directly (see one column per vocabulary)
opal.variables(o, "datashield", "CNSIM4")

# apply annotations directly
annot <- data.frame(variable=c("LAB_HDL", "LAB_GLUC_ADJUSTED", "LAB_TRIG"),
                    taxonomy=rep("Mlst_harmo", 3), vocabulary=rep("status", 3), term=rep("complete", 3))
annot
opal.annotate(o, "datashield", "CNSIM4", annot)
opal.annotations(o, "datashield", "CNSIM4")

# remove annotations directly
annot <- data.frame(variable=c("LAB_HDL", "LAB_TRIG"),
                    taxonomy=rep("Mlst_harmo", 2), vocabulary=rep("status", 2), term=c("impossible", NA))
annot
opal.annotate(o, "datashield", "CNSIM4", annot)
opal.annotations(o, "datashield", "CNSIM4")

# reminder: taxonomy details can be retrieved from opal
opal.taxonomies(o)
opal.vocabularies(o, taxonomy = "Mlstr_area")
opal.terms(o, taxonomy = "Mlstr_area", vocabulary = "Sociodemographic_economic_characteristics")

opal.logout(o)
