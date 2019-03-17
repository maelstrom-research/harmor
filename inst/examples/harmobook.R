library(opalr)
library(harmor)

o <- opal.login()
#taxo <- opal.taxonomy(o, "Mlstr_area")
#vars <- opal.variables(opal, datasource = "CPTP", table = "Coreqx_final")
makeHarmonizationBook(o, "CPTP", "Coreqx_final")
opal.logout(o)

harmor:::.makeHarmonizationBook(vars, taxo,
                                vocabularies.filter = function(voc) { voc$name == "Sociodemographic_economic_characteristics" },
                                outDir = "../harmobook-demo")

harmor:::.makeHarmonizationBook(vars, taxo,
                                vocabularies.filter = function(voc) { TRUE },
                                outDir = "../harmobook-demo")
