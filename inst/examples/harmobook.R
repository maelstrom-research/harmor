library(opalr)
library(harmor)

o <- opal.login()
taxo <- opal.taxonomy(o, "Mlstr_area")
vars <- opal.variables(o, datasource = "CPTP", table = "Coreqx_final")
makeHarmonizationBook(o, "CPTP", "Coreqx_final", vocabularies = "Sociodemographic_economic_characteristics")
harmor:::.makeHarmonizationBook(vars, taxo,
                                vocabularies.filter = function(voc) { voc == "Sociodemographic_economic_characteristics" },
                                outDir = "../harmobook-demo")
harmor:::.makeHarmonizationBook(vars, taxo,
                                vocabularies.filter = function(voc) { TRUE },
                                outDir = "../harmobook-demo")
opal.logout(o)

