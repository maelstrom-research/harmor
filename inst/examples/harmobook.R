library(opalr)
library(harmor)

o <- opal.login()
#taxo <- opal.taxonomy(o, "Mlstr_area")
#vars <- opal.variables(opal, datasource = "CPTP", table = "Coreqx_final")
makeHarmonizationBook(o, "CPTP", "Coreqx_final")
opal.logout(o)

harmor:::.makeHarmonizationBook(vars, taxo, outDir = "/home/yannick/projects/maelstrom-research/harmobook-demo")
