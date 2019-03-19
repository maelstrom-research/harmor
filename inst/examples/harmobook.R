library(opalr)
library(harmor)

o <- opal.login("administrator", "password", "https://opal-demo.obiba.org")
# complete book with all the domains
makeHarmonizationBook(o, "CPTP", "Coreqx_final", outDir = "../harmobook-demo")
# book for a set of domains
makeHarmonizationBook(o, "CPTP", "Coreqx_final", vocabularies = c("Lifestyle_behaviours","Diseases"),
                      outDir = "../harmobook-demo")
opal.logout(o)

