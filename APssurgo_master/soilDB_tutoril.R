# Soil Database Interface: the soilDB package
# http://ncss-tech.github.io/AQP/soilDB/soilDB-Intro.html


# run these commands in the R console
# stable version from CRAN + dependencies
install.packages("RODBC", dep=TRUE)
install.packages("aqp", dep = TRUE)
install.packages("soilDB", dep=TRUE)

library(RODBC)
library(aqp)
library(soilDB)

# get the manual page for a package
help(soilDB)
help(aqp)
# package demo
demo(aqp)
# use fuzzy matching to search for a topic or keyword
help.search(boxplot)
# shortcut:
?plot