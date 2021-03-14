#!/usr/bin/env Rscript
## 
## Source for data:
##  https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/_inhalt.html
##
library("curl")
library("stringr")
library("data.table")

# URL to download zip archive with data in GV100 format
URL <- "https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/Administrativ/Archiv/GV100ADQ/GV100AD3011.zip?__blob=publicationFile"
# Name of file inside the zip archive with the data
filename <- "GV100AD_301120.asc"

# Make sure working directory is parent of directory containing this script.
if (!exists('pathName')) {
  args <- commandArgs()
  scriptName <- substr(args[substr(args,1,7) == '--file='], 8, 10000L)
  pathName <- substr(scriptName, 1, nchar(scriptName) - nchar(strsplit(scriptName, '.*[/|\\]')[[1]][2]))
  setwd(file.path(pathName, ".."))
}

zipfile <- file.path(tempdir(), "gv100ad.zip")
if (!file.exists(zipfile))
  curl_download(URL, zipfile, quiet=FALSE)

# Read the weird and wonderful fixed width format of the destatis
df <- read.fwf(unz(zipfile, filename),
               c(2, 8, 8, 4, 50, 50, 6, 72, 20),
               colClasses="character",
               strip.white=TRUE)

df$V2 <- NULL # Gebietsstand
df$V4 <- NULL # Leer
df$V6 <- NULL # Sitz Verwaltung
df$V7 <- NULL # Schlüsselfelder
df$V8 <- NULL # Leer
df$V9 <- NULL # Frei

## Satzart 10 == Landesdaten
GermanStates <- subset(df, V1 == "10")
setDT(GermanStates)
GermanStates[, V1 := NULL] # Satzart
setnames(GermanStates, c("stateId", "state"))
setkey(GermanStates, stateId)

## Satzart 40 == Kreisdaten
GermanCounties <- subset(df, V1 == "40")
setDT(GermanCounties)
GermanCounties[, V1 := NULL] # Satzart
setnames(GermanCounties, c("countyId", "county"))
GermanCounties[, stateId := str_sub(countyId, 1, 2)]
setcolorder(GermanCounties, c("stateId", "countyId", "county"))
setkey(GermanCounties, countyId)

save(GermanCounties, GermanStates, file="data/GermanRegions.rda")
