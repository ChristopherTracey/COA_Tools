#-------------------------------------------------------------------------------
# Name:        4_habitats.r
# Purpose:     
# Author:      Christopher Tracey
# Created:     2019-02-15
# Updated:     2019-02-20
#
# Updates:
# * added primary macrogroups
#
# To Do List/Future ideas:
#
#-------------------------------------------------------------------------------

# clear the environments
rm(list=ls())

if (!requireNamespace("here", quietly=TRUE)) install.packages("here")
  require(here)

source(here::here("scripts", "00_PathsAndSettings.r"))


# check to make sure all the ELCODEs are correct
loadSGCN()


## Habitat Names
HabitatName <- read.csv(here::here("_data","input","lu_HabitatName.csv"), stringsAsFactors=FALSE)
trackfiles("Habitat Names Lookup", here::here("_data","input","lu_HabitatName.csv")) # write to file tracker
db <- dbConnect(SQLite(), dbname=databasename) # connect to the database
  dbWriteTable(db, "lu_HabitatName", HabitatName, overwrite=TRUE) # write the table to the sqlite
dbDisconnect(db) # disconnect the db

## Terrestrial Habitat Layer
HabTerr <- read.csv(here::here("_data","input","lu_HabTerr.csv"), stringsAsFactors=FALSE)
trackfiles("Terrestrial Habitats", here::here("_data","input","lu_HabTerr.csv")) # write to file tracker
db <- dbConnect(SQLite(), dbname=databasename) # connect to the database
  dbWriteTable(db, "lu_HabTerr", HabTerr, overwrite=TRUE) # write the table to the sqlite
dbDisconnect(db) # disconnect the db

## Lotic Habitat Layer
HabLotic <- read.csv(here::here("_data","input","lu_LoticData.csv"), stringsAsFactors=FALSE)
HabLotic <- HabLotic[c("unique_id","COMID","GNIS_NAME","SUM_23","DESC_23","MACRO_GR","Shape_Length")]
trackfiles("Lotic Habitats", here::here("_data","input","lu_LoticData.csv")) # write to file tracker
db <- dbConnect(SQLite(), dbname=databasename) # connect to the database
dbWriteTable(db, "lu_LoticData", HabLotic, overwrite=TRUE) # write the table to the sqlite
dbDisconnect(db) # disconnect the db

## Special Habitats - caves and seasonal pools
HabSpecial <- read.csv(here::here("_data","input","lu_SpecialHabitats.csv"), stringsAsFactors=FALSE)
trackfiles("Special Habitats", here::here("_data","input","lu_SpecialHabitats.csv")) # write to file tracker
db <- dbConnect(SQLite(), dbname=databasename) # connect to the database
dbWriteTable(db, "lu_SpecialHabitats", HabSpecial, overwrite=TRUE) # write the table to the sqlite
dbDisconnect(db) # disconnect the db




