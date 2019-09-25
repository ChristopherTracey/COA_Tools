#-------------------------------------------------------------------------------
# Name:        2a_insertNaturalBoundaries.r
# Purpose:     
# Author:      Christopher Tracey
# Created:     2019-02-20
# Updated:     
#
# Updates:
#
# To Do List/Future ideas:
#
#-------------------------------------------------------------------------------

if (!requireNamespace("here", quietly=TRUE)) install.packages("here")
require(here)
if (!requireNamespace("RSQLite", quietly=TRUE)) install.packages("RSQLite")
require(RSQLite)

# Set input paths ----
databasename <- "coa_bridgetest.sqlite" 
databasename <- here("_data","output",databasename)

## Natural Boundaries
NaturalBoundaries <- read.csv(here("_data","input","lu_NaturalBoundaries.csv"), stringsAsFactors=FALSE)
#NaturalBoundaries <- CountyName[order(CountyName$COUNTY_NAM),]
db <- dbConnect(SQLite(), dbname=databasename) # connect to the database
dbWriteTable(db, "lu_NaturalBoundaries", NaturalBoundaries, overwrite=TRUE) # write the table to the sqlite
dbDisconnect(db) # disconnect the db
rm(NaturalBoundaries)

## HUC names
HUCname <- read.csv(here("_data","input","lu_HUCname.csv"), stringsAsFactors=FALSE)
HUCname <- HUCname[order(HUCname$HUC12name),]
db <- dbConnect(SQLite(), dbname=databasename) # connect to the database
dbWriteTable(db, "lu_HUCname", HUCname, overwrite=TRUE) # write the table to the sqlite
dbDisconnect(db) # disconnect the db
rm(HUCname)