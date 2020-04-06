#-------------------------------------------------------------------------------
# Name:        1_insertSGCN.r
# Purpose:     Create an empty, new COA databases
# Author:      Christopher Tracey
# Created:     2019-02-14
# Updated:     2019-02-14
#
# To Do List/Future ideas:
# * modify to run off the lu_sgcn data on the arc server
#-------------------------------------------------------------------------------



if (!requireNamespace("here", quietly=TRUE)) install.packages("here")
require(here)

source(here::here("scripts", "00_PathsAndSettings.r"))

## Read SGCN list in
SGCN <- read.csv(here::here("_data","input","lu_sgcn.csv"), stringsAsFactors=FALSE) # read in the SGCN list

# QC to make sure that the ELCODES match the first part of the ELSeason code.
if(length(setdiff(SGCN$ELCODE, gsub("(.+?)(\\_.*)", "\\1", SGCN$ELSeason)))==0){
  print("ELCODEs and ELSeason strings match")
} else {
  print(paste("Codes for ", setdiff(SGCN$ELCODE, gsub("(.+?)(\\_.*)", "\\1", SGCN$ELSeason)), "do not match", sep=""))
}

# check for leading/trailing whitespace
SGCN$SNAME <- trimws(SGCN$SNAME, which="both")
SGCN$SCOMNAME <- trimws(SGCN$SCOMNAME, which="both")
SGCN$ELSeason <- trimws(SGCN$ELSeason, which="both")
SGCN$TaxaDisplay <- trimws(SGCN$TaxaDisplay, which="both")

# write the lu_sgcn table to the database
db <- dbConnect(SQLite(), dbname=databasename) # connect to the database
dbWriteTable(db, "lu_SGCN", SGCN, overwrite=TRUE) # write the table to the sqlite
dbDisconnect(db) # disconnect the db
rm(SGCN)

## Taxa Group import
taxagrp <- read.csv(here::here("_data","input","lu_taxagrp.csv"), stringsAsFactors=FALSE)
taxagrp$OID <- NULL

db <- dbConnect(SQLite(), dbname=databasename) # connect to the database
dbWriteTable(db, "lu_taxagrp", taxagrp, overwrite=TRUE) # write the table to the sqlite
dbDisconnect(db) # disconnect the db
rm(taxagrp)

