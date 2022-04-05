#-------------------------------------------------------------------------------
# Name:        8_Indexes.r
# Purpose:     
# Author:      Christopher Tracey
# Created:     2019-04-09
# Updated:     
#
# To Do List/Future ideas:
# * 
#-------------------------------------------------------------------------------

# clear the environments
rm(list=ls())

if (!requireNamespace("here", quietly=TRUE)) install.packages("here")
require(here)

source(here::here("scripts", "00_PathsAndSettings.r"))

db <- dbConnect(SQLite(), dbname=databasename) # connect to the database
  dbExecute(db, "CREATE INDEX habitat ON lu_HabTerr (unique_id, Code);")
  dbExecute(db, "CREATE INDEX habitataq ON lu_LoticData (unique_id, SUM_23);")
  dbExecute(db, "CREATE INDEX maindex ON lu_sgcnXpu_all (unique_id,ELSeason);")
  dbExecute(db, "CREATE INDEX muni ON lu_muni (unique_id);")
  dbExecute(db, "CREATE INDEX natbound ON lu_NaturalBoundaries (unique_id);")
  dbExecute(db, "CREATE INDEX proland ON lu_ProtectedLands_25 (unique_id);")
  dbExecute(db, "CREATE INDEX threats ON lu_threats (unique_id);")
dbDisconnect(db)

# CREATE INDEX habitat ON lu_HabTerr (unique_id, Code);
# CREATE INDEX habitataq ON lu_LoticData (unique_id, SUM_23);
# CREATE INDEX maindex ON lu_sgcnXpu_all (unique_id,ELSeason);
# CREATE INDEX muni ON lu_muni (unique_id);
# CREATE INDEX natbound ON lu_NaturalBoundaries (unique_id);
# CREATE INDEX proland ON lu_ProtectedLands_25 (unique_id);
# CREATE INDEX threats ON lu_threats (unique_id);




