#### MUSEUM OCCURRENCES OF TICK VECTORS
#### Occurrences checked in GBIF and Essig Museum databases

my.packages <- c("rgbif", "data.table", "dplyr", "tidyr", "maps", "ecoengine")
lapply(my.packages, require, character.only = TRUE)
# Load functions for compiling GBIF records
source("R_functions/gbif_functions")


##############################################################
#### Get GBIF records
##############################################################

# Load vector of tick names
ticks <- c("Ixodes pacificus", "Ixodes scapularis", "Amblyomma americanum")

# Look up taxon keys and record counts from GBIF
tickCheck <- lapply(ticks, occCheckFunc)
names(tickCheck) <- ticks

# Get a vector of taxon keys and counts
tickKeys <- lapply(1:length(tickCheck), function(x) tickCheck[[x]]$key) %>% unlist()
tickCounts <- lapply(1:length(tickCheck), function(x) tickCheck[[x]]$count) %>% unlist()

# Create list with each element as a table of records for each taxon key
tickList <- lapply(1:length(tickKeys), function(x) occFunc(tickKeys[x], tickCounts[x]))
# Combine into one data set
tickData <- tickList %>% rbindlist(., fill = TRUE) %>% as.data.frame()

### Filtering data set
# GBIF returned two additional species; filter data set to just original three names
tickData <- tickData %>% dplyr::filter(., name == ticks[1] | name == ticks[2] | name == ticks[3])

# Missing values
summary(tickData)
# 23 records are missing lat/long, 115 are missing year

# select only records that are geo-referenced
tickGeo <- tickData %>% dplyr::filter(., !is.na(decimalLongitude))

# add names to the list, with periods instead of spaces
tickNames <- c("Ixodes.pacificus", "Ixodes.scapularis", "Amblyomma.americanum")
names(tickGeo) <- tickNames

# check the raw number of records from GBIF
lapply(tickList, nrow)

# map the tick occurrences on the US map
pdf("US_map_tick_gbif_records_2017-05-19.pdf")
  map("state")
  title("I. pacificus = green, I. scapularis = red, A. americanum = blue")
  points(x = tickYr$Ixodes.pacificus$decimalLongitude,
         y = tickYr$Ixodes.pacificus$decimalLatitude,
         col = "darkgreen")
  points(x = tickYr$Ixodes.scapularis$decimalLongitude,
         y = tickYr$Ixodes.scapularis$decimalLatitude,
         col = "red")
  points(x = tickYr$Amblyomma.americanum$decimalLongitude,
         y = tickYr$Amblyomma.americanum$decimalLatitude,
         col = "blue")
dev.off()

# Save list object
save(tickYr, file = "Tick_records_GBIF_2017-05-19.Rdata")


#########################################################
#### Occurrences from Essig database
#########################################################
eeFunc <- function(x){
  eeOcc <- ee_observations(scientific_name = x, 
                           quiet = TRUE, page = "all")
  eeData <- eeOcc$data
  return(eeData)
}

# Note: Ixodes scapularis returns an error and error handling using tryCatch() returns wonky results (don't know why). Running just remaining two species
tickList2 <- lapply(ticks[c(1,3)], eeFunc)

# select only records that are geo-referenced
tickGeo2 <- lapply(1:length(tickList2), function(x) tickList2[[x]][!is.na(tickList2[[x]]$latitude),])
# select only records that have year data
tickYr2 <- lapply(1:length(tickGeo2), function(x) tickGeo2[[x]][!is.na(tickGeo2[[x]]$begin_date),])
names(tickYr2) <- ticks[c(1,3)]

# check the raw number of records from ecoengine
lapply(tickList2, nrow)
# Check the final number of records, filtered to only those that are georeferenced and have a year
lapply(tickYr2, nrow)

# Save list object
save(tickYr2, file = "Tick_records_Essig_2014-12-02.Rdata")


