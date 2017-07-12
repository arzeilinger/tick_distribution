#### Functions for compiling GBIF records using rgbif package

#### Find GBIF key and counts of records for each given species
occCheckFunc <- function(x){
  require(rgbif); require(tidyr)
  # Function to find GBIF taxon key for given species and get the number of records
  taxKey <- name_lookup(query = x, rank = "species", return = "data")$key
  # Multiple keys are returned for each query; need to find the keys with non-zero counts
  # Look for number of records (count) for each key and combine only those that are non-zero
  taxCount <- lapply(taxKey, function(x) occ_count(taxonKey = x)) %>% unlist()
  # Counts and Keys that are non-zero
  goodTaxCount <- taxCount[which(taxCount > 0)]
  goodTaxKey <- taxKey[which(taxCount > 0)]
  taxCheck <- list(key = goodTaxKey,
                   count = goodTaxCount,
                   sum = sum(goodTaxCount))
  return(taxCheck)
}


#### Download occurrences of each species (or taxon key) in a list
# Takes taxon key and count of records for that key as inputs;
# uses records count to set the limit
occFunc <- function(taxKey, taxCount){
  require(rgbif)
  occData <- occ_search(taxonKey = taxKey, 
                        return = "data", limit = taxCount,
                        fields = c("name", "key", "order", "family", 
                                   "year", "month", "day",
                                   "decimalLongitude", "decimalLatitude",
                                   "country", "stateProvince", "locality",  
                                   "datasetName", "elevation"))
  return(occData)
}
