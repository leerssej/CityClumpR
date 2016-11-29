# R Script - dplyr predominant
# Author: leerssej
# Date: Updated 28 Nov 2016
# Desc: Construct String Distance matrix
# Desc: Sample and determine threshold and assign: this set used 82.5% similarity
# Desc: Cluster all within threshold, then filter off where city is different
# Desc: Give counts to designate where ties existed
# Desc: Dump out into .csv for manual review and curation
# Desc: reload and reformat names accordingly

options(stringsAsFactors = FALSE)
library(magrittr) 
library(tidyverse)
library(stringdist)

###### 96. REM2 NameCluster(JaroWinkler) - better for similarity of groups of words ####
###### Stringdistance clustering with exclusion of separate cities ####
load("clusterREM2RejoinedFullPlus") #clusterREM2RejoinedFullPlus
glimpse(clusterREM2RejoinedFullPlus)
# Pull just the Unique WinningNamesm their post overmergeQnorm tested ID, and their identifying information.
# This is the development for the string distance matrix 
# nrmGmCity = composite of the gmLocality and the gmColloquial_areas against the suggested gmCity
REM2geoLocationGroupsAndDimensions <- 
    clusterREM2RejoinedFullPlus %>% 
    select(m1mg, rctGmLocality, rctGmColloquial_area, rctGmCountry, stdCity, gcWinningName) %>%
    mutate(nrmGmLclty = ifelse(is.na(rctGmLocality),"",rctGmLocality),
           nrmGmCllquialArea= ifelse(is.na(rctGmColloquial_area),"", rctGmColloquial_area)
           ) %>% 
    unite(nrmGmCity, nrmGmLclty, nrmGmCllquialArea, sep="") %>%
    # filter(rctGmCountry %in% c("South Africa")) %>% # Slice for testing
    select(-rctGmLocality, -rctGmColloquial_area) %>% 
    na.omit %>%
    distinct
glimpse(REM2geoLocationGroupsAndDimensions)
save(REM2geoLocationGroupsAndDimensions, file = "REM2geoLocationGroupsAndDimensions")

m1WinNames <- REM2geoLocationGroupsAndDimensions

# Run a matrix of stringdistance in just the names
m1WinNameMatrix <- 
    stringdistmatrix(m1WinNames$gcWinningName, m1WinNames$gcWinningName, method = "jw")
# glimpse(m1WinNameMatrix)
rownames(m1WinNameMatrix) <- m1WinNames$gcWinningName
colnames(m1WinNameMatrix) <- m1WinNames$m1mg

# save(m1WinNameMatrix, file = "m1WinNameREM2Matrix_vMinColsCity")
# write.csv(m1WinNameMatrix, "m1WinNameREM2Matrix_vMinColsCity.csv", na = "")

# min_m1 function - selects the minimum original ID of all those that are
# recommended to be collected together at the specified threshold
min_m1mg <- function(m1, thresh) {
    winNames <- names(m1WinNameMatrix[m1,][m1WinNameMatrix[m1, ] < thresh])
    min(winNames)
}
# Apply min_m1 function
m1WinNames$MstrLvl_m1mg <- sapply(m1WinNames$gcWinningName, min_m1mg, .175)
glimpse(m1WinNames)

# save(m1WinNames, file = "m1WinNamesREM2_tmpMin_m1mg")
# load("m1WinNamesREM2_tmpMin_m1mg")

## get the conversion map of m1mg to MstrLvl_m1mg
# This is the list of all the stringdistance merging to the master level
# chiefly (same name same city)
# filter out only those cities that have attachedNames different from their original one 
# iow: (they have MasterLevelSuggestions in addition to their own)
glimpse(m1WinNames)
Initm1mg2MstrLvl_m1mgMap <-
    m1WinNames %>% 
    select(m1mg, MstrLvl_m1mg) %>%
    distinct %>%
    mutate(MstrLvl_m1mg = as.numeric(MstrLvl_m1mg))# %>% 
    # filter(m1mg != MstrLvl_m1mg)
glimpse(Initm1mg2MstrLvl_m1mgMap)

## Deny any attempts to join across rctCity names
# Build a listing of m1mg to nrmGmCity names
## then I will use this to join on the location information for the m1mg and then after changing the key, the MstrLvlm1mg location information
## an exclusion test to screen out discrepancies will follow.
## to start I need a list of m1mg and all the CityNames that I will join to the Master level recommendations list
names(REM2geoLocationGroupsAndDimensions)
gmGroupCityLocMap<- 
    REM2geoLocationGroupsAndDimensions %>% 
    select(m1mg, rctGmCountry, nrmGmCity, stdCity, gcWinningName) %>%
    rename(MstrLvl_m1mg = m1mg, sgstGmCountry = rctGmCountry, sgstGmCity = nrmGmCity, sgstStdCity = stdCity, sgstMstrLvlName = gcWinningName) %>% #This is the key where I join these attached to the MstrLvlRecommedations
    #later I will filter out any strange that remain
    # but by definition there really ought not to be any oddities included
    # filter(rctGmLocality == "Nagoya-shi") %>% # testing code
    distinct
glimpse(gmGroupCityLocMap)

# Compiling the masterLevel suggestions and dimensions with the original data
## Join the MasterLevelsuggestions to the original data
FullREM2geoClstrdStrngDistMasterLvlClusterIDs <- 
    clusterREM2RejoinedFullPlus %>% 
    left_join(Initm1mg2MstrLvl_m1mgMap)
glimpse(FullREM2geoClstrdStrngDistMasterLvlClusterIDs)
## Join the masterLevel dimensions to the above full and suggestions
FullREM2geoClstrdStrngDistMasterLvlIDsAndClusterNameAndLocation <- 
    FullREM2geoClstrdStrngDistMasterLvlClusterIDs %>% 
    left_join(gmGroupCityLocMap) %>%
    # filter(rctGmCountry == "South Africa") %>% 
    distinct
glimpse(FullREM2geoClstrdStrngDistMasterLvlIDsAndClusterNameAndLocation)

# Screen down the suggestions to only allow those in the same nrmGmCity or stdCity to work
REM2geoClstrdStrngDistMasterLvlSuggestionsFlagged <- 
    FullREM2geoClstrdStrngDistMasterLvlIDsAndClusterNameAndLocation %>% 
    mutate(flgSameCity = ifelse(sgstGmCity == rctGmLocality, 1,
                         ifelse(sgstGmCity == rctGmColloquial_area, 1,
                         ifelse(sgstStdCity == stdCity, 1,
                                0)))
           )
glimpse(REM2geoClstrdStrngDistMasterLvlSuggestionsFlagged)

#Build corrMstrLvl_m1mg and corrMstrLvlName
REM2geoClstrdStrngDistMasterLvlAccepted <- 
    REM2geoClstrdStrngDistMasterLvlSuggestionsFlagged %>% 
mutate(corrMstrLvl_m1mg = ifelse(flgSameCity != 1 | is.na(flgSameCity), m1mg, MstrLvl_m1mg),
           corrMstrLvlName = ifelse(flgSameCity != 1 | is.na(flgSameCity), gcWinningName, sgstMstrLvlName)
           )
glimpse(REM2geoClstrdStrngDistMasterLvlAccepted)
#16,715 for REM2ainderGroup
write.csv(REM2geoClstrdStrngDistMasterLvlAccepted, "REM2geoClstrdStrngDistMasterLvlAccepted.csv", row.names = F, na = "")

#platWinningWinningNames - Summary to tie back in later (to avoid the lengthy Mutate time)
SmrzdplatWinningWinningNameCountList <-
    REM2geoClstrdStrngDistMasterLvlAccepted %>% 
    select(platLocID = corrMstrLvl_m1mg, platName = corrMstrLvlName, gcWinningName) %>% 
    distinct %>% 
    na.omit %>% 
    group_by(platLocID) %>% 
    summarize(count = n_distinct(gcWinningName))
glimpse(SmrzdplatWinningWinningNameCountList)
#platWinningWinningNames - Summary to export
platWinningWinningNameListSummary <-
    REM2geoClstrdStrngDistMasterLvlAccepted %>% 
    select(platLocID = corrMstrLvl_m1mg, platName = corrMstrLvlName, gcWinningName) %>% 
    left_join(SmrzdplatWinningWinningNameCountList) %>% 
    ungroup %>% 
    arrange(desc(count), platName) %>% 
    distinct
glimpse(platWinningWinningNameListSummary)
save(platWinningWinningNameListSummary, file = "platREM2WinningWinningNameListSummary")
### ### Shunt ### ### ###
# shunt added to skip the name reviewing step
## the name below should really not include Reviewed 
write.csv(platWinningWinningNameListSummary, "platREM2WinningWinningNameListSummaryReviewed.csv", row.names = F, na = "")
## ## ## ## Review the above files where count >1 
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# and then
# save the file under same name with "Reviewed" added to the end
# SpecificallY: (Step Out and choose the prefered names
# Clear the decks
rm(list = ls())
# read the winningNames back in
dir()
platWinningWinningNameListReviewed <- read.csv("platREM2WinningWinningNameListSummaryReviewed.csv", stringsAsFactors = F)
glimpse(platWinningWinningNameListReviewed)

# prep the winningName list for insertion
platWinningWinningNameListReviewed %<>% 
    mutate(corrMstrLvl_m1mg = platLocID, platName) %>% 
    select(corrMstrLvl_m1mg, platName)
glimpse(platWinningWinningNameListReviewed)

REM2geoClstrdStrngDistMasterLvlAccepted <- read.csv("REM2geoClstrdStrngDistMasterLvlAccepted.csv", stringsAsFactors = F, na.strings = c("", " ", "NA"))

glimpse(REM2geoClstrdStrngDistMasterLvlAccepted)
#join the WinningWinningNameList    
MasterREM2PlatinumclusterData <- 
    REM2geoClstrdStrngDistMasterLvlAccepted %>% 
    left_join(platWinningWinningNameListReviewed) %>% 
    mutate(platID = corrMstrLvl_m1mg)
glimpse(MasterREM2PlatinumclusterData)

write.csv(MasterREM2PlatinumclusterData, "MasterREM2PlatinumclusterData.csv", row.names = F, na = "")
