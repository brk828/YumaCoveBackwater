# B. Kesner October 3, 2023
# Script to reproduce Yuma cove harvest model from 2022 protocol

# Before running any script, declare the backwater you are interested in
StudyBackwater <- "Yuma Cove backwater"

SurvivalDAL <- 120 # Day cutoff for post-stocking survival
Sp <- "XYTE" # Species of interest GIEL bonytail, XYTE razorback
MinReleaseDate <- as.Date("2013-01-01") # Limit to fish release on or after this date

# Load useful lab functions
source("LabFunctions.R")

# Load data workspace or downlod and load if more than 7 days old
if(file.exists("data/BWScanningIndex.RData")){
  data_info <- file.info("data/BWScanningIndex.RData")
  data_date <- as.Date(data_info$mtime)
  if(data_date>Sys.Date() - 7){
    load("data/BWScanningIndex.RData")
  } else {
    download_backwater("data")
    load("data/BWScanningIndex.RData")
  }
} else {
  download_backwater("data")
  load("data/BWScanningIndex.RData")
}


# Load data workspace or downlod and load if more than 7 days old
if(file.exists("data/NFWGTable.RData")){
  data_info <- file.info("data/NFWGTable.RData")
  data_date <- as.Date(data_info$mtime)
  if(data_date>Sys.Date() - 7){
    load("data/NFWGTable.RData")
  } else {
    download_nfwg("data")
    load("data/NFWGTable.RData")
  }
} else {
  download_nfwg("data")
  load("data/NFWGTable.RData")
}

# remove unnecessary functions
rm(euclid, split_hourly, download_nfwg, download_basin, download_backwater, data_date, data_info,
   Unit, TripTable, TagStocking, TagEffort)

packages(ggplot2)
packages(dplyr)     # data manipulation
packages(magrittr)  # allows use of %<>% assignment pipe
packages(glmmTMB) # General linear mixed model analysis built on TMB automatic differentiation engine
packages(lubridate)

NFWGTableBW <-   NFWGTable %>% 
  select(CollectionDate, PIT1 = First134PIT, PIT2 = Second134PIT, Location, Species, Sex, TL, Status, Recapture, 
         Method) %>%
  filter(Species == Sp, Location == StudyBackwater, CollectionDate>=MinReleaseDate) %>%
  mutate(Month = month(CollectionDate), Year = year(CollectionDate))

SpBWPITIndex <- BWPITIndex %>%
  filter(Backwater == StudyBackwater, Species == Sp) %>%
  filter(ReleaseDate>=MinReleaseDate | TaggingDate>=MinReleaseDate)

rm(NFWGTable, BWPITIndex)


# Need to do something different below:

# Join contacts with NFWG dataframe to match contacts with capture
# grouping by PITIndex ensures that fish with more than one PIT tag scanned will be counted as one fish
SpBWContacts <- BWContacts %>%
  inner_join(SpBWPITIndex %>% 
               select(PIT, PITIndex, Sex, ReleaseDate, ReleaseTL, TLCM, ReleaseFY), 
             by = "PIT") %>%
  mutate(DAL = as.integer(difftime(Date, ReleaseDate, unit = 'days'))) %>%
  group_by(PITIndex, Sex) %>%
  summarise(MaxDAL = max(DAL), Contacts = n()) %>%
  ungroup

