# B. Kesner October 3, 2023
# Script to reproduce Yuma cove harvest model from 2022 protocol

# Before running any script, declare the backwater you are interested in
StudyBackwater <- "Yuma Cove backwater"

SurvivalDAL <- 120 # Day cutoff for post-stocking survival
Sp <- "XYTE" # Species of interest GIEL bonytail, XYTE razorback
MinReleaseDate <- as.Date("2013-01-01") # Limit to fish release on or after this date

# Minimum TL for Size Class 2
SizeClass2 <- 350
# Minimum TL for Size Class 3
SizeClass3 <- 500

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

# Restrict PITindex dataframe to study backwater only
PITIndexBW <- BWPITIndex %>%
  filter(Backwater == StudyBackwater, Species == Sp) %>%
  filter(ReleaseDate>=MinReleaseDate | TaggingDate>=MinReleaseDate)

rm(BWPITIndex)

ContactsBW <- BWContacts %>% filter(Location == StudyBackwater) %>%
  left_join(PITIndexBW %>% select(PIT, PITIndex), by = "PIT") %>%
  mutate(PITIndex = ifelse(PIT == "003D772DD7", "003D772DD7", PITIndex)) # One tag listed as recap in database

rm(BWContacts)

ContactsBWNoIndex <- ContactsBW %>%
  filter(is.na(PITIndex)) %>%
  group_by(PIT) %>%
  summarise(Contacts = n(), FirstScan = min(DateTime), LastScan = max(DateTime)) %>%
  ungroup() %>%
  arrange(desc(Contacts))

# Create dataframe of only the most recent contact of all contacts from the backwater
ContactLastBW <- ContactsBW %>% 
  filter(!is.na(PITIndex)) %>%
  arrange(PITIndex, desc(DateTime)) %>%
  group_by(PITIndex) %>%
  slice(1) %>%
  mutate(LastScan = as.Date(Date)) %>% 
  select(PITIndex, LastScan, ScanHr)

# Clean up table, add size classes, create event and disposition fields for future NFWG table structure,
# and add a recapture field based on actual previous records instead of relying on database classification
NFWGTableBW <-   NFWGTable %>% 
  select(CollectionDate, PIT1 = First134PIT, PIT2 = Second134PIT, Location, Species, Sex, TL, WT, Status, 
         Method) %>%
  filter(Species == Sp, Location == StudyBackwater, CollectionDate>=MinReleaseDate) %>%
  mutate(CollectionDate = as.Date(CollectionDate, format = "%Y-%m-%d"), Month = month(CollectionDate), Year = year(CollectionDate),
         SizeClass = case_when(
           TL < SizeClass2 ~ 1,
           TL >= SizeClass2 & TL < SizeClass3 ~ 2,
           TL >= SizeClass3 ~ 3),
         Event = ifelse(Status == "Backwater release", "Stocking", "Capture"),
         Disposition = "Released") %>%
  arrange(PIT1, CollectionDate) %>%
  group_by(PIT1) %>%
  mutate(Recapture = ifelse(row_number() > 1, "Y", "N")) %>%
  ungroup() %>% left_join(ContactLastBW, by = c("PIT1" = "PITIndex")) %>%
  mutate(MaxDAL = ifelse(!is.na(LastScan), 
                         as.integer(difftime(LastScan, CollectionDate, unit = 'days')), 0))

rm(NFWGTable)

# All records from ContactLastBW should match a first capture record in the NFWG table
if(nrow(NFWGTableBW %>% filter(Recapture == "N", !is.na(LastScan))) - nrow(ContactLastBW)!=0) {
  warning("The number of matching PIT tags in the NFWG does not match the number of records
          in ContactsLastBW.")
}

rm(BWCaptures, BWReleases, ReachTable, Zone)


