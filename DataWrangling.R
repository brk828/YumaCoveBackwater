# B. Kesner October 3, 2023
# Prelimnary script to create backwater specific dataframes for any analysis
# this must be run from a script with input parameters included

MinReleaseDate <- as.Date("2013-01-01") 
# Load useful lab functions
source("c:/GIT/RDependencies/LabFunctions.R")

# default values for variables if script run separately
if(!exists("StudyBackwater")){StudyBackwater <- "Yuma Cove backwater"}
if(!exists("Sp")){Sp <- "XYTE"}
if(!exists("SurvivalDAL")){SurvivalDAL <- 90}
# Minimum TL for Size Class 2
if(!exists("SizeClass2")){SizeClass2 <- 350}
# Minimum TL for Size Class 3
if(!exists("SizeClass3")){SizeClass3 <- 500}


packages(ggplot2)
packages(dplyr)     # data manipulation
packages(magrittr)  # allows use of %<>% assignment pipe
packages(glmmTMB) # General linear mixed model analysis built on TMB automatic differentiation engine
packages(lubridate) # date and time manipulation
packages(readxl) # import Excel spreadsheets
packages(openxlsx) # package openxlsx is required

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
if(file.exists("data/NFWGAnalysis.RData")){
  data_info <- file.info("data/NFWGAnalysis.RData")
  data_date <- as.Date(data_info$mtime)
  if(data_date>Sys.Date() - 7){
    load("data/NFWGAnalysis.RData")
  } else {
    download_nfwg("data")
    load("data/NFWGAnalysis.RData")
  }
} else {
  download_nfwg("data")
  load("data/NFWGAnalysis.RData")
}

# remove unnecessary functions
rm(euclid, split_hourly, download_nfwg, download_basin, download_backwater, data_date, data_info,
   Unit, TripTable, BWPITTwoBackwaters)

# If Minimum date supplied, then use that otherwise 
# find the most recent stocking into the backwater to use as 
# starting date.
if(!exists("MinReleaseDate")){
  MinReleaseDate <-NFWGAnalysis %>%
    filter(location == StudyBackwater, event == "stocking") %>%
    summarize(MaxCollectionDate = max(handling_date)) %>%
    pull(MaxCollectionDate) %>% as.Date()
}

# Restrict PITindex dataframe to study backwater only
PITIndexBW <- BWPITIndex %>%
  filter(Backwater == StudyBackwater, Species == Sp) %>%
  filter(ReleaseDate>=MinReleaseDate | TaggingDate>=MinReleaseDate)

rm(BWPITIndex)

# Clean up table, add size classes, create event and disposition fields for future NFWG table structure,
# and add a recapture field based on actual previous records instead of relying on database classification
NFWGTableBW <- NFWGAnalysis %>% 
  left_join(LocationTable %>% select(LID, Complex), by = c("location_id" = "LID")) %>%
  mutate(Backwater = ifelse(is.na(Complex), location, Complex), 
         CollectionDate = as.Date(handling_date)) %>%
  select(Backwater, CollectionDate, PIT1 = pit1, PIT2 = pit2, Species = species, Method = primary_method,
         Sex = sex, TL = total_length, WT = weight, event, disposition) %>%
  filter(Species == Sp, Backwater == StudyBackwater, CollectionDate>=MinReleaseDate)

if(StudyBackwater == "Yuma Cove backwater"){
  Harvest2024 <- read_excel('data/2024FallHarvest.xlsx', "Sheet1") %>%
    rename(PIT1 = PIT10, WT = Mass) %>%
    filter(!is.na(PIT1)) %>%
    mutate(CollectionDate = as.Date(Date), Backwater = "Yuma Cove backwater", PIT2 = NA, Method = "Netting") %>%
    select(Backwater, CollectionDate, PIT1, PIT2, Species, Method, Sex, TL, WT, event, disposition, Recapture)

HarvestPITIndex <- Harvest2024 %>% 
  filter(Recapture == "No") %>%
  mutate(PIT = PIT1, PITIndex = PIT1, Reach = 2, DateVerified = CollectionDate, FirstScan = NA, 
         StockingID = NA, ReleaseTL = NA, ReleaseDate = NA, ReleaseYear = NA, ReleaseFY = NA,
         TaggingTL = TL, TaggingDate = CollectionDate, TaggingYear = year(CollectionDate), 
         TaggingFY = ifelse(month(as.Date(CollectionDate)) > 9, year(as.Date(CollectionDate)) + 1, year(as.Date(CollectionDate))),
         TLCM = as.integer(TL*0.1)) %>%
  select(Backwater, PIT, Reach, DateVerified, FirstScan, Species, StockingID, Sex, ReleaseTL, ReleaseDate,
         ReleaseYear, ReleaseFY, TaggingTL, TaggingDate, TaggingYear, TaggingFY, TLCM, PITIndex)

Harvest2024 <- Harvest2024 %>% select(-Recapture)
  
Stocking2025 <- read_excel('data/2025Stocking.xlsx', "Sheet1") %>%
  rename(PIT1 = PIT10) %>%
  mutate(CollectionDate = as.Date(Date), Backwater = "Yuma Cove backwater", PIT2 = NA, 
         Method = "Netting", WT = NA) %>%
  select(Backwater, CollectionDate, PIT1, PIT2, Species, Method, Sex, TL, WT, event, disposition)
  
StockingPITIndex <- Stocking2025 %>%
  mutate(PIT = PIT1, PITIndex = PIT1, Reach = 2, DateVerified = CollectionDate, FirstScan = NA, 
         StockingID = NA, ReleaseTL = TL, ReleaseDate = CollectionDate, ReleaseYear =  year(CollectionDate), 
         ReleaseFY = ifelse(month(as.Date(CollectionDate)) > 9, year(as.Date(CollectionDate)) + 1, year(as.Date(CollectionDate))),
         TaggingTL = NA, TaggingDate = NA, TaggingYear = NA, TaggingFY = NA, TLCM = as.integer(TL*0.1)) %>%
  select(Backwater, PIT, Reach, DateVerified, FirstScan, Species, StockingID, Sex, ReleaseTL, ReleaseDate,
         ReleaseYear, ReleaseFY, TaggingTL, TaggingDate, TaggingYear, TaggingFY, TLCM, PITIndex)
  
  NFWGTableBW <- rbind(NFWGTableBW, Harvest2024, Stocking2025)

PITIndexBW <- rbind(PITIndexBW, HarvestPITIndex, StockingPITIndex)
}

# One tag listed as recap in database for Yuma Cove Backwater 003D772DD7
ContactsBW <- BWContacts %>% filter(Backwater == StudyBackwater) %>%
  left_join(PITIndexBW %>% select(PIT, PITIndex, DateVerified), by = "PIT") %>%
  mutate(PITIndex = ifelse(PIT == "003D772DD7", "003D772DD7", PITIndex),
         DateVerified = as.Date(DateVerified),
         ScanYear = year(Date),
         DAL = ifelse(is.na(DateVerified), 0, 
                      as.integer(difftime(Date, DateVerified, unit = 'days')))) %>%
  select(Backwater, PIT, PITIndex, Date, DateTime, ScanHr, ScanYear, UnitType, 
         DateVerified, DAL)

rm(BWContacts)

EffortBW <- BWEffort %>%
  filter(Backwater == StudyBackwater)
rm(BWEffort)

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
  dplyr::slice(1) %>%
  mutate(LastScan = as.Date(Date)) %>% 
  select(PITIndex, LastScan, ScanHr, DateVerified)

NFWGTableBW <- NFWGTableBW %>%
  mutate(CollectionDate = as.Date(CollectionDate, format = "%Y-%m-%d"), 
         Month = month(CollectionDate), 
         Year = year(CollectionDate),
         SizeClass = case_when(
           TL < SizeClass2 ~ 1,
           TL >= SizeClass2 & TL < SizeClass3 ~ 2,
           TL >= SizeClass3 ~ 3),
         Event = event,
         Disposition = disposition) %>%
  arrange(PIT1, CollectionDate) %>%
  group_by(PIT1) %>%
  mutate(Recapture = ifelse(row_number() > 1, "Y", "N")) %>%
  ungroup() %>% left_join(ContactLastBW, by = c("PIT1" = "PITIndex")) %>%
  mutate(MaxDAL = ifelse(!is.na(LastScan), 
                         as.integer(difftime(LastScan, CollectionDate, unit = 'days')), 0)) %>%
  select(-event, -disposition)


# Add month names for graphing and tables
NFWGTableBW$MonthName <- month.name[NFWGTableBW$Month]
rm(NFWGAnalysis, LocationTable)

# All records from ContactLastBW should match a first capture record in the NFWG table
if(nrow(NFWGTableBW %>% filter(Recapture == "N", !is.na(LastScan))) - nrow(ContactLastBW)!=0) {
  warning("The number of matching PIT tags in the NFWG does not match the number of records
          in ContactsLastBW.")
}

rm(BWCaptures, BWReleases, ReachTable, Zone, ContactLastBW)

wb <- createWorkbook() # creates object to hold workbook sheets
addWorksheet(wb, "ContactSummary") # add worksheet
writeData(wb, "ContactSummary", NFWGTableBW) # write dataframe
saveWorkbook(wb, paste0("output/", StudyBackwater, "_Summary_",
                        format(Sys.time(), "%Y%m%d"), ".xlsx"), overwrite = TRUE)





