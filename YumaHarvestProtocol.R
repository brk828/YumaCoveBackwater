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
packages(lubridate) # Date manipulation
packages(tidyr) # pivot_wider and other functions
packages(gt) # GT tables

# Restrict PITindex dataframe to study backwater only
PITIndexBW <- BWPITIndex %>%
  filter(Backwater == StudyBackwater, Species == Sp) %>%
  filter(ReleaseDate>=MinReleaseDate | TaggingDate>=MinReleaseDate)

rm(BWPITIndex)
# One tag listed as recap in database for Yuma Cove Backwater 003D772DD7
ContactsBW <- BWContacts %>% filter(Location == StudyBackwater) %>%
  left_join(PITIndexBW %>% select(PIT, PITIndex, DateVerified), by = "PIT") %>%
  mutate(PITIndex = ifelse(PIT == "003D772DD7", "003D772DD7", PITIndex),
         DateVerified = as.Date(DateVerified),
         ScanYear = year(Date),
         DAL = ifelse(is.na(DateVerified), 0, 
                      as.integer(difftime(Date, DateVerified, unit = 'days')))) %>%
  select(PIT, PITIndex, Date, DateTime, ScanHr, ScanYear, DateVerified, DAL)

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
  dplyr::slice(1) %>%
  mutate(LastScan = as.Date(Date)) %>% 
  select(PITIndex, LastScan, ScanHr)

# Clean up table, add size classes, create event and disposition fields for future NFWG table structure,
# and add a recapture field based on actual previous records instead of relying on database classification
NFWGTableBW <-   NFWGTable %>% 
  select(CollectionDate, PIT1 = First134PIT, PIT2 = Second134PIT, Location, Species, Sex, TL, WT, Status, 
         Method) %>%
  filter(Species == Sp, Location == StudyBackwater, CollectionDate>=MinReleaseDate) %>%
  mutate(CollectionDate = as.Date(CollectionDate, format = "%Y-%m-%d"), 
         Month = month(CollectionDate), 
         Year = year(CollectionDate),
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

# Add month names for graphing and tables
NFWGTableBW$MonthName <- month.name[NFWGTableBW$Month]
rm(NFWGTable)

# All records from ContactLastBW should match a first capture record in the NFWG table
if(nrow(NFWGTableBW %>% filter(Recapture == "N", !is.na(LastScan))) - nrow(ContactLastBW)!=0) {
  warning("The number of matching PIT tags in the NFWG does not match the number of records
          in ContactsLastBW.")
}

rm(BWCaptures, BWReleases, ReachTable, Zone, ContactLastBW)

# What is the makeup of the tagged population.  Summary statistics for the pre-harvest population over time
# For each calendar year, which fish have a marking date less than February 1 of the year and a MaxDAL over
# 120 days.  How about just a continuous trend of the number of fish (by sex) that meet these conditions
# so fish are added to the population on the day they are marked but only if they have a scanninng record 
# with at least 120 days after marking, and are removed once their last scanning date has past.

# Dataframe with just first capture or release must have a scan 120 days or later to be included
FirstRecordBW <- NFWGTableBW %>% 
  filter(Recapture == "N", MaxDAL >=SurvivalDAL)

# Create empty dataframe with dates from Jan 2013 through twice the required SurvivalDAL
# If it was set to just SurvivalDAL, fish would have only one day to be scanned to qualify
SurvivalDates <- data.frame(Date = seq(MinReleaseDate, Sys.Date() - SurvivalDAL*2, by =  "day"))

CrossDF <- SurvivalDates %>%
  mutate(key = 1) %>%
  full_join(FirstRecordBW %>% mutate(key = 1), by = "key", relationship = "many-to-many") %>%
  select(-key)

KnownSurvival <- CrossDF %>% 
  filter(CollectionDate <= Date &
          LastScan >= Date + days(SurvivalDAL)) %>%
          select(Date, PIT1, CollectionDate, SizeClass, Sex, TL, WT, Status, LastScan, MaxDAL) 

rm(CrossDF)
################################################################################
######  Plotting Known Survival for sexes, total, and original stocking
KnownSurvivalPlotSex <- ggplot(KnownSurvival, aes(x = Date)) +
  geom_line(stat = "count", aes(color = Sex)) + 
  scale_x_date(date_breaks = "1 month", 
               labels = function(x) ifelse(month(x) == 1, format(x, "%Y"), "")) +
  theme_minimal() +
  theme(axis.ticks.x = element_line(color = "black", size = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(color = "black")) +
  labs(x = "Date", y = "Count", color = "Sex",
       title = paste0("Known survivors in ", StudyBackwater, 
                      " marked before date (x-axis) and contacted at least ", 
                      SurvivalDAL, " days after date"))

KnownSurvivalPlotTotal <- ggplot(KnownSurvival, aes(x = Date)) +
  geom_line(stat = "count") +
  scale_x_date(date_breaks = "1 month", 
               labels = function(x) ifelse(month(x) == 1, format(x, "%Y"), "")) +
  theme_minimal() +
  theme(axis.ticks.x = element_line(color = "black", size = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(color = "black")) +
  labs(x = "Date", y = "Count",
       title = paste0("Known survivors in ", StudyBackwater, 
                      " based on fish marked before date (x-axis) and contacted at least ", 
                      SurvivalDAL, " days after date"))

InitialStockDate <- as.Date(min(FirstRecordBW$CollectionDate))

KnownSurvivalPlotFirstStocking <- ggplot(KnownSurvival %>% 
                                           filter(CollectionDate == InitialStockDate), aes(x = Date)) +
  geom_line(stat = "count") +
  scale_x_date(date_breaks = "1 month", 
               labels = function(x) ifelse(month(x) == 1, format(x, "%Y"), "")) +
  theme_minimal() +
  theme(axis.ticks.x = element_line(color = "black", size = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(color = "black")) +
  labs(x = "Date", y = "Count",
       title = paste0("Known survivors in ", StudyBackwater, 
                      " from initial stocking contacted at least ", 
                      SurvivalDAL, " days after date (x-axis)"))

InitialKnown <- nrow(FirstRecordBW %>% filter(CollectionDate == InitialStockDate))
RecentKnown <- nrow(FirstRecordBW %>% 
                      filter(CollectionDate == InitialStockDate, 
                             LastScan > as.Date(paste0(year(Sys.Date()),"-03-01"))))
NumberOfYears <- year(Sys.Date()) - year(InitialStockDate)
AnnualSurvival <- format(round(exp(log(RecentKnown/InitialKnown)/NumberOfYears),3))    

KnownSurvivalPlotFirstStocking <- KnownSurvivalPlotFirstStocking + 
  annotate("text", x = mean(KnownSurvival$Date), 
                            y = InitialKnown - 5, 
                            label = paste0("Post-Stocking Annual Survival = ",
                                           AnnualSurvival), hjust = 0, vjust = 1)


# Summarize the current population makeup as determined through scanning
CurrentKnownPopulation <- FirstRecordBW %>% 
  filter(year(LastScan) == year(Sys.Date()), month(LastScan) > 3)

CurrentKnownPivot <- CurrentKnownPopulation %>% 
  mutate(Sex = factor(Sex, levels = rev(sort(unique(Sex)))),
         Event = factor(Event, levels = rev(sort(unique(Event)))) ) %>%
  group_by(Event, Year, MonthName, Sex) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  rename(Month = MonthName) %>%
  pivot_wider(names_from = Sex, values_from = Count)

CurrentKnownPivot[is.na(CurrentKnownPivot)] <- 0

# Create the gt table
CurrentKnownGT <- gt(CurrentKnownPivot) 

# Summarize previouos year's population makeup as determined by scanning
LastKnownPopulation <- FirstRecordBW %>% 
  filter(year(LastScan) >= year(Sys.Date())-1, year(CollectionDate) < year(Sys.Date())-1)

LastKnownPopulationSummary <- LastKnownPopulation %>% 
  group_by(Year, Event, Sex) %>%
  summarise(Count = n())


### Capture event summaries to estimate untagged adult population (Size Class 2+)
CaptureSummary <- NFWGTableBW %>%
  filter(Event == "Capture") %>%
  group_by(Year, Month, SizeClass, Recapture, Sex) %>%
  summarise(Count = n(), meanTL = mean(TL), meanMass = mean(WT)) %>%
  ungroup()

packages(tidyr)
packages(gt)

Size2PlusPivot <- NFWGTableBW %>%
  filter(Event == "Capture", SizeClass > 1) %>%
  arrange(Year, Month) %>%
  group_by(Year, MonthName, Recapture) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  rename(Month = MonthName) %>%
  pivot_wider(names_from = Recapture, values_from = Count)

Size2PlusPivot[is.na(Size2PlusPivot)] <- 0

# Create the gt table
Size2GT <- gt(Size2PlusPivot)

SizeClassPivot <- NFWGTableBW %>%
  filter(Event == "Capture") %>%
  arrange(Year, Month) %>%
  group_by(Year, MonthName, SizeClass) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  rename(Month = MonthName) %>%
  pivot_wider(names_from = SizeClass, values_from = Count)

SizeClassPivot[is.na(SizeClassPivot)] <- 0

# Create the gt table
SizeClassGT <- gt(SizeClassPivot)


##### Length Weight analysis and figure
LengthWeightData <- NFWGTableBW %>%
  filter(!is.na(TL), !is.na(WT))

# Define the l-w model
model <- function(log_a, b) {
  a <- exp(log_a)
  predicted_weight <- a * LengthWeightData$TL^b
  -sum(dnorm(LengthWeightData$WT, mean = predicted_weight, sd = 1, log = TRUE))
}

packages(bbmle)
LengthWeightModel <- mle(model, start = list(log_a = 0, b = 1))

Estimates <- coef(LengthWeightModel)
a <- exp(Estimates["log_a"])
b <- Estimates["b"]
rm(Estimates)

# Create a sequence of lengths from min to max
Lengths <- seq(min(LengthWeightData$TL), max(LengthWeightData$TL), length.out = 100)

Predicted <- a * Lengths^b
PredictedDF <- data.frame(
  TL = Lengths,
  Mass = Predicted)
rm(Lengths, Predicted)

# Create the formula string
FormulaText <- paste0("Mass = ", round(a, 2), " * TL ^ ", round(b, 2))

LWPlot <- ggplot(LengthWeightData, aes(x = TL, y = WT)) +
  geom_point() +
  labs(x = "Total Length (mm)", y = "Mass (g)", 
       title = paste0(StudyBackwater, " length vs weight"))

LWPlot <- LWPlot + 
  geom_line(data = PredictedDF, aes(x = TL, y = Mass), color = "red")

LWPlot <- LWPlot + annotate("text", x = min(LengthWeightData$TL), 
                            y = max(LengthWeightData$WT), 
                            label = FormulaText, hjust = 0, vjust = 1)


print(SizeClassGT)
print(Size2GT)
print(CurrentKnownGT)

LWPlot
KnownSurvivalPlotFirstStocking
KnownSurvivalPlotSex
KnownSurvivalPlotTotal
