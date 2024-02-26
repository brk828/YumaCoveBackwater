# Specific backwater known survival tables and plots
packages(tidyr)
packages(gt)

# Dataframe with just first capture or release 
FirstRecordBW <- NFWGTableBW %>% 
  filter(Recapture == "N")

# Filter for "Survivors".  Fish scanned at least once 
# SurvivalDAL days after release or first capture.  
FirstRecordSurvivors <- FirstRecordBW %>%
  filter(MaxDAL >= SurvivalDAL)

# Find the initial stock date out of available records.
InitialStockDate <- as.Date(min(FirstRecordBW$CollectionDate))

# Initial stocking records will match initial stocking date, summarized by sex
InitialStockingS <- FirstRecordBW %>%
  filter(CollectionDate == InitialStockDate) %>%
  group_by(Sex) %>%
  summarise(Count = n(), MeanTL = as.integer(mean(TL)))

# Initial stocking survivors, scanned after SurvivalDAL period post-release.
InitialKnownS <- FirstRecordSurvivors %>% filter(CollectionDate == InitialStockDate) %>%
  group_by(Sex) %>%
  summarise(Count = n(), MeanTL = as.integer(mean(TL))) %>%
  ungroup()

# Of initial stocking this is the sex breakdown of known survivors
RecentKnownS <- FirstRecordSurvivors %>% 
  filter(CollectionDate == InitialStockDate, 
         year(LastScan) == year(Sys.Date()), 
         month(LastScan) > 3) %>%
  group_by(Sex) %>%
  summarise(Count = n(), MeanTL = as.integer(mean(TL))) %>%
  ungroup()

# Summarize the current population makeup as determined through PIT scanning
CurrentKnown <- FirstRecordSurvivors %>% 
  filter(year(LastScan) == year(Sys.Date()), month(LastScan) > 3)

# Going to Pivot current known to produce table summarizing current known population
CurrentKnownPivot <- CurrentKnown %>% 
  mutate(Sex = factor(Sex, levels = rev(sort(unique(Sex)))),
         Event = factor(Event, levels = rev(sort(unique(Event)))) ) %>%
  group_by(Event, Year, MonthName, Sex) %>%
  summarise(Count = n(), `TL (mm)` = as.integer(mean(TL))) %>%
  ungroup() %>%
  rename(Month = MonthName) %>%
  pivot_wider(names_from = Sex, values_from = Count)

CurrentKnownPivot[is.na(CurrentKnownPivot)] <- 0

# Create the gt table
CurrentKnownGT <- gt(CurrentKnownPivot) 

# Create empty dataframe with dates from Jan 2013 through twice the required SurvivalDAL
# If it was set to just SurvivalDAL, fish would have only one day to be scanned to qualify
SurvivalDates <- data.frame(Date = seq(MinReleaseDate, Sys.Date() - SurvivalDAL*2, 
                                       by =  "day"))

CrossDF <- SurvivalDates %>%
  mutate(key = 1) %>%
  full_join(FirstRecordSurvivors %>% mutate(key = 1), 
            by = "key", relationship = "many-to-many") %>%
  select(-key)

KnownSurvival <- CrossDF %>% 
  filter(CollectionDate <= Date &
           LastScan >= Date) %>%
  select(Date, PIT1, CollectionDate, SizeClass, Sex, TL, WT, Status, LastScan, MaxDAL) 

rm(CrossDF)
################################################################################
######  Plotting Known Survival for sexes, total, and original stocking
KnownSurvivalPlotSex <- ggplot(KnownSurvival, aes(x = Date)) +
  geom_line(stat = "count", aes(linetype = Sex), width = 1) + 
  scale_x_date(date_breaks = "1 month", 
               labels = function(x) ifelse(month(x) == 1, format(x, "%Y"), "")) +
  theme_minimal() +
  theme(axis.ticks.x = element_line(color = "black", size = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(color = "black")) +
  labs(x = "Date", y = "Count", color = "Sex")

KnownSurvivalPlotTotal <- ggplot(KnownSurvival, aes(x = Date)) +
  geom_line(stat = "count") +
  scale_x_date(date_breaks = "1 month", 
               labels = function(x) ifelse(month(x) == 1, format(x, "%Y"), "")) +
  theme_minimal() +
  theme(axis.ticks.x = element_line(color = "black", size = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(color = "black")) +
  labs(x = "Date", y = "Count")


KnownSurvivalPlotFirstStocking <- ggplot(KnownSurvival %>% 
                                           filter(CollectionDate == InitialStockDate), 
                                         aes(x = Date)) +
  geom_line(stat = "count", aes(linetype = Sex), width = 1) + 
  scale_x_date(date_breaks = "1 month", 
               labels = function(x) ifelse(month(x) == 1, format(x, "%Y"), "")) +
  theme_minimal() +
  theme(axis.ticks.x = element_line(color = "black", size = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(color = "black")) +
  labs(x = "Date", y = "Count")


NumberOfYears <- year(Sys.Date()) - year(InitialStockDate)
AnnualSurvival <- format(round(exp(log(sum(RecentKnownS$Count)/
                                         sum(InitialKnownS$Count))/NumberOfYears),3))    

# Yearly Known survival PITs are determined by the known survivors as of December 1 each year
KnownYearly <- KnownSurvival %>%
  mutate(Month = month(Date), Year = year(Date)) %>% 
  filter(Month == 12) %>%
  arrange(Date, PIT1) %>%
  group_by(Year, PIT1) %>%
  dplyr::slice(1) %>%
  select(Year, PIT1) %>%
  summarise(Count = n())

CurrentKnownPop <- nrow(CurrentKnown)
CoreDataFrames <- append(CoreDataFrames, 
                         c("RecentKnownS", "InitialKnownS", "InitialStockingS", 
                           "KnownYearly", "CurrentKnown"))

CurrentDataFrames <- ls()[vapply(ls(), function(x) is.data.frame(get(x)), logical(1))]

# Find the dataframes that were created by the script
NewDataFrames <- setdiff(CurrentDataFrames, CoreDataFrames)

# Remove the new dataframes
rm(list = NewDataFrames)

