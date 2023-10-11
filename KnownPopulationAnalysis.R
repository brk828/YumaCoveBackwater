# Specific backwater known survival tables and plots

# Dataframe with just first capture or release must have a scan 120 days or later to be included
FirstRecordBW <- NFWGTableBW %>% 
  filter(Recapture == "N", MaxDAL >=SurvivalDAL)


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


CurrentDataFrames <- ls()[vapply(ls(), function(x) is.data.frame(get(x)), logical(1))]

# Find the dataframes that were created by the script
NewDataFrames <- setdiff(CurrentDataFrames, CoreDataFrames)

# Remove the new dataframes
rm(list = NewDataFrames)

