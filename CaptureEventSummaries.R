### Capture event summaries to estimate untagged adult population (Size Class 2+)
CaptureSummary <- NFWGTableBW %>%
  filter(Event == "Capture") %>%
  group_by(Year, Month, SizeClass, Recapture, Sex) %>%
  summarise(Count = n(), meanTL = mean(TL), meanMass = mean(WT)) %>%
  ungroup()

CaptureFallRecent <- NFWGTableBW %>%
  filter(Year == max(Year), Month > 9) %>%
  group_by(PIT1) %>%
  dplyr::slice(1) %>%
  ungroup() %>%
  select(PIT1, SizeClass, TL, WT)

TotalCaptures <- sum(CaptureSummary$Count)
packages(tidyr)
packages(gt)

# Create a table that represents "adult" fish captured during sampling events
# SizeClass 2 or greater
Size2PlusPivot <- NFWGTableBW %>%
  filter(Event == "Capture", SizeClass > 1) %>%
  arrange(Year, Month) %>%
  group_by(Year, MonthName, Month, Recapture) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = Recapture, values_from = Count)

Size2PlusPivot[is.na(Size2PlusPivot)] <- 0

Size2PlusPivot$Proportion <- round(Size2PlusPivot$Y/
                                           (Size2PlusPivot$N + Size2PlusPivot$Y), 3)

# Pull month of latest survey for each year with an autumn sample
LastMonth <- Size2PlusPivot %>%
  filter(Month>9) %>%
  group_by(Year) %>%
  summarize(Month = max(Month))

# Create the gt table
Size2GT <- gt(Size2PlusPivot %>%
                select(-Month) %>%
                rename(Month = MonthName)) %>%
  tab_spanner(label = "Recaptured",
    columns = c(Y, N))
gtsave(Size2GT, paste0("output/AdultRecaptureProportion ", StudyBackwater, ".html"))

# This dataframe pulls PITs from the last sample for each year
# used by other script to estimate total population
Size2PlusAutumn <- NFWGTableBW %>%
  filter(Event == "Capture", SizeClass > 1) %>%
  inner_join(LastMonth, by = c("Year" = "Year", "Month" = "Month")) %>%
  group_by(Year, PIT1) %>%
    summarise(Recapture = min(Recapture), Contacts = n()) %>%
  ungroup() %>% 
  left_join(KnownYearly, by = c("Year" = "Year", "PIT1" = "PIT1")) %>%
  filter(Recapture == "N"|Recapture == "Y", !is.na(Count)) %>%
  select(-Count) %>%
  group_by(Year) %>%
  summarise(Recaptures = sum(Recapture == "Y"), TotalCaptures = n(),
            RecaptureProp = Recaptures/TotalCaptures) %>%
  filter(TotalCaptures>10)

# Add confidence intervals
Size2PlusAutumn$CI95Low <- NA
Size2PlusAutumn$CI95High <- NA

for(i in 1:nrow(Size2PlusAutumn)) {
  test <- binom.test(Size2PlusAutumn$Recaptures[i], Size2PlusAutumn$TotalCaptures[i], conf.level = 0.95)
  Size2PlusAutumn$CI95High[i] <- test$conf.int[1]
  Size2PlusAutumn$CI95Low[i] <- test$conf.int[2]
}

BackwaterPopulation <- KnownYearly %>%
  group_by(Year) %>%
  summarise(Survivors = n_distinct(PIT1)) %>%
  ungroup() %>%
  inner_join(Size2PlusAutumn, by = "Year") %>%
  select(-Recaptures, -TotalCaptures) %>%
  mutate(Estimate = as.integer(Survivors/RecaptureProp), 
         CI95Low = as.integer(Survivors/CI95Low),
         CI95High = as.integer(Survivors/CI95High))

PopulationNow <- BackwaterPopulation$Estimate[BackwaterPopulation$Year == max(BackwaterPopulation$Year)]

AllYears <- data.frame(Year = seq(min(BackwaterPopulation$Year), 
                                  max(BackwaterPopulation$Year)+1, by = 1))

# Merge your data with all_years, this will create NA for years without estimates
BackwaterPopulation <- merge(AllYears, BackwaterPopulation, all.x = TRUE)

EstimatesPlot <- ggplot(BackwaterPopulation, aes(x = as.factor(Year), y = Estimate)) +
  geom_point() +
  scale_y_continuous(limits = c(0, max(BackwaterPopulation$CI95High))) +
  geom_errorbar(aes(ymin = CI95Low, ymax = CI95High), width = 0.2) +
  geom_text(aes(label = as.integer(Estimate)), hjust = -.5) +
  labs(x = "Year", y = "Estimate") +
  theme_minimal() +
  theme(axis.ticks.x = element_line(color = "black", size = 0.5),
                       panel.grid.major.x = element_blank(),
                       panel.grid.minor.x = element_blank(),
                       axis.line = element_line(color = "black"))

png(paste0("output/PopulationEstimates ", StudyBackwater, ".png"), width = 6, height = 4, units = 'in', res = 300)   
EstimatesPlot
dev.off()
# This produces a complete picture of the Size classes captured during capture events
SizeClassPivot <- NFWGTableBW %>%
  filter(Event == "Capture") %>%
  arrange(Year, Month) %>%
  group_by(Year, MonthName, SizeClass) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  rename(Month = MonthName) %>%
  pivot_wider(names_from = SizeClass, values_from = Count)

SizeClassPivot[is.na(SizeClassPivot)] <- 0

# if any column is missing, add zeros
if (!"1" %in% colnames(SizeClassPivot)) {
  SizeClassPivot$`1` <- 0
}
if (!"2" %in% colnames(SizeClassPivot)) {
  SizeClassPivot$`2` <- 0
}
if (!"3" %in% colnames(SizeClassPivot)) {
  SizeClassPivot$`3` <- 0
}

# Create the gt table
SizeClassGT <- gt(SizeClassPivot %>%
                    select(Year, Month, `1`, `2`, `3`)) %>%
  tab_spanner(label = "Size class",
              columns = c(3, 4, 5))

gtsave(SizeClassGT, paste0("output/CaptureSizeClasses ", StudyBackwater, ".html"))
CurrentKnownOld <- CurrentKnown %>% filter(Year < year(Sys.Date())-3)

CurrentDataFrames <- ls()[vapply(ls(), function(x) is.data.frame(get(x)), logical(1))]

CoreDataFrames <- append(CoreDataFrames, c("Size2PlusAutumn", "CaptureFallRecent", 
                                           "CurrentKnownOld"))
# Find the dataframes that were created by the script
NewDataFrames <- setdiff(CurrentDataFrames, CoreDataFrames)

# Remove the new dataframes
rm(list = NewDataFrames)

