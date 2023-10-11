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

CurrentDataFrames <- ls()[vapply(ls(), function(x) is.data.frame(get(x)), logical(1))]

# Find the dataframes that were created by the script
NewDataFrames <- setdiff(CurrentDataFrames, CoreDataFrames)

# Remove the new dataframes
rm(list = NewDataFrames)