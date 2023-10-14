# B. Kesner October 3, 2023
# Script to produce scanning proficiency analysis for a backwater
packages(lubridate)

# Generalize the stalwart analysis so that the last date available to the 
# analysis is the date with at least 20 known stalwarts
StalwartInfo <- data.frame(LastDate = unique(NFWGTableBW$LastScan)) %>%
  filter(!is.na(LastDate)) %>%
  mutate(key = 1) %>%
  full_join(NFWGTableBW %>%
              filter(CollectionDate == min(CollectionDate))  
            %>% mutate(key = 1), by = "key", relationship = "many-to-many") %>%
  select(-key) %>%
  filter(LastScan >= LastDate) %>%
  group_by(LastDate) %>%
  summarise(Count = n_distinct(PIT1)) %>%
  filter(Count >= 20) %>%
  filter(LastDate == max(LastDate))

StalwartDate <- StalwartInfo$LastDate[1]
StalwartCount <- StalwartInfo$Count[1]

rm(StalwartInfo)

StalwartNFWG <- NFWGTableBW %>% 
  filter(CollectionDate == min(CollectionDate), 
         LastScan >= StalwartDate)

StalwartContacts <- ContactsBW %>%
  filter(Date<StalwartDate) %>%
  select(PIT, PITIndex, Date, UnitType) %>%
  mutate(UnitType =  tolower(UnitType)) %>%
  inner_join(StalwartNFWG, by = c("PIT" = "PIT1")) %>%
  group_by(UnitType, Date, PITIndex) %>%
  summarise(Contacts = n()) %>%
  ungroup()

StalwartDailyContacts <- StalwartContacts %>%
  group_by(Date, UnitType) %>%
  summarise(Contacts = sum(Contacts), 
            Uniques = n_distinct(PITIndex),
            Proportion = Uniques/StalwartCount) %>%
  ungroup () 

StalwartMonthlyContacts <- StalwartContacts %>%
  arrange(Date) %>%
  mutate(Year = year(Date), Month = month(Date)) %>%
  group_by(UnitType, Year, Month) %>%
  summarise(Contacts = sum(Contacts), 
            Uniques = n_distinct(PITIndex),
            Proportion = Uniques/StalwartCount) %>%
  ungroup () 

AllContacts <- ContactsBW %>%
  filter(Date<StalwartDate) %>%
  select(PITIndex, Date, UnitType) %>%
  mutate(UnitType =  tolower(UnitType)) %>%
  group_by(UnitType, Date, PITIndex) %>%
  summarise(Contacts = n()) %>%
  ungroup()

AllContactsDaily <- AllContacts %>%
  group_by(Date, UnitType) %>%
  summarise(Contacts = sum(Contacts), 
            Uniques = n_distinct(PITIndex)) %>%
  ungroup () 

AllContactsMonthly <- AllContacts %>%
  arrange(Date) %>%
  mutate(Year = year(Date), Month = month(Date)) %>%
  group_by(UnitType, Year, Month) %>%
  summarise(Contacts = sum(Contacts), 
            Uniques = n_distinct(PITIndex)) %>%
  ungroup () 
# Expands all efforts to create an effort record per day of deployment
# calculates partial day efforts at start and end dates of deployments
# and totals the "scan day" effort for each date for which there is an deployment 
# by unit type
DailyEffort <- EffortBW %>% 
  filter(Issue != "Y") %>%
  select(EID, Deploy, Retrieve, UnitType) %>%
  mutate(StartDate = as.Date(Deploy), EndDate = as.Date(Retrieve),
         UnitType =  tolower(UnitType)) %>%
  tidyr::uncount(weights = as.integer(EndDate - StartDate) + 1,.remove = FALSE) %>%
  group_by(EID) %>%
  mutate(Date = StartDate + row_number() - 1) %>%
  ungroup() %>%
  mutate(ScanDay = ifelse(Date == StartDate, 
                          difftime(ceiling_date(Date +1), Deploy, units = "days"), 
                          ifelse(Date == EndDate, 
                                 difftime(Retrieve, floor_date(Date), units = "days"),1))) %>%
  group_by(Date, UnitType) %>%
  summarise(ScanDays = as.numeric(sum(ScanDay))) %>%
  ungroup() %>%
  filter(Date < StalwartDate, Date > min(StalwartNFWG$CollectionDate)) #%>%
  #pivot_wider(names_from = UnitType, values_from = ScanDays)


BWScanDates <- expand.grid(Date = seq(min(DailyEffort$Date), 
                                    StalwartDate,
                                    by = "day"),
                          UnitType = as.character(unique(DailyEffort$UnitType))) %>%
  left_join(DailyEffort, by = c("Date", "UnitType"))

BWScanDates[is.na(BWScanDates)] <- 0

rm(DailyEffort)

BWScanMonths <- BWScanDates %>%
  arrange(UnitType, Date) %>%
  mutate(Year = year(Date), Month = month(Date)) %>%
  group_by(UnitType, Year, Month) %>%
  summarise(ScanDays = sum(ScanDays)) %>%
  ungroup() %>%
  left_join(StalwartMonthlyContacts %>% 
              select(-Contacts), by = c("UnitType", "Year", "Month")) %>%
  rename(StalwartN = Uniques, StalwartProp = Proportion) %>%
  left_join(AllContactsMonthly %>% select(-Contacts), 
            by = c("UnitType", "Year", "Month")) %>%
  mutate(AllEstimate = ifelse(StalwartProp > 0, Uniques/StalwartProp, 0),
         YearMonth = as.Date(paste0(Year, "-", Month, "-01")))

BWScanMonths[is.na(BWScanMonths)] <- 0

BWScanMonths$MonthName <- factor(month.name[BWScanMonths$Month], levels = month.name)

# Effort for months greater than zero scanning effort
BWScanMonthsE <- BWScanMonths %>%
  filter(ScanDays>0)

BWMonthlyEffortBox <-  ggplot(BWScanMonthsE, aes(x = MonthName, y = ScanDays)) +
  geom_boxplot(fill = "grey") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

BWMonthlyEffortPlot <- ggplot(BWScanMonthsE, aes(x = YearMonth, y = ScanDays, fill = UnitType)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


StalwartPropBox <-  ggplot(BWScanMonthsE, aes(x = MonthName, y = StalwartProp)) +
  geom_boxplot(fill = 'grey') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Months were nearly all Stalwarts were scanned.
BWScanPopulation <- BWScanMonths %>%
  filter(StalwartProp >= .20) %>%
  group_by(Year) %>%
    summarise(MeanTaggedPop = mean(AllEstimate), 
              SD = sd(AllEstimate),
              Count = n()) %>%
  ungroup() %>%
  mutate(TaggedSE = SD/sqrt(Count)) %>%
  select(-SD, -Count) %>%
  inner_join(Size2PlusAutumn, by = "Year") %>%
  mutate(AllEstimate = MeanTaggedPop/RecaptureProp, 
         Lower = (MeanTaggedPop/CI95Low),
         Upper = (MeanTaggedPop/CI95High))

BWPopulationPlot <- ggplot(BWScanPopulation, aes(x = Year)) +
  geom_point(aes(y = AllEstimate)) +
  geom_smooth(aes(y = AllEstimate))

CurrentDataFrames <- ls()[vapply(ls(), function(x) is.data.frame(get(x)), logical(1))]

# Find the dataframes that were created by the script
NewDataFrames <- setdiff(CurrentDataFrames, CoreDataFrames)

# Remove the new dataframes
rm(list = NewDataFrames)
