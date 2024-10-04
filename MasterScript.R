# Before running any script, declare the backwater you are interested in
StudyBackwater <- "Yuma Cove backwater"

# StudyBackwater <- "IPCA (Pond 4)"

SurvivalDAL <- 120 # Day cutoff for post-stocking survival
Sp <- "XYTE" # Species of interest GIEL bonytail, XYTE razorback
# MinReleaseDate <- as.Date("2013-01-01") # Limit to fish release on or after this date

# Minimum TL for Size Class 2
SizeClass2 <- 350
# Minimum TL for Size Class 3
SizeClass3 <- 500

source("DataWrangling.R")

# retain a vector of dataframes from data wrangling
CoreDataFrames <- ls()[vapply(ls(), function(x) is.data.frame(get(x)), logical(1))]


source("KnownPopulationAnalysis.R")
KnownSurvivalPlotFirstStocking # Initial stocking known survival plot
KnownSurvivalPlotSex # Stocking and recruitment known survival plot by sex
KnownSurvivalPlotTotal # Total known survival plot
print(CurrentKnownGT) # Table of current known population by marking event and sex


source("CaptureEventSummaries.R")
print(SizeClassGT) # Capture event summaries across size classes
print(Size2GT) # Size class 2+ (adult) capture event summaries by recapture status

source("LengthWeightAnalysis.R")
LWPlot # Lenght weight plot for all handling data that includes TL and weight



source("ScanDaysAnalysis.R")
BWMonthlyEffortPlot
StalwartPropBox
BWMonthlyPopulationPlot
