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
FormulaText <- paste0("Mass = ", formatC(a, format = "e", digits = 2),
                      " * TL ^ ", round(b, 2))

LWPlot <- ggplot(LengthWeightData, aes(x = TL, y = WT)) +
  geom_point() +
  labs(x = "Total Length (mm)", y = "Mass (g)", 
       title = paste0(StudyBackwater, " length vs weight"))

LWPlot <- LWPlot + 
  geom_line(data = PredictedDF, aes(x = TL, y = Mass), color = "red")

LWPlot <- LWPlot + annotate("text", x = min(LengthWeightData$TL), 
                            y = max(LengthWeightData$WT), 
                            label = FormulaText, hjust = 0, vjust = 1)

rm(a, b, FormulaText)
CurrentDataFrames <- ls()[vapply(ls(), function(x) is.data.frame(get(x)), logical(1))]

# Find the dataframes that were created by the script
NewDataFrames <- setdiff(CurrentDataFrames, CoreDataFrames)

# Remove the new dataframes
rm(list = NewDataFrames)
