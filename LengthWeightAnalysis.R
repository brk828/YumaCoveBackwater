##### Length Weight analysis and figure
LengthWeightData <- NFWGTableBW %>%
  filter(!is.na(TL), !is.na(WT))

LengthWeightDataCount <- nrow(LengthWeightData)
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

RecentYear <- max(LengthWeightData$Year)

LWPlot <- ggplot(LengthWeightData, aes(x = TL, y = WT)) +
  geom_point(aes(color = (Year == RecentYear)), size = 2) +
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "grey")) +
  labs(x = "Total Length (mm)", y = "Mass (g)") +
  theme_minimal() +
  theme(axis.ticks.x = element_line(color = "black", size = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(color = "black")) +
  theme(legend.position="none")

LWPlot <- LWPlot + 
  geom_line(data = PredictedDF, aes(x = TL, y = Mass), size = 1, color = "blue")

LWPlot <- LWPlot + annotate("text", x = min(LengthWeightData$TL), 
                            y = max(LengthWeightData$WT), 
                            label = FormulaText, hjust = 0, vjust = 1)

png("output/LWPlot.png", width = 6, height = 4, units = 'in', res = 300)   
LWPlot
dev.off()
ggsave(filename = "output/LWPlot.png", plot = LWPlot)

rm(FormulaText)

CaptureFallRecent <- CaptureFallRecent %>%
  mutate(WT = ifelse(is.na(WT), as.integer(a * TL^b), WT)) %>%
  left_join(CurrentKnown %>% select(PIT1, Backwater), by = "PIT1") %>%
  mutate(KnownAlive = ifelse(is.na(Backwater),0,1)) %>%
  select(-Backwater)

RecentBiomassS <- CaptureFallRecent %>%
  mutate(WT = ifelse(is.na(WT), as.integer(a * TL^b), WT)) %>%
  left_join(CurrentKnown %>% select(PIT1, Backwater), by = "PIT1") %>%
  mutate(KnownAlive = ifelse(is.na(Backwater),0,1)) %>%
  select(-Backwater) %>%
  group_by(SizeClass) %>%
  summarise(`Mean TL (mm)` = as.integer(mean(TL)),
            `Biomass (kg)` = sum(WT)/1000, 
            Count = n(),
            Alive = sum(KnownAlive),
            `Alive Biomass (kg)` = sum(WT*KnownAlive)/1000)

RecentBiomassGT <- gt(RecentBiomassS)
CurrentDataFrames <- ls()[vapply(ls(), function(x) is.data.frame(get(x)), logical(1))]

# Find the dataframes that were created by the script
NewDataFrames <- setdiff(CurrentDataFrames, CoreDataFrames)

# Remove the new dataframes
rm(list = NewDataFrames)
