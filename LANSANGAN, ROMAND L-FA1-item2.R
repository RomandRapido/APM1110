library(tidyverse)
library(moments)
library(e1071)

results <- read.csv("results.csv", header = TRUE)
meanResults <- sapply(results, mean, na.rm = TRUE)
medianResults <- sapply(results, median, na.rm = TRUE) |> as.numeric()
sdResults <- sapply(results, sd, na.rm = TRUE)
names(medianResults) <- colnames(results)
columnNames <- colnames(results[, 2:5])

skewnessCalc <- function(columnNamesVar, meanDf, medianDf, sdDf) {
  skewResults <- setNames(numeric(length(columnNamesVar)), columnNamesVar)
  
  for (columnName in columnNamesVar) {
    skewness <- (3 * (meanDf[columnName] - medianDf[columnName])) / sdDf[columnName]
    skewResults[columnName] <- skewness
  }
  
  return(data.frame(Skewness = skewResults))
}

skewResultsPearson <- skewnessCalc(columnNames, meanResults, medianResults, sdResults)

skewResultsPackages <- sapply(results[, columnNames], function(x) c(moments = moments::skewness(x, na.rm = TRUE), e1071 = e1071::skewness(x, na.rm = TRUE)))

pearsonSkewnessForVis <- data.frame(Method = rep("Pearson Approx.", length(columnNames)), 
                                   Subject = columnNames, 
                                   Skewness = unlist(skewResultsPearson))

momentsSkewnessForVis <- data.frame(Method = rep("Moments", length(columnNames)), 
                                    Subject = columnNames, 
                                    Skewness = skewResultsPackages["moments", ])

e1071SkewnessForVis <- data.frame(Method = rep("e1071", length(columnNames)), 
                                  Subject = columnNames, 
                                  Skewness = skewResultsPackages["e1071", ])

visData <- rbind(pearsonSkewnessForVis, momentsSkewnessForVis, e1071SkewnessForVis)
visData

ggplot(visData, aes(x = Subject, y = Skewness, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  labs(title = "Comparison of Skewness Values Across Different Methods", x = "Subject", y = "Skewness")

summaryTable <- data.frame(Subject = columnNames, Mean = meanResults[columnNames], Median = medianResults[columnNames], SD = sdResults[columnNames], SkewnessPearson = skewResultsPearson, SkewnessMoments = skewResultsPackages["moments",], SkewnessE1071 = skewResultsPackages["e1071",])
summaryTable
colnames(summaryTable)[5] <- "SkewnessPearson"

results_no_na <- results %>% 
  select(columnNames) %>%  # select the columns you are interested in
  na.omit()  # remove rows with NA values

results_long <- results_no_na %>% 
  pivot_longer(cols = everything(), names_to = "Subject", values_to = "Value")

ggplot(results_long, aes(x = Subject, y = Value)) +
  geom_boxplot() +
  labs(title = "Box Plot for Asymmetry", x = "", y = "Value")

#making table
library(kableExtra)
tableIto <- rowMeans(summaryTable[c(5:7)])
dataFamo <- cbind(summaryTable,tableIto)
colnames(dataFamo)[8] <- "Average Skewness"
basic_table <- kable(dataFamo[c(1, 5:8)], "html")

styled_table <- basic_table %>%
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover", "condensed"),
    position = "center"
  ) %>%
  row_spec(0, bold = TRUE, color = "white", background = "#3498db") %>%
  column_spec(1, background = "#ecf0f1", color = "#2c3e50", bold = TRUE)

print(styled_table)
