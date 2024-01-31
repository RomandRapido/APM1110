stemLeafDataList <- function(dataHere) {
  stems <- dataHere %/% 10
  leaves <- dataHere %% 10
  finalList <- list()
  
  for (i in 1:length(dataHere)) {
    stemName <- as.character(stems[i])
    if (stemName %in% names(finalList)) {
      finalList[[stemName]] <- c(finalList[[stemName]], leaves[i])
    } else {
      finalList[[stemName]] <- c(leaves[i])
    }
  }
  
  return (finalList)
}
plotStemLeaf <- function(stemLeafList, main = "Stem and Leaf Plot", col = "black") {
  stemOrder <- sort(as.numeric(names(stemLeafList)), decreasing = FALSE)
  
  maxY <- length(stemOrder)
  
  plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, maxY + 1), 
       xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = main)
  
  yPos <- maxY
  for (stem in stemOrder) {
    leaves <- paste(sort(as.numeric(stemLeafList[[as.character(stem)]])), collapse = "")
    text(0.1, yPos, paste(stem, "|", leaves), pos = 4, col = col)
    yPos <- yPos - 1
  }
}

femaleScores <- c(57, 59, 78, 79, 60, 65, 68, 71, 75, 48, 51, 55, 56, 41, 43,
                  44, 75, 78, 80, 81, 83, 83, 85)
maleScores <- c(48, 49, 49, 30, 30, 31, 32, 35, 37, 41, 86, 42, 51, 53, 56,
                42, 44, 50, 51, 65, 67, 51, 56, 58, 64, 64, 75)

par(mfrow=c(1,2), mar=c(5.1, 4.1, 4.3, 2.1))
hist(femaleScores, xlab = "Score", main="Female Scores", col = c("#f8766d", "#00ba38", "#619cff"))
hist(maleScores, xlab = "Score", main="Male Scores", col = c("#f8766d", "#00ba38", "#619cff"))
mtext("Distribution of Scores", side = 3, line = -1.5, outer = TRUE, cex = 1.5)


maleScoresList <- stemLeafDataList(maleScores)
femaleScoresList <- stemLeafDataList(femaleScores)
par(mfrow=c(1,2), mar=c(5.1, 4.1, 4.3, 2.1))
plotStemLeaf(femaleScoresList, main = "Female Scores", col = "#f8766d")
plotStemLeaf(maleScoresList, main = "Male Scores", col = "#619cff")
mtext("Distribution of Scores", side = 3, line = -1.5, outer = TRUE, cex = 1.5)


#item 2
dataHere <- data.frame(gender = as.factor(character(0)), score = integer(0))

for (i in 1:length(femaleScores)) {
  dataHere <- rbind(dataHere, data.frame(gender = factor("f"), score = femaleScores[i]))
}

for (i in 1:length(maleScores)) {
  dataHere <- rbind(dataHere, data.frame(gender = factor("m"), score = maleScores[i]))
}
par(mfrow=c(1,1))
levels(dataHere$gender) <- c("Female", "Male")
bp <-boxplot(score ~ gender, data = dataHere, ylab = "Score",xlab = "Gender", col = c("#f8766d", "#619cff"), main="Distribution of Scores")
points(bp$group, bp$out, pch = 19, col = "black")