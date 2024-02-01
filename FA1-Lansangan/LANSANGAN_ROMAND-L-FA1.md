Formative Assessment 1
================

#### Probability and Probability Distribution

#### By Romand Lansangan

# 1) Pearson’s Approximation Report

Pearson has given an approximate formula for the skewness that is easier
to calculate than the exact formula
$(\text{skew} = \frac{3(\text{mean} - \text{median})}{\text{standard deviation}})$.
In the following report, it is in the best interest of the author to
check its validity and determine if Pearson’s approximation gives a
reasonable estimation.

## The Data

The data file contains examination results for a class of 119 students
pursuing a computing degree. The data is given as a text file called
[results.txt](https://www.wiley.com/go/Horgan/probabilitywithr2e%20containing%20a%20results.txt)(the
link was provided in the book but not available as of moment). The data
was converted by the author to a CSV file
[results.csv](https://github.com/RomandRapido/APM1110/blob/main/FA1-Lansangan/results.csv)
for convenience in reading and analyzing.

![](https://github.com/RomandRapido/APM1110/blob/main/FA1-Lansangan/images/table1.png?raw=true)

The CSV file contains 5 columns: gender, arch1, prog1, arch2, and prog2.
The gender column contains the gender (‘f’ or ‘m’) of the student, and
the rest of the columns contain integer values ranging from 3-100,
serving as the scores of the students in their respective subjects.

It is worth noting that there appear to be NA values in some entries in
the CSV file. Such data were omitted during the calculation process, but
this does not affect the results in any way.

## Methodology

Importing of necessary packages:

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(moments)
library(e1071)
```

    ## 
    ## Attaching package: 'e1071'
    ## 
    ## The following objects are masked from 'package:moments':
    ## 
    ##     kurtosis, moment, skewness

The author uses RStudio and R language to calculate and graph the whole
process. The author began by calculating the mean, median, and standard
deviation (sd) for each column.

``` r
results <- read.csv("results.csv", header = TRUE)
meanResults <- sapply(results[c(2:5)], mean, na.rm = TRUE)
medianResults <- sapply(results[c(2:5)], median, na.rm = TRUE) |> as.numeric()
sdResults <- sapply(results[c(2:5)], sd, na.rm = TRUE)
```

![](https://github.com/RomandRapido/APM1110/blob/main/FA1-Lansangan/images/table2.png?raw=true)

Then the author wrote a function for calculating skewness using
Pearson’s Approximation.

### Custom function using Pearson’s formula:

``` r
names(medianResults) <- colnames(results[c(2:5)])
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
```

![](https://github.com/RomandRapido/APM1110/blob/main/FA1-Lansangan/images/table3.png?raw=true)

Then to calculate the validity of the results, the author imported and
used two libraries that has a skewness calculator method. The two
libraries are “e1071” and “moments.”

``` r
skewResultsPackages <- sapply(results[, columnNames], function(x) c(moments = moments::skewness(x, na.rm = TRUE), e1071 = e1071::skewness(x, na.rm = TRUE)))
```

![](https://github.com/RomandRapido/APM1110/blob/main/FA1-Lansangan/images/table4.png?raw=true)

Using “ggplot2” from “Tidyverse” package, the author used bar graph to
visualize the discrepancies between all methods—or the lack thereof.

``` r
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
```

    ##                    Method Subject   Skewness
    ## Skewness1 Pearson Approx.   arch1 -0.6069042
    ## Skewness2 Pearson Approx.   prog1 -0.6432290
    ## Skewness3 Pearson Approx.   arch2  0.5421286
    ## Skewness4 Pearson Approx.   prog2 -0.3562908
    ## arch1             Moments   arch1 -0.5129462
    ## prog1             Moments   prog1 -0.3334265
    ## arch2             Moments   arch2  0.4481600
    ## prog2             Moments   prog2 -0.3018269
    ## arch11              e1071   arch1 -0.5063276
    ## prog11              e1071   prog1 -0.3291610
    ## arch21              e1071   arch2  0.4423272
    ## prog21              e1071   prog2 -0.2977574

``` r
ggplot(visData, aes(x = Subject, y = Skewness, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  labs(title = "Comparison of Skewness Values Across Different Methods", x = "Subject", y = "Skewness")
```

![](LANSANGAN_ROMAND-L-FA1_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Interpretation:

Analysing the skewness given by different methods it could be observed
that most of them gives an indication of fairly symmetrical dataset. As
a rule of thumb, skewness between -0.5 to 0.5 means that the data are
fairly symmetrical while between -1 and -0.5 or 1 and 0.5 gives an
impression that the data are moderately skewed. It is worth noting that
negative symbol in skewness means that the distribution of data is
longer on the left side of its peak and vice-versa. With the exception
of “arch1”, when the average skewness was calculated, all columns fall
under fairly skewed—and not to mention the direction of each variable
are the same for all methods.

![](https://github.com/RomandRapido/APM1110/blob/main/FA1-Lansangan/images/table5.png?raw=true)

The Pearson skewness approximation for “arch1,” “prog2,” and “arch2” are
relatively close to the values obtained from the “Moments” and “e1071”
packages. This suggests that it is indeed reasonable when it comes to
the said subjects/column. However, the disparaty becomes apparent when
it comes to the subject of “prog1” for it almost double the skewness
from the imported packages. The large discrepancy suggests that
Pearson’s approximation might not be as reasonable for this variable.
Not this begs the question as why that’s the case—but that could
explored further with using other statistical methods and
assumptions—which are all outside of the scope of this report.

Nevertheless, in general, the magnitude and the direction of the
skewness from Pearson’s approximation matched those from “moments” and
“e1071” library therefore indicating that it is indeed reasonable—but
one should be aware of the assumptions and when to not use the said
approximation due to the instances such as those data under “prog1”
column.

# 2) Class Score Distribution Report

## Data:

``` r
femaleScores <- c(57, 59, 78, 79, 60, 65, 68, 71, 75, 48, 51, 55, 56, 41, 43,
                  44, 75, 78, 80, 81, 83, 83, 85)
maleScores <- c(48, 49, 49, 30, 30, 31, 32, 35, 37, 41, 86, 42, 51, 53, 56,
                42, 44, 50, 51, 65, 67, 51, 56, 58, 64, 64, 75)
```

## 2A:

### Instruction:

Form the stem-and-leaf display for each gender and discuss the
advantages of this representation compared to the traditional histogram.

### Answer:

As a reference, here are stem-and-leaf and histogram representation of
the data given;

A custom function that takes the stems and leaves of the values inside
vector then returns them in a list:

``` r
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

print(stemLeafDataList(femaleScores))
```

    ## $`5`
    ## [1] 7 9 1 5 6
    ## 
    ## $`7`
    ## [1] 8 9 1 5 5 8
    ## 
    ## $`6`
    ## [1] 0 5 8
    ## 
    ## $`4`
    ## [1] 8 1 3 4
    ## 
    ## $`8`
    ## [1] 0 1 3 3 5

``` r
print(stemLeafDataList(maleScores))
```

    ## $`4`
    ## [1] 8 9 9 1 2 2 4
    ## 
    ## $`3`
    ## [1] 0 0 1 2 5 7
    ## 
    ## $`8`
    ## [1] 6
    ## 
    ## $`5`
    ## [1] 1 3 6 0 1 1 6 8
    ## 
    ## $`6`
    ## [1] 5 7 4 4
    ## 
    ## $`7`
    ## [1] 5

A function to display the plotted stem and leaf:

``` r
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
```

As a reference, here are stem-and-leaf and histogram representation of
the data given;

#### Hisogram:

``` r
par(mfrow=c(1,2), mar=c(5.1, 4.1, 4.3, 2.1))
hist(femaleScores, xlab = "Score", main="Female Scores", col = c("#f8766d", "#00ba38", "#619cff"))
hist(maleScores, xlab = "Score", main="Male Scores", col = c("#f8766d", "#00ba38", "#619cff"))
mtext("Distribution of Scores", side = 3, line = -1.5, outer = TRUE, cex = 1.5)
```

![](LANSANGAN_ROMAND-L-FA1_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

#### Stem-and-leaf:

``` r
maleScoresList <- stemLeafDataList(maleScores)
femaleScoresList <- stemLeafDataList(femaleScores)
par(mfrow=c(1,2), mar=c(5.1, 4.1, 4.3, 2.1))
plotStemLeaf(femaleScoresList, main = "Female Scores", col = "#f8766d")
plotStemLeaf(maleScoresList, main = "Male Scores", col = "#619cff")
mtext("Distribution of Scores", side = 3, line = -1.5, outer = TRUE, cex = 1.5)
```

![](LANSANGAN_ROMAND-L-FA1_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

The primary advantage of doing a stem-and-leaf plot instead of a
histogram is that stem-and-leaf shows the specific data instead of
clustering them in a certain range of numbers. In this way, one can get
a good grasp as to what exactly are the numbers being plotted. In a
histogram, although it is much easier to absorb, it may not be a good
representation when one wants to know what exact number is being
repeated. For example, on the “7” cluster of the female scores. If one
looks at the histogram, one can be led to think that students scored
70-79 in the examination—which is true but a rather short-sighted and
tunneled perception for all but one are ranging from 75-79. This could
be solved by shrinking the size of each cluster of histogram but the
fact remains that if one wants to get into the detail (for whatever
reason), he/she can do so using steam-and-leaf representation.

## 2B:

### Instruction:

Construct a box-plot for each gender and discuss the findings.

### Answer:

Putting the data in a data frame

``` r
dataHere <- data.frame(gender = as.factor(character(0)), score = integer(0))

for (i in 1:length(femaleScores)) {
  dataHere <- rbind(dataHere, data.frame(gender = factor("f"), score = femaleScores[i]))
}

for (i in 1:length(maleScores)) {
  dataHere <- rbind(dataHere, data.frame(gender = factor("m"), score = maleScores[i]))
}
print(dataHere)
```

    ##    gender score
    ## 1       f    57
    ## 2       f    59
    ## 3       f    78
    ## 4       f    79
    ## 5       f    60
    ## 6       f    65
    ## 7       f    68
    ## 8       f    71
    ## 9       f    75
    ## 10      f    48
    ## 11      f    51
    ## 12      f    55
    ## 13      f    56
    ## 14      f    41
    ## 15      f    43
    ## 16      f    44
    ## 17      f    75
    ## 18      f    78
    ## 19      f    80
    ## 20      f    81
    ## 21      f    83
    ## 22      f    83
    ## 23      f    85
    ## 24      m    48
    ## 25      m    49
    ## 26      m    49
    ## 27      m    30
    ## 28      m    30
    ## 29      m    31
    ## 30      m    32
    ## 31      m    35
    ## 32      m    37
    ## 33      m    41
    ## 34      m    86
    ## 35      m    42
    ## 36      m    51
    ## 37      m    53
    ## 38      m    56
    ## 39      m    42
    ## 40      m    44
    ## 41      m    50
    ## 42      m    51
    ## 43      m    65
    ## 44      m    67
    ## 45      m    51
    ## 46      m    56
    ## 47      m    58
    ## 48      m    64
    ## 49      m    64
    ## 50      m    75

#### Box-plot:

``` r
par(mfrow=c(1,1))
levels(dataHere$gender) <- c("Female", "Male")
bp <-boxplot(score ~ gender, data = dataHere, ylab = "Score",xlab = "Gender", col = c("#f8766d", "#619cff"), main="Distribution of Scores")
points(bp$group, bp$out, pch = 19, col = "black")
```

![](LANSANGAN_ROMAND-L-FA1_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

Upon analyzing the box-plot for each gender, it can be clearly observed
that females, as a cluster, performed better compared to their male
counterparts—as evident by their higher median score (the line in the
middle of the box). The vertical size of each box indicated that there
exists a greater spread of scores around the median among females
compared to males. Excluding the outliers, the male scores have a lower
minimum and a lower maximum as was shown by the length of the whiskers
(the lines extending from the boxes). As for the outlier, there appears
to be an outlier from the male cluster which marks that there exists one
male who scored exceptionally high, surpassing the general performance
of his peers. To summarize, the box=plot showed that females appear to
have performed better overall compared to males in this specific test
with a notable high-scoring male outlier. However, take the word
“better” with a grain of salt for further statistical testing should be
done to confer this term to the data—“better” should be understood as
descriptive rather than inferential observation.

### Additional:

If you are wondering what function did I use to produce the table on
item1 (although the return value is in html code which bums a little but
thou shall get the point),

``` r
summaryTable <- data.frame(Subject = columnNames, Mean = meanResults[columnNames], Median = medianResults[columnNames], SD = sdResults[columnNames], SkewnessPearson = skewResultsPearson, SkewnessMoments = skewResultsPackages["moments",], SkewnessE1071 = skewResultsPackages["e1071",])
summaryTable
```

    ##       Subject     Mean Median       SD   Skewness SkewnessMoments SkewnessE1071
    ## arch1   arch1 63.56897   68.5 24.37469 -0.6069042      -0.5129462    -0.5063276
    ## prog1   prog1 59.01709   64.0 23.24012 -0.6432290      -0.3334265    -0.3291610
    ## arch2   arch2 51.97391   48.0 21.99061  0.5421286       0.4481600     0.4423272
    ## prog2   prog2 53.78378   57.0 27.08082 -0.3562908      -0.3018269    -0.2977574

``` r
colnames(summaryTable)[5] <- "SkewnessPearson"


library(kableExtra)
```

    ## 
    ## Attaching package: 'kableExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

``` r
basic_table <- kable(summaryTable, "html")

styled_table <- basic_table %>%
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover", "condensed"),
    position = "center"
  ) %>%
  row_spec(0, bold = TRUE, color = "white", background = "#3498db") %>%
  column_spec(1, background = "#ecf0f1", color = "#2c3e50", bold = TRUE)

print(styled_table)
```

    ## <table class="table table-striped table-hover table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
    ##  <thead>
    ##   <tr>
    ##    <th style="text-align:left;font-weight: bold;color: white !important;background-color: rgba(52, 152, 219, 255) !important;">   </th>
    ##    <th style="text-align:left;font-weight: bold;color: white !important;background-color: rgba(52, 152, 219, 255) !important;"> Subject </th>
    ##    <th style="text-align:right;font-weight: bold;color: white !important;background-color: rgba(52, 152, 219, 255) !important;"> Mean </th>
    ##    <th style="text-align:right;font-weight: bold;color: white !important;background-color: rgba(52, 152, 219, 255) !important;"> Median </th>
    ##    <th style="text-align:right;font-weight: bold;color: white !important;background-color: rgba(52, 152, 219, 255) !important;"> SD </th>
    ##    <th style="text-align:right;font-weight: bold;color: white !important;background-color: rgba(52, 152, 219, 255) !important;"> SkewnessPearson </th>
    ##    <th style="text-align:right;font-weight: bold;color: white !important;background-color: rgba(52, 152, 219, 255) !important;"> SkewnessMoments </th>
    ##    <th style="text-align:right;font-weight: bold;color: white !important;background-color: rgba(52, 152, 219, 255) !important;"> SkewnessE1071 </th>
    ##   </tr>
    ##  </thead>
    ## <tbody>
    ##   <tr>
    ##    <td style="text-align:left;font-weight: bold;color: rgba(44, 62, 80, 255) !important;background-color: rgba(236, 240, 241, 255) !important;"> arch1 </td>
    ##    <td style="text-align:left;"> arch1 </td>
    ##    <td style="text-align:right;"> 63.56897 </td>
    ##    <td style="text-align:right;"> 68.5 </td>
    ##    <td style="text-align:right;"> 24.37469 </td>
    ##    <td style="text-align:right;"> -0.6069042 </td>
    ##    <td style="text-align:right;"> -0.5129462 </td>
    ##    <td style="text-align:right;"> -0.5063276 </td>
    ##   </tr>
    ##   <tr>
    ##    <td style="text-align:left;font-weight: bold;color: rgba(44, 62, 80, 255) !important;background-color: rgba(236, 240, 241, 255) !important;"> prog1 </td>
    ##    <td style="text-align:left;"> prog1 </td>
    ##    <td style="text-align:right;"> 59.01709 </td>
    ##    <td style="text-align:right;"> 64.0 </td>
    ##    <td style="text-align:right;"> 23.24012 </td>
    ##    <td style="text-align:right;"> -0.6432290 </td>
    ##    <td style="text-align:right;"> -0.3334265 </td>
    ##    <td style="text-align:right;"> -0.3291610 </td>
    ##   </tr>
    ##   <tr>
    ##    <td style="text-align:left;font-weight: bold;color: rgba(44, 62, 80, 255) !important;background-color: rgba(236, 240, 241, 255) !important;"> arch2 </td>
    ##    <td style="text-align:left;"> arch2 </td>
    ##    <td style="text-align:right;"> 51.97391 </td>
    ##    <td style="text-align:right;"> 48.0 </td>
    ##    <td style="text-align:right;"> 21.99061 </td>
    ##    <td style="text-align:right;"> 0.5421286 </td>
    ##    <td style="text-align:right;"> 0.4481600 </td>
    ##    <td style="text-align:right;"> 0.4423272 </td>
    ##   </tr>
    ##   <tr>
    ##    <td style="text-align:left;font-weight: bold;color: rgba(44, 62, 80, 255) !important;background-color: rgba(236, 240, 241, 255) !important;"> prog2 </td>
    ##    <td style="text-align:left;"> prog2 </td>
    ##    <td style="text-align:right;"> 53.78378 </td>
    ##    <td style="text-align:right;"> 57.0 </td>
    ##    <td style="text-align:right;"> 27.08082 </td>
    ##    <td style="text-align:right;"> -0.3562908 </td>
    ##    <td style="text-align:right;"> -0.3018269 </td>
    ##    <td style="text-align:right;"> -0.2977574 </td>
    ##   </tr>
    ## </tbody>
    ## </table>
