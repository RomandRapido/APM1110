Summative Assessment 1
================

#### Probability and Probability Distribution

#### By Romand Lansangan

### 1)

A company has three factories producing a product. Factory 1 produces
$x_1$ of the product, Factory 2 produces $x_2$ , and Factory 3 produces
$x_3$ , where $\Sigma_{i=1}^3x_i = 1$ . The defective rates of the
products are $y_1$ , $y_2$ , and \$y_3% , respectively, where
$\Sigma_{i=1}^3y_i = 0.12$ . Write a program (user input for $x_i$ and
$y_1$ ) to calculate the probability that a randomly selected product is
defective.

Note that your program should render prompt message to satisfy the
following conditions:

- $(0.10 \leq x_i \leq 0.40) \wedge (\Sigma_{i=1}^3x_i = 1)$
- $(0.05 \leq y_i \leq 0.10) \wedge (\Sigma_{i=1}^3y_i = 0.12)$

#### Input algorithm for x:

    repeat {
      x <- 1;
      halu_x = c()
      while (x <= 3){
        var = as.double(readline(prompt = paste("Enter the probability of Factory ", x, " producing a product: ")))
        if (0.10 <= var && var <= 0.40){
          halu_x = c(halu_x, var)
          x <- x + 1;
        }else{
          print(paste("Probability should be greater than 0.10 and less than 0.40"))
        }
      }
      sum_x <- sum(halu_x);
      if (sum_x == 1){
        break
      }else{
        print("Probabilities must add up to 1")
      }
    }

### Input algorithm for y:

    repeat {
      y <- 1;
      halu_y = c()
      while (y <= 3){
        var = as.double(readline(prompt = paste("Enter the probability of Factory ", y, " to produce a defective: ")))
        if (0.05 <= var && var <= 0.10){
          halu_y = c(halu_y, var)
          y <- y + 1;
        }else{
          print(paste("Probability should be greater than 0.10 and less than 0.40"))
        }
      }
      sum_y <- sum(halu_y);
      if (sum_y == 0.12){
        break
      }else{
        print("Probabilities must add up to 0.12")
      }
    }

Note: I commented it out for the sake of presentation in markdown
format. Nevertheless, the code does work when ran through R compiler.

We shall use the following arbitrary values to calculate for the desired
probability.

``` r
halu_x <- c(0.33, 0.33, 0.34)
halu_y <- c(0.0322, 0.0637, 0.0241)
```

``` r
data_1 <- data.frame(
  factory = c(1:3),
  prob_production = halu_x,
  prob_defective_withRespectTo_factory = halu_y
  )
data_1
```

    ##   factory prob_production prob_defective_withRespectTo_factory
    ## 1       1            0.33                               0.0322
    ## 2       2            0.33                               0.0637
    ## 3       3            0.34                               0.0241

#### Checking if the values does meet the criteria:

The sum of all x (prob_production) should be equals to 1,

``` r
sum(data_1$prob_production) == 1
```

    ## [1] TRUE

The sum of all y (prob_defective_withRespectTo_factory) should be equals
to 0.12,

``` r
tolerance <- .Machine$double.eps^0.5

sum(data_1$prob_defective_withRespectTo_factory) - 0.12 < tolerance
```

    ## [1] TRUE

Tolerance is a very small number to avoid floating-point arithmetic
issues.

``` r
tolerance
```

    ## [1] 1.490116e-08

#### Calculating for the probability of defective products among all the factories

Let $E$ be the event where a factory produces the defective product.

Let $A_1$ be the event where Factory 1 produces a product.

Let $A_2$ be the event where Factory 2 produces a product.

Let $A_3$ be the event where Factory 3 produces a product.

With that, the $E \cap A_k$ where $k \in \{1,2,3\}$ means that a product
produced from Factory $k$ is defective.

To calculate the probability of each factory producing a defection (with
respect to the whole sample space), we shall use the Multiplication Law
of Probability.

Given the event $E_1$ and $E_2$ , the Multiplication Law of Probability
is defined as the following:

$$
P(E_{1} \cap E_{2}) = P(E_{2}|E_{1})P(E_{1})
$$

Applying the Multiplication Law of Probability to the problem:

$$
P(E \cap A_1) = P(E|A_1)P(A_1)
$$

$$
P(E \cap A_2) = P(E|A_2)P(A_2)
$$

$$
P(E \cap A_3) = P(E|A_3)P(A_3)
$$

Note that $P(E|A_1)$ is the rate at which the Factory 1 produced a
defective. This goes the same for the rest of factories.

$$
P(E \cap A_1) = 0.0322 \times 0.33
$$

$$
P(E \cap A_2) = 0.0637 \times 0.33
$$

$$
P(E \cap A_3) = 0.0241 \times 0.33
$$

``` r
data_1$prob_defective_withRespectTo_all = data_1$prob_defective_withRespectTo_factory * data_1$prob_production
data_1
```

    ##   factory prob_production prob_defective_withRespectTo_factory
    ## 1       1            0.33                               0.0322
    ## 2       2            0.33                               0.0637
    ## 3       3            0.34                               0.0241
    ##   prob_defective_withRespectTo_all
    ## 1                         0.010626
    ## 2                         0.021021
    ## 3                         0.008194

$$
P(E \cap A_1) = 0.010626
$$

$$
P(E \cap A_2) = 0.021021
$$

$$
P(E \cap A_3) = 0.016147
$$

To calculate for $P(E)$ we shall use the Law of Total Probability.

The Law of Total Probability is defined as the following:

If a sample space $S$ can be partitioned into k mutually exclusive and
exhaustive events, $A_{1}$, $A_{2}$, $A_{3}$, … , $A_{k}$, then for any
event $E$:

$$
P(E) = P(A_{1})P(E|A_{1}) + P(A_{2})P(E|A_{2}) + P(A_{3})P(E|{A_{3}}) + ... P(A_{k})P(E|A_{k})
$$

Applying the Law of Total Probability to our problem,

$$
P(E) = P(E \cap A_1) + P(E \cap A_2) + P(E \cap A_3)
$$

Which is also,

$$
P(E) = 0.010626 + 0.021021 + 0.016147
$$

``` r
sum(data_1$prob_defective_withRespectTo_all)
```

    ## [1] 0.039841

The probability that a randomly selected product is defective is:

$$
P(E) = 0.047794
$$

### 2)

With your own computing experience, develop a front end to R that allows
the user

##### to input the values of a univariate discrete random variable and the associated probabilities and to obtain the mean and variance, and

    uiOption2 <- function() {
      sidebarLayout(
        sidebarPanel(
          h2("Table Data"),
          helpText("Input values and their associated probabilities for bivariate discrete random variable."),
          div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            div(
              style = "flex: 1; padding-right: 20px; display: flex; align-items: center;",
              actionButton("addX", "Add X value", style = "color: black;")
            ),
            div(
              style = "flex: 1; display: flex; align-items: center;",
              actionButton("addY", "Add Y value", style = "color: black;")
            )
          ),
          br(),
          useShinyalert(),
          textInput("removeID_2", "Remove Row with ID", placeholder = "ID to remove (eg. x1, y1)"),
          div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            div(
              style = "flex: 1; padding-right: 20px; display: flex; align-items: center;",
              actionButton("remove_2", "Remove", style = "color: red;")
            ),
            div(
              style = "flex: 1; display: flex; align-items: center;",
              actionButton("removeAll_2", "Clear All Data", style = "color: red;")
            )
          ),
          br(),
          DTOutput("dataTable_2"),
          actionButton("submit_2", "Submit Data", style = "color: green;")
        ),
        mainPanel(
          h3("For Bivariate Discrete Random Variable", align = "center"),
          h4("Marginal Probability Table"),
          DTOutput("dataTable_marginal"),
          h4("Conditional Probability Tables"),
          h5("P(Y|X)"),
          DTOutput("dataTable_conditional_x"),
          h5("P(X|Y)"),
          DTOutput("dataTable_conditional_y"),
          plotlyOutput("bivariatePlot"),
          plotlyOutput("bivariateCDFPlot")
        )
      )
    }

    serverOption1 <- function(input, output, session) {
      output$dataTable <- renderDT({
      datatable(df$data, editable = TRUE,rownames = FALSE,options = list(pageLength = 5, responsive = TRUE, autoWidth = TRUE), 
                class = 'cell-border stripe')
        }, server = FALSE)
      observeEvent(input$dataTable_cell_edit, {
        info <- input$dataTable_cell_edit
        str(info)
        i <- info$row
        j <- info$col + 1
        v <- info$value
        vAsNum <- suppressWarnings(as.numeric(v))
        if (j == 1){
          if(!any(df$data$id == v)){
              df$data[i, j] <<- DT::coerceValue(v, df$data[i, j])
          }else{
            shinyalert("id should be unique")
            output$dataTable <- renderDT({
              datatable(df$data, editable = TRUE, rownames = FALSE,options = list(pageLength = 5, responsive = TRUE, autoWidth = TRUE), 
                        class = 'cell-border stripe')
                }, server = FALSE)
          }
        }else if (j==2){
          if (!is.na(vAsNum) && vAsNum%%1 ==0){
            df$data[i, j] <<- DT::coerceValue(v, df$data[i, j])
          }else{
            shinyalert("Values here should be discrete")
            output$dataTable <- renderDT({
              datatable(df$data, editable = TRUE, rownames = FALSE,options = list(pageLength = 5, responsive = TRUE, autoWidth = TRUE), class = 'cell-border stripe')
              }, server = FALSE)
          }
        }
        else if(j==3){
          if (v <= 1 && v >= 0){
              df$data[i, j] <<- DT::coerceValue(v, df$data[i, j])
          }else{
            shinyalert("probability (P) should be 0<=P<=1")
            output$dataTable <- renderDT({
              datatable(df$data, editable = TRUE, rownames = FALSE,options = list(pageLength = 5, responsive = TRUE, autoWidth = TRUE), 
                        class = 'cell-border stripe')
              }, server = FALSE)
          }
        }else{
          df$data[i, j] <<- DT::coerceValue(v, df$data[i, j])
        }
      })

      observeEvent(input$add, {
        addThis <- data.frame(id = max(df$data$id) + 1, value = c(0), probability = c(0.00))
        df$data <-rbind(df$data, addThis)
      })
      
      observeEvent(input$remove, {
        idToBeRemoved <- input$removeID
        if (!is.null(idToBeRemoved) && idToBeRemoved != ""){
          idAsNum <- as.numeric(idToBeRemoved)
          if (!is.na(idAsNum) && idAsNum%%1==0) {
              df$data <- df$data[df$data$id != as.numeric(idToBeRemoved), ]
          }else{
            shinyalert("id should be integer")
          }
        }
      })
      
      observeEvent(input$removeAll, {
        df$data <- data.frame(id = c(0), value = c(0), probability = c(1))
      })
      
      stats <- reactiveValues(meanVal = NA, varVal = NA, updatePlots = FALSE)
      
      observeEvent(input$submit, {
        if (sum(df$data$probability) == 1){
          stats$meanVal <- mean(df$data$value, na.rm = TRUE)
          stats$varVal <- var(df$data$value, na.rm = TRUE)
          output$pdfPlot <- renderPlot({
            isolate(dfCopy <- df$data)
            ggplot(dfCopy, aes(x = value, weight = probability)) +
              geom_density(alpha = 0.5, color = "#3e2e42", fill = "#8b1ebd") +
              labs(title = "PDF: Probability Density Function",
                   x = "Value",
                   y = "Density") +
              theme_minimal() +
              theme(plot.title = element_text(hjust = 0.5),
                    panel.background = element_rect(fill = "#3f3d40"))
          })
        
          output$cdfPlot <- renderPlot({
            isolate(dfCopy <- df$data)
            dfCopy <- dfCopy[order(dfCopy$value), ]
            dfCopy$cumprob <- cumsum(dfCopy$probability) / sum(dfCopy$probability)
              ggplot(dfCopy, aes(x = value, y = cumprob)) +
                geom_area(alpha = 0.5, color = "#8b1ebd", fill = "#8b1ebd") +
                labs(title = "CDF: Cumulative Distribution Function",
                     x = "Value",
                     y = "Cumulative Probability") +
                theme_minimal() +
                theme(plot.title = element_text(hjust = 0.5),
                      panel.background = element_rect(fill = "#3f3d40"))
            })
          }else{
          shinyalert("the sum of all probability should be 1") 
        }
      })
      
      output$mean <- renderText({
        stats$meanVal<- mean(df$data$value, na.rm = TRUE)
      })
      
      output$var <- renderText({
        stats$varVal <- var(df$data$value, na.rm = TRUE)
      })
      
      isolate({
        output$pdfPlot <- renderPlot({
            isolate(dfCopy <- df$data)
            ggplot(dfCopy, aes(x = value, weight = probability)) +
              geom_density(alpha = 0.5, color = "#3e2e42", fill = "#8b1ebd") +
              labs(title = "PDF: Probability Density Function",
                   x = "Value",
                   y = "Density") +
              theme_minimal() +
              theme(plot.title = element_text(hjust = 0.5),
                    panel.background = element_rect(fill = "#3f3d40"))
          })
        
          output$cdfPlot <- renderPlot({
            isolate(dfCopy <- df$data)
            dfCopy <- dfCopy[order(dfCopy$value), ]
            dfCopy$cumprob <- cumsum(dfCopy$probability) / sum(dfCopy$probability)
              ggplot(dfCopy, aes(x = value, y = cumprob)) +
                geom_area(alpha = 0.5, color = "#8b1ebd", fill = "#8b1ebd") +
                labs(title = "CDF: Cumulative Distribution Function",
                     x = "Value",
                     y = "Cumulative Probability") +
                theme_minimal() +
                theme(plot.title = element_text(hjust = 0.5),
                      panel.background = element_rect(fill = "#3f3d40"))
            })
      })
    }

#### to input the values of a bivariate discrete random variable and the associated probabilities and to obtain the marginal and conditional distributions.

    uiOption2 <- function() {
      sidebarLayout(
        sidebarPanel(
          h2("Table Data"),
          helpText("Input values and their associated probabilities for bivariate discrete random variable."),
          div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            div(
              style = "flex: 1; padding-right: 20px; display: flex; align-items: center;",
              actionButton("addX", "Add X value", style = "color: black;")
            ),
            div(
              style = "flex: 1; display: flex; align-items: center;",
              actionButton("addY", "Add Y value", style = "color: black;")
            )
          ),
          br(),
          useShinyalert(),
          textInput("removeID_2", "Remove Row with ID", placeholder = "ID to remove (eg. x1, y1)"),
          div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            div(
              style = "flex: 1; padding-right: 20px; display: flex; align-items: center;",
              actionButton("remove_2", "Remove", style = "color: red;")
            ),
            div(
              style = "flex: 1; display: flex; align-items: center;",
              actionButton("removeAll_2", "Clear All Data", style = "color: red;")
            )
          ),
          br(),
          DTOutput("dataTable_2"),
          actionButton("submit_2", "Submit Data", style = "color: green;")
        ),
        mainPanel(
          h3("For Bivariate Discrete Random Variable", align = "center"),
          h4("Marginal Probability Table"),
          DTOutput("dataTable_marginal"),
          h4("Conditional Probability Tables"),
          h5("P(Y|X)"),
          DTOutput("dataTable_conditional_x"),
          h5("P(X|Y)"),
          DTOutput("dataTable_conditional_y"),
          plotlyOutput("bivariatePlot"),
          plotlyOutput("bivariateCDFPlot")
        )
      )
    }

    serverOption2 <- function(input, output, session) {
      output$dataTable_2 <- renderDT({
      datatable(df_2$data, editable = TRUE,rownames = FALSE,options = list(pageLength = 5, responsive = TRUE, autoWidth = TRUE), 
                class = 'cell-border stripe')
        }, server = FALSE)
      
      observeEvent(input$addX, {
        rows <- strtoi(gsub("x", "", df_2$data[nrow(df_2$data),1])) + 1
        df_2$data[nrow(df_2$data)+1,] <- 0
        df_2$data[nrow(df_2$data),"id"] <- paste("x",rows, sep="")
      })
      
      observeEvent(input$addY, {
        cols <- strtoi(gsub("y", "", colnames(df_2$data)[ncol(df_2$data)])) + 1
        colsParsed <- paste("y", cols, sep = "")
        df_2$data[, colsParsed] <- 0
      })
      
      observeEvent(input$removeAll_2, {
        df_2$data = data.frame(id = c("x", "x1", "x2"), y = c("",1, 2), y1 = c("1", 0.25, 0.25), y2 = c("2", 0.25, 0.25))
      })
      
      observeEvent(input$remove_2, {
        idToRemove = input$removeID_2
        if(idToRemove == "y" || idToRemove == "x"){
          shinyalert("Must not remove this row/col")
        }else if(grepl("y", idToRemove)){
          df_2$data[, idToRemove] <- NULL
        }else{
          indexToRemove <- which(df_2$data$id == idToRemove)
          df_2$data <- df_2$data[-indexToRemove, ]
        }
        df_2$data[1,2] <- ""
      })
      
      observeEvent(input$dataTable_2_cell_edit, {
        info <- input$dataTable_2_cell_edit
        i <- info$row
        j <- info$col + 1
        v <- info$value
        vAsNum <- suppressWarnings(as.numeric(v))
        if (j == 1){
          shinyalert("You cannot edit id!")
          output$dataTable_2 <- renderDT({
            datatable(df_2$data, editable = TRUE,rownames = FALSE,options = list(pageLength = 5, responsive = TRUE, autoWidth = TRUE), 
                      class = 'cell-border stripe')
          }, server = FALSE)
        }else if(is.na(vAsNum)){
          shinyalert("Must be numeric")
          output$dataTable_2 <- renderDT({
            datatable(df_2$data, editable = TRUE,rownames = FALSE,options = list(pageLength = 5, responsive = TRUE, autoWidth = TRUE), 
                      class = 'cell-border stripe')
          }, server = FALSE)
        }
        else if(j==2 && i == 1){
          shinyalert("This must be blank!")
          output$dataTable_2 <- renderDT({
            datatable(df_2$data, editable = TRUE,rownames = FALSE,options = list(pageLength = 5, responsive = TRUE, autoWidth = TRUE), 
                      class = 'cell-border stripe')
              }, server = FALSE)
        }else if(j==2 || i == 1){
          if (vAsNum%%1 ==0){
            df_2$data[i, j] <<- DT::coerceValue(v, df_2$data[i, j])
          }else{
            shinyalert("Values in here must be discrete")
            output$dataTable_2 <- renderDT({
            datatable(df_2$data, editable = TRUE,rownames = FALSE,options = list(pageLength = 5, responsive = TRUE, autoWidth = TRUE), 
                      class = 'cell-border stripe')
              }, server = FALSE)
          }
        }else{
          if (v <= 1 && v >= 0){
              df_2$data[i, j] <<- DT::coerceValue(v, df_2$data[i, j])
          }else{
            shinyalert("probability (P) should be 0<=P<=1")
            output$dataTable_2 <- renderDT({
            datatable(df_2$data, editable = TRUE,rownames = FALSE,options = list(pageLength = 5, responsive = TRUE, autoWidth = TRUE), 
                      class = 'cell-border stripe')
              }, server = FALSE)
          }
        }
      })
      
      observeEvent(input$submit_2, {
        df_2$data[df_2$data == ""] <- NA

        df_2$data[-1] <- lapply(df_2$data[-1], function(x) as.numeric(as.character(x)))

        row_names <- as.character(df_2$data[-1, 'y'])

        mat <- as.matrix(df_2$data[-1, -c(1:2)])
        cols <- colnames(df_2$data[-1,-1])
        rownames(mat) <- df_2$data[-1,]$y
        colnames(mat) <- df_2$data[1,-c(1,2)]
        
        marginal_prob_of_y <- colSums(mat)
        marginal_prob_of_x <- rowSums(mat)
        
        if (sum(marginal_prob_of_x) == 1 && sum(marginal_prob_of_y) == 1){
          mat_marginal <- rbind(mat, marginal_prob_of_y)
          marginal_prob_of_x <- rowSums(mat_marginal)
          mat_marginal <- cbind(mat_marginal, marginal_prob_of_x)
          rownames(mat_marginal)[rownames(mat_marginal) == "marginal_prob_of_y"] = "Marginal Probability of y"
          colnames(mat_marginal)[colnames(mat_marginal) == "marginal_prob_of_x"] = "Marginal Probability of x"

          output$dataTable_marginal <- renderDT({
              datatable(mat_marginal)
            })
          
          jointProbsForX <- c(mat_marginal[-(nrow(mat_marginal)), -ncol(mat_marginal)])
          marginalX <- c(mat_marginal[-nrow(mat_marginal),ncol(mat_marginal)])
          jointProbsForY <- c(t(mat_marginal[-(nrow(mat_marginal)), -ncol(mat_marginal)]))
          marginalY <- c(mat_marginal[nrow(mat_marginal),-ncol(mat_marginal)])
          conditional_prob_Y_given_X <- jointProbsForX / marginalX
          conditional_prob_X_given_Y <- t(jointProbsForY) / marginalY
          conditional_prob_Y_given_X_matrix <- matrix(conditional_prob_Y_given_X, nrow=nrow(mat), ncol=ncol(mat))
          conditional_prob_X_given_Y_matrix <- (matrix(t(conditional_prob_X_given_Y), ncol=nrow(mat), nrow=ncol(mat)))
          rownames(conditional_prob_Y_given_X_matrix) <- rownames(mat)
          colnames(conditional_prob_Y_given_X_matrix) <- colnames(mat)
          rownames(conditional_prob_X_given_Y_matrix) <- colnames(mat)
          colnames(conditional_prob_X_given_Y_matrix) <- rownames(mat)

          output$dataTable_conditional_x <- renderDT({
            datatable(conditional_prob_Y_given_X_matrix)
          })
          output$dataTable_conditional_y <- renderDT({
            datatable(t(conditional_prob_X_given_Y_matrix))
          })
          output$bivariatePlot <- renderPlotly({
            x_vals <- as.numeric(rownames(mat))
            y_vals <- as.numeric(colnames(mat)) 
            probabilities <- c(mat)
            
            plot_ly(x = ~x_vals, y = ~y_vals, z = ~matrix(probabilities, nrow = length(y_vals), byrow = TRUE),
                    type = "surface") %>%
              layout(title = "P.D.F.",
                     scene = list(xaxis = list(title = "X"),
                                  yaxis = list(title = "Y"),
                                  zaxis = list(title = "Probability")))
          })
          
          output$bivariateCDFPlot <- renderPlotly({
          cdf_mat <- matrix(0, nrow = nrow(mat), ncol = ncol(mat))
        
          for (i in 1:nrow(mat)) {
            for (j in 1:ncol(mat)) {
              cdf_mat[i, j] <- sum(mat[1:i, 1:j])
            }
          }
        
          x_vals <- as.numeric(rownames(mat))
          y_vals <- as.numeric(colnames(mat))
        
          plot_ly(x = ~x_vals, y = ~y_vals, z = ~cdf_mat, type = "surface") %>%
            layout(title = "C.D.F.",
                   scene = list(xaxis = list(title = "X"),
                                yaxis = list(title = "Y"),
                                zaxis = list(title = "Cumulative Probability")))
        })
        }else{
          shinyalert("Probabilities must sum up to 1")
        }
      })
      
      isolate({
        df_2$data[df_2$data == ""] <- NA

        df_2$data[-1] <- lapply(df_2$data[-1], function(x) as.numeric(as.character(x)))

        row_names <- as.character(df_2$data[-1, 'y'])

        mat <- as.matrix(df_2$data[-1, -c(1:2)])
        cols <- colnames(df_2$data[-1,-1])
        rownames(mat) <- df_2$data[-1,]$y
        colnames(mat) <- df_2$data[1,-c(1,2)]
        
        marginal_prob_of_y <- colSums(mat)
        marginal_prob_of_x <- rowSums(mat)
        
        mat_marginal <- rbind(mat, marginal_prob_of_y)
        marginal_prob_of_x <- rowSums(mat_marginal)
        mat_marginal <- cbind(mat_marginal, marginal_prob_of_x)
        rownames(mat_marginal)[rownames(mat_marginal) == "marginal_prob_of_y"] = "Marginal Probability of y"
          colnames(mat_marginal)[colnames(mat_marginal) == "marginal_prob_of_x"] = "Marginal Probability of x"

          output$dataTable_marginal <- renderDT({
              datatable(mat_marginal)
            })
          
          jointProbsForX <- c(mat_marginal[-(nrow(mat_marginal)), -ncol(mat_marginal)])
          marginalX <- c(mat_marginal[-nrow(mat_marginal),ncol(mat_marginal)])
          jointProbsForY <- c(t(mat_marginal[-(nrow(mat_marginal)), -ncol(mat_marginal)]))
          marginalY <- c(mat_marginal[nrow(mat_marginal),-ncol(mat_marginal)])
          conditional_prob_Y_given_X <- jointProbsForX / marginalX
          conditional_prob_X_given_Y <- t(jointProbsForY) / marginalY
          conditional_prob_Y_given_X_matrix <- matrix(conditional_prob_Y_given_X, nrow=nrow(mat), ncol=ncol(mat))
          conditional_prob_X_given_Y_matrix <- (matrix(t(conditional_prob_X_given_Y), ncol=nrow(mat), nrow=ncol(mat)))
          rownames(conditional_prob_Y_given_X_matrix) <- rownames(mat)
          colnames(conditional_prob_Y_given_X_matrix) <- colnames(mat)
          rownames(conditional_prob_X_given_Y_matrix) <- colnames(mat)
          colnames(conditional_prob_X_given_Y_matrix) <- rownames(mat)

          output$dataTable_conditional_x <- renderDT({
            datatable(conditional_prob_Y_given_X_matrix)
          })
          output$dataTable_conditional_y <- renderDT({
            datatable(t(conditional_prob_X_given_Y_matrix))
          })
          output$bivariatePlot <- renderPlotly({
            x_vals <- as.numeric(rownames(mat))
            y_vals <- as.numeric(colnames(mat)) 
            probabilities <- c(mat)
            
            plot_ly(x = ~x_vals, y = ~y_vals, z = ~mat,
                    type = "surface") %>%
              layout(title = "PDF: Probability Density Function",
                     scene = list(xaxis = list(title = "X"),
                                  yaxis = list(title = "Y"),
                                  zaxis = list(title = "Probability")))
          })
          
          output$bivariateCDFPlot <- renderPlotly({
          cdf_mat <- matrix(0, nrow = nrow(mat), ncol = ncol(mat))
        
          for (i in 1:nrow(mat)) {
            for (j in 1:ncol(mat)) {
              cdf_mat[i, j] <- sum(mat[1:i, 1:j])
            }
          }
        
          x_vals <- as.numeric(rownames(mat))
          y_vals <- as.numeric(colnames(mat))
        
          plot_ly(x = ~x_vals, y = ~y_vals, z = ~cdf_mat, type = "surface") %>%
            layout(title = "CDF: Cumulative Distribution Function",
                   scene = list(xaxis = list(title = "X"),
                                yaxis = list(title = "Y"),
                                zaxis = list(title = "Cumulative Probability")))
        })
      })
    }

Your program should provide a facility to calculate the mean and
variance of each distribution, and to plot the pdf and cdf. In each
program, do validity checks that the probabilities are in the interval
\[0, 1\], and that they sum to one.

    library(shiny)
    library(DT)
    library(shinyalert)
    library(ggplot2)
    library(plotly)

    set.seed(123)
          values <- seq(-3, 3, length.out = 100)
          discrete_values <- round(values)
          probabilities <- dnorm(values)
          probabilities <- probabilities / sum(probabilities)
          ids <- 1:length(values)
          df <- reactiveValues(data = data.frame(id = ids, value = discrete_values, probability = probabilities))
          
    df_2 <- reactiveValues(data = data.frame(id = c("x", "x1", "x2", "x3"), y = c("",1, 2, 4), y1 = c("1", 0.25, 0.13, 0.02), y2 = c("2", 0.15, 0.1, 0.15), y3 = c("3", 0.1, 0.02, 0.08)))

    ui <- fluidPage(
      titlePanel("Summative Assessment 1: Probability and Probability Distribution"),
      h5("By Romand Lansangan"),
      tags$head(
        tags$style(HTML("
          input { color: blue; } 
          .dataTables_wrapper .dataTables_filter input { color: black; }
          .dataTables_wrapper {
            overflow-x: auto; 
          }
          #pdfPlot, #cdfPlot, #bivariatePlot, #bivariateCDFPlot{
            border: 2px solid #3e2e42;
            border-radius: 5px;
          }
        "))
      ),
      selectInput("dropdown", "Choose an option:", choices = c("Univariate", "Bivariate")),
      uiOutput("dynamicUI")
    )

    server <- function(input, output, session) {
      output$dynamicUI <- renderUI({
        if (input$dropdown == "Univariate") {
          uiOption1()
        } else if (input$dropdown == "Bivariate") {
          uiOption2()
        }
      })
      
      observe({
        req(input$dropdown)
        if(input$dropdown == "Univariate") {
          serverOption1(input, output, session)
        } else if(input$dropdown == "Bivariate") {
            
          serverOption2(input, output, session)
        }
      })
      
    }


    shinyApp(ui = ui, server = server)

This is a rather fun project to work on I must say. I will provide a
link for the [R
File](https://github.com/RomandRapido/APM1110/blob/main/SA1-Lansangan/LANSANGAN_ROMAND-SA1_files/LANSANGAN_ROMAND-SA1-item2.r)
for you to run the application yourself. Nevertheless, I will explain
the functionality here.

##### Features:

To navigate the website more comfortably, please maximize your window.

![](https://github.com/RomandRapido/APM1110/blob/main/SA1-Lansangan/LANSANGAN_ROMAND-SA1_files/figure-gfm/2_1.JPG?raw=true)

This application offers two modes: UNIVARIATE and BIVARIATE, as per your
instruction. You can switch between these modes using the dropdown box
at the top left of your screen.

![](https://github.com/RomandRapido/APM1110/blob/main/SA1-Lansangan/LANSANGAN_ROMAND-SA1_files/figure-gfm/2_2.JPG?raw=true)

Add Row/Values: In the UNIVARIATE mode, you’ll find an “Add Row” button
to add new rows with empty values to the table below. In the BIVARIATE
mode, you’ll find separate buttons to add X and Y values.

![](https://github.com/RomandRapido/APM1110/blob/main/SA1-Lansangan/LANSANGAN_ROMAND-SA1_files/figure-gfm/2_3.JPG?raw=true)

“Remove All” clears the entire table to its default state for easier
customization. “Remove” button deletes the row or column specified in
the text box above it. Table Section:

![](https://github.com/RomandRapido/APM1110/blob/main/SA1-Lansangan/LANSANGAN_ROMAND-SA1_files/figure-gfm/2_4.JPG?raw=true)

The table allows you to edit values. Ensure that values are discrete,
and probabilities fall within the range of \[0, 1\].

![](https://github.com/RomandRapido/APM1110/blob/main/SA1-Lansangan/LANSANGAN_ROMAND-SA1_files/figure-gfm/2_5.JPG?raw=true)

After editing values, click the “Submit” button. The app checks if the
probabilities sum up to 1. If they don’t, a message will pop up. If
successful, it displays necessary details on the main panel.

For UNIVARIATE mode, it calculates and displays the Mean and Variance of
the entered data, and the plots for Probability Density Function (PDF)
and Cumulative Distribution Function (CDF).

For BIVARIATE mode, it generates Marginal Probability Tables,
Conditional Probability Tables (P(Y\|X) and P(X\|Y)), and plots
Probability Density Function (PDF) and Cumulative Distribution Function
(CDF) using Plotly for visual analysis. Plotly makes the plots
interactive so you can go ahead and play with it.

Note the Marginal Probability was being calculated by adding the
probabilities of the same column, same with rows. Conditional
Probability was being calculated by dividing the joint probability by
the marginal probability of the given axis. For example

$$
P(X=1|Y=2) = \frac{P(X=1 \cap Y=2)}{P(Y=2)}
$$

where $P(X=1 \cap Y=2)$ is the joint probability while $P(Y=2)$ is the
marginal probability at Y=2.

### 3)

By generating 10,000 searches in R, carry out a simulation experiment
for a search engine going through a list of sites for a given key
phrase, until the key phrase is found. You should allow your program to
input the probability p that any site will contain the key phrase.

- Plot the simulated pdf and calculate its mean and variance, and
- Obtain the simulated conditional distribution of searches when three
  searches have been carried out without success. Calculate its mean and
  variance, and satisfy yourself that they are equivalent to the
  simulated distribution of the complete set.

As test data assume each site has a 60% chance of containing the key
phrase.

To satisfy yourself that the Markov memoryless property holds, obtain
estimates of

- $P(X=4|X>4)$ and $P(X=1)$
- $P(X=5|X>3)$ and $P(X=2)$

where x is the number of searches to the first success.

#### Solution:

So this is problem that begs for geometric distribution simulation.

#### Input algorithm for probability:

`#{r} repeat {   prob <- as.double(readline(prompt = "Enter the probability for success: "))   if (0 <= prob && prob <= 1){     break;   } }`

I shall comment it out and assume the following value as user input (as
per instruction):

``` r
prob <- 0.60
```

#### Simulating the experiment:

``` r
searches <- 10000
simulated <- rgeom(searches, prob)
```

#### Plotting the result:

``` r
library(ggplot2)

df <- data.frame(Trials = factor(simulated))

ggplot(df, aes(x = Trials, fill = after_stat(count))) +
  geom_bar(color = "#3e2e42", show.legend = FALSE) + 
  scale_fill_gradient(low = "#70587a", high = "#8b1ebd") +
  labs(title = "Histogram of Geometric Distribution",
       x = "Number of Trials",
       y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "#3f3d40"))
```

![](LANSANGAN_ROMAND-SA1_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

#### Calculating the mean and variance:

``` r
simulated_mean <- mean(simulated)
simulated_variance <- var(simulated)
print(paste("Mean of the simulated experiment: ", simulated_mean))
```

    ## [1] "Mean of the simulated experiment:  0.6511"

``` r
print(paste("Variance of the simulated experiment: ", simulated_variance))
```

    ## [1] "Variance of the simulated experiment:  1.08367715771577"

#### Proving the Markov property holds in geometeric sequence:

Assume that the probability of each website containing the key phrase is
60% or 0.6.

We must prove: \* $P(X=4|X>4) = P(X=1)$ \* $P(X=5|X>3) = P(X=2)$

For the first part, let us get the P(x=1) and P(x=2) out of the way.

The Probability Density function is defined as the following:

$$
P(X=x) = q^{x−1}p
$$

where p is the probability of success, q is the probability of failure
(1 - p), and x = 1, 2, 3,…

Consequently since p = 0.60, the probability of failure will be q =
0.40.

###### For P(X=1)

$$
P(X=1) = 0.40^{1-1} \times 0.60
$$

$$
P(X=1) = 0.40^0 \times 0.60
$$

$$
P(X=1) = 0.60
$$

##### For P(X=2)

$$
P(X=2) = 0.40^{2-1} \times 0.60
$$

$$
P(X=2) = 0.40^1 \times 0.60
$$

``` r
0.40*0.60
```

    ## [1] 0.24

$$
P(X=2) = 0.24
$$

Then let us now calculate both the conditional probability.

Conditional probability is defined as the following formula:

If A and B are two events in a sample space S, then the conditional
probability of A given B is defined as

$$
P(A|B) = \frac{P(A \cap B)}{P(B)}
$$

Where P(B) \> 0.

##### For P(X=4\|X\>3):

$$
P(X=4|X>3) = \frac{P(X=4 \cap X>3)}{P(X>3)}
$$

The $P(X=4 \cap X>4)$ could also be rewritten as P(X=4) because in
discrete world, x = 4 is the only one that satisfies both x = 4 and x \>
3.

Also, it is worth nothing that:

$$
P(X > 3) = 1 - P(X \leq 3)
$$

With those in mind,

$$
P(X=4|X>3) = \frac{P(X=4)}{1-P(X \leq 3)}
$$

$$
P(X=4) = 0.40^{4-1} \times 0.60
$$

$$
P(X=4) = 0.40^3 \times 0.60
$$

``` r
(0.40^3) * 0.60
```

    ## [1] 0.0384

$$
P(X=4) = 0.0384
$$

The Cumulative Distribution Function of the geometric sequence is
defined as:

$$
P(X \leq x) = 1-q^x
$$ So,

$$
1 - P(X \leq 3) = 1 - (1-q^3) = q^3
$$

Since q = 0.40,

$$
P(X=4|X>3) = \frac{0.0384}{0.40^3}
$$

``` r
0.0384 / (0.40^3)
```

    ## [1] 0.6

$$
P(X=4|X>3) = 0.6
$$

##### For P(X=5\|X\>3):

$$
P(X=5|X>3) = \frac{P(X=5 \cap X>3)}{P(X>3)} = \frac{P(X=5)}{1-P(X \leq 3)}
$$

$$
P(X=5) = 0.40^{5-1} \times 0.60 = 0.40^4 \times 0.60
$$

``` r
(0.40^4) * 0.60
```

    ## [1] 0.01536

$$
P(X=5) = 0.01536
$$

$$
P(X=5|X>3) = \frac{0.01536}{0.40^3}
$$

``` r
(0.01536) / (0.40^3)
```

    ## [1] 0.24

$$
P(X=5|X>3) = 0.24
$$

Since $P(X=4|X>3) = P(X=1)$ (both are 0.60) and $P(X=5|X>3) = P(X=2)$
(both are 0.24) is true, the Markov memoryless property holds in the
Geometric distribution.

Also, to check the validity of our simulation, lets estimate the
Probabilities mentioned above.

``` r
simulated_table <- table(simulated)
simulated_table
```

    ## simulated
    ##    0    1    2    3    4    5    6    7    8    9   10 
    ## 6062 2399  899  399  153   59   10    9    6    1    3

Note that X is assumed to be equals to the nth trial. So, X=1 means that
it’s the success that happened on the first try. In the simulation, the
column name indicates the number of failures before achieving the first
success. Therefore, the first trial is equivalent to “0” in our table.

##### For P(X=4\|X\>3):

``` r
prob_for_xeq4 <- (sum(simulated_table[names(simulated_table) == 3]) / sum(simulated_table)) 
prob_for_xgreater3 <- (sum(simulated_table[names(simulated_table) >= 3]) / sum(simulated_table)) 
prob_for_xeq4 / prob_for_xgreater3
```

    ## [1] 0.6263736

#### For P(X=1)

``` r
prob_for_xeq1 <- (sum(simulated_table[names(simulated_table) == 0]) / sum(simulated_table)) 
prob_for_xeq1
```

    ## [1] 0.6062

##### For P(X=5\|X\>3):

``` r
prob_for_xeq5 <- (sum(simulated_table[names(simulated_table) == 4]) / sum(simulated_table)) 
prob_for_xeq5 / prob_for_xgreater3
```

    ## [1] 0.2401884

##### For P(X=2)

``` r
prob_for_xeq2 <- (sum(simulated_table[names(simulated_table) == 1]) / sum(simulated_table)) 
prob_for_xeq2
```

    ## [1] 0.2399

Both of which are relatively close to their respective partner and their
theoretical equivalent.

Therefore validating that the Markov memoryless property also holds on
our simulation.
