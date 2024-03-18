library(shiny)
library(DT)
library(shinyalert)
library(ggplot2)
library(plotly)

uiOption1 <- function() {
  sidebarLayout(
    sidebarPanel(
      h2("Table Data"),
      helpText("Input values and their associated probabilities for a univariate discrete random variable."),
      actionButton("add","Add Row"),
      br(),
      useShinyalert(),
      textInput("removeID", "Remove Row with ID", placeholder = "ID to remove"),
      div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        div(
          style = "flex: 1; padding-right: 20px; display: flex; align-items: center;",
          actionButton("remove", "Remove", style = "color: red;")
        ),
        div(
          style = "flex: 1; display: flex; align-items: center;",
          actionButton("removeAll", "Clear All Data", style = "color: red;")
        )
      ),
      br(),
      DTOutput("dataTable"),
      actionButton("submit", "Submit Data", style = "color: green;")
    ),
    mainPanel(
      h3("For Univariate Discrete Random Variable", align = "center"),
      div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        div(
          style = "flex: 1; padding-right: 20px; display: flex; align-items: center;",
          h5("Mean:"),
          textOutput("mean", container = span)
        ),
        div(
          style = "flex: 1; display: flex; align-items: center;",
          h5("Variance:"),
          textOutput("var", container = span)
        )
      ),
      plotOutput("pdfPlot"),
      plotOutput("cdfPlot")
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
