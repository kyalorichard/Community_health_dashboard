library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(corrplot)
library(cluster)
library(factoextra)

# UI
ui <- fluidPage(
  titlePanel("Advanced Health Data Quality Checker and Clustering Tool"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      checkboxInput("showSummary", "Show Summary Statistics", TRUE),
      checkboxInput("checkMissing", "Check Missing Values", TRUE),
      checkboxInput("checkDuplicates", "Check Duplicates", TRUE),
      checkboxInput("checkOutliers", "Check Outliers", TRUE),
      checkboxInput("checkDistribution", "Check Data Distribution", TRUE),
      checkboxInput("checkCorrelation", "Check Correlation Matrix", TRUE),
      hr(),
      numericInput("kClusters", "Number of Clusters (K-means)", value = 3, min = 2),
      selectInput("clusterMethod", "Select Clustering Method", 
                  choices = c("K-means", "Hierarchical")),
      actionButton("runChecks", "Run Data Quality Checks"),
      actionButton("runClustering", "Run Clustering Analysis")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview", DTOutput("dataTable")),
        tabPanel("Summary Statistics", verbatimTextOutput("summary")),
        tabPanel("Missing Values", plotlyOutput("missingPlot")),
        tabPanel("Duplicates", verbatimTextOutput("duplicates")),
        tabPanel("Outliers", plotlyOutput("outlierPlot")),
        tabPanel("Distribution", plotlyOutput("distributionPlot")),
        tabPanel("Correlation Matrix", plotOutput("correlationPlot")),
        tabPanel("Clustering Analysis", plotlyOutput("clusterPlot"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  data <- reactiveVal()
  
  # Load data
  observeEvent(input$file, {
    req(input$file)
    data(read_csv(input$file$datapath))
  })
  
  # Data Preview
  output$dataTable <- renderDT({
    req(data())
    datatable(data(), options = list(pageLength = 10))
  })
  
  # Summary Statistics
  output$summary <- renderPrint({
    req(data())
    if (input$showSummary) {
      summary(data())
    }
  })
  
  # Missing Values Analysis
  output$missingPlot <- renderPlotly({
    req(data())
    if (input$checkMissing) {
      missing_data <- colSums(is.na(data()))
      missing_df <- data.frame(Variable = names(missing_data), Missing = missing_data)
      plot <- ggplot(missing_df, aes(x = Variable, y = Missing)) +
        geom_bar(stat = "identity", fill = "skyblue") +
        theme_minimal() +
        labs(title = "Missing Values per Column", x = "Variables", y = "Missing Count")
      ggplotly(plot)
    }
  })
  
  # Duplicate Records Check
  output$duplicates <- renderPrint({
    req(data())
    if (input$checkDuplicates) {
      duplicates <- data() %>% duplicated() %>% sum()
      paste("Number of Duplicate Rows: ", duplicates)
    }
  })
  
  # Outlier Detection
  output$outlierPlot <- renderPlotly({
    req(data())
    if (input$checkOutliers) {
      numeric_cols <- data() %>% select(where(is.numeric))
      melted_data <- reshape2::melt(numeric_cols)
      plot <- ggplot(melted_data, aes(x = variable, y = value)) +
        geom_boxplot(aes(color = variable)) +
        theme_minimal() +
        labs(title = "Boxplot for Outlier Detection", x = "Variable", y = "Value")
      ggplotly(plot)
    }
  })
  
  # Data Distribution Analysis
  output$distributionPlot <- renderPlotly({
    req(data())
    if (input$checkDistribution) {
      numeric_cols <- data() %>% select(where(is.numeric))
      plots <- lapply(names(numeric_cols), function(col) {
        ggplot(numeric_cols, aes_string(x = col)) +
          geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.6) +
          geom_density(color = "red") +
          theme_minimal() +
          labs(title = paste("Distribution of", col))
      })
      subplot(plots, nrows = 2)
    }
  })
  
  # Correlation Matrix Analysis
  output$correlationPlot <- renderPlot({
    req(data())
    if (input$checkCorrelation) {
      numeric_cols <- data() %>% select(where(is.numeric))
      corr_matrix <- cor(numeric_cols, use = "complete.obs")
      corrplot(corr_matrix, method = "color", type = "upper", tl.cex = 0.8)
    }
  })
  
  # Clustering Analysis
  output$clusterPlot <- renderPlotly({
    req(data())
    req(input$runClustering)
    
    numeric_cols <- data() %>% select(where(is.numeric))
    if (input$clusterMethod == "K-means") {
      kmeans_result <- kmeans(numeric_cols, centers = input$kClusters)
      numeric_cols$Cluster <- as.factor(kmeans_result$cluster)
      plot <- ggplot(numeric_cols, aes(x = numeric_cols[, 1], y = numeric_cols[, 2], color = Cluster)) +
        geom_point(size = 3) +
        theme_minimal() +
        labs(title = "K-means Clustering", x = names(numeric_cols)[1], y = names(numeric_cols)[2])
      ggplotly(plot)
    } else if (input$clusterMethod == "Hierarchical") {
      hc <- hclust(dist(numeric_cols))
      plot(hc, main = "Hierarchical Clustering Dendrogram", sub = "", xlab = "")
    }
  })
}

# Run the App
shinyApp(ui, server)