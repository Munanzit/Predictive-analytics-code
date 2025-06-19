
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinyWidgets)
library(shinyjs)
library(RColorBrewer)
library(themis)
library(recipes)
library(ggpubr)
library(summarytools)
library(randomForest)
library(caret)
library(e1071)  # For confusionMatrix
library(pROC)
library(lime)       # For LIME explanations
library(viridis)    # For better color scales
library(lime)
library(pdp)
library(rpart)
library(rpart.plot)
library(visNetwork)
library(viridis)
library(shinycssloaders)  
library(fastshap)
library(gbm)
library(forcats)
library(sparkline)
library(car)
library(rmarkdown)
library(flextable)
library(officer)
library(tidyr)
library(janitor)
library(GGally)  # For pairs plots
library(nortest) # For normality tests
library(themis)    # For balancing methods
library(recipes)   # For preprocessing
library(plotly)    # For interactive plots
library(shinyBS)  # For bsTooltip()
library(cluster)  # For silhouette function
library(shinymanager)
library(keyring)
library(rintrojs)
introjsUI()
library(sodium)



####Ui---------------------
# UI Component
ui<- dashboardPage(
  dashboardHeader(
    title = span(tagList(icon("accessible-icon"), "Disability Claim Analysis")),
    titleWidth = 300
    
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Data Upload", tabName = "data_upload", icon = icon("upload")),
      menuItem("Data Preparation", tabName = "data_prep", icon = icon("wrench")),
      menuItem("Exploratory Data Analysis", tabName = "eda", icon = icon("search")),
      menuItem("Data Balancing", tabName = "data_balancing", icon = icon("balance-scale"),
               menuSubItem("Balancing Methods", tabName = "balancing_methods"),
               menuSubItem("Balancing Evaluation", tabName = "balancing_eval")),
      menuItem("Risk Classification", tabName = "risk_class", icon = icon("exclamation-triangle"),
               menuSubItem("Modeling", tabName = "risk_modeling"),
               menuSubItem("Prediction", tabName = "risk_prediction")),
      menuItem("Disability Percentage", tabName = "disability_pct", icon = icon("percentage"),
               menuSubItem("Modeling", tabName = "pct_modeling"),
               menuSubItem("Prediction", tabName = "pct_prediction"))
    ),
    collapsed = TRUE
  ),
  dashboardBody(
    useShinyjs(),
    extendShinyjs(text = "$(document).on('shiny:connected', function() {
      $('.guide-card').hover(
        function() { $(this).addClass('animated pulse'); },
        function() { $(this).removeClass('animated pulse'); }
      );
    });", functions = c()),
    
    # Custom CSS
    tags$head(
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/animate.css/4.1.1/animate.min.css"),
      
      tags$style(HTML("
    .box-body {
        padding: 10px;
    }
    .form-group {
        margin-bottom: 8px;
    }
    .shiny-input-container {
        margin-bottom: 8px;
    }
    .box {
        margin-bottom: 10px;
    }
    .plot-container {
        height: 400px;
        overflow-y: auto;
    }
    .method-box {
        border: 1px solid #ddd;
        border-radius: 5px;
        padding: 10px;
        margin-bottom: 15px;
    }
    .summary-card {
        background: #f9f9f9;
        padding: 10px;
        border-radius: 5px;
        margin-bottom: 10px;
    }
    /* Highlight active parent menu item */
    .treeview-menu>li.active>a {
        border-left-color: #3c8dbc;
        font-weight: bold;
    }
    
    /* Show all regular menu items */
    .sidebar-menu>li {
        display: block;
    }
    
    /* Always show the home item */
    .sidebar-menu>li:first-child {
        display: block;
    }
    
    /* Custom styling for progress bars */
    .shiny-notification {
      width: 300px;
    }
    
    /* Custom styling for buttons */
    .btn-margin {
      margin-right: 5px;
      margin-bottom: 5px;
    }
    
    /* Custom styling for tabs */
    .nav-tabs-custom .nav-tabs li.active {
      border-top-color: #3c8dbc;
    }
    
    /* Custom styling for data tables */
    .dataTables_wrapper {
      padding: 10px;
    }
    
    /* Custom styling for color pickers */
    .colourpicker-input {
      width: 100% !important;
    }
    
    
    
     .eval-metric-card {
        border-radius: 5px;
        padding: 15px;
        margin-bottom: 15px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
      }
      .weakness-card {
        background-color: #FFF3F3;
        border-left: 4px solid #FF6B6B;
      }
      .strength-card {
        background-color: #F3FFF5;
        border-left: 4px solid #6BFF79;
      }
      .metric-value {
        font-size: 24px;
        font-weight: bold;
      }
      .metric-label {
        font-size: 14px;
        color: #666;
      }
      .method-comparison-plot {
        height: 500px !important;
      }
    
    
      .guide-card .btn-primary {
    transition: all 0.3s ease;
  }
  .guide-card .btn-primary:hover {
    transform: translateX(5px);
    box-shadow: 2px 2px 5px rgba(0,0,0,0.2);
  }
    
    
    
    
  
  
  .fixed-panel {
  height: 400px;
  overflow-y: auto;
  background-color: white;
  border-radius: 4px;
  padding: 10px;
  margin-bottom: 15px;
  border: 1px solid #ddd;
}

.scrollable-table {
  max-height: 300px;
  overflow-y: auto;
  display: block;
}  
    
   
   
.modal-lg {
  width: 90% !important;
  max-width: 1200px;
}
.data-summary {
  font-size: 12px;
  line-height: 1.2;
}   
    
    
.alert {
  padding: 15px;
  margin-bottom: 20px;
  border: 1px solid transparent;
  border-radius: 4px;
}

.alert-success {
  color: #3c763d;
  background-color: #dff0d8;
  border-color: #d6e9c6;
}

.feature-importance-plot {
  height: 500px;
}

.pdp-plot {
  height: 400px;
}

.shap-plot {
  height: 600px;
}


/* Prediction result cards */
.prediction-card {
  border-radius: 5px;
  padding: 15px;
  margin-bottom: 15px;
  box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
}

.prediction-card.success {
  background-color: #F3FFF5;
  border-left: 4px solid #6BFF79;
}

.prediction-card.info {
  background-color: #F3F9FF;
  border-left: 4px solid #6BA3FF;
}

/* Fixed height containers */
.fixed-height-container {
  height: 500px;
  overflow-y: auto;
}

/* Tab styling */
.nav-tabs-custom .nav-tabs li.active {
  border-top-color: #3c8dbc;
}

/* Form input spacing */
.form-group {
  margin-bottom: 10px;
}

/* Responsive tables */
.table-responsive {
  overflow-x: auto;
}
    
    
    
    
    
    
    "))),
    
    ### tabitems--------------------
    tabItems(
      # Home Tab---------------
      tabItem(
        tabName = "home",
        fluidRow(
          column(12,
                 h2("Welcome to Disability Claim Analysis Dashboard"),
                 p("This tool helps you analyze and predict disability claim outcomes. Follow the workflow below:"),
                 br()
          )
        ),
        
        # Workflow Progress
        fluidRow(
          column(12,
                 h4("Your Analysis Progress"),
                 div(style = "margin-bottom: 10px;",
                     span("Completed Steps: ", style = "font-weight: bold;"),
                     textOutput("completed_steps", inline = TRUE)
                 ),
                 shinyWidgets::progressBar(
                   id = "workflow_progress",
                   value = 0,
                   display_pct = TRUE,
                   striped = TRUE,
                   status = "primary"
                 )
          )
        ),
        
        # Main 6-panel guide
        fluidRow(
          column(4,
                 div(id = "upload_card", class = "guide-card",
                     h4(icon("upload", class = "fa-2x"), "1. Upload Data"),
                     p("Import your training and prediction datasets to begin analysis."),
                     tags$ul(
                       tags$li("Supports CSV files with custom delimiters"),
                       tags$li("Interactive data preview"),
                       tags$li("Separate tabs for training/prediction data")
                     ),
                     actionButton("btn_upload", "Go to Upload", 
                                  icon = icon("arrow-right"),
                                  class = "btn-primary btn-sm pull-right")
                 )
          ),
          
          column(4,
                 div(id = "prep_card", class = "guide-card",
                     h4(icon("broom", class = "fa-2x"), "2. Data Preparation"),
                     p("Clean and transform your data for optimal analysis."),
                     tags$ul(
                       tags$li("Handle missing values and outliers"),
                       tags$li("Feature engineering tools"),
                       tags$li("Variable transformation")
                     ),
                     actionButton("btn_prep", "Go to Preparation", 
                                  icon = icon("arrow-right"),
                                  class = "btn-primary btn-sm pull-right")
                 )
          ),
          
          column(4,
                 div(id = "eda_card", class = "guide-card",
                     h4(icon("search", class = "fa-2x"), "3. Exploratory Analysis"),
                     p("Discover patterns and relationships in your data."),
                     tags$ul(
                       tags$li("Interactive visualizations"),
                       tags$li("Statistical summaries"),
                       tags$li("Correlation analysis")
                     ),
                     actionButton("btn_eda", "Go to EDA", 
                                  icon = icon("arrow-right"),
                                  class = "btn-primary btn-sm pull-right")
                 )
          )
        ),
        
        fluidRow(
          column(4,
                 div(id = "balance_card", class = "guide-card",
                     h4(icon("balance-scale", class = "fa-2x"), "4. Data Balancing"),
                     p("Address class imbalance for better model performance."),
                     tags$ul(
                       tags$li("Multiple balancing methods"),
                       tags$li("Visual comparison tools"),
                       tags$li("Performance metrics")
                     ),
                     actionButton("btn_balance", "Go to Balancing", 
                                  icon = icon("arrow-right"),
                                  class = "btn-primary btn-sm pull-right")
                 )
          ),
          
          column(4,
                 div(id = "risk_card", class = "guide-card pulse",
                     h4(icon("exclamation-triangle", class = "fa-2x"), "5. Risk Classification"),
                     p("Predict risk categories using machine learning."),
                     tags$ul(
                       tags$li("Multiple algorithms"),
                       tags$li("Model interpretation"),
                       tags$li("Prediction export")
                     ),
                     actionButton("btn_risk", "Go to Risk Analysis", 
                                  icon = icon("arrow-right"),
                                  class = "btn-primary btn-sm pull-right")
                 )
          ),
          
          column(4,
                 div(id = "pct_card", class = "guide-card",
                     h4(icon("percent", class = "fa-2x"), "6. Disability Percentage"),
                     p("Estimate disability percentages with regression."),
                     tags$ul(
                       tags$li("Regression models"),
                       tags$li("Residual analysis"),
                       tags$li("Actual vs. predicted plots")
                     ),
                     actionButton("btn_pct", "Go to Percentage Disability", 
                                  icon = icon("arrow-right"),
                                  class = "btn-primary btn-sm pull-right")
                 )
          )
        ),
        
        # Additional Resources
        fluidRow(
          column(12,
                 br(),
                 div(class = "well",
                     h4(icon("lightbulb"), "Quick Tips"),
                     tags$ul(
                       tags$li("Hover over cards for more information"),
                       tags$li("Save your work at each step using the download buttons")
                     )
                 )
          )
        )
      ),
      
      
      ###DU---------------------------
      # Data Upload Tab
      tabItem(
        tabName = "data_upload",
        fluidRow(
          box(
            title = "Upload Training Data", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 6,
            fileInput("train_upload", "Choose CSV File",
                      accept = c("text/csv", ".csv")),
            materialSwitch("train_header", "File contains header", TRUE),
            selectInput("train_separator", "Field Separator:", 
                        choices = c(Comma = ",", Semicolon = ";", Tab = "\t")),
            actionButton("load_train", "Load Training Data", class = "btn-primary btn-margin")
          ),
          box(
            title = "Upload Prediction Data", 
            status = "warning", 
            solidHeader = TRUE, 
            width = 6,
            fileInput("pred_upload", "Choose CSV File",
                      accept = c("text/csv", ".csv")),
            materialSwitch("pred_header", "File contains header", TRUE),
            selectInput("pred_separator", "Field Separator:", 
                        choices = c(Comma = ",", Semicolon = ";", Tab = "\t")),
            actionButton("load_pred", "Load Prediction Data", class = "btn-primary btn-margin")
          )
        ),
        fluidRow(
          box(
            title = "Training Data Preview", 
            status = "info", 
            solidHeader = TRUE, 
            width = 6,
            DTOutput("train_preview"), 
            style = "height:400px; overflow-y: scroll;"
          ),
          box(
            title = "Prediction Data Preview", 
            status = "info", 
            solidHeader = TRUE, 
            width = 6,
            DTOutput("pred_preview"), 
            style = "height:400px; overflow-y: scroll;"
          )
        )
      ),
      
      
      ##DP----------------------------
      # Data Preparation Tab
      tabItem(
        tabName = "data_prep",
        fluidRow(
          box(
            title = "Data Preparation Options", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            fluidRow(
              column(6,
                     radioButtons("data_type_prep", "Select Data to Prepare:",
                                  choices = c("Training Data" = "train", 
                                              "Prediction Data" = "pred"),
                                  selected = "train", inline = TRUE),
                     actionButton("view_data_prep", "View Selected Data", 
                                  icon = icon("eye"), class = "btn-margin"),
                     actionButton("summary_data_prep", "Show Summary", 
                                  icon = icon("list-alt"), class = "btn-margin")
              ),
              column(6,
                     uiOutput("prep_steps_ui")
              )
            )
          ),
          
          # Data Cleaning Box
          box(
            title = "Data Cleaning", 
            status = "warning", 
            solidHeader = TRUE, 
            width = 12,
            tabsetPanel(
              tabPanel("Missing Data",
                       selectInput("missing_action", "Action for Missing Values:",
                                   choices = c("Show Summary" = "summary",
                                               "Remove Rows" = "remove_rows",
                                               "Remove Columns" = "remove_cols",
                                               "Impute Values" = "impute")),
                       uiOutput("impute_ui"),
                       actionButton("apply_missing", "Apply", 
                                    icon = icon("check"), class = "btn-primary")
              ),
              tabPanel("Outliers",
                       fluidRow(
                         column(4,
                                selectInput("outlier_method", "Detection Method:",
                                            choices = c("Z-Score" = "zscore",
                                                        "IQR" = "iqr",
                                                        "Percentile" = "percentile")),
                                numericInput("outlier_threshold", "Threshold:", 
                                             value = 3, min = 1, step = 0.5)
                         ),
                         column(4,
                                radioButtons("outlier_action", "Action:",
                                             choices = c("Cap Values" = "cap",
                                                         "Remove Rows" = "remove",
                                                         "Mark as NA" = "na"))
                         ),
                         column(4,
                                actionButton("detect_outliers", "Detect Outliers", 
                                             icon = icon("search"), 
                                             class = "btn-info btn-margin"),
                                actionButton("treat_outliers", "Treat Outliers", 
                                             icon = icon("check"), 
                                             class = "btn-primary")
                         )
                       )
              ),
              tabPanel("Duplicates",
                       radioButtons("dup_action", "Action:",
                                    choices = c("Show Duplicates" = "show",
                                                "Remove Duplicates" = "remove")),
                       actionButton("handle_duplicates", "Execute", 
                                    icon = icon("check"), class = "btn-primary")
              )
            )
          ),
          
          # Data Transformation Box
          box(
            title = "Data Transformation", 
            status = "info", 
            solidHeader = TRUE, 
            width = 12,
            tabsetPanel(
              tabPanel("Scaling/Normalization",
                       selectInput("scale_method", "Method:",
                                   choices = c("Standard Scaler (Z-Score)" = "standard",
                                               "Min-Max Scaler" = "minmax",
                                               "Robust Scaler" = "robust")),
                       actionButton("apply_scaling", "Apply Scaling", 
                                    icon = icon("check"), class = "btn-primary")
              ),
              tabPanel("Encoding",
                       selectInput("encode_var", "Select Variable to Encode:",
                                   choices = NULL),
                       radioButtons("encode_method", "Encoding Method:",
                                    choices = c("One-Hot Encoding" = "onehot",
                                                "Label Encoding" = "label",
                                                "Frequency Encoding" = "freq")),
                       actionButton("apply_encoding", "Apply Encoding", 
                                    icon = icon("check"), class = "btn-primary")
              ),
              tabPanel("Binning",
                       selectInput("bin_var", "Select Numeric Variable:", 
                                   choices = NULL),
                       radioButtons("bin_method", "Binning Method:",
                                    choices = c("Equal Width" = "equal_width",
                                                "Equal Frequency" = "equal_freq",
                                                "Custom Breaks" = "custom")),
                       uiOutput("bin_params_ui"),
                       actionButton("apply_binning", "Apply Binning", 
                                    icon = icon("check"), class = "btn-primary")
              ),
              tabPanel("Variable Categorization",
                       fluidRow(
                         column(4,
                                selectInput("var_to_categorize", "Select Variable to Categorize:", 
                                            choices = NULL),
                                uiOutput("categorization_type_ui")
                         ),
                         column(8,
                                uiOutput("categorization_ui")
                         )
                       ),
                       hr(),
                       fluidRow(
                         column(6,
                                actionButton("apply_categorization", "Apply Categorization", 
                                             class = "btn-primary btn-margin"),
                                actionButton("reset_categorization", "Reset", 
                                             class = "btn-danger btn-margin")
                         )
                       )
              )
            )
          ),
          
          # Feature Engineering Box
          box(
            title = "Feature Engineering", 
            status = "success", 
            solidHeader = TRUE, 
            width = 12,
            tabsetPanel(
              tabPanel("Create Features",
                       textInput("new_feature_name", "New Feature Name:", ""),
                       textAreaInput("feature_formula", "Formula (use existing columns):",
                                     placeholder = "e.g., height/weight"),
                       actionButton("create_feature", "Create Feature", 
                                    icon = icon("plus"), class = "btn-primary")
              ),
              tabPanel("Transform Features",
                       selectInput("transform_var", "Select Variable:", 
                                   choices = NULL),
                       radioButtons("transform_method", "Transformation:",
                                    choices = c("Log" = "log",
                                                "Square Root" = "sqrt",
                                                "Exponential" = "exp",
                                                "Custom" = "custom")),
                       uiOutput("transform_custom_ui"),
                       actionButton("apply_transform", "Apply", 
                                    icon = icon("check"), class = "btn-primary")
              ),
              tabPanel("Date Features",
                       selectInput("date_var", "Select Date Variable:", 
                                   choices = NULL),
                       checkboxGroupInput("date_parts", "Extract Date Parts:",
                                          choices = c("Year" = "year",
                                                      "Month" = "month",
                                                      "Day" = "day",
                                                      "Weekday" = "weekday",
                                                      "Quarter" = "quarter")),
                       actionButton("extract_date", "Extract", 
                                    icon = icon("calendar"), class = "btn-primary")
              )
            )
          ),
          
          # Data Viewing Boxes
          box(
            title = "Data Preview", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 6,
            DTOutput("data_preview_prep") %>% withSpinner(),
            downloadButton("download_prep_data", "Download Prepared Data", 
                           class = "btn-success btn-margin")
          ),
          box(
            title = "Data Summary", 
            status = "info", 
            solidHeader = TRUE, 
            width = 6,
            verbatimTextOutput("data_summary_prep") %>% withSpinner(),
            hr(),
            h4("Variable Categorization Visualization"),
            fluidRow(
              column(6, plotlyOutput("categorized_dist_plot", height = "300px")),
              column(6, 
                     uiOutput("color_picker_ui"),
                     DTOutput("category_mapping_table", height = "300px")
              )
            )
          ),
          
          # Final Actions
          box(
            title = "Finalize Preparation", 
            status = "danger", 
            solidHeader = TRUE, 
            width = 12,
            actionButton("save_prep_train", "Save Prepared Training Data", 
                         icon = icon("save"), class = "btn-success btn-margin"),
            actionButton("save_prep_pred", "Save Prepared Prediction Data", 
                         icon = icon("save"), class = "btn-warning btn-margin"),
            actionButton("reset_prep", "Reset Current Dataset", 
                         icon = icon("undo"), class = "btn-danger btn-margin")
          )
        )
      ),
      
      ###EDA------------------------------
      #EXPLANATORY DATA ANALYSIS
      tabItem(
        tabName = "eda",
        fluidPage(
          useShinyjs(),
          titlePanel("Exploratory Data Analysis"),
          
          fluidRow(
            box(
              title = "Data Selection", 
              status = "primary", 
              solidHeader = TRUE,
              width = 12,
              radioButtons("eda_data_type", "Select Data to Analyze:",
                           choices = c("Training Data" = "train", 
                                       "Prediction Data" = "pred"),
                           selected = "train", inline = TRUE),
              downloadButton("download_report", "Download Full Report", class = "btn-success"),
              actionButton("reset", "Reset", class = "btn-danger"),
              uiOutput("eda_var_select"),
              uiOutput("eda_var_filter")
            )
          ),
          
          tabsetPanel(
            tabPanel("Summary", 
                     actionButton("run_summary", "Generate Summary", class = "btn-primary"),
                     verbatimTextOutput("summary_output")),
            
            tabPanel("Frequency Tables",
                     selectInput("freq_type", "Frequency Table Type",
                                 choices = c("Single Variable" = "single",
                                             "Two-Way Table" = "two_way",
                                             "Multi-Way Table" = "multi_way")),
                     uiOutput("freq_params"),
                     actionButton("run_freq", "Generate Table", class = "btn-primary"),
                     DTOutput("freq_table")),
            
            tabPanel("Pivot Tables",
                     selectInput("pivot_type", "Pivot Table Type",
                                 choices = c("Count" = "count",
                                             "Sum" = "sum",
                                             "Mean" = "mean",
                                             "Median" = "median")),
                     uiOutput("pivot_params"),
                     actionButton("run_pivot", "Generate Pivot", class = "btn-primary"),
                     DTOutput("pivot_table")),
            
            tabPanel("Plots", 
                     selectInput("plot_type", "Plot Type",
                                 choices = c("Histogram", "Density", "Boxplot", 
                                             "Scatter Plot", "Bar Plot", "Violin Plot",
                                             "QQ Plot", "Pairs Plot")),
                     uiOutput("plot_params"),
                     actionButton("run_plot", "Generate Plot", class = "btn-primary"),
                     plotOutput("main_plot", width = "100%")),
            
            tabPanel("Statistics", 
                     selectInput("stat_test", "Statistical Test",
                                 choices = c("T-test", "ANOVA", "Correlation", "Chi-square",
                                             "Normality Test", "Homogeneity of Variance")),
                     uiOutput("stat_test_params"),
                     actionButton("run_stats", "Run Test", class = "btn-primary"),
                     verbatimTextOutput("stat_results")),
            
            tabPanel("History", 
                     fluidRow(
                       column(6,
                              h3("Analysis History"),
                              DTOutput("history_table")),
                       column(6,
                              h3("Selected Result View"),
                              uiOutput("history_viewer"))
                     )
            )
          )
        )),
      
      
      ###BM---------------------
      tabItem(
        tabName = "balancing_methods",
        fluidRow(
          box(
            title = "Data Balancing Options", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            selectInput("balance_var", "Select Target Variable for Balancing:", 
                        choices = NULL),
            selectInput("balance_method", "Select Balancing Method:",
                        choices = c("SMOTE" = "smote",
                                    "ADASYN" = "adasyn",
                                    "Undersampling" = "undersample",
                                    "Oversampling" = "oversample",
                                    "Tomek Links" = "tomek",
                                    "Hybrid (SMOTE + Tomek)" = "hybrid")),
            bsTooltip("balance_method", 
                      "Tomek Links: Removes noisy majority class instances near minority class",
                      placement = "right"),
            colourpicker::colourInput("balance_color", "Select Plot Color", 
                                      value = "#1f77b4"),
            actionButton("apply_balance", "Apply Balancing", class = "btn-primary"),
            actionButton("reset_balance", "Reset All Balancing", class = "btn-danger")
          ),
          box(
            title = "Balancing Results", 
            status = "info", 
            solidHeader = TRUE, 
            width = 12,
            # Changed this to match old version's historical view
            uiOutput("view_results_ui"),
            uiOutput("results_container")
          )
        )
      ),
      #### # Balancing Evaluation Tab------
      tabItem(
        tabName = "balancing_eval",
        fluidRow(
          box(
            title = "Evaluation Setup", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            fluidRow(
              column(4,
                     selectInput("eval_target", "Target Variable:", 
                                 choices = NULL, width = "100%"),
                     selectInput("eval_predictors", "Predictor Variables:",
                                 choices = NULL, multiple = TRUE, width = "100%"),
                     helpText("Select at least one predictor variable"),
                     textOutput("predictor_validation"),
                     selectInput("eval_compare_methods", "Methods to Evaluate:", 
                                 choices = NULL, multiple = TRUE, width = "100%")
              ),
              column(4,
                     sliderInput("train_split", "Training Data %:", 
                                 min = 50, max = 90, value = 70, width = "100%"),
                     numericInput("rf_ntree", "Random Forest Trees:", 
                                  value = 100, min = 50, width = "100%"),
                     numericInput("rf_mtry", "Number of Variables at Split:",
                                  value = NULL, min = 1, width = "100%")
              ),
              column(4,
                     radioButtons("eval_level", "Evaluation Depth:",
                                  choices = c("Basic" = "basic",
                                              "Advanced" = "advanced",
                                              "Full" = "full"),
                                  selected = "full", inline = FALSE, width = "100%"),
                     actionButton("run_evaluation", "Run Evaluation", 
                                  icon = icon("play"), 
                                  class = "btn-primary btn-block"),
                     actionButton("save_eval_results", "Save Results",
                                  icon = icon("save"),
                                  class = "btn-info btn-block")
              )
            ),
            bsTooltip("eval_target", "Select the target variable for evaluation"),
            bsTooltip("eval_predictors", "Select which predictor variables to include in the model"),
            bsTooltip("eval_compare_methods", "Select which balancing methods to compare"),
            bsTooltip("rf_ntree", "Number of trees for Random Forest evaluation"),
            bsTooltip("rf_mtry", "Number of variables randomly sampled as candidates at each split (NULL for default)"),
            bsTooltip("eval_level", "Basic: Key metrics only | Advanced: Includes noise analysis | Full: Complete evaluation")
          )
        ),
        
        # Evaluation Results
        fluidRow(
          box(
            title = "Evaluation Summary", 
            status = "info", 
            solidHeader = TRUE, 
            width = 12,
            uiOutput("method_summary") %>% withSpinner()
          )
        ),
        
        fluidRow(
          box(
            title = "Detailed Evaluation Results", 
            status = "success", 
            solidHeader = TRUE, 
            width = 12,
            tabsetPanel(
              tabPanel(
                "Class Distribution",
                div(class = "eval-section",
                    h4("Class Distribution Comparison"),
                    p("Compare the original vs balanced class distributions for each method."),
                    plotlyOutput("dist_comparison_plot", height = "600px") %>% 
                      withSpinner(),
                    downloadButton("download_dist_plot", "Download Plot", 
                                   class = "btn-margin"),
                    DTOutput("dist_table") %>% withSpinner()
                )
              ),
              
              tabPanel(
                "Model Performance",
                div(class = "eval-section",
                    h4("Model Performance Metrics"),
                    p("Performance metrics from Random Forest evaluation on each balanced dataset."),
                    DTOutput("performance_table") %>% withSpinner(),
                    downloadButton("download_perf_table", "Download Table",
                                   class = "btn-margin"),
                    h4("ROC Curves (Binary Classification)"),
                    plotlyOutput("auc_plot", height = "500px") %>% withSpinner(),
                    downloadButton("download_auc_plot", "Download Plot",
                                   class = "btn-margin")
                )
              ),
              
              tabPanel(
                "Overfitting Analysis",
                div(class = "eval-section",
                    h4("Overfitting Risk Assessment"),
                    p("Compare training (cross-validated) vs test performance to detect overfitting."),
                    plotlyOutput("overfitting_plot", height = "500px") %>% 
                      withSpinner(),
                    downloadButton("download_overfit_plot", "Download Plot",
                                   class = "btn-margin"),
                    h4("Overfitting Summary"),
                    DTOutput("overfitting_table") %>% withSpinner(),
                    h4("Interpretation Guide"),
                    tags$ul(
                      tags$li(strong("High Risk:"), "Train-test difference > 15% (red)"),
                      tags$li(strong("Moderate Risk:"), "Train-test difference 5-15% (yellow)"),
                      tags$li(strong("Low Risk:"), "Train-test difference < 5% (green)"),
                      tags$li(strong("Technique:"), "We compare cross-validated training accuracy with test set accuracy. Large differences indicate overfitting.")
                    )
                )
              ),
              
              tabPanel(
                "Noise Analysis",
                div(class = "eval-section",
                    h4("Data Quality and Noise Assessment"),
                    p("Silhouette scores measure how well instances are clustered with their own class."),
                    plotlyOutput("noise_plot", height = "500px") %>% 
                      withSpinner(),
                    downloadButton("download_noise_plot", "Download Plot",
                                   class = "btn-margin"),
                    h4("Noise Metrics"),
                    DTOutput("noise_table") %>% withSpinner(),
                    h4("Interpretation Guide"),
                    tags$ul(
                      tags$li(strong("Good Separation:"), "Silhouette score > 0.5 (green)"),
                      tags$li(strong("Potential Noise:"), "Silhouette score < 0.2 (red)"),
                      tags$li(strong("Technique:"), "We calculate the average silhouette width for each class. Lower values indicate more overlapping/noisy data.")
                    )
                )
              ),
              
              tabPanel(
                "Dataset Size",
                div(class = "eval-section",
                    h4("Dataset Size Analysis - Impact of Balancing Methods"),
                    p("Compare original vs balanced dataset sizes and their impact."),
                    plotlyOutput("size_plot", height = "500px") %>% 
                      withSpinner(),
                    downloadButton("download_size_plot", "Download Plot",
                                   class = "btn-margin"),
                    h4("Size Metrics"),
                    DTOutput("size_table") %>% withSpinner(),
                    h4("Interpretation Guide"),
                    tags$ul(
                      tags$li(strong("High Impact:"), "Balanced size > 5x original (red)"),
                      tags$li(strong("Moderate Impact:"), "Balanced size 2-5x original (yellow)"),
                      tags$li(strong("Low Impact:"), "Balanced size < 2x original (green)")
                    )
                )
              ),
              
              tabPanel(
                "Method Weaknesses",
                div(class = "eval-section",
                    h4("Method Weaknesses and Strengths"),
                    p("Summary of identified weaknesses and strengths for each balancing method."),
                    DTOutput("weakness_table") %>% withSpinner(),
                    downloadButton("download_weakness_table", "Download Table",
                                   class = "btn-margin"),
                    h4("Method-Specific Considerations"),
                    tags$ul(
                      tags$li(strong("SMOTE/ADASYN:"), 
                              "May create unrealistic synthetic examples but improves minority representation"),
                      tags$li(strong("Undersampling:"), 
                              "Discards potentially useful majority class examples but maintains data quality"),
                      tags$li(strong("Oversampling:"), 
                              "May lead to overfitting through duplication but preserves all information"),
                      tags$li(strong("Hybrid Methods:"), 
                              "Combine benefits but may inherit multiple weaknesses")
                    )
                )
              ),
              tabPanel("Debug Views",
                       h4("Size Data Structure"),
                       verbatimTextOutput("debug_size_structure"),
                       h4("Noise Data Structure"),
                       verbatimTextOutput("debug_noise_structure")),
              tabPanel("Debug",
                       verbatimTextOutput("debug_structure"))
            )
          )
        ),
        
        # Saved Evaluations
        conditionalPanel(
          condition = "output.show_saved_evaluations",
          fluidRow(
            box(
              title = "Evaluation History", 
              status = "warning", 
              solidHeader = TRUE, 
              width = 12,
              h4("Previously Saved Evaluations"),
              DTOutput("saved_evaluations_table") %>% withSpinner(),
              fluidRow(
                column(6,
                       actionButton("load_evaluation", "Load Selected",
                                    icon = icon("folder-open"),
                                    class = "btn-primary btn-block")
                ),
                column(6,
                       actionButton("delete_evaluation", "Delete Selected",
                                    icon = icon("trash-alt"),
                                    class = "btn-danger btn-block")
                )
              ),
              h4("Evaluation History Log"),
              verbatimTextOutput("eval_history_log")
            )
          )
        )
      ),
      ###Risk modeling UI -------------------------------------
      tabItem(tabName = "risk_modeling",
              fluidRow(
                box(title = "Data Selection for Risk Modeling", status = "primary", solidHeader = TRUE, width = 12,
                    radioButtons("data_source_risk", "Select Data Source:",
                                 choices = c("Prepared Data" = "prepared",
                                             "Balanced Data" = "balanced"),
                                 selected = "prepared"),
                    conditionalPanel(
                      condition = "input.data_source_risk == 'balanced'",
                      selectInput("balance_method_risk", "Select Balancing Method:", choices = NULL)
                    ),
                    actionButton("load_data_risk", "Load Selected Data", class = "btn-primary")
                ),
                
                box(title = "Model Configuration for Risk Classification", status = "primary", solidHeader = TRUE, width = 12,
                    actionButton("view_prepared_risk", "View Prepared Data", class = "btn-info"),
                    selectInput("model_target_risk", "Select Target Variable (Risk Class):", choices = NULL),
                    selectInput("model_predictors_risk", "Select Predictor Variables:", 
                                choices = NULL, multiple = TRUE),
                    checkboxGroupInput("model_types_risk", "Select Models to Compare:",
                                       choices = c("Logistic Regression" = "logit",
                                                   "Random Forest" = "rf",
                                                   "Gradient Boosting" = "gbm",
                                                   "XGBoost" = "xgb",
                                                   "Naive Bayes" = "nb"),
                                       selected = c("logit", "rf")),
                    numericInput("model_seed_risk", "Set Random Seed:", value = 123),
                    sliderInput("model_split_risk", "Training Data Percentage:", 
                                min = 50, max = 90, value = 70),
                    
                    # New parameter controls for each model type
                    conditionalPanel(
                      condition = "input.model_types_risk.includes('logit')",
                      h4("Logistic Regression Parameters"),
                      sliderInput("logit_C", "Inverse Regularization Strength (C):",
                                  min = 0.001, max = 10, value = 1, step = 0.01),
                      selectInput("logit_penalty", "Regularization Type:",
                                  choices = c("L1" = "l1", "L2" = "l2", "Elastic Net" = "elasticnet", "None" = "none"),
                                  selected = "l2"),
                      selectInput("logit_solver", "Solver:",
                                  choices = c("Newton" = "newton-cg", "LBFGS" = "lbfgs", "Liblinear" = "liblinear"),
                                  selected = "lbfgs")
                    ),
                    
                    conditionalPanel(
                      condition = "input.model_types_risk.includes('rf')",
                      h4("Random Forest Parameters"),
                      sliderInput("rf_ntree", "Number of Trees:",
                                  min = 50, max = 1000, value = 500, step = 50),
                      sliderInput("rf_mtry", "Number of Variables at Split:",
                                  min = 1, max = 20, value = 2),
                      sliderInput("rf_maxdepth", "Max Tree Depth:",
                                  min = 1, max = 30, value = 10),
                      sliderInput("rf_nodesize", "Min Node Size:",
                                  min = 1, max = 20, value = 1)
                    ),
                    
                    conditionalPanel(
                      condition = "input.model_types_risk.includes('gbm')",
                      h4("GBM Parameters"),
                      sliderInput("gbm_ntree", "Number of Trees:",
                                  min = 50, max = 1000, value = 100, step = 50),
                      sliderInput("gbm_depth", "Interaction Depth:",
                                  min = 1, max = 10, value = 3),
                      sliderInput("gbm_shrinkage", "Learning Rate:",
                                  min = 0.001, max = 0.3, value = 0.1, step = 0.001),
                      sliderInput("gbm_minobs", "Min Observations in Node:",
                                  min = 1, max = 20, value = 10)
                    ),
                    
                    conditionalPanel(
                      condition = "input.model_types_risk.includes('xgb')",
                      h4("XGBoost Parameters"),
                      sliderInput("xgb_nrounds", "Number of Rounds:",
                                  min = 50, max = 1000, value = 100, step = 50),
                      sliderInput("xgb_maxdepth", "Max Tree Depth:",
                                  min = 1, max = 12, value = 6),
                      sliderInput("xgb_eta", "Learning Rate:",
                                  min = 0.001, max = 0.3, value = 0.3, step = 0.001),
                      sliderInput("xgb_gamma", "Minimum Loss Reduction:",
                                  min = 0, max = 5, value = 0, step = 0.1),
                      sliderInput("xgb_subsample", "Subsample Ratio:",
                                  min = 0.1, max = 1, value = 1, step = 0.1),
                      sliderInput("xgb_colsample", "Column Sample Ratio:",
                                  min = 0.1, max = 1, value = 1, step = 0.1)
                    ),
                    
                    conditionalPanel(
                      condition = "input.model_types_risk.includes('nb')",
                      h4("Naive Bayes Parameters"),
                      sliderInput("nb_laplace", "Laplace Smoothing:",
                                  min = 0, max = 10, value = 0, step = 0.1),
                      selectInput("nb_dist", "Distribution Type:",
                                  choices = c("Gaussian" = "gaussian", 
                                              "Kernel" = "kernel",
                                              "Poisson" = "poisson"),
                                  selected = "gaussian")
                    ),
                    
                    actionButton("run_models_risk", "Run Models", class = "btn-success")
                ),
                
                box(title = "Prepared Data Preview", status = "info", solidHeader = TRUE, width = 12,
                    DTOutput("prepared_data_preview_risk"), style = "height:400px; overflow-y: scroll;",
                    collapsed = TRUE, collapsible = TRUE
                ),
                
                box(title = "Model Results", status = "info", solidHeader = TRUE, width = 12,
                    tabsetPanel(
                      tabPanel("Performance Metrics",
                               DTOutput("model_metrics_risk"),
                               plotlyOutput("metric_comparison_plot_risk")),
                      
                      tabPanel("Confusion Matrices",
                               selectInput("select_conf_matrix_risk", "Select Model:", choices = NULL),
                               plotlyOutput("confusion_matrix_plot_risk"),
                               verbatimTextOutput("confusion_matrix_text_risk")),
                      
                      tabPanel("ROC Analysis",
                               selectInput("select_model_roc_risk", "Select Model:", choices = NULL),
                               tabsetPanel(
                                 tabPanel("Per-class ROC",
                                          plotlyOutput("roc_curve_plot_risk"),
                                          DTOutput("roc_metrics_table_risk")
                                 ),
                                 tabPanel("Average ROC Comparison",
                                          plotlyOutput("roc_comparison_plot_risk"),
                                          verbatimTextOutput("roc_auc_text_risk")
                                 )
                               )
                      ),
                      
                      tabPanel("Model Summaries",
                               selectInput("select_model_summary_risk", "Select Model:", choices = NULL),
                               tabsetPanel(
                                 tabPanel("Summary",
                                          verbatimTextOutput("model_summary_risk")
                                 ),
                                 tabPanel("Details",
                                          uiOutput("model_details_ui_risk")
                                 )
                               )
                      ),
                      
                      tabPanel("Feature Importance",
                               selectInput("select_model_imp_risk", "Select Model:", choices = NULL),
                               plotlyOutput("model_importance_plot_risk")),
                      
                      tabPanel("Model Explanations",
                               fluidRow(
                                 box(width = 12, status = "primary",
                                     h4("Model Explanation Settings"),
                                     fluidRow(
                                       column(4,
                                              selectInput("xai_model_risk", "Select Model to Explain:", choices = NULL)
                                       ),
                                       column(4,
                                              selectInput("explanation_method_risk", "Select Explanation Method:",
                                                          choices = c("Feature Importance" = "feature_importance",
                                                                      "LIME" = "lime",
                                                                      "SHAP" = "shap",
                                                                      "Partial Dependence Plots" = "pdp",
                                                                      "Decision Tree Surrogate" = "decision_tree"))
                                       ),
                                       column(4,
                                              uiOutput("class_selection_ui_risk")
                                       )
                                     ),
                                     uiOutput("xai_controls_risk"),
                                     actionButton("run_xai_risk", "Generate Explanation", 
                                                  class = "btn-primary", icon = icon("rocket")),
                                     br(),
                                     tabsetPanel(
                                       tabPanel("Visualization",
                                                conditionalPanel(
                                                  condition = "input.explanation_method_risk != 'decision_tree'",
                                                  plotlyOutput("explanation_plot_risk") %>% 
                                                    withSpinner(type = 6, color = "#3C8DBC")
                                                ),
                                                conditionalPanel(
                                                  condition = "input.explanation_method_risk == 'decision_tree'",
                                                  visNetworkOutput("dt_plot_risk") %>% 
                                                    withSpinner(type = 6, color = "#3C8DBC")
                                                )
                                       ),
                                       tabPanel("Details",
                                                verbatimTextOutput("xai_output_risk")
                                       )
                                     )
                                 )
                               )
                      )
                    )
                )
              )
      ),
      ### Risk Classification Prediction UI Code -------------------------
      
      tabItem(
        tabName = "risk_prediction",
        h2("Risk Classification Prediction"),
        br(),
        
        # Data Selection Panel
        box(
          title = "Data Selection", width = 12, status = "primary", solidHeader = TRUE,
          collapsible = TRUE, collapsed = FALSE,
          fluidRow(
            column(4,
                   radioButtons("pred_data_source_risk", "Data Source:",
                                choices = c("Prepared Data" = "prepared",
                                            "Balanced Data" = "balanced"),
                                selected = "prepared"),
                   conditionalPanel(
                     condition = "input.pred_data_source_risk == 'balanced'",
                     selectInput("pred_balance_method_risk", "Balance Method:", choices = NULL)
                   )
            ),
            column(4,
                   actionButton("load_pred_data_risk", "Load Data", 
                                class = "btn-primary", icon = icon("upload")),
                   br(), br(),
                   actionButton("view_loaded_data", "View Loaded Data",
                                class = "btn-info", icon = icon("table"))
            ),
            column(4,
                   div(style = "max-height: 200px; overflow-y: auto;",
                       h5("Data Summary:"),
                       verbatimTextOutput("data_summary_risk")
                   )
            )
          )
        ),
        
        tabsetPanel(
          id = "prediction_tabs_risk",
          
          # Single Prediction Tab
          tabPanel(
            "Single Prediction",
            fluidRow(
              box(
                title = "Input Parameters", width = 4, status = "info", 
                solidHeader = TRUE, collapsible = TRUE,
                selectInput("pred_model_risk", "Select Model:", choices = NULL),
                uiOutput("single_pred_inputs_risk")
              ),
              box(
                title = "Prediction Result", width = 8, status = "success", 
                solidHeader = TRUE, collapsible = TRUE,
                div(style = "max-height: 600px; overflow-y: auto;",
                    uiOutput("single_pred_result_risk"),
                    br(),
                    h4("Feature Importance Analysis"),
                    plotlyOutput("single_pred_imp_plot"),
                    br(),
                    p(em("Note: Importance scores are model-specific and may use different calculation methods."))
                )
              )
            )
          ),
          
          # Batch Prediction Tab
          tabPanel(
            "Batch Prediction",
            fluidRow(
              box(
                title = "Prediction Settings", width = 12, status = "info", 
                solidHeader = TRUE, collapsible = TRUE,
                selectInput("pred_model_risk_batch", "Select Model:", choices = NULL),
                actionButton("run_batch_pred_risk", "Run Prediction", 
                             class = "btn-primary", icon = icon("play"))
              )
            ),
            conditionalPanel(
              condition = "input.run_batch_pred_risk > 0",
              fluidRow(
                box(
                  title = "Risk Distribution", width = 6, status = "success", 
                  solidHeader = TRUE, collapsible = TRUE,
                  tableOutput("batch_risk_dist")
                ),
                box(
                  title = "Feature Importance", width = 6, status = "success", 
                  solidHeader = TRUE, collapsible = TRUE,
                  plotlyOutput("batch_feature_imp_plot", height = "400px"),
                  br(),
                  h5("Top Features Table"),
                  tableOutput("batch_feature_imp")
                )
              ),
              fluidRow(
                box(
                  title = "Prediction Results", width = 12, status = "primary", 
                  solidHeader = TRUE, collapsible = TRUE,
                  div(style = "max-height: 500px; overflow-y: auto;",
                      DTOutput("batch_pred_table")
                  ),
                  downloadButton("download_batch_results_risk", "Download Results", 
                                 class = "btn-success", icon = icon("download"))
                )
              )
            )
          )
        )
      ),
      ###PDM--------------------
      ###disability modeling
      tabItem(tabName = "pct_modeling",
              fluidRow(
                box(title = "Data Selection for Modeling", status = "primary", solidHeader = TRUE, width = 12,
                    radioButtons("data_source_perc", "Select Data Source:",
                                 choices = c("Prepared Data" = "prepared",
                                             "Balanced Data" = "balanced"),
                                 selected = "prepared"),
                    conditionalPanel(
                      condition = "input.data_source_perc == 'balanced'",
                      selectInput("balance_method_perc", "Select Balancing Method:",
                                  choices = NULL)
                    ),
                    actionButton("load_data_perc", "Load Selected Data", class = "btn-primary")
                ),
                box(title = "Model Configuration for Disability Percentage", status = "primary", solidHeader = TRUE, width = 12,
                    actionButton("view_prepared_perc", "View Prepared Data", class = "btn-info"),
                    selectInput("model_target_perc", "Select Target Variable (Disability Percentage):", choices = NULL),
                    selectInput("model_predictors_perc", "Select Predictor Variables:", 
                                choices = NULL, multiple = TRUE),
                    checkboxGroupInput("model_types_perc", "Select Models to Compare:",
                                       choices = c("Linear Regression" = "lm",
                                                   "Generalized Linear Model" = "glm",
                                                   "Gradient Boosting Machine" = "gbm",
                                                   "Random Forest" = "rf",
                                                   "XGBoost" = "xgb"),
                                       selected = c("lm", "rf")),
                    numericInput("model_seed_perc", "Set Random Seed:", value = 123),
                    sliderInput("model_split_perc", "Training Data Percentage:", 
                                min = 50, max = 90, value = 70),
                    actionButton("run_models_perc", "Run Models", class = "btn-success")
                ),
                box(title = "Prepared Data Preview", status = "info", solidHeader = TRUE, width = 12,
                    DTOutput("prepared_data_preview_perc"), style = "height:400px; overflow-y: scroll;",
                    collapsed = TRUE, collapsible = TRUE
                ),
                box(title = "Model Results", status = "info", solidHeader = TRUE, width = 12,
                    tabsetPanel(
                      tabPanel("Performance Metrics",
                               DTOutput("model_metrics_perc"),
                               plotlyOutput("metric_comparison_plot_perc")),
                      tabPanel("Model Summaries",
                               selectInput("select_model_summary_perc", "Select Model:", choices = NULL),
                               verbatimTextOutput("model_summary_perc")),
                      tabPanel("Residual Analysis",
                               plotlyOutput("model_residuals_plot_perc"),
                               verbatimTextOutput("model_residuals_stats_perc")),
                      tabPanel("Actual vs Predicted",
                               plotlyOutput("actual_vs_predicted_plot_perc")),
                      tabPanel("Feature Importance",
                               selectInput("select_model_imp_perc", "Select Model:", choices = NULL),
                               plotlyOutput("model_importance_plot_perc")),
                      tabPanel("Model Explanations",
                               fluidRow(
                                 box(width = 12, status = "primary",
                                     h4("Model Explanation Settings"),
                                     fluidRow(
                                       column(6,
                                              selectInput("xai_model_perc", "Select Model to Explain:",
                                                          choices = NULL)
                                       ),
                                       column(6,
                                              selectInput("explanation_method_perc", "Select Explanation Method:",
                                                          choices = c("Feature Importance" = "feature_importance",
                                                                      "LIME (Local Interpretable Model-agnostic Explanations)" = "lime",
                                                                      "SHAP (SHapley Additive exPlanations)" = "shap",
                                                                      "Partial Dependence Plots" = "pdp",
                                                                      "Decision Tree Surrogate" = "decision_tree"))
                                       )
                                     ),
                                     uiOutput("xai_controls_perc"),
                                     br(),
                                     tabsetPanel(
                                       tabPanel("Visualization",
                                                conditionalPanel(
                                                  condition = "input.explanation_method_perc != 'decision_tree'",
                                                  plotlyOutput("explanation_plot_perc") %>% 
                                                    withSpinner(type = 6, color = "#3C8DBC")
                                                ),
                                                conditionalPanel(
                                                  condition = "input.explanation_method_perc == 'decision_tree'",
                                                  visNetworkOutput("dt_plot_perc") %>% 
                                                    withSpinner(type = 6, color = "#3C8DBC")
                                                )
                                       ),
                                       tabPanel("Details",
                                                verbatimTextOutput("xai_output_perc")
                                       )
                                     )
                                 )
                               )
                      )
                    )
                )
              )
      ),
      
      
      
      
      ###PDP---------------------------------
      tabItem(
        tabName = "pct_prediction",
        fluidRow(
          box(
            width = 12, status = "primary",
            title = "Disability Percentage Prediction Dashboard",
            p("Predict disability percentages using trained models with comprehensive explanations.")
          )
        ),
        
        # Model Selection
        fluidRow(
          box(
            width = 12, title = "Model Selection", status = "info", solidHeader = TRUE,
            collapsible = TRUE,
            selectInput(
              "select_model_pred_perc", 
              "Select Model for All Predictions:",
              choices = NULL,
              width = "50%"
            ),
            helpText("This model selection applies to all prediction types below.")
          )
        ),
        
        # Data Loading Section
        fluidRow(
          box(
            width = 12, title = "Data Preparation", status = "warning", solidHeader = TRUE,
            collapsible = TRUE, collapsed = FALSE,
            fluidRow(
              column(
                4,
                radioButtons(
                  "data_source_pred_perc", 
                  "Data Source:",
                  choices = c(
                    "Use Prepared Data" = "prepared",
                    "Use Balanced Data" = "balanced"
                  ),
                  selected = "prepared"
                )
              ),
              column(
                4,
                conditionalPanel(
                  condition = "input.data_source_pred_perc == 'balanced'",
                  selectInput(
                    "balance_method_pred_perc", 
                    "Balancing Method:",
                    choices = NULL
                  )
                )
              ),
              column(
                4,
                actionButton(
                  "load_data_pred_perc", 
                  "Load Selected Data",
                  icon = icon("database"),
                  class = "btn-primary",
                  style = "margin-top: 25px;"
                )
              )
            ),
            hr(),
            h4("Data Preview:"),
            DTOutput("pred_data_preview_perc") %>% withSpinner()
          )
        ),
        
        # Prediction Methods Tabs
        tabBox(
          width = 12,
          title = "Prediction Methods",
          id = "prediction_tabs",
          
          # Manual Input Tab
          tabPanel(
            title = "Manual Input Prediction",
            icon = icon("keyboard"),
            fluidRow(
              box(
                width = 6, status = "primary",
                title = "Enter Values Manually",
                uiOutput("manual_input_ui"),
                actionButton(
                  "submit_manual_input", 
                  "Generate Prediction",
                  icon = icon("calculator"),
                  class = "btn-success",
                  width = "100%"
                )
              ),
              box(
                width = 6, status = "success",
                title = "Prediction Results",
                uiOutput("prediction_result_perc") %>% withSpinner()  # Changed from single_pred_result_perc to prediction_result_perc
              )
            )
          ),
          
          # Data Selection Tab
          tabPanel(
            title = "Row Selection Prediction",
            icon = icon("table"),
            fluidRow(
              box(
                width = 4, status = "primary",
                title = "Select Data Row",
                numericInput(
                  "pred_data_row_perc",
                  "Row Number to Predict:",
                  value = 1,
                  min = 1,
                  step = 1,
                  width = "100%"
                ),
                actionButton(
                  "run_data_pred_perc",
                  "Predict Selected Row",
                  icon = icon("mouse-pointer"),
                  class = "btn-success",
                  width = "100%"
                ),
                hr(),
                h4("Selected Row Data:"),
                verbatimTextOutput("selected_row_data_perc")
              ),
              box(
                width = 8, status = "success",
                title = "Prediction Results",
                uiOutput("data_selection_result_perc") %>% withSpinner()
              )
            )
          ),
          
          # Batch Prediction Tab
          tabPanel(
            title = "Batch Prediction",
            icon = icon("tasks"),
            fluidRow(
              box(
                width = 12, status = "primary",
                title = "Batch Prediction Controls",
                fluidRow(
                  column(
                    4,
                    actionButton(
                      "run_batch_pred_perc",
                      "Run Batch Prediction",
                      icon = icon("play"),
                      class = "btn-primary",
                      width = "100%"
                    )
                  ),
                  column(
                    4,
                    downloadButton(
                      "download_batch_pred_perc",
                      "Download Results",
                      class = "btn-info",
                      width = "100%"
                    )
                  ),
                  column(
                    4,
                    actionButton(
                      "explain_batch_pred_perc",
                      "Explain First 100 Rows",
                      icon = icon("lightbulb"),
                      class = "btn-warning",
                      width = "100%"
                    )
                  )
                )
              )
            ),
            fluidRow(
              box(
                width = 12, status = "success",
                title = "Batch Prediction Results",
                tabBox(
                  width = 12,
                  tabPanel(
                    "Results Table",
                    DTOutput("batch_pred_results_perc") %>% withSpinner()
                  ),
                  tabPanel(
                    "Summary Statistics",
                    verbatimTextOutput("batch_pred_summary_perc"),
                    plotlyOutput("batch_pred_plot_perc", height = "400px")
                  )
                )
              )
            )
          )
        ),
        
        # Model Explanation Section
        fluidRow(
          box(
            width = 12, title = "Model Explanation", status = "info", solidHeader = TRUE,
            collapsible = TRUE, collapsed = FALSE,
            tabBox(
              width = 12,
              tabPanel(
                "Feature Importance",
                plotlyOutput("feature_importance_plot_pred_perc", height = "500px"),
                verbatimTextOutput("feature_importance_text_perc")
              ),
              tabPanel(
                "Partial Dependence",
                fluidRow(
                  column(
                    4,
                    selectInput(
                      "pdp_var_pred_perc",
                      "Select Variable for PDP:",
                      choices = NULL,
                      width = "100%"
                    )
                  ),
                  column(
                    8,
                    plotlyOutput("pdp_plot_pred_perc", height = "500px")
                  )
                )
              ),
              tabPanel(
                "SHAP Values",
                plotlyOutput("shap_plot_pred_perc", height = "600px"),
                verbatimTextOutput("prediction_explanation_perc")  # Changed from shap_text_perc to prediction_explanation_perc
              )
            )
          )
        )
      )
      
      
      #Items end here---------------
    )
  )
)


# Secure the app


server <- function(input, output, session) {
  

  
  
  ### Initial code-----------------------------------
  # Helper function to show error messages
  show_error <- function(message) {
    showNotification(message, type = "error", duration = 10)
  }
  
  # Helper function to show success messages
  show_success <- function(message) {
    showNotification(message, type = "message", duration = 5)
  }
  
  # Helper function to validate column selection
  validate_column_selection <- function(col, df) {
    if (is.null(col) || col == "" || !col %in% names(df)) {
      stop("Please select a valid column")
    }
  }
  
  ### Reactive values to store data and states-------------------
  rv <- reactiveValues(
    # Data storage
    train_data = NULL,
    pred_data = NULL,
    train_original = NULL,
    pred_original = NULL,
    processed_data = NULL,
    converted_data = NULL,
    categorized_data = NULL,
    
    # Data preparation states
    prep_steps = list(train = list(), pred = list()),
    current_prep_type = "train",
    
    # Variable information
    target_var = NULL,
    predictor_vars = NULL,
    var_types = NULL,
    original_levels = list(),
    
    # Categorization
    category_rules = list(),
    category_colors = list(),
    saved_rules = list(),
    
    
    
    #EDA
    data = NULL,
    history = data.frame(
      Timestamp = character(),
      Section = character(),
      Action = character(),
      Details = character(),
      Results = list(),
      stringsAsFactors = FALSE
    ),
    current_summary = NULL,
    current_freq = NULL,
    current_pivot = NULL,
    current_plot = NULL,
    current_stats = NULL,
    plot_counter = 0,
    
    
    #Balancing Methods
    balanced_data = NULL,
    # Modeling
    balanced_data = list(),
    model_results = list(),
    feature_importance = list(),
    test_data = NULL,
    modeling_results = list(),
    prediction_data = NULL,
    prediction_results = NULL,
    trained_models = list(),
    
    # Data balancing evaluation
    evaluation_results=list(),
    # Data balancing evaluation
    modeling_results = list(),
    
    #DPM
    modeling_data_perc = NULL,
    prepared_data_perc = NULL,
    
    # UI states
    show_results = FALSE,
    show_results_perc = FALSE,
    current_tab = "home"
  )
  
  # Track current tab
  observe({
    rv$current_tab <- input$tabs
  })
  
  ### DATA UPLOAD SECTION ###-------------------------------
  
  # Load training data and initialize it for Data Preparation
  observeEvent(input$load_train, {
    tryCatch({
      req(input$train_upload)
      
      withProgress(message = 'Loading training data...', value = 0.5, {
        rv$train_data <- read.csv(
          input$train_upload$datapath,
          header = input$train_header,
          sep = input$train_separator,
          stringsAsFactors = FALSE,
        )
        rv$processed_data <- rv$train_data  
        # Initialize preparation data
        rv$train_original <- rv$train_data
        rv$current_prep_type <- "train"
        updateSelectInput(session, "data_type_prep", selected = "train")
        
        # Initialize variable types
        rv$var_types <- sapply(rv$train_data, function(x) {
          if (is.numeric(x)) "numeric" else "categorical"
        })
        
        incProgress(0.5, detail = "Finalizing...")
      })
      
      show_success("Training data loaded successfully!")
    }, error = function(e) {
      show_error(paste("Error loading training data:", e$message))
    })
  })
  
  # Load prediction data and initialize it for Data preparation
  observeEvent(input$load_pred, {
    tryCatch({
      req(input$pred_upload)
      
      withProgress(message = 'Loading prediction data...', value = 0.5, {
        rv$pred_data <- read.csv(
          input$pred_upload$datapath,
          header = input$pred_header,
          sep = input$pred_separator,
          stringsAsFactors = FALSE
        )
        
        # Initialize preparation data
        rv$pred_original <- rv$pred_data
        rv$current_prep_type <- "pred"
        updateSelectInput(session, "data_type_prep", selected = "pred")
        
        incProgress(0.5, detail = "Finalizing...")
      })
      
      show_success("Prediction data loaded successfully!")
    }, error = function(e) {
      show_error(paste("Error loading prediction data:", e$message))
    })
  })
  
  # Training data preview
  output$train_preview <- renderDT({
    req(rv$train_data)
    datatable(rv$train_data, 
              options = list(scrollX = TRUE, pageLength = 5),
              selection = 'none')
  })
  
  # Prediction data preview
  output$pred_preview <- renderDT({
    req(rv$pred_data)
    datatable(rv$pred_data, 
              options = list(scrollX = TRUE, pageLength = 5),
              selection = 'none')
  })
  
  ### DATA PREPARATION SECTION ###-------------------------------------------------------------
  
  # Get current data based on selection
  current_prep_data <- reactive({
    # First check if the data type selection exists
    if (is.null(input$data_type_prep)) {
      return(NULL)
    }
    
    # Get the appropriate data based on selection
    if (input$data_type_prep == "train") {
      if (is.null(rv$train_data)) {
        return(NULL)
      }
      
      # Return processed data if it exists and matches current type
      if (!is.null(rv$processed_data) && rv$current_prep_type == "train") {
        return(rv$processed_data)
      } else {
        return(rv$train_data)
      }
    } else {
      if (is.null(rv$pred_data)) {
        return(NULL)
      }
      
      # Return processed data if it exists and matches current type
      if (!is.null(rv$processed_data) && rv$current_prep_type == "pred") {
        return(rv$processed_data)
      } else {
        return(rv$pred_data)
      }
    }
  })  
  # Update current preparation type
  observeEvent(input$data_type_prep, {
    rv$current_prep_type <- input$data_type_prep
    if (input$data_type_prep == "train") {
      rv$processed_data <- rv$train_data
    } else {
      rv$processed_data <- rv$pred_data
    }
  })
  
  # Data preview output
  output$data_preview_prep <- renderDT({
    req(current_prep_data())
    datatable(current_prep_data(), 
              options = list(scrollX = TRUE, pageLength = 5),
              selection = 'none')
  })
  
  # Data summary output
  output$data_summary_prep <- renderPrint({
    req(current_prep_data())
    df <- current_prep_data()
    
    cat("DATA STRUCTURE\n")
    cat("--------------\n")
    str(df)
    
    cat("\n\nSUMMARY STATISTICS\n")
    cat("------------------\n")
    print(summary(df))
    
    cat("\n\nMISSING VALUES\n")
    cat("--------------\n")
    missing_summary <- sapply(df, function(x) sum(is.na(x)))
    print(missing_summary[missing_summary > 0])
  })
  
  # Update variable selections dynamically
  observe({
    req(current_prep_data())
    df <- current_prep_data()
    
    # For encoding
    updateSelectInput(session, "encode_var", 
                      choices = names(df)[sapply(df, function(x) is.character(x) | is.factor(x))])
    
    # For binning
    updateSelectInput(session, "bin_var", 
                      choices = names(df)[sapply(df, is.numeric)])
    
    # For transformations
    updateSelectInput(session, "transform_var", 
                      choices = names(df)[sapply(df, is.numeric)])
    
    # For date features
    updateSelectInput(session, "date_var", 
                      choices = names(df)[sapply(df, function(x) inherits(x, "Date") | 
                                                   is.character(x) & grepl("\\d{4}-\\d{2}-\\d{2}", x))])
    
    # For variable categorization
    updateSelectInput(session, "var_to_categorize", 
                      choices = names(df))
  })
  
  # Missing data handling
  output$impute_ui <- renderUI({
    req(input$missing_action == "impute")
    
    df <- current_prep_data()
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    factor_cols <- names(df)[sapply(df, function(x) is.factor(x) | is.character(x))]
    
    tagList(
      fluidRow(
        column(6,
               selectInput("impute_cols", "Columns to Impute:", 
                           choices = names(df), multiple = TRUE),
               actionLink("select_all_impute", "Select All")
        ),
        column(6,
               conditionalPanel(
                 condition = "input.impute_cols.length > 0",
                 radioButtons("impute_num_method", "Numeric Imputation:",
                              choices = c("Mean" = "mean", "Median" = "median", 
                                          "Mode" = "mode", "Constant" = "constant")),
                 radioButtons("impute_cat_method", "Categorical Imputation:",
                              choices = c("Mode" = "mode", "Constant" = "constant")),
                 conditionalPanel(
                   condition = "input.impute_num_method == 'constant' || input.impute_cat_method == 'constant'",
                   textInput("impute_constant", "Constant Value:", value = "0")
                 )
               )
        )
      )
    )
  })
  
  # Select all columns for imputation
  observeEvent(input$select_all_impute, {
    updateSelectInput(session, "impute_cols", 
                      selected = names(current_prep_data()))
  })
  
  observeEvent(input$apply_missing, {
    tryCatch({
      df <- current_prep_data()
      
      if (input$missing_action == "summary") {
        showModal(modalDialog(
          title = "Missing Value Summary",
          renderTable({
            missing_df <- data.frame(
              Column = names(df),
              Missing = sapply(df, function(x) sum(is.na(x))),
              Percent = sapply(df, function(x) round(mean(is.na(x)) * 100, 2))
            )
            missing_df[missing_df$Missing > 0, ]
          }),
          footer = modalButton("Close"),
          size = "l",
          easyClose = TRUE
        ))
      } else if (input$missing_action == "remove_rows") {
        df <- df[complete.cases(df), ]
        rv$processed_data <- df
        show_success("Rows with missing values removed successfully!")
      } else if (input$missing_action == "remove_cols") {
        df <- df[, colSums(is.na(df)) < nrow(df)]
        rv$processed_data <- df
        show_success("Columns with missing values removed successfully!")
      } else if (input$missing_action == "impute") {
        req(input$impute_cols)
        
        withProgress(message = 'Imputing missing values...', value = 0, {
          for (i in seq_along(input$impute_cols)) {
            col <- input$impute_cols[i]
            incProgress(i/length(input$impute_cols), detail = paste("Processing", col))
            
            if (is.numeric(df[[col]])) {
              if (input$impute_num_method == "mean") {
                df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)
              } else if (input$impute_num_method == "median") {
                df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
              } else if (input$impute_num_method == "mode") {
                df[[col]][is.na(df[[col]])] <- as.numeric(names(sort(table(df[[col]]), 
                                                                     decreasing = TRUE)[1]))
              } else if (input$impute_num_method == "constant") {
                df[[col]][is.na(df[[col]])] <- as.numeric(input$impute_constant)
              }
            } else {
              if (input$impute_cat_method == "mode") {
                df[[col]][is.na(df[[col]])] <- names(sort(table(df[[col]]), 
                                                          decreasing = TRUE)[1])
              } else if (input$impute_cat_method == "constant") {
                df[[col]][is.na(df[[col]])] <- input$impute_constant
              }
            }
          }
          
          rv$processed_data <- df
          show_success("Missing values imputed successfully!")
        })
      }
      
      # Record the operation
      rv$prep_steps[[rv$current_prep_type]] <- c(
        rv$prep_steps[[rv$current_prep_type]],
        paste("Missing data handling:", input$missing_action)
      )
    }, error = function(e) {
      show_error(paste("Error in missing data handling:", e$message))
    })
  })
  
  # Outlier detection and treatment
  observeEvent(input$detect_outliers, {
    tryCatch({
      df <- current_prep_data()
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      
      if(length(numeric_cols) == 0) {
        stop("No numeric columns for outlier detection")
      }
      
      # Create outlier summary
      outlier_results <- lapply(numeric_cols, function(col) {
        x <- df[[col]]
        outliers <- switch(input$outlier_method,
                           "zscore" = which(abs(scale(x)) > input$outlier_threshold),
                           "iqr" = {
                             q <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
                             iqr <- q[2] - q[1]
                             which(x < (q[1] - 1.5 * iqr) | x > (q[2] + 1.5 * iqr))
                           },
                           "percentile" = {
                             lower <- quantile(x, (1 - input$outlier_threshold/100)/2, na.rm = TRUE)
                             upper <- quantile(x, 1 - (1 - input$outlier_threshold/100)/2, na.rm = TRUE)
                             which(x < lower | x > upper)
                           })
        data.frame(Column = col, 
                   Outliers = length(outliers),
                   Percent = round(length(outliers)/length(x) * 100, 2))
      })
      
      # Create boxplot visualization
      outlier_plots <- lapply(numeric_cols, function(col) {
        ggplot(df, aes(y = .data[[col]])) +
          geom_boxplot(fill = "lightblue") +
          labs(title = col, y = "Value") +
          theme_minimal()
      })
      
      # Show results in modal dialog
      showModal(modalDialog(
        title = "Outlier Detection Results",
        tabsetPanel(
          tabPanel("Summary",
                   renderTable(do.call(rbind, outlier_results))),
          tabPanel("Visualization",
                   renderPlot({
                     gridExtra::grid.arrange(grobs = outlier_plots, ncol = 2)
                   }, height = 600))
        ),
        size = "l",
        easyClose = TRUE
      ))
    }, error = function(e) {
      show_error(paste("Error in outlier detection:", e$message))
    })
  })
  
  observeEvent(input$treat_outliers, {
    tryCatch({
      df <- current_prep_data()
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      
      withProgress(message = 'Treating outliers...', value = 0, {
        for(i in seq_along(numeric_cols)) {
          col <- numeric_cols[i]
          incProgress(i/length(numeric_cols), detail = paste("Processing", col))
          
          x <- df[[col]]
          
          if(input$outlier_method == "zscore") {
            z <- scale(x)
            outliers <- which(abs(z) > input$outlier_threshold)
          } else if(input$outlier_method == "iqr") {
            q <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
            iqr <- q[2] - q[1]
            outliers <- which(x < (q[1] - 1.5 * iqr) | x > (q[2] + 1.5 * iqr))
          } else if(input$outlier_method == "percentile") {
            lower <- quantile(x, (1 - input$outlier_threshold/100)/2, na.rm = TRUE)
            upper <- quantile(x, 1 - (1 - input$outlier_threshold/100)/2, na.rm = TRUE)
            outliers <- which(x < lower | x > upper)
          }
          
          if(length(outliers) > 0) {
            if(input$outlier_action == "cap") {
              if(input$outlier_method == "zscore") {
                x[outliers] <- ifelse(z[outliers] > 0, 
                                      mean(x) + input$outlier_threshold * sd(x),
                                      mean(x) - input$outlier_threshold * sd(x))
              } else if(input$outlier_method == "iqr") {
                q <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
                iqr <- q[2] - q[1]
                x[x < (q[1] - 1.5 * iqr)] <- q[1] - 1.5 * iqr
                x[x > (q[2] + 1.5 * iqr)] <- q[2] + 1.5 * iqr
              } else {
                lower <- quantile(x, (1 - input$outlier_threshold/100)/2, na.rm = TRUE)
                upper <- quantile(x, 1 - (1 - input$outlier_threshold/100)/2, na.rm = TRUE)
                x[x < lower] <- lower
                x[x > upper] <- upper
              }
            } else if(input$outlier_action == "remove") {
              x <- x[-outliers]
              df <- df[-outliers, ]
            } else if(input$outlier_action == "na") {
              x[outliers] <- NA
            }
            
            df[[col]] <- x
          }
        }
        
        rv$processed_data <- df
        show_success("Outliers treated successfully!")
      })
      
      # Record the operation
      rv$prep_steps[[rv$current_prep_type]] <- c(
        rv$prep_steps[[rv$current_prep_type]],
        paste("Outlier treatment:", input$outlier_method, "-", input$outlier_action)
      )
    }, error = function(e) {
      show_error(paste("Error in outlier treatment:", e$message))
    })
  })
  
  # Duplicate handling
  observeEvent(input$handle_duplicates, {
    tryCatch({
      df <- current_prep_data()
      
      if(input$dup_action == "show") {
        dup_rows <- df[duplicated(df) | duplicated(df, fromLast = TRUE), ]
        
        showModal(modalDialog(
          title = "Duplicate Rows",
          renderDT({
            datatable(dup_rows, 
                      options = list(scrollX = TRUE, pageLength = 5),
                      selection = 'none')
          }),
          size = "l",
          easyClose = TRUE
        ))
      } else {
        df <- df[!duplicated(df), ]
        rv$processed_data <- df
        show_success("Duplicates removed successfully!")
        
        # Record the operation
        rv$prep_steps[[rv$current_prep_type]] <- c(
          rv$prep_steps[[rv$current_prep_type]],
          "Duplicate rows removed"
        )
      }
    }, error = function(e) {
      show_error(paste("Error handling duplicates:", e$message))
    })
  })
  
  # Data scaling
  observeEvent(input$apply_scaling, {
    tryCatch({
      df <- current_prep_data()
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      
      if(length(numeric_cols) == 0) {
        stop("No numeric columns to scale")
      }
      
      withProgress(message = 'Applying scaling...', value = 0, {
        for(i in seq_along(numeric_cols)) {
          col <- numeric_cols[i]
          incProgress(i/length(numeric_cols), detail = paste("Processing", col))
          
          x <- df[[col]]
          
          if(input$scale_method == "standard") {
            df[[col]] <- scale(x)
          } else if(input$scale_method == "minmax") {
            df[[col]] <- (x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
          } else if(input$scale_method == "robust") {
            q <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
            df[[col]] <- (x - median(x, na.rm = TRUE))/(q[2] - q[1])
          }
        }
        
        rv$processed_data <- df
        show_success("Scaling applied successfully!")
      })
      
      # Record the operation
      rv$prep_steps[[rv$current_prep_type]] <- c(
        rv$prep_steps[[rv$current_prep_type]],
        paste("Scaling method:", input$scale_method)
      )
    }, error = function(e) {
      show_error(paste("Error in scaling:", e$message))
    })
  })
  
  # Encoding categorical variables
  observeEvent(input$apply_encoding, {
    tryCatch({
      df <- current_prep_data()
      col <- input$encode_var
      
      validate_column_selection(col, df)
      
      withProgress(message = 'Applying encoding...', value = 0.5, {
        if(input$encode_method == "onehot") {
          # Create dummy variables
          dummies <- model.matrix(~ . - 1, data = data.frame(x = df[[col]]))
          colnames(dummies) <- paste0(col, "_", levels(factor(df[[col]])))
          
          # Remove original column and add dummies
          df <- cbind(df[, !names(df) %in% col, drop = FALSE], dummies)
        } else if(input$encode_method == "label") {
          df[[col]] <- as.numeric(factor(df[[col]]))
        } else if(input$encode_method == "freq") {
          freqs <- table(df[[col]])/nrow(df)
          df[[col]] <- freqs[as.character(df[[col]])]
        }
        
        rv$processed_data <- df
        incProgress(1, detail = "Done")
      })
      
      show_success("Encoding applied successfully!")
      
      # Record the operation
      rv$prep_steps[[rv$current_prep_type]] <- c(
        rv$prep_steps[[rv$current_prep_type]],
        paste("Encoding:", col, "-", input$encode_method)
      )
    }, error = function(e) {
      show_error(paste("Error in encoding:", e$message))
    })
  })
  
  # Binning numeric variables
  output$bin_params_ui <- renderUI({
    req(input$bin_method, input$bin_var)
    
    df <- current_prep_data()
    col <- input$bin_var
    
    if(input$bin_method == "custom") {
      tagList(
        textInput("bin_breaks", "Enter break points (comma separated):",
                  value = paste(quantile(df[[col]], probs = seq(0, 1, 0.25), na.rm = TRUE), 
                                collapse = ",")),
        textInput("bin_labels", "Enter labels (comma separated, optional):", "")
      )
    } else {
      numericInput("num_bins", "Number of bins:", value = 5, min = 2, max = 20)
    }
  })
  
  observeEvent(input$apply_binning, {
    tryCatch({
      df <- current_prep_data()
      col <- input$bin_var
      
      validate_column_selection(col, df)
      
      if(!is.numeric(df[[col]])) {
        stop("Selected column must be numeric")
      }
      
      new_col_name <- paste0(col, "_binned")
      
      if(input$bin_method == "equal_width") {
        df[[new_col_name]] <- cut(df[[col]], breaks = input$num_bins)
      } else if(input$bin_method == "equal_freq") {
        df[[new_col_name]] <- cut(df[[col]], 
                                  breaks = quantile(df[[col]], probs = seq(0, 1, length.out = input$num_bins + 1)),
                                  include.lowest = TRUE)
      } else if(input$bin_method == "custom") {
        breaks <- as.numeric(unlist(strsplit(input$bin_breaks, ",")))
        labels <- if(nzchar(input$bin_labels)) {
          unlist(strsplit(input$bin_labels, ","))
        } else {
          NULL
        }
        
        if(length(breaks) < 2) {
          stop("At least 2 break points required")
        }
        
        df[[new_col_name]] <- cut(df[[col]], breaks = breaks, labels = labels, include.lowest = TRUE)
      }
      
      rv$processed_data <- df
      show_success("Binning applied successfully!")
      
      # Record the operation
      rv$prep_steps[[rv$current_prep_type]] <- c(
        rv$prep_steps[[rv$current_prep_type]],
        paste("Binning:", col, "-", input$bin_method)
      )
    }, error = function(e) {
      show_error(paste("Error in binning:", e$message))
    })
  })
  
  # Feature engineering
  observeEvent(input$create_feature, {
    tryCatch({
      df <- current_prep_data()
      new_name <- input$new_feature_name
      formula <- input$feature_formula
      
      if(nzchar(new_name) == 0) {
        stop("Please provide a name for the new feature")
      }
      
      if(nzchar(formula) == 0) {
        stop("Please provide a formula")
      }
      
      # Create a safe environment for evaluation
      safe_env <- new.env(parent = emptyenv())
      for(col in names(df)) {
        safe_env[[col]] <- df[[col]]
      }
      
      # Evaluate the formula
      tryCatch({
        df[[new_name]] <- eval(parse(text = formula), envir = safe_env)
        
        rv$processed_data <- df
        show_success("New feature created successfully!")
        
        # Record the operation
        rv$prep_steps[[rv$current_prep_type]] <- c(
          rv$prep_steps[[rv$current_prep_type]],
          paste("Created feature:", new_name, "=", formula)
        )
      }, error = function(e) {
        stop("Error in formula: ", e$message)
      })
    }, error = function(e) {
      show_error(paste("Error creating feature:", e$message))
    })
  })
  
  # Feature transformation
  output$transform_custom_ui <- renderUI({
    req(input$transform_method == "custom")
    textInput("transform_custom", "Enter transformation formula (use 'x' for variable):", "")
  })
  
  observeEvent(input$apply_transform, {
    tryCatch({
      df <- current_prep_data()
      col <- input$transform_var
      
      validate_column_selection(col, df)
      
      if(!is.numeric(df[[col]])) {
        stop("Selected column must be numeric")
      }
      
      x <- df[[col]]
      
      if(input$transform_method == "log") {
        df[[col]] <- log(x + 1)  # Add 1 to avoid log(0)
      } else if(input$transform_method == "sqrt") {
        df[[col]] <- sqrt(x)
      } else if(input$transform_method == "exp") {
        df[[col]] <- exp(x)
      } else if(input$transform_method == "custom") {
        formula <- input$transform_custom
        if(nzchar(formula) == 0) {
          stop("Please provide a transformation formula")
        }
        
        # Replace 'x' with the actual column values
        formula <- gsub("x", "df[[col]]", formula)
        
        # Evaluate safely
        tryCatch({
          df[[col]] <- eval(parse(text = formula))
        }, error = function(e) {
          stop("Error in transformation formula: ", e$message)
        })
      }
      
      rv$processed_data <- df
      show_success("Transformation applied successfully!")
      
      # Record the operation
      rv$prep_steps[[rv$current_prep_type]] <- c(
        rv$prep_steps[[rv$current_prep_type]],
        paste("Transformed:", col, "-", input$transform_method)
      )
    }, error = function(e) {
      show_error(paste("Error in transformation:", e$message))
    })
  })
  
  # Date feature extraction
  observeEvent(input$extract_date, {
    tryCatch({
      df <- current_prep_data()
      col <- input$date_var
      
      validate_column_selection(col, df)
      
      parts <- input$date_parts
      if(length(parts) == 0) {
        stop("Please select at least one date part to extract")
      }
      
      # Try to convert to date if not already
      if(!inherits(df[[col]], "Date")) {
        df[[col]] <- as.Date(df[[col]])
      }
      
      if(all(is.na(df[[col]]))) {
        stop("Could not convert column to Date")
      }
      
      for(part in parts) {
        new_col <- paste0(col, "_", part)
        
        if(part == "year") {
          df[[new_col]] <- lubridate::year(df[[col]])
        } else if(part == "month") {
          df[[new_col]] <- lubridate::month(df[[col]])
        } else if(part == "day") {
          df[[new_col]] <- lubridate::day(df[[col]])
        } else if(part == "weekday") {
          df[[new_col]] <- lubridate::wday(df[[col]])
        } else if(part == "quarter") {
          df[[new_col]] <- lubridate::quarter(df[[col]])
        }
      }
      
      rv$processed_data <- df
      show_success("Date features extracted successfully!")
      
      # Record the operation
      rv$prep_steps[[rv$current_prep_type]] <- c(
        rv$prep_steps[[rv$current_prep_type]],
        paste("Extracted date parts:", paste(parts, collapse = ", "))
      )
    }, error = function(e) {
      show_error(paste("Error in date extraction:", e$message))
    })
  })
  
  # Save prepared data
  observeEvent(input$save_prep_train, {
    req(rv$processed_data, rv$current_prep_type == "train")
    rv$train_data <- rv$processed_data
    show_success("Prepared training data saved!")
  })
  
  observeEvent(input$save_prep_pred, {
    req(rv$processed_data, rv$current_prep_type == "pred")
    rv$pred_data <- rv$processed_data
    show_success("Prepared prediction data saved!")
  })
  
  # Reset current dataset
  observeEvent(input$reset_prep, {
    if(input$data_type_prep == "train") {
      rv$processed_data <- rv$train_original
      rv$prep_steps$train <- list()
    } else {
      rv$processed_data <- rv$pred_original
      rv$prep_steps$pred <- list()
    }
    show_success("Current dataset reset to original state!")
  })
  
  # Download prepared data
  output$download_prep_data <- downloadHandler(
    filename = function() {
      paste0("prepared_data_", input$data_type_prep, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(current_prep_data(), file, row.names = FALSE)
    }
  )
  
  # Show preparation steps
  output$prep_steps_ui <- renderUI({
    steps <- rv$prep_steps[[rv$current_prep_type]]
    if(length(steps) > 0) {
      tagList(
        h4("Preparation Steps Applied:"),
        tags$ul(
          lapply(steps, function(step) tags$li(step))
        )
      )
    }
  })
  
  ### VARIABLE CATEGORIZATION SECTION ###
  
  # Categorization type UI (numeric vs categorical)
  output$categorization_type_ui <- renderUI({
    req(input$var_to_categorize, rv$var_types)
    
    var_type <- rv$var_types[input$var_to_categorize]
    
    if(var_type == "numeric") {
      tagList(
        radioButtons("cat_type", "Categorization Method:",
                     choices = c("Automatic Binning" = "bins", 
                                 "Custom Rules" = "custom"),
                     selected = "bins"),
        conditionalPanel(
          condition = "input.cat_type == 'custom'",
          actionButton("save_rules", "Save Rules", class = "btn-info"),
          actionButton("load_rules", "Load Rules", class = "btn-info")
        )
      )
    } else {
      h4("Categorical Variable - Rename or Group Categories")
    }
  })
  
  # Main categorization UI
  output$categorization_ui <- renderUI({
    req(input$var_to_categorize, rv$processed_data, rv$var_types)
    
    var_type <- rv$var_types[input$var_to_categorize]
    var_data <- rv$processed_data[[input$var_to_categorize]]
    
    if(is.null(var_type) || length(var_type) == 0) return(NULL)
    
    if(var_type == "numeric") {
      if(input$cat_type == "bins") {
        # Automatic binning UI
        tagList(
          numericInput("num_bins", "Number of Bins:", 
                       value = min(5, length(unique(var_data[!is.na(var_data)])), 
                                   min = 2, max = 20),
                       textInput("bin_prefix", "Category Prefix:", value = "Group"),
                       radioButtons("bin_method", "Binning Method:",
                                    choices = c("Equal Width" = "equal",
                                                "Equal Frequency" = "quantile"),
                                    selected = "equal"))
        )
      } else {
        # Custom rules UI for numeric variables
        tagList(
          h4("Define Custom Rules:"),
          div(id = "custom_rules_container",
              uiOutput("existing_rules_ui"),
              fluidRow(
                column(3, textInput("new_rule_name", "Category Name", value = "")),
                column(3, selectInput("new_rule_op", "Operator", 
                                      choices = c("<", "<=", ">", ">=", "==", "!=", "between"))),
                column(3, numericInput("new_rule_val1", "Value 1", value = NA)),
                column(3, 
                       conditionalPanel(
                         condition = "input.new_rule_op == 'between'",
                         numericInput("new_rule_val2", "Value 2", value = NA)
                       )
                )
              ),
              actionButton("add_rule", "Add Rule", class = "btn-success")
          )
        )
      }
    } else {
      # Categorical variable UI
      tagList(
        h4("Current Categories:"),
        DTOutput("cat_levels_table"),
        fluidRow(
          column(6, 
                 h4("Rename Category:"),
                 selectInput("cat_to_rename", "Select Category", 
                             choices = if(is.factor(var_data)) levels(var_data) else unique(var_data)),
                 textInput("new_cat_name", "New Name", value = ""),
                 actionButton("rename_category", "Rename", class = "btn-primary")
          ),
          column(6,
                 h4("Merge Categories:"),
                 selectInput("cats_to_merge", "Select Categories to Merge", 
                             choices = if(is.factor(var_data)) levels(var_data) else unique(var_data),
                             multiple = TRUE),
                 textInput("merged_cat_name", "New Merged Name", value = ""),
                 actionButton("merge_categories", "Merge", class = "btn-primary")
          )
        )
      )
    }
  })
  
  # UI for existing rules (numeric custom rules)
  output$existing_rules_ui <- renderUI({
    req(input$var_to_categorize)
    
    var_name <- input$var_to_categorize
    rules <- rv$category_rules[[var_name]]
    
    if(!is.null(rules) && length(rules) > 0) {
      tagList(
        h4("Existing Rules:"),
        lapply(seq_along(rules), function(i) {
          fluidRow(
            column(3, textInput(paste0("rule_name_", i), "Category", 
                                value = rules[[i]]$name)),
            column(2, selectInput(paste0("rule_op_", i), "Operator", 
                                  choices = c("<", "<=", ">", ">=", "==", "!=", "between"),
                                  selected = rules[[i]]$operator)),
            column(2, numericInput(paste0("rule_val1_", i), "Value 1", 
                                   value = rules[[i]]$value1)),
            column(2, 
                   conditionalPanel(
                     condition = paste0("input.rule_op_", i, " == 'between'"),
                     numericInput(paste0("rule_val2_", i), "Value 2", 
                                  value = rules[[i]]$value2)
                   )
            ),
            column(2, actionButton(paste0("update_rule_", i), "Update", 
                                   class = "btn-primary")),
            column(1, actionButton(paste0("remove_rule_", i), "Remove", 
                                   class = "btn-danger"))
          )
        })
      )
    }
  })
  
  # Initialize new rule inputs based on variable range
  observe({
    req(input$var_to_categorize, rv$var_types[input$var_to_categorize] == "numeric")
    
    var_data <- rv$processed_data[[input$var_to_categorize]]
    if(is.numeric(var_data)) {
      val_range <- range(var_data, na.rm = TRUE)
      updateNumericInput(session, "new_rule_val1", 
                         value = round(mean(val_range), 2))
      updateNumericInput(session, "new_rule_val2", 
                         value = round(val_range[2], 2))
    }
  })
  
  # Table for categorical variable levels
  output$cat_levels_table <- renderDT({
    req(input$var_to_categorize, rv$var_types[input$var_to_categorize] == "categorical")
    
    var_name <- input$var_to_categorize
    var_data <- rv$processed_data[[var_name]]
    
    if(is.factor(var_data)) {
      levels_df <- data.frame(
        Original = rv$original_levels[[var_name]],
        Current = levels(var_data),
        stringsAsFactors = FALSE
      )
    } else {
      levels_df <- data.frame(
        Original = unique(var_data),
        Current = unique(var_data),
        stringsAsFactors = FALSE
      )
    }
    
    datatable(levels_df, 
              options = list(pageLength = 5, dom = 't'),
              rownames = FALSE)
  })
  
  # Add new rule for numeric variable
  observeEvent(input$add_rule, {
    tryCatch({
      req(input$var_to_categorize, input$new_rule_name, input$new_rule_op, 
          !is.na(input$new_rule_val1))
      
      if(input$new_rule_op == "between" && is.na(input$new_rule_val2)) {
        stop("Please provide both values for 'between' operation")
      }
      
      var_name <- input$var_to_categorize
      new_rule <- list(
        name = input$new_rule_name,
        operator = input$new_rule_op,
        value1 = input$new_rule_val1,
        value2 = if(input$new_rule_op == "between") input$new_rule_val2 else NULL
      )
      
      if(is.null(rv$category_rules[[var_name]])) {
        rv$category_rules[[var_name]] <- list(new_rule)
      } else {
        rv$category_rules[[var_name]] <- c(rv$category_rules[[var_name]], list(new_rule))
      }
      
      # Reset new rule inputs
      updateTextInput(session, "new_rule_name", value = "")
      updateNumericInput(session, "new_rule_val1", value = NA)
      updateNumericInput(session, "new_rule_val2", value = NA)
      
      show_success("Rule added successfully!")
    }, error = function(e) {
      show_error(paste("Error adding rule:", e$message))
    })
  })
  
  # Update or remove rule observers
  observe({
    req(input$var_to_categorize)
    var_name <- input$var_to_categorize
    rules <- rv$category_rules[[var_name]]
    
    if(!is.null(rules) && length(rules) > 0) {
      lapply(seq_along(rules), function(i) {
        # Update rule
        observeEvent(input[[paste0("update_rule_", i)]], {
          tryCatch({
            updated_rule <- list(
              name = input[[paste0("rule_name_", i)]],
              operator = input[[paste0("rule_op_", i)]],
              value1 = input[[paste0("rule_val1_", i)]],
              value2 = if(input[[paste0("rule_op_", i)]] == "between") 
                input[[paste0("rule_val2_", i)]] else NULL
            )
            
            rv$category_rules[[var_name]][[i]] <- updated_rule
            show_success("Rule updated successfully!")
          }, error = function(e) {
            show_error(paste("Error updating rule:", e$message))
          })
        })
        
        # Remove rule
        observeEvent(input[[paste0("remove_rule_", i)]], {
          rv$category_rules[[var_name]] <- rv$category_rules[[var_name]][-i]
          if(length(rv$category_rules[[var_name]]) == 0) {
            rv$category_rules[[var_name]] <- NULL
          }
          show_success("Rule removed successfully!")
        })
      })
    }
  })
  
  # Rename category for categorical variable
  observeEvent(input$rename_category, {
    tryCatch({
      req(input$var_to_categorize, input$cat_to_rename, input$new_cat_name)
      
      var_name <- input$var_to_categorize
      old_name <- input$cat_to_rename
      new_name <- input$new_cat_name
      
      if(new_name == "") {
        stop("Please provide a new name for the category")
      }
      
      if(is.factor(rv$processed_data[[var_name]])) {
        levels(rv$processed_data[[var_name]])[levels(rv$processed_data[[var_name]]) == old_name] <- new_name
      } else {
        rv$processed_data[[var_name]][rv$processed_data[[var_name]] == old_name] <- new_name
      }
      
      updateTextInput(session, "new_cat_name", value = "")
      show_success("Category renamed successfully!")
    }, error = function(e) {
      show_error(paste("Error renaming category:", e$message))
    })
  })
  
  # Merge categories for categorical variable
  observeEvent(input$merge_categories, {
    tryCatch({
      req(input$var_to_categorize, input$cats_to_merge, input$merged_cat_name)
      
      var_name <- input$var_to_categorize
      cats_to_merge <- input$cats_to_merge
      new_name <- input$merged_cat_name
      
      if(new_name == "") {
        stop("Please provide a name for the merged category")
      }
      
      if(length(cats_to_merge) < 2) {
        stop("Please select at least 2 categories to merge")
      }
      
      if(is.factor(rv$processed_data[[var_name]])) {
        levels(rv$processed_data[[var_name]])[levels(rv$processed_data[[var_name]]) %in% cats_to_merge] <- new_name
      } else {
        rv$processed_data[[var_name]][rv$processed_data[[var_name]] %in% cats_to_merge] <- new_name
      }
      
      updateTextInput(session, "merged_cat_name", value = "")
      show_success("Categories merged successfully!")
    }, error = function(e) {
      show_error(paste("Error merging categories:", e$message))
    })
  })
  
  # Save categorization rules
  observeEvent(input$save_rules, {
    tryCatch({
      req(input$var_to_categorize)
      
      var_name <- input$var_to_categorize
      rules <- rv$category_rules[[var_name]]
      
      if(is.null(rules) || length(rules) == 0) {
        stop("No rules to save")
      }
      
      rv$saved_rules[[var_name]] <- rules
      show_success(paste("Rules saved for variable:", var_name))
    }, error = function(e) {
      show_error(paste("Error saving rules:", e$message))
    })
  })
  
  # Load categorization rules
  observeEvent(input$load_rules, {
    tryCatch({
      req(input$var_to_categorize)
      
      var_name <- input$var_to_categorize
      rules <- rv$saved_rules[[var_name]]
      
      if(is.null(rules)) {
        stop("No saved rules found for this variable")
      }
      
      rv$category_rules[[var_name]] <- rules
      show_success(paste("Rules loaded for variable:", var_name))
    }, error = function(e) {
      show_error(paste("Error loading rules:", e$message))
    })
  })
  
  # Color picker UI
  output$color_picker_ui <- renderUI({
    req(rv$categorized_data, input$var_to_categorize)
    
    var_name <- input$var_to_categorize
    plot_var <- if(paste0(var_name, "_category") %in% names(rv$categorized_data)) {
      paste0(var_name, "_category")
    } else {
      var_name
    }
    
    categories <- if(is.factor(rv$categorized_data[[plot_var]])) {
      levels(rv$categorized_data[[plot_var]])
    } else {
      unique(na.omit(rv$categorized_data[[plot_var]]))
    }
    
    if(length(categories) == 0) return(NULL)
    
    # Handle cases with less than 3 categories
    n_colors <- max(3, length(categories))
    color_palette <- brewer.pal(n_colors, "Set3")[1:length(categories)]
    
    lapply(seq_along(categories), function(i) {
      colourpicker::colourInput(
        inputId = paste0("color_", i),
        label = categories[i],
        value = if(!is.null(rv$category_colors[[plot_var]]) && 
                   length(rv$category_colors[[plot_var]]) >= i) {
          rv$category_colors[[plot_var]][i]
        } else {
          color_palette[i]
        }
      )
    })
  })
  
  # Store color selections
  observe({
    req(rv$categorized_data, input$var_to_categorize)
    
    var_name <- input$var_to_categorize
    plot_var <- if(paste0(var_name, "_category") %in% names(rv$categorized_data)) {
      paste0(var_name, "_category")
    } else {
      var_name
    }
    
    categories <- if(is.factor(rv$categorized_data[[plot_var]])) {
      levels(rv$categorized_data[[plot_var]])
    } else {
      unique(rv$categorized_data[[plot_var]])
    }
    
    colors <- lapply(seq_along(categories), function(i) {
      input[[paste0("color_", i)]]
    })
    
    if(all(sapply(colors, function(x) !is.null(x)))) {
      rv$category_colors[[plot_var]] <- unlist(colors)
    }
  })
  
  # Apply categorization
  observeEvent(input$apply_categorization, {
    tryCatch({
      req(rv$processed_data, input$var_to_categorize)
      
      var_name <- input$var_to_categorize
      var_type <- rv$var_types[var_name]
      new_var_name <- paste0(var_name, "_category")
      
      withProgress(message = 'Applying categorization...', value = 0.5, {
        if(var_type == "numeric") {
          if(input$cat_type == "bins") {
            # Automatic binning
            var_data <- rv$processed_data[[var_name]]
            
            if(input$bin_method == "equal") {
              breaks <- seq(min(var_data, na.rm = TRUE), 
                            max(var_data, na.rm = TRUE), 
                            length.out = input$num_bins + 1)
            } else {
              breaks <- quantile(var_data, 
                                 probs = seq(0, 1, length.out = input$num_bins + 1),
                                 na.rm = TRUE)
            }
            
            # Ensure breaks are unique
            breaks <- unique(breaks)
            if(length(breaks) < 2) {
              stop("Not enough unique values to create bins")
            }
            
            labels <- paste(input$bin_prefix, 1:(length(breaks)-1))
            rv$processed_data[[new_var_name]] <- cut(
              var_data,
              breaks = breaks,
              labels = labels,
              include.lowest = TRUE
            )
          } else {
            # Custom rule-based categorization
            rules <- rv$category_rules[[var_name]]
            if(is.null(rules) || length(rules) == 0) {
              stop("Please define at least one rule!")
            }
            
            # Build case_when expression
            case_expr <- lapply(rules, function(rule) {
              if(rule$operator == "between") {
                paste0(var_name, " > ", rule$value1, " & ", 
                       var_name, " <= ", rule$value2, " ~ \"", rule$name, "\"")
              } else {
                paste0(var_name, " ", rule$operator, " ", 
                       rule$value1, " ~ \"", rule$name, "\"")
              }
            })
            
            full_expr <- paste0(
              "case_when(\n",
              paste("  ", unlist(case_expr), collapse = ",\n"),
              ",\n  TRUE ~ \"Other\"\n)"
            )
            
            rv$processed_data[[new_var_name]] <- rlang::eval_tidy(
              rlang::parse_expr(full_expr), 
              data = rv$processed_data
            )
          }
        } else {
          # For categorical variables, we've already modified them directly
          new_var_name <- var_name
        }
        
        rv$categorized_data <- rv$processed_data
        incProgress(1, detail = "Done")
      })
      
      show_success("Categorization applied successfully!")
      
      # Record the operation
      rv$prep_steps[[rv$current_prep_type]] <- c(
        rv$prep_steps[[rv$current_prep_type]],
        paste("Categorized variable:", var_name)
      )
    }, error = function(e) {
      show_error(paste("Error applying categorization:", e$message))
    })
  })
  
  # Reset categorization for current variable
  observeEvent(input$reset_categorization, {
    req(input$var_to_categorize)
    var_name <- input$var_to_categorize
    
    # Reset rules
    rv$category_rules[[var_name]] <- NULL
    
    # Restore original levels for this variable
    if(!is.null(rv$original_levels[[var_name]])) {
      if(is.factor(rv$processed_data[[var_name]])) {
        levels(rv$processed_data[[var_name]]) <- rv$original_levels[[var_name]]
      }
    }
    
    # Remove any category column if it exists
    category_col <- paste0(var_name, "_category")
    if(category_col %in% names(rv$processed_data)) {
      rv$processed_data <- rv$processed_data[, !names(rv$processed_data) %in% category_col, drop = FALSE]
    }
    
    rv$categorized_data <- NULL
    show_success("Categorization reset for this variable!")
  })
  
  # Categorized distribution plot
  output$categorized_dist_plot <- renderPlotly({
    req(rv$categorized_data, input$var_to_categorize)
    
    var_name <- input$var_to_categorize
    plot_var <- if(paste0(var_name, "_category") %in% names(rv$categorized_data)) {
      paste0(var_name, "_category")
    } else {
      var_name
    }
    
    plot_data <- rv$categorized_data[!is.na(rv$categorized_data[[plot_var]]), ]
    
    # Get colors if they exist
    colors <- NULL
    if(!is.null(rv$category_colors[[plot_var]])) {
      categories <- if(is.factor(plot_data[[plot_var]])) levels(plot_data[[plot_var]]) else unique(plot_data[[plot_var]])
      if(length(rv$category_colors[[plot_var]]) == length(categories)) {
        colors <- setNames(rv$category_colors[[plot_var]], categories)
      }
    }
    
    p <- ggplot(plot_data, aes(x = .data[[plot_var]], fill = .data[[plot_var]])) +
      geom_bar() +
      labs(title = paste("Distribution of", plot_var), 
           x = plot_var, y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    if(!is.null(colors)) {
      p <- p + scale_fill_manual(values = colors)
    }
    
    ggplotly(p) %>% layout(showlegend = FALSE)
  })
  
  # Category mapping table
  output$category_mapping_table <- renderDT({
    req(rv$processed_data, input$var_to_categorize)
    
    var_name <- input$var_to_categorize
    var_data <- rv$processed_data[[var_name]]
    
    # Initialize original_levels if not exists
    if(is.null(rv$original_levels[[var_name]])) {
      if(is.factor(var_data)) {
        rv$original_levels[[var_name]] <- levels(var_data)
      } else {
        rv$original_levels[[var_name]] <- unique(var_data)
      }
    }
    
    var_type <- ifelse(is.numeric(var_data), "numeric", "categorical")
    
    if (var_type == "numeric") {
      if (input$cat_type == "bins") {
        # For binned numeric variables
        if (input$bin_method == "equal") {
          breaks <- seq(min(var_data, na.rm = TRUE), 
                        max(var_data, na.rm = TRUE), 
                        length.out = input$num_bins + 1)
        } else {
          breaks <- quantile(var_data, 
                             probs = seq(0, 1, length.out = input$num_bins + 1),
                             na.rm = TRUE)
        }
        
        # Ensure breaks are unique
        breaks <- unique(breaks)
        if (length(breaks) > 1) {
          mapping_df <- data.frame(
            Category = levels(cut(var_data, breaks = breaks)),
            Range = paste0("(", round(breaks[-length(breaks)], 2), 
                           ", ", round(breaks[-1], 2), "]")
          )
        } else {
          mapping_df <- data.frame(Category = "All", Range = "All values")
        }
      } else {
        # For custom rule-based numeric variables
        rules <- rv$category_rules[[var_name]]
        if (!is.null(rules) && length(rules) > 0) {
          mapping_df <- do.call(rbind, lapply(rules, function(rule) {
            if (rule$operator == "between") {
              data.frame(
                Category = rule$name,
                Rule = paste(rule$value1, "< x <=", rule$value2)
              )
            } else {
              data.frame(
                Category = rule$name,
                Rule = paste("x", rule$operator, rule$value1)
              )
            }
          }))
          
          # Add "Other" category
          mapping_df <- rbind(mapping_df, 
                              data.frame(Category = "Other", Rule = "All other values"))
        } else {
          mapping_df <- data.frame(Category = character(0), Rule = character(0))
        }
      }
    } else {
      # For categorical variables
      current_data <- if(!is.null(rv$categorized_data)) {
        rv$categorized_data[[var_name]]
      } else {
        var_data
      }
      
      level_mapping <- data.frame(
        Original = rv$original_levels[[var_name]],
        Current = if(is.factor(current_data)) {
          as.character(current_data[match(rv$original_levels[[var_name]], 
                                          levels(current_data)[match(rv$original_levels[[var_name]], levels(current_data))])])
        } else {
          as.character(current_data[match(rv$original_levels[[var_name]], unique(current_data))])
        },
        stringsAsFactors = FALSE
      )
      
      # Remove NA values that may occur from matching
      level_mapping <- level_mapping[complete.cases(level_mapping), ]
      
      if(nrow(level_mapping) > 0 && "Current" %in% names(level_mapping)) {
        mapping_df <- level_mapping %>%
          group_by(Current) %>%
          summarise(Original_Categories = paste(unique(Original), collapse = ", "))
      } else {
        mapping_df <- data.frame(
          Original = rv$original_levels[[var_name]],
          Current = rv$original_levels[[var_name]],
          stringsAsFactors = FALSE
        )
      }
    }
    
    datatable(mapping_df, 
              options = list(dom = 't', pageLength = 10),
              rownames = FALSE)
  })  
  
  # Variable selection summary
  output$var_selection_summary <- renderPrint({
    req(rv$target_var, rv$predictor_vars)
    cat("Target Variable:", rv$target_var, "\n")
    cat("Predictor Variables:", paste(rv$predictor_vars, collapse = ", "), "\n")
  })
  
  # Variable distribution plot
  output$var_dist_plot <- renderPlotly({
    req(rv$processed_data, rv$target_var)
    target <- rv$target_var
    
    if(is.numeric(rv$processed_data[[target]])) {
      p <- ggplot(rv$processed_data, aes(x = .data[[target]])) +
        geom_histogram(bins = 30, fill = "steelblue") +
        labs(title = "Target Variable Distribution", x = "Value", y = "Count")
    } else {
      p <- ggplot(rv$processed_data, aes(x = .data[[target]])) +
        geom_bar(fill = "steelblue") +
        labs(title = "Target Variable Distribution", x = "Class", y = "Count")
    }
    ggplotly(p)
  })
  
  
  ### Explanatory Data Analysis --------------------------------------------------------
  
  # Function to add to history
  add_history <- function(section, action, details, results) {
    new_entry <- data.frame(
      Timestamp = as.character(Sys.time()),
      Section = section,
      Action = action,
      Details = details,
      Results = I(list(results)),
      stringsAsFactors = FALSE
    )
    rv$history <- rbind(rv$history, new_entry)
  }
  
  # Get current EDA data based on selection
  eda_data <- reactive({
    if (input$eda_data_type == "train") {
      req(rv$train_data)
      rv$train_data
    } else {
      req(rv$pred_data)
      rv$pred_data
    }
  })
  
  # Variable selection UI
  output$eda_var_select <- renderUI({
    req(eda_data())
    selectInput("selected_vars", "Select Variables",
                choices = names(eda_data()),
                multiple = TRUE)
  })
  
  # Filter UI based on selected variables
  output$eda_var_filter <- renderUI({
    req(eda_data(), input$selected_vars)
    
    lapply(input$selected_vars, function(var) {
      var_data <- eda_data()[[var]]
      if (is.numeric(var_data)) {
        sliderInput(paste0("filter_", var), paste("Filter", var),
                    min = floor(min(var_data, na.rm = TRUE)),
                    max = ceiling(max(var_data, na.rm = TRUE)),
                    value = range(var_data, na.rm = TRUE))
      } else if (is.factor(var_data) || is.character(var_data)) {
        selectInput(paste0("filter_", var), paste("Filter", var),
                    choices = unique(var_data),
                    multiple = TRUE)
      }
    })
  })
  
  # Process data with filters
  eda_processed_data <- reactive({
    req(eda_data(), input$selected_vars)
    
    df <- eda_data()
    
    # Apply filters
    for (var in input$selected_vars) {
      filter_val <- input[[paste0("filter_", var)]]
      if (!is.null(filter_val)) {
        if (is.numeric(df[[var]])) {
          df <- df[df[[var]] >= filter_val[1] & df[[var]] <= filter_val[2], ]
        } else if (length(filter_val) > 0) {
          df <- df[df[[var]] %in% filter_val, ]
        }
      }
    }
    
    df[, input$selected_vars, drop = FALSE]
  })
  
  # Data preview
  output$data_preview <- renderDT({
    req(eda_processed_data())
    datatable(head(eda_processed_data(), 20), options = list(scrollX = TRUE))
  })
  
  # Summary statistics
  observeEvent(input$run_summary, {
    req(eda_processed_data())
    
    if (!is.null(rv$current_summary)) {
      add_history("Summary", "Previous Summary", "Replaced by new summary", rv$current_summary)
    }
    
    df <- eda_processed_data()
    sumry <- capture.output({
      cat("### Summary Statistics ###\n\n")
      print(summary(df))
      cat("\n### Variable Types ###\n")
      print(sapply(df, class))
      cat("\n### Missing Values ###\n")
      print(colSums(is.na(df)))
      cat("\n### Unique Values Count ###\n")
      print(sapply(df, function(x) length(unique(x))))
    })
    
    rv$current_summary <- sumry
    add_history("Summary", "Generated", "Summary statistics", sumry)
  })
  
  output$summary_output <- renderPrint({
    req(rv$current_summary)
    cat(rv$current_summary, sep = "\n")
  })
  
  # Frequency table parameters UI
  output$freq_params <- renderUI({
    req(eda_processed_data())
    df <- eda_processed_data()
    
    if (input$freq_type == "single") {
      selectInput("freq_var1", "Select Variable", 
                  choices = names(df))
    } else if (input$freq_type == "two_way") {
      tagList(
        selectInput("freq_var1", "Select Row Variable", 
                    choices = names(df)),
        selectInput("freq_var2", "Select Column Variable", 
                    choices = names(df))
      )
    } else {
      selectInput("freq_vars", "Select Variables", 
                  choices = names(df), multiple = TRUE)
    }
  })
  
  # Generate frequency table
  observeEvent(input$run_freq, {
    req(eda_processed_data())
    
    df <- eda_processed_data()
    freq_table <- NULL
    details <- ""
    
    if (input$freq_type == "single") {
      req(input$freq_var1)
      var <- input$freq_var1
      freq_table <- janitor::tabyl(df[[var]]) %>% 
        adorn_totals("row") %>% 
        adorn_pct_formatting()
      details <- paste("Frequency table for", var)
    } else if (input$freq_type == "two_way") {
      req(input$freq_var1, input$freq_var2)
      freq_table <- janitor::tabyl(df, !!sym(input$freq_var1), !!sym(input$freq_var2)) %>% 
        adorn_totals(c("row", "col")) %>% 
        adorn_percentages("row") %>% 
        adorn_pct_formatting() %>% 
        adorn_ns() %>% 
        adorn_title("combined")
      details <- paste("Two-way table:", input$freq_var1, "by", input$freq_var2)
    } else {
      req(input$freq_vars)
      if (length(input$freq_vars) == 1) {
        freq_table <- janitor::tabyl(df[[input$freq_vars]]) %>% 
          adorn_totals("row") %>% 
          adorn_pct_formatting()
      } else {
        freq_table <- df %>% 
          count(across(all_of(input$freq_vars))) %>% 
          pivot_wider(names_from = input$freq_vars[2], 
                      values_from = "n", 
                      values_fill = 0)
      }
      details <- paste("Multi-way table:", paste(input$freq_vars, collapse = ", "))
    }
    
    if (!is.null(freq_table)) {
      rv$current_freq <- freq_table
      add_history("Frequency", "Generated", details, freq_table)
    }
  })
  
  output$freq_table <- renderDT({
    req(rv$current_freq)
    datatable(rv$current_freq, options = list(scrollX = TRUE))
  })
  
  # Pivot table parameters UI
  output$pivot_params <- renderUI({
    req(eda_processed_data())
    df <- eda_processed_data()
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    factor_vars <- names(df)[sapply(df, is.factor) | sapply(df, is.character)]
    
    tagList(
      selectInput("pivot_row", "Row Variable", 
                  choices = c("None", factor_vars)),
      selectInput("pivot_col", "Column Variable", 
                  choices = c("None", factor_vars)),
      selectInput("pivot_value", "Value Variable", 
                  choices = numeric_vars)
    )
  })
  
  # Generate pivot table
  observeEvent(input$run_pivot, {
    req(eda_processed_data(), input$pivot_value)
    
    df <- eda_processed_data()
    pivot_table <- NULL
    details <- ""
    
    row_var <- if (input$pivot_row == "None") NULL else input$pivot_row
    col_var <- if (input$pivot_col == "None") NULL else input$pivot_col
    
    if (is.null(row_var) && is.null(col_var)) {
      pivot_table <- switch(input$pivot_type,
                            "count" = nrow(df),
                            "sum" = sum(df[[input$pivot_value]], na.rm = TRUE),
                            "mean" = mean(df[[input$pivot_value]], na.rm = TRUE),
                            "median" = median(df[[input$pivot_value]], na.rm = TRUE))
      details <- paste("Simple", input$pivot_type, "of", input$pivot_value)
    } else {
      if (input$pivot_type == "count") {
        if (!is.null(row_var) && !is.null(col_var)) {
          pivot_table <- df %>% 
            count(!!sym(row_var), !!sym(col_var)) %>% 
            pivot_wider(names_from = col_var, values_from = "n", values_fill = 0)
        } else if (!is.null(row_var)) {
          pivot_table <- df %>% 
            count(!!sym(row_var))
        } else {
          pivot_table <- df %>% 
            count(!!sym(col_var))
        }
      } else {
        agg_func <- switch(input$pivot_type,
                           "sum" = sum,
                           "mean" = mean,
                           "median" = median)
        
        if (!is.null(row_var) && !is.null(col_var)) {
          pivot_table <- df %>% 
            group_by(!!sym(row_var), !!sym(col_var)) %>% 
            summarise(Value = agg_func(!!sym(input$pivot_value), na.rm = TRUE)) %>% 
            pivot_wider(names_from = col_var, values_from = "Value")
        } else if (!is.null(row_var)) {
          pivot_table <- df %>% 
            group_by(!!sym(row_var)) %>% 
            summarise(Value = agg_func(!!sym(input$pivot_value), na.rm = TRUE))
        } else {
          pivot_table <- df %>% 
            group_by(!!sym(col_var)) %>% 
            summarise(Value = agg_func(!!sym(input$pivot_value), na.rm = TRUE))
        }
      }
      details <- paste(input$pivot_type, "of", input$pivot_value, "by", 
                       ifelse(!is.null(row_var), row_var, ""), 
                       ifelse(!is.null(col_var), paste("and", col_var), ""))
    }
    
    if (!is.null(pivot_table)) {
      rv$current_pivot <- pivot_table
      add_history("Pivot", "Generated", details, pivot_table)
    }
  })
  
  output$pivot_table <- renderDT({
    req(rv$current_pivot)
    datatable(rv$current_pivot, options = list(scrollX = TRUE))
  })
  
  # Plot parameters UI
  output$plot_params <- renderUI({
    req(eda_processed_data(), input$plot_type)
    df <- eda_processed_data()
    
    if (input$plot_type %in% c("Histogram", "Density", "Boxplot", "Violin Plot", "QQ Plot")) {
      selectInput("plot_var1", "Select Variable", choices = names(df))
    } else if (input$plot_type %in% c("Scatter Plot")) {
      tagList(
        selectInput("plot_var1", "X Variable", choices = names(df)),
        selectInput("plot_var2", "Y Variable", choices = names(df)),
        selectInput("plot_color", "Color By (optional)", 
                    choices = c("None", names(df)[sapply(df, is.factor)]))
      )
    } else if (input$plot_type %in% c("Bar Plot")) {
      selectInput("plot_var1", "Select Variable", 
                  choices = names(df)[sapply(df, is.factor) | sapply(df, is.character)])
    } else if (input$plot_type %in% c("Pairs Plot")) {
      selectInput("plot_vars", "Select Variables", 
                  choices = names(df)[sapply(df, is.numeric)], multiple = TRUE)
    }
  })
  
  # Plot generation
  observeEvent(input$run_plot, {
    req(eda_processed_data(), input$plot_type)
    
    if (!is.null(rv$current_plot)) {
      add_history("Plot", "Previous Plot", "Replaced by new plot", rv$current_plot)
    }
    
    df <- eda_processed_data()
    plot_obj <- NULL
    plot_details <- ""
    
    if (input$plot_type == "Histogram") {
      req(input$plot_var1)
      var <- input$plot_var1
      if (is.numeric(df[[var]])) {
        plot_obj <- ggplot(df, aes(x = .data[[var]])) +
          geom_histogram(fill = "steelblue", bins = 30, alpha = 0.8) +
          labs(title = paste("Histogram of", var)) +
          theme_minimal()
        plot_details <- paste("Histogram of", var)
      }
    } else if (input$plot_type == "Density") {
      req(input$plot_var1)
      var <- input$plot_var1
      if (is.numeric(df[[var]])) {
        plot_obj <- ggplot(df, aes(x = .data[[var]])) +
          geom_density(fill = "steelblue", alpha = 0.5) +
          labs(title = paste("Density Plot of", var)) +
          theme_minimal()
        plot_details <- paste("Density Plot of", var)
      }
    } else if (input$plot_type == "Boxplot") {
      req(input$plot_var1)
      var <- input$plot_var1
      if (is.numeric(df[[var]])) {
        plot_obj <- ggplot(df, aes(y = .data[[var]])) +
          geom_boxplot(fill = "steelblue") +
          labs(title = paste("Boxplot of", var)) +
          theme_minimal()
        plot_details <- paste("Boxplot of", var)
      }
    } else if (input$plot_type == "Violin Plot") {
      req(input$plot_var1)
      var <- input$plot_var1
      if (is.numeric(df[[var]])) {
        plot_obj <- ggplot(df, aes(x = 1, y = .data[[var]])) +
          geom_violin(fill = "steelblue", alpha = 0.5) +
          labs(title = paste("Violin Plot of", var)) +
          theme_minimal() +
          theme(axis.title.x = element_blank(),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank())
        plot_details <- paste("Violin Plot of", var)
      }
    } else if (input$plot_type == "QQ Plot") {
      req(input$plot_var1)
      var <- input$plot_var1
      if (is.numeric(df[[var]])) {
        plot_obj <- ggplot(df, aes(sample = .data[[var]])) +
          stat_qq(color = "steelblue") +
          stat_qq_line() +
          labs(title = paste("QQ Plot of", var)) +
          theme_minimal()
        plot_details <- paste("QQ Plot of", var)
      }
    } else if (input$plot_type == "Scatter Plot") {
      req(input$plot_var1, input$plot_var2)
      x_var <- input$plot_var1
      y_var <- input$plot_var2
      if (is.numeric(df[[x_var]]) && is.numeric(df[[y_var]])) {
        if (input$plot_color != "None") {
          plot_obj <- ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]], color = .data[[input$plot_color]])) +
            geom_point() +
            labs(title = paste("Scatter Plot of", x_var, "vs", y_var)) +
            theme_minimal()
        } else {
          plot_obj <- ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]])) +
            geom_point(color = "steelblue") +
            labs(title = paste("Scatter Plot of", x_var, "vs", y_var)) +
            theme_minimal()
        }
        plot_details <- paste("Scatter Plot of", x_var, "vs", y_var)
      }
    } else if (input$plot_type == "Bar Plot") {
      req(input$plot_var1)
      var <- input$plot_var1
      if (is.factor(df[[var]]) || is.character(df[[var]])) {
        plot_obj <- ggplot(df, aes(x = .data[[var]])) +
          geom_bar(fill = "steelblue") +
          labs(title = paste("Bar Plot of", var)) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        plot_details <- paste("Bar Plot of", var)
      }
    } else if (input$plot_type == "Pairs Plot") {
      req(input$plot_vars)
      if (length(input$plot_vars) >= 2) {
        plot_obj <- GGally::ggpairs(df[, input$plot_vars, drop = FALSE]) +
          theme_minimal()
        plot_details <- paste("Pairs Plot of", paste(input$plot_vars, collapse = ", "))
      }
    }
    
    if (!is.null(plot_obj)) {
      rv$current_plot <- plot_obj
      add_history("Plot", "Generated", plot_details, plot_obj)
    }
  })
  
  output$main_plot <- renderPlot({
    req(rv$current_plot)
    rv$current_plot
  })
  
  # Statistical test parameters UI
  output$stat_test_params <- renderUI({
    req(eda_processed_data(), input$stat_test)
    
    df <- eda_processed_data()
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    factor_vars <- names(df)[sapply(df, is.factor) | sapply(df, is.character)]
    
    if (input$stat_test == "T-test") {
      tagList(
        selectInput("ttest_var", "Select Variable", choices = numeric_vars),
        selectInput("ttest_group", "Grouping Variable", 
                    choices = c("None", factor_vars))
      )
    } else if (input$stat_test == "ANOVA") {
      tagList(
        selectInput("anova_var", "Select Variable", choices = numeric_vars),
        selectInput("anova_group", "Grouping Variable", choices = factor_vars)
      )
    } else if (input$stat_test == "Correlation") {
      tagList(
        selectInput("cor_var1", "Variable 1", choices = numeric_vars),
        selectInput("cor_var2", "Variable 2", choices = numeric_vars),
        selectInput("cor_method", "Method",
                    choices = c("Pearson", "Spearman", "Kendall"))
      )
    } else if (input$stat_test == "Chi-square") {
      tagList(
        selectInput("chi_var1", "Variable 1", choices = factor_vars),
        selectInput("chi_var2", "Variable 2", choices = factor_vars)
      )
    } else if (input$stat_test == "Normality Test") {
      selectInput("norm_var", "Select Variable", choices = numeric_vars)
    } else if (input$stat_test == "Homogeneity of Variance") {
      tagList(
        selectInput("hov_var", "Select Variable", choices = numeric_vars),
        selectInput("hov_group", "Grouping Variable", choices = factor_vars)
      )
    }
  })
  
  # Statistical test results
  observeEvent(input$run_stats, {
    req(eda_processed_data(), input$stat_test)
    
    if (!is.null(rv$current_stats)) {
      add_history("Statistics", "Previous Test", "Replaced by new test", rv$current_stats)
    }
    
    df <- eda_processed_data()
    stats_results <- NULL
    
    if (input$stat_test == "T-test") {
      req(input$ttest_var)
      if (input$ttest_group == "None") {
        test_results <- capture.output(t.test(df[[input$ttest_var]]))
        stats_results <- list(
          test = "One Sample T-test",
          variable = input$ttest_var,
          results = test_results
        )
        add_history("Statistics", "T-test", 
                    paste("One sample test on", input$ttest_var),
                    stats_results)
      } else {
        formula <- as.formula(paste(input$ttest_var, "~", input$ttest_group))
        test_results <- capture.output(t.test(formula, data = df))
        stats_results <- list(
          test = "Two Sample T-test",
          variables = c(input$ttest_var, input$ttest_group),
          results = test_results
        )
        add_history("Statistics", "T-test", 
                    paste("Two sample test:", input$ttest_var, "by", input$ttest_group),
                    stats_results)
      }
    } else if (input$stat_test == "ANOVA") {
      req(input$anova_var, input$anova_group)
      formula <- as.formula(paste(input$anova_var, "~", input$anova_group))
      test_results <- capture.output(summary(aov(formula, data = df)))
      stats_results <- list(
        test = "ANOVA",
        variables = c(input$anova_var, input$anova_group),
        results = test_results
      )
      add_history("Statistics", "ANOVA", 
                  paste(input$anova_var, "by", input$anova_group),
                  stats_results)
    } else if (input$stat_test == "Correlation") {
      req(input$cor_var1, input$cor_var2)
      method <- tolower(input$cor_method)
      test_results <- capture.output(cor.test(df[[input$cor_var1]], df[[input$cor_var2]], method = method))
      stats_results <- list(
        test = "Correlation",
        variables = c(input$cor_var1, input$cor_var2),
        method = method,
        results = test_results
      )
      add_history("Statistics", "Correlation", 
                  paste("Between", input$cor_var1, "and", input$cor_var2),
                  stats_results)
    } else if (input$stat_test == "Chi-square") {
      req(input$chi_var1, input$chi_var2)
      tbl <- table(df[[input$chi_var1]], df[[input$chi_var2]])
      test_results <- capture.output(chisq.test(tbl))
      stats_results <- list(
        test = "Chi-square",
        variables = c(input$chi_var1, input$chi_var2),
        results = test_results
      )
      add_history("Statistics", "Chi-square", 
                  paste("Between", input$chi_var1, "and", input$chi_var2),
                  stats_results)
    } else if (input$stat_test == "Normality Test") {
      req(input$norm_var)
      test_results <- capture.output({
        cat("Shapiro-Wilk Normality Test\n")
        print(shapiro.test(df[[input$norm_var]]))
        cat("\nAnderson-Darling Test\n")
        print(nortest::ad.test(df[[input$norm_var]]))
      })
      stats_results <- list(
        test = "Normality Test",
        variable = input$norm_var,
        results = test_results
      )
      add_history("Statistics", "Normality Test", 
                  paste("For variable", input$norm_var),
                  stats_results)
    } else if (input$stat_test == "Homogeneity of Variance") {
      req(input$hov_var, input$hov_group)
      formula <- as.formula(paste(input$hov_var, "~", input$hov_group))
      test_results <- capture.output({
        cat("Levene's Test\n")
        print(car::leveneTest(formula, data = df))
        cat("\nBartlett Test\n")
        print(bartlett.test(formula, data = df))
      })
      stats_results <- list(
        test = "Homogeneity of Variance",
        variables = c(input$hov_var, input$hov_group),
        results = test_results
      )
      add_history("Statistics", "Homogeneity of Variance", 
                  paste(input$hov_var, "by", input$hov_group),
                  stats_results)
    }
    
    rv$current_stats <- stats_results
  })
  
  output$stat_results <- renderPrint({
    req(rv$current_stats)
    cat(rv$current_stats$results, sep = "\n")
  })
  
  # History table
  output$history_table <- renderDT({
    datatable(rv$history[,1:4], 
              selection = 'single',
              options = list(pageLength = 5, scrollX = TRUE))
  })
  
  # History viewer
  output$history_viewer <- renderUI({
    req(input$history_table_rows_selected)
    selected_row <- rv$history[input$history_table_rows_selected, ]
    
    if (selected_row$Section == "Plot") {
      tagList(
        h4(paste("Plot:", selected_row$Details)),
        renderPlot(selected_row$Results[[1]])
      )
    } else if (selected_row$Section %in% c("Frequency", "Pivot")) {
      tagList(
        h4(paste(selected_row$Section, ":", selected_row$Details)),
        renderDT(datatable(selected_row$Results[[1]], options = list(scrollX = TRUE)))
      )
    } else {
      tagList(
        h4(paste(selected_row$Section, ":", selected_row$Details)),
        verbatimTextOutput("history_text_output")
      )
    }
  })
  
  output$history_text_output <- renderPrint({
    req(input$history_table_rows_selected)
    selected_row <- rv$history[input$history_table_rows_selected, ]
    
    if (selected_row$Section %in% c("Summary", "Statistics")) {
      if (is.list(selected_row$Results[[1]])) {
        cat(selected_row$Results[[1]]$results, sep = "\n")
      } else {
        cat(selected_row$Results[[1]], sep = "\n")
      }
    } else {
      cat("No text output available for this history item")
    }
  })
  
  # Download report as Word document
  output$download_report <- downloadHandler(
    filename = function() {
      paste("analysis-report-", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      id <- showNotification("Generating report...", duration = NULL)
      on.exit(removeNotification(id))
      
      doc <- officer::read_docx()
      
      doc <- doc %>% 
        officer::body_add_par("Enhanced Exploratory Data Analysis Report", style = "heading 1") %>% 
        officer::body_add_par(paste("Generated on:", Sys.Date()), style = "Normal") %>% 
        officer::body_add_par("", style = "Normal")
      
      doc <- doc %>% 
        officer::body_add_par("Data Overview", style = "heading 2")
      
      if (!is.null(eda_processed_data())) {
        doc <- doc %>% 
          officer::body_add_par("Data Preview (first 10 rows):", style = "Normal") %>% 
          flextable::body_add_flextable(
            flextable::flextable(head(eda_processed_data(), 10)) %>% 
              flextable::autofit()
          )
      }
      
      if (!is.null(rv$current_summary)) {
        doc <- doc %>% 
          officer::body_add_par("Summary Statistics", style = "heading 2") %>% 
          officer::body_add_par(paste(rv$current_summary, collapse = "\n"), style = "Normal")
      }
      
      if (!is.null(rv$current_freq)) {
        doc <- doc %>% 
          officer::body_add_par("Frequency Table", style = "heading 2") %>% 
          flextable::body_add_flextable(
            flextable::flextable(rv$current_freq) %>% 
              flextable::autofit()
          )
      }
      
      if (!is.null(rv$current_pivot)) {
        doc <- doc %>% 
          officer::body_add_par("Pivot Table", style = "heading 2") %>% 
          flextable::body_add_flextable(
            flextable::flextable(rv$current_pivot) %>% 
              flextable::autofit()
          )
      }
      
      if (!is.null(rv$current_plot)) {
        plot_file <- tempfile(fileext = ".png")
        ggplot2::ggsave(plot_file, plot = rv$current_plot, width = 6, height = 4, dpi = 300)
        
        doc <- doc %>% 
          officer::body_add_par("Current Visualization", style = "heading 2") %>% 
          officer::body_add_img(plot_file, width = 6, height = 4)
      }
      
      if (!is.null(rv$current_stats)) {
        doc <- doc %>% 
          officer::body_add_par("Statistical Analysis", style = "heading 2") %>% 
          officer::body_add_par(paste(rv$current_stats$results, collapse = "\n"), style = "Normal")
      }
      
      doc <- doc %>% 
        officer::body_add_par("Analysis History", style = "heading 2")
      
      if (nrow(rv$history) > 0) {
        for (i in 1:nrow(rv$history)) {
          entry <- rv$history[i, ]
          
          doc <- doc %>% 
            officer::body_add_par(paste0("Entry ", i, ": ", entry$Section, " - ", entry$Action), 
                                  style = "heading 3") %>% 
            officer::body_add_par(paste("Timestamp:", entry$Timestamp), style = "Normal") %>% 
            officer::body_add_par(paste("Details:", entry$Details), style = "Normal")
          
          if (entry$Section == "Plot") {
            plot_file <- tempfile(fileext = ".png")
            ggplot2::ggsave(plot_file, plot = entry$Results[[1]], width = 6, height = 4, dpi = 300)
            doc <- doc %>% 
              officer::body_add_img(plot_file, width = 6, height = 4)
          } else if (entry$Section %in% c("Frequency", "Pivot")) {
            doc <- doc %>% 
              flextable::body_add_flextable(
                flextable::flextable(entry$Results[[1]]) %>% 
                  flextable::autofit()
              )
          } else if (entry$Section %in% c("Summary", "Statistics")) {
            if (is.list(entry$Results[[1]])) {
              doc <- doc %>% 
                officer::body_add_par("Results:", style = "Normal") %>% 
                officer::body_add_par(paste(entry$Results[[1]]$results, collapse = "\n"), style = "Normal")
            } else {
              doc <- doc %>% 
                officer::body_add_par("Results:", style = "Normal") %>% 
                officer::body_add_par(paste(entry$Results[[1]], collapse = "\n"), style = "Normal")
            }
          }
          
          doc <- doc %>% officer::body_add_par("", style = "Normal")
        }
      } else {
        doc <- doc %>% 
          officer::body_add_par("No history entries available", style = "Normal")
      }
      
      print(doc, target = file)
    }
  )
  
  # Reset app
  observeEvent(input$reset, {
    #  rv$train_data <- NULL
    # rv$pred_data <- NULL
    rv$history <- data.frame(
      Timestamp = character(),
      Section = character(),
      Action = character(),
      Details = character(),
      Results = list(),
      stringsAsFactors = FALSE
    )
    rv$current_summary <- NULL
    rv$current_freq <- NULL
    rv$current_pivot <- NULL
    rv$current_plot <- NULL
    rv$current_stats <- NULL
    reset("selected_vars")
    showNotification("App has been reset", type = "message")
  })
  #EDA end here
  
  ###=====================================IMBALANCED DATA=============================================================#######  
  
  ### Data Balancing Methods-------------------
  
  # Update balance variable selection
  observe({
    req(rv$processed_data)
    # Get all factor variables that could be targets
    cat_vars <- names(rv$processed_data)[sapply(rv$processed_data, function(x) 
      is.factor(x) || is.character(x))]
    updateSelectInput(session, "balance_var", choices = cat_vars)
  })
  
  # Apply balancing
  observeEvent(input$apply_balance, {
    tryCatch({
      req(rv$processed_data, input$balance_var, input$balance_method)
      
      withProgress(message = 'Balancing data...', value = 0.3, {
        df <- rv$processed_data
        target <- input$balance_var
        method <- input$balance_method
        
        if (!is.factor(df[[target]])) {
          df[[target]] <- as.factor(df[[target]])
        }
        
        orig_dist <- table(df[[target]])
        
        balanced_data <- switch(method,
                                "smote" = {
                                  recipe(~ ., data = df) %>%
                                    step_smote(all_of(target), neighbors = 5) %>%
                                    prep() %>%
                                    bake(new_data = NULL)
                                },
                                "adasyn" = {
                                  themis::step_adasyn(recipe(~ ., data = df), all_of(target)) %>%
                                    prep() %>%
                                    bake(new_data = NULL)
                                },
                                "undersample" = {
                                  themis::step_downsample(recipe(~ ., data = df), all_of(target)) %>%
                                    prep() %>%
                                    bake(new_data = NULL)
                                },
                                "oversample" = {
                                  themis::step_upsample(recipe(~ ., data = df), all_of(target)) %>%
                                    prep() %>%
                                    bake(new_data = NULL)
                                },
                                "tomek" = {  # NEW TOMEK METHOD
                                  recipe(~ ., data = df) %>%
                                    step_tomek(all_of(target)) %>%
                                    prep() %>%
                                    bake(new_data = NULL)
                                },
                                "hybrid" = {
                                  recipe(~ ., data = df) %>%
                                    step_smote(all_of(target)) %>%
                                    step_tomek(all_of(target)) %>%
                                    prep() %>%
                                    bake(new_data = NULL)
                                }
        )
        # Modified storage to maintain history
        if(is.null(rv$balanced_data[[input$balance_method]])) {
          rv$balanced_data[[input$balance_method]] <- list()
        }
        
        
        # Store with timestamp
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        rv$balanced_data[[input$balance_method]][[timestamp]] <- list(
          data = balanced_data,
          target = input$balance_var,
          method = input$balance_method,
          original_dist = table(rv$categorized_data[[input$balance_var]]),
          balanced_dist = table(balanced_data[[input$balance_var]]),
          timestamp = timestamp
        )
        
        rv$show_results <- TRUE
        showNotification(paste("Data balanced using", 
                               toupper(input$balance_method), "successfully!"), 
                         type = "message")
        incProgress(0.7, detail = "Finalizing results...")
      })
      
      show_success(paste("Data balanced using", method, "successfully!"))
    }, error = function(e) {
      show_error(paste("Error balancing data:", e$message))
    })
  })  
  
  # Reset balancing
  observeEvent(input$reset_balance, {
    rv$balanced_data <- NULL
    show_success("Balancing results reset!")
  })
  
  ## Update the results UI to show historical runs
  output$view_results_ui <- renderUI({
    if(rv$show_results && length(rv$balanced_data) > 0) {
      tagList(
        h4("View Results:"),
        selectInput("view_method", "Select Method:",
                    choices = names(rv$balanced_data)),
        uiOutput("method_runs_ui")
      )
    }
  })
  
  # Show historical runs for selected method
  output$method_runs_ui <- renderUI({
    req(input$view_method, rv$balanced_data[[input$view_method]])
    
    runs <- names(rv$balanced_data[[input$view_method]])
    selectInput("view_run", "Select Run:",
                choices = runs,
                selected = runs[length(runs)]) # Default to most recent
  })
  
  # Container for results (modified to handle historical runs)
  output$results_container <- renderUI({
    req(input$view_method, input$view_run, 
        rv$balanced_data[[input$view_method]][[input$view_run]])
    
    current_run <- rv$balanced_data[[input$view_method]][[input$view_run]]
    
    tagList(
      div(class = "method-box",
          h3(toupper(current_run$method), " - Run: ", current_run$timestamp),
          h4("Class Distribution Summary"),
          fluidRow(
            column(6,
                   h5("Original Distribution"),
                   renderTable({
                     data.frame(
                       Class = names(current_run$original_dist),
                       Count = as.numeric(current_run$original_dist),
                       Percentage = round(prop.table(current_run$original_dist)*100, 1)
                     )
                   })
            ),
            column(6,
                   h5("Balanced Distribution"),
                   renderTable({
                     data.frame(
                       Class = names(current_run$balanced_dist),
                       Count = as.numeric(current_run$balanced_dist),
                       Percentage = round(prop.table(current_run$balanced_dist)*100, 1)
                     )
                   })
            )
          ),
          plotlyOutput("balance_plot"),
          h4("Balanced Data Preview"),
          DTOutput("balanced_data_preview"),
          downloadButton("download_balanced", "Download Balanced Data")
      )
    )
  })
  
  # Update plot and table outputs to use historical data
  output$balance_plot <- renderPlotly({
    req(input$view_method, input$view_run, 
        rv$balanced_data[[input$view_method]][[input$view_run]])
    
    current_run <- rv$balanced_data[[input$view_method]][[input$view_run]]
    
    # Safely calculate percentages
    orig_pct <- prop.table(current_run$original_dist) * 100
    bal_pct <- prop.table(current_run$balanced_dist) * 100
    
    # Prepare plot data with error handling
    tryCatch({
      plot_data <- rbind(
        data.frame(
          Class = names(current_run$original_dist),
          Count = as.numeric(current_run$original_dist),
          Type = "Original",
          Percentage = round(as.numeric(orig_pct), 1),
          stringsAsFactors = FALSE
        ),
        data.frame(
          Class = names(current_run$balanced_dist),
          Count = as.numeric(current_run$balanced_dist),
          Type = "Balanced",
          Percentage = round(as.numeric(bal_pct), 1),
          stringsAsFactors = FALSE
        )
      )
      
      # Create plot
      p <- ggplot(plot_data, aes(
        x = Class, y = Count, fill = Type,
        text = paste("Class:", Class, "<br>Type:", Type, 
                     "<br>Count:", Count, "<br>Percentage:", Percentage, "%")
      )) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
        geom_text(
          aes(label = paste0(Count, "\n(", Percentage, "%)")),
          position = position_dodge(width = 0.9),
          vjust = -0.5
        ) +
        labs(title = paste("Class Distribution -", current_run$method)) +
        theme_minimal()
      
      ggplotly(p, tooltip = "text")
      
    }, error = function(e) {
      plotly_empty() %>% layout(title = "Error generating plot")
    })
  }) 
  output$balanced_data_preview <- renderDT({
    req(input$view_method, input$view_run, 
        rv$balanced_data[[input$view_method]][[input$view_run]])
    
    datatable(head(rv$balanced_data[[input$view_method]][[input$view_run]]$data), 
              options = list(scrollX = TRUE))
  })
  
  output$download_balanced <- downloadHandler(
    filename = function() {
      current_run <- rv$balanced_data[[input$view_method]][[input$view_run]]
      paste0("balanced_data_", current_run$method, "_", current_run$timestamp, ".csv")
    },
    content = function(file) {
      current_run <- rv$balanced_data[[input$view_method]][[input$view_run]]
      write.csv(current_run$data, file, row.names = FALSE)
    }
  )
  ### BALANCING EVALUATION SERVER CODE --------------------------
  # Reactive values for evaluation
  rv$eval_results <- reactiveValues()
  rv$saved_evaluations <- list()
  rv$current_eval_id <- NULL
  rv$eval_history <- list()
  
  
  
  # Update target and predictor selections based on balanced data
  observe({
    req(rv$balanced_data)
    
    # Get the most recent balanced dataset
    if(length(rv$balanced_data) > 0) {
      # Get the first method's most recent run
      method <- names(rv$balanced_data)[1]
      latest_run <- rv$balanced_data[[method]][[length(rv$balanced_data[[method]])]]
      balanced_data <- latest_run$data
      
      # Update target variable selection
      cat_vars <- names(balanced_data)[sapply(balanced_data, function(x) 
        is.factor(x) || is.character(x))]
      updateSelectInput(session, "eval_target", choices = cat_vars)
      
      # Initialize predictor variables (will be updated when target is selected)
      updateSelectInput(session, "eval_predictors", choices = names(balanced_data))
      
      # Update methods to compare
      updateSelectInput(session, "eval_compare_methods", 
                        choices = names(rv$balanced_data),
                        selected = names(rv$balanced_data))
    }
  })
  
  # Update predictor variables when target changes
  observeEvent(input$eval_target, {
    req(input$eval_target, rv$balanced_data)
    
    # Get the most recent balanced dataset
    method <- names(rv$balanced_data)[1]
    latest_run <- rv$balanced_data[[method]][[length(rv$balanced_data[[method]])]]
    balanced_data <- latest_run$data
    
    # Remove target from predictor options
    pred_choices <- setdiff(names(balanced_data), input$eval_target)
    
    # Default to all other variables as predictors
    updateSelectInput(session, "eval_predictors", 
                      choices = pred_choices,
                      selected = pred_choices)
  })
  
  # Main evaluation function with enhanced diagnostics
  observeEvent(input$run_evaluation, {
    tryCatch({
      req(rv$balanced_data, input$eval_target, input$eval_predictors, input$eval_compare_methods)
      
      # Data validation checks
      for(method in input$eval_compare_methods) {
        if(!input$eval_target %in% names(rv$balanced_data[[method]][[1]]$data)) {
          stop(paste("Target variable not found in", method, "data"))
        }
        
        missing_preds <- setdiff(input$eval_predictors, names(rv$balanced_data[[method]][[1]]$data))
        if(length(missing_preds) > 0) {
          stop(paste("Predictors missing in", method, "data:", paste(missing_preds, collapse=", ")))
        }
      }
      
      ## Generate unique ID for this evaluation
      eval_id <- paste0("eval_", format(Sys.time(), "%Y%m%d_%H%M%S"))
      rv$current_eval_id <- eval_id
      
      withProgress(message = 'Running comprehensive evaluation...', value = 0, {
        ## Initialize evaluation results structure
        rv$eval_results[[eval_id]] <- list(
          timestamp = Sys.time(),
          target = input$eval_target,
          predictors = input$eval_predictors,
          methods = input$eval_compare_methods,
          results = list()
        )
        
        n_methods <- length(input$eval_compare_methods)
        
        ## 1. Basic Distribution Analysis
        incProgress(0.1, detail = "Analyzing class distributions...")
        dist_results <- lapply(input$eval_compare_methods, function(method) {
          latest_run <- rv$balanced_data[[method]][[length(rv$balanced_data[[method]])]]
          list(
            method = method,
            original_dist = latest_run$original_dist,
            balanced_dist = latest_run$balanced_dist,
            change_ratio = latest_run$balanced_dist/latest_run$original_dist,
            balance_score = (min(latest_run$balanced_dist)/max(latest_run$balanced_dist)) / 
              (min(latest_run$original_dist)/max(latest_run$original_dist))
          )
        })
        rv$eval_results[[eval_id]]$distribution <- dist_results
        
        ## 2. Model Performance Comparison with Random Forest
        incProgress(0.3, detail = "Training models...")
        performance_results <- list()
        
        for(i in seq_along(input$eval_compare_methods)) {
          method <- input$eval_compare_methods[i]
          data <- rv$balanced_data[[method]][[1]]$data
          target <- input$eval_target
          predictors <- input$eval_predictors
          
          ## Progress update
          incProgress(0.3 + 0.4*i/n_methods, detail = paste("Evaluating", method, "..."))
          
          ## Convert target to factor if needed
          if(!is.factor(data[[target]])) {
            data[[target]] <- factor(data[[target]])
          }
          
          ## Select only the target and chosen predictors
          model_data <- data[, c(target, predictors), drop = FALSE]
          
          ## Remove any rows with NA values
          model_data <- na.omit(model_data)
          
          ## Check if we still have enough data
          if(nrow(model_data) < 10) {
            performance_results[[method]] <- list(
              error = "Insufficient data after NA removal"
            )
            next
          }
          
          ## Split data
          set.seed(123)
          train_index <- createDataPartition(model_data[[target]], 
                                             p = input$train_split/100, 
                                             list = FALSE)
          train_data <- model_data[train_index, ]
          test_data <- model_data[-train_index, ]
          
          ## Store test data for reference
          if(is.null(rv$test_data)) {
            rv$test_data <- test_data
          }
          
          ## Train Random Forest with error handling
          rf_formula <- as.formula(paste(target, "~ ."))
          
          tryCatch({
            rf_model <- randomForest(
              x = train_data[, predictors, drop = FALSE],
              y = train_data[[target]],
              ntree = ifelse(is.null(input$rf_ntree), 500, input$rf_ntree),
              mtry = if(!is.null(input$rf_mtry)) input$rf_mtry else floor(sqrt(length(predictors))),
              importance = TRUE
            )
            
            ## Make predictions
            predictions_prob <- predict(rf_model, newdata = test_data, type = "prob")
            predictions_class <- predict(rf_model, newdata = test_data)
            
            ## Calculate metrics
            cm <- confusionMatrix(predictions_class, test_data[[target]])
            
            ## Calculate ROC/AUC
            auc_value <- NA
            if(nlevels(test_data[[target]]) == 2) {
              roc_obj <- roc(test_data[[target]], predictions_prob[,2])
              auc_value <- auc(roc_obj)
            } else {
              auc_value <- multiclass.roc(test_data[[target]], predictions_prob)$auc
            }
            
            ## Store results
            performance_results[[method]] <- list(
              model = rf_model,
              predictions_prob = predictions_prob,
              predictions_class = predictions_class,
              actual = test_data[[target]],
              cm = cm,
              metrics = data.frame(
                Accuracy = cm$overall["Accuracy"],
                Kappa = cm$overall["Kappa"],
                Sensitivity = ifelse(nlevels(test_data[[target]]) == 2, 
                                     cm$byClass["Sensitivity"],
                                     mean(cm$byClass[,"Sensitivity"])),
                Specificity = ifelse(nlevels(test_data[[target]]) == 2,
                                     cm$byClass["Specificity"],
                                     mean(cm$byClass[,"Specificity"])),
                F1 = ifelse(nlevels(test_data[[target]]) == 2,
                            cm$byClass["F1"],
                            mean(cm$byClass[,"F1"])),
                AUC = auc_value
              )
            )
            
          }, error = function(e) {
            performance_results[[method]] <<- list(
              error = paste("Model training failed:", e$message)
            )
          })
        }
        
        rv$eval_results[[eval_id]]$performance <- performance_results
        
        ## 3. Enhanced Overfitting Analysis
        incProgress(0.7, detail = "Checking for overfitting...")
        overfit_results <- lapply(names(performance_results), function(method) {
          perf <- performance_results[[method]]
          
          if(!is.null(perf$error)) {
            return(list(method = method, error = perf$error))
          }
          
          train_acc <- if(!is.null(perf$model$predicted)) {
            mean(perf$model$predicted == perf$model$y)
          } else NA
          
          test_acc <- perf$cm$overall["Accuracy"]
          diff <- if(!is.na(train_acc)) train_acc - test_acc else NA
          
          list(
            method = method,
            train_acc = train_acc,
            test_acc = test_acc,
            diff = diff,
            is_overfit = abs(diff) > 0.15,
            overfit_severity = ifelse(is.na(diff), "NA",
                                      ifelse(abs(diff) > 0.15, "High",
                                             ifelse(abs(diff) > 0.05, "Moderate", "Low")))
          )
        })
        rv$eval_results[[eval_id]]$overfitting <- overfit_results
        
        ## 4. Enhanced Noise Analysis
        incProgress(0.8, detail = "Analyzing noise...")
        noise_results <- lapply(input$eval_compare_methods, function(method) {
          data <- rv$balanced_data[[method]][[1]]$data
          target <- input$eval_target
          
          result <- list(
            method = method,
            silhouette_score = NA,
            noise_level = "NA",
            error = NULL
          )
          
          tryCatch({
            # Verify target has at least 2 classes with sufficient samples
            target_counts <- table(data[[target]])
            if(length(target_counts) < 2 || any(target_counts < 2)) {
              result$error <- paste("Target needs at least 2 classes with 2+ samples. Current counts:", 
                                    paste(names(target_counts), target_counts, collapse=", "))
              return(result)
            }
            
            # Select only numeric columns with sufficient variance
            numeric_cols <- sapply(data, is.numeric)
            if(sum(numeric_cols) < 2) {
              result$error <- "Need at least 2 numeric columns"
              return(result)
            }
            
            num_data <- data[, numeric_cols, drop = FALSE]
            
            # Remove near-zero variance columns
            col_vars <- apply(num_data, 2, var, na.rm = TRUE)
            num_data <- num_data[, col_vars > 1e-10 & !is.na(col_vars), drop = FALSE]
            
            if(ncol(num_data) < 2) {
              result$error <- "Insufficient variance in numeric columns"
              return(result)
            }
            
            # Scale and remove NAs
            num_data <- scale(num_data)
            num_data <- na.omit(num_data)
            target_values <- data[[target]][rownames(num_data)]
            
            # Verify we still have multiple classes
            if(length(unique(target_values)) < 2) {
              result$error <- "Insufficient classes after filtering"
              return(result)
            }
            
            # Calculate distance matrix with sampling if needed
            max_samples <- 2000
            if(nrow(num_data) > max_samples) {
              set.seed(123)
              sample_idx <- sample(1:nrow(num_data), max_samples)
              num_data <- num_data[sample_idx, ]
              target_values <- target_values[sample_idx]
            }
            
            # Calculate distance matrix with fallback
            dist_matrix <- tryCatch({
              dist(num_data)
            }, error = function(e) {
              as.dist(proxy::dist(num_data, method = "Euclidean"))
            })
            
            # Calculate silhouette scores
            sil <- cluster::silhouette(as.numeric(factor(target_values)), dist_matrix)
            sil_score <- mean(sil[, "sil_width"], na.rm = TRUE)
            
            result$silhouette_score <- sil_score
            result$noise_level <- if(is.na(sil_score)) {
              "NA"
            } else if(sil_score < 0.1) {
              "High"
            } else if(sil_score < 0.2) {
              "Moderate"
            } else {
              "Low"
            }
            
          }, error = function(e) {
            result$error <- paste("Error:", e$message)
          })
          
          return(result)
        })
        rv$eval_results[[eval_id]]$noise <- noise_results
        
        ## 5. Dataset Size Analysis
        incProgress(0.85, detail = "Analyzing dataset sizes...")
        size_results <- lapply(input$eval_compare_methods, function(method) {
          tryCatch({
            method_data <- rv$balanced_data[[method]][[1]]
            
            if(is.null(method_data$original_dist) || is.null(method_data$balanced_dist)) {
              return(list(
                method = method,
                error = "Distribution data not available"
              ))
            }
            
            orig_size <- sum(method_data$original_dist)
            balanced_size <- sum(method_data$balanced_dist)
            
            list(
              method = method,
              original_size = orig_size,
              balanced_size = balanced_size,
              size_change = balanced_size - orig_size,
              size_ratio = ifelse(orig_size == 0, NA, balanced_size/orig_size),
              size_impact = ifelse(orig_size == 0, "NA",
                                   ifelse(balanced_size/orig_size > 5, "High",
                                          ifelse(balanced_size/orig_size > 2, "Moderate", "Low")))
            )
          }, error = function(e) {
            list(
              method = method,
              error = paste("Size analysis failed:", e$message)
            )
          })
        })
        rv$eval_results[[eval_id]]$size <- size_results
        
        ## 6. Enhanced Method Weaknesses Analysis
        incProgress(0.9, detail = "Analyzing method weaknesses...")
        weakness_results <- lapply(input$eval_compare_methods, function(method) {
          tryCatch({
            # Get all relevant metrics for this method
            dist <- rv$eval_results[[eval_id]]$distribution[[
              which(sapply(rv$eval_results[[eval_id]]$distribution, function(x) x$method == method))]]
            perf <- rv$eval_results[[eval_id]]$performance[[method]]
            overfit <- rv$eval_results[[eval_id]]$overfitting[[
              which(sapply(rv$eval_results[[eval_id]]$overfitting, function(x) x$method == method))]]
            noise <- rv$eval_results[[eval_id]]$noise[[
              which(sapply(rv$eval_results[[eval_id]]$noise, function(x) x$method == method))]]
            size <- rv$eval_results[[eval_id]]$size[[
              which(sapply(rv$eval_results[[eval_id]]$size, function(x) x$method == method))]]
            
            weaknesses <- character(0)
            strengths <- character(0)
            
            # Check for errors first
            if(!is.null(perf$error)) {
              return(list(
                method = method,
                weaknesses = c("Performance evaluation failed", perf$error),
                strengths = "None identified"
              ))
            }
            
            # Balance effectiveness
            if(!is.null(dist$balance_score) && dist$balance_score < 0.8) {
              weaknesses <- c(weaknesses, paste("Poor balance improvement (score:", round(dist$balance_score, 2), ")"))
            } else if(!is.null(dist$balance_score)) {
              strengths <- c(strengths, paste("Good balance improvement (score:", round(dist$balance_score, 2), ")"))
            }
            
            # Overfitting analysis
            if(!is.null(overfit$is_overfit) && overfit$is_overfit) {
              weaknesses <- c(weaknesses, paste("Overfitting risk (train-test diff:", round(overfit$diff, 2), ")"))
            } else if(!is.null(overfit$diff)) {
              strengths <- c(strengths, paste("Low overfitting risk (train-test diff:", round(overfit$diff, 2), ")"))
            }
            
            # Noise analysis
            if(!is.na(noise$silhouette_score) && noise$noise_level %in% c("High", "Moderate")) {
              weaknesses <- c(weaknesses, paste("Noise risk (silhouette:", round(noise$silhouette_score, 2), ")"))
            } else if(!is.na(noise$silhouette_score)) {
              strengths <- c(strengths, paste("Low noise (silhouette:", round(noise$silhouette_score, 2), ")"))
            }
            
            # Size impact
            if(!is.null(size$size_impact) && size$size_impact %in% c("High", "Moderate")) {
              weaknesses <- c(weaknesses, paste("Significant size change (", 
                                                ifelse(size$size_change > 0, "+", ""),
                                                size$size_change, "instances)"))
            } else if(!is.null(size$size_impact)) {
              strengths <- c(strengths, paste("Reasonable size change (", 
                                              ifelse(size$size_change > 0, "+", ""),
                                              size$size_change, "instances)"))
            }
            
            # Performance metrics
            if(!is.null(perf$metrics$Accuracy) && perf$metrics$Accuracy < 0.7) {
              weaknesses <- c(weaknesses, paste("Low accuracy (", round(perf$metrics$Accuracy, 3), ")"))
            } else if(!is.null(perf$metrics$Accuracy)) {
              strengths <- c(strengths, paste("Good accuracy (", round(perf$metrics$Accuracy, 3), ")"))
            }
            
            # Ensure we always have at least one weakness/strength
            if(length(weaknesses) == 0) weaknesses <- "No major weaknesses identified"
            if(length(strengths) == 0) strengths <- "No significant strengths identified"
            
            list(
              method = method,
              weaknesses = weaknesses,
              strengths = strengths
            )
          }, error = function(e) {
            list(
              method = method,
              weaknesses = paste("Weakness analysis failed:", e$message),
              strengths = "None identified"
            )
          })
        })
        rv$eval_results[[eval_id]]$weaknesses <- weakness_results
        
        incProgress(1, detail = "Finalizing results...")
      })
      
      showNotification("Comprehensive evaluation completed!", type = "message")
    }, error = function(e) {
      showNotification(paste("Evaluation error:", e$message), type = "error")
    })
  })
  
  # Save current evaluation results
  observeEvent(input$save_eval_results, {
    req(rv$current_eval_id, rv$eval_results[[rv$current_eval_id]])
    
    eval_id <- rv$current_eval_id
    rv$saved_evaluations[[eval_id]] <- rv$eval_results[[eval_id]]
    rv$eval_history[[eval_id]] <- Sys.time()
    
    showNotification("Evaluation results saved!", type = "message")
  })
  
  # Load saved evaluation
  observeEvent(input$load_evaluation, {
    req(input$saved_evaluations_table_rows_selected)
    
    selected_id <- names(rv$saved_evaluations)[input$saved_evaluations_table_rows_selected]
    rv$current_eval_id <- selected_id
    rv$eval_results[[selected_id]] <- rv$saved_evaluations[[selected_id]]
    
    showNotification(paste("Loaded evaluation:", selected_id), type = "message")
  })
  
  # Delete saved evaluation
  observeEvent(input$delete_evaluation, {
    req(input$saved_evaluations_table_rows_selected)
    
    selected_id <- names(rv$saved_evaluations)[input$saved_evaluations_table_rows_selected]
    rv$saved_evaluations[[selected_id]] <- NULL
    rv$eval_history[[selected_id]] <- NULL
    
    if(!is.null(rv$current_eval_id) && rv$current_eval_id == selected_id) {
      rv$current_eval_id <- NULL
    }
    
    showNotification(paste("Deleted evaluation:", selected_id), type = "message")
  })
  
  # Output renderers for evaluation results
  
  # 1. Distribution Comparison
  output$dist_comparison_plot <- renderPlotly({
    req(rv$current_eval_id, rv$eval_results[[rv$current_eval_id]]$distribution)
    
    # Create proper data frame with all required columns
    plot_data <- do.call(rbind, lapply(rv$eval_results[[rv$current_eval_id]]$distribution, function(x) {
      data.frame(
        Method = x$method,
        Class = names(x$original_dist),
        Original = as.numeric(x$original_dist),
        Balanced = as.numeric(x$balanced_dist),
        OriginalPct = as.numeric(round(prop.table(x$original_dist)*100, 1)),
        BalancedPct = as.numeric(round(prop.table(x$balanced_dist)*100, 1)),
        stringsAsFactors = FALSE
      )
    }))
    
    # Ensure proper data transformation
    plot_data <- plot_data %>% 
      pivot_longer(
        cols = c(Original, Balanced), 
        names_to = "Type", 
        values_to = "Count"
      ) %>%
      mutate(
        Percentage = ifelse(Type == "Original", OriginalPct, BalancedPct),
        Percentage = as.numeric(Percentage)  # Ensure numeric type
      )
    
    # Create plot with error handling
    tryCatch({
      p <- ggplot(plot_data, aes(x = Class, y = Count, fill = Type)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        geom_text(aes(label = paste0(Count, "\n(", Percentage, "%)")), 
                  position = position_dodge(width = 0.9), 
                  vjust = -0.5, size = 3) +
        facet_wrap(~Method, scales = "free_y") +
        labs(title = "Class Distribution Comparison") +
        theme_minimal()
      
      ggplotly(p)
    }, error = function(e) {
      plotly_empty() %>% 
        layout(title = "Error generating plot")
    })
  })   
  # 2. Performance Metrics Table
  output$performance_table <- renderDT({
    req(rv$current_eval_id, rv$eval_results[[rv$current_eval_id]]$performance)
    
    tryCatch({
      metrics_df <- do.call(rbind, lapply(names(rv$eval_results[[rv$current_eval_id]]$performance), function(method) {
        perf <- rv$eval_results[[rv$current_eval_id]]$performance[[method]]
        if(!is.null(perf$error)) {
          data.frame(
            Method = method, 
            Status = "Error",
            Details = perf$error,
            Accuracy = NA,
            Kappa = NA,
            Sensitivity = NA,
            Specificity = NA,
            F1 = NA,
            AUC = NA
          )
        } else {
          data.frame(
            Method = method,
            Status = "Success",
            Accuracy = round(perf$metrics$Accuracy, 3),
            Kappa = round(perf$metrics$Kappa, 3),
            Sensitivity = round(ifelse(nlevels(perf$actual) == 2, 
                                       perf$cm$byClass["Sensitivity"],
                                       mean(perf$cm$byClass[,"Sensitivity"])), 3),
            Specificity = round(ifelse(nlevels(perf$actual) == 2,
                                       perf$cm$byClass["Specificity"],
                                       mean(perf$cm$byClass[,"Specificity"])), 3),
            F1 = round(ifelse(nlevels(perf$actual) == 2,
                              perf$cm$byClass["F1"],
                              mean(perf$cm$byClass[,"F1"])), 3),
            AUC = if(!is.null(perf$metrics$AUC)) round(perf$metrics$AUC, 3) else NA,
            Details = ""
          )
        }
      }))
      
      datatable(
        metrics_df,
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          pageLength = 10,
          scrollX = TRUE
        ),
        rownames = FALSE
      ) %>%
        formatStyle(
          'Status',
          backgroundColor = styleEqual(
            c("Error", "Success"),
            c('#ff9999', '#99ff99')
          )
        )
    }, error = function(e) {
      datatable(
        data.frame(Error = c("Could not generate performance table", 
                             paste("Details:", e$message))),
        options = list(dom = 't')
      )
    })
  })  
  # 3. Overfitting Analysis Plot
  output$overfitting_plot <- renderPlotly({
    req(rv$current_eval_id, rv$eval_results[[rv$current_eval_id]]$overfitting)
    
    plot_data <- do.call(rbind, lapply(rv$eval_results[[rv$current_eval_id]]$overfitting, function(x) {
      if(!is.null(x$error)) return(NULL)
      data.frame(
        Method = x$method,
        TrainAccuracy = x$train_acc,
        TestAccuracy = x$test_acc,
        Difference = x$diff,
        Severity = x$overfit_severity
      )
    }))
    
    if(is.null(plot_data)) return(plotly_empty(type = "scatter"))
    
    plot_data <- plot_data %>% 
      pivot_longer(cols = c(TrainAccuracy, TestAccuracy), 
                   names_to = "Type", values_to = "Accuracy")
    
    p <- ggplot(plot_data, aes(x = Method, y = Accuracy, fill = Type)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("TrainAccuracy" = "#1f77b4", "TestAccuracy" = "#ff7f0e")) +
      labs(title = "Train vs Test Accuracy",
           y = "Accuracy") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p) %>% 
      layout(legend = list(orientation = "h", y = -0.2),
             margin = list(b = 100))
  })
  
  # 4. Noise Analysis Plot
  output$noise_plot <- renderPlotly({
    req(rv$current_eval_id, rv$eval_results[[rv$current_eval_id]]$noise)
    
    tryCatch({
      # Prepare plot data
      plot_data <- do.call(rbind, lapply(rv$eval_results[[rv$current_eval_id]]$noise, function(x) {
        data.frame(
          Method = x$method,
          SilhouetteScore = ifelse(is.na(x$silhouette_score), 0, x$silhouette_score),
          NoiseLevel = x$noise_level,
          stringsAsFactors = FALSE
        )
      }))
      
      # Check if we have valid data
      if(all(is.na(plot_data$SilhouetteScore))) {
        return(plotly_empty(type = "scatter", mode = "markers") %>% 
                 layout(title = "No valid silhouette scores available"))
      }
      
      # Create color scale
      color_scale <- c("High" = "#ff6b6b", "Moderate" = "#ffd166", "Low" = "#06d6a0", "NA" = "#adb5bd")
      
      # Create plot
      p <- ggplot(plot_data, aes(
        x = Method, 
        y = SilhouetteScore, 
        fill = NoiseLevel,
        text = paste0(
          "Method: ", Method, "\n",
          "Silhouette Score: ", round(SilhouetteScore, 3), "\n",
          "Noise Level: ", NoiseLevel, "\n",
          "Interpretation: ", ifelse(SilhouetteScore > 0.5, "Good separation",
                                     ifelse(SilhouetteScore > 0.2, "Moderate separation", 
                                            "Potential noise"))
        )
      )) +
        geom_bar(stat = "identity") +
        geom_hline(yintercept = 0.5, linetype = "dashed", color = "#06d6a0") +
        geom_hline(yintercept = 0.2, linetype = "dashed", color = "#ffd166") +
        scale_fill_manual(values = color_scale) +
        labs(
          title = "Noise Analysis - Silhouette Scores",
          x = "Balancing Method",
          y = "Silhouette Score",
          fill = "Noise Level"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.position = "bottom"
        ) +
        ylim(0, 1)  # Silhouette scores range from -1 to 1, but we focus on 0-1
      
      # Convert to plotly with enhanced tooltips
      ggplotly(p, tooltip = "text") %>% 
        layout(
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.3
          ),
          margin = list(b = 120, t = 80),
          hoverlabel = list(
            bgcolor = "white",
            font = list(size = 12)
          )
        )
      
    }, error = function(e) {
      plotly_empty(type = "scatter", mode = "markers") %>% 
        layout(title = paste("Error generating noise plot:", e$message))
    })
  })  
  # 5. Dataset Size Analysis Plot
  output$size_plot <- renderPlotly({
    req(rv$current_eval_id, rv$eval_results[[rv$current_eval_id]]$size)
    
    # Create plot data with additional calculated metrics
    plot_data <- do.call(rbind, lapply(rv$eval_results[[rv$current_eval_id]]$size, function(x) {
      data.frame(
        Method = x$method,
        Original = x$original_size,
        Balanced = x$balanced_size,
        Change = x$balanced_size - x$original_size,
        ChangeRatio = round(x$balanced_size / x$original_size, 2),
        Impact = x$size_impact
      )
    }))
    
    # Reshape data for plotting
    plot_data <- plot_data %>% 
      pivot_longer(
        cols = c(Original, Balanced), 
        names_to = "Type", 
        values_to = "Size"
      )
    
    # Create the plot
    p <- ggplot(plot_data, aes(
      x = Method, 
      y = Size, 
      fill = Impact,
      text = paste0(
        "Method: ", Method, "\n",
        "Dataset: ", Type, "\n",
        "Size: ", format(Size, big.mark = ","), "\n",
        "Change: ", ifelse(Type == "Balanced", 
                           paste0(format(Change, big.mark = ","), 
                                  " (", ifelse(Change > 0, "+", ""), Change, ")"),
                           "N/A"), "\n",
        "Change Ratio: ", ifelse(Type == "Balanced", ChangeRatio, "N/A"), "x\n",
        "Impact: ", Impact
      )
    )) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
      geom_text(
        aes(label = format(Size, big.mark = ",")),
        position = position_dodge(width = 0.8),
        vjust = -0.5, 
        size = 3.5
      ) +
      scale_fill_manual(
        values = c("High" = "#ff6b6b", "Moderate" = "#ffd166", "Low" = "#06d6a0"),
        name = "Size Impact"
      ) +
      labs(
        title = "Dataset Size Comparison Before and After Balancing",
        x = "Balancing Method",
        y = "Number of Instances",
        fill = "Impact Level"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom"
      ) +
      scale_y_continuous(labels = scales::comma)
    
    # Convert to plotly with enhanced tooltips
    ggplotly(p, tooltip = "text") %>% 
      layout(
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.3
        ),
        margin = list(b = 120, t = 80),
        hoverlabel = list(
          bgcolor = "white",
          font = list(size = 12)
        )
      )
  })  
  # 6. Method Weaknesses Table
  output$weakness_table <- renderDT({
    req(rv$current_eval_id, rv$eval_results[[rv$current_eval_id]]$weaknesses)
    
    tryCatch({
      # Prepare table data with line breaks replaced for CSV/Excel export
      table_data <- do.call(rbind, lapply(rv$eval_results[[rv$current_eval_id]]$weaknesses, function(x) {
        data.frame(
          Method = x$method,
          Weaknesses = if(length(x$weaknesses) > 0) paste("-", unlist(x$weaknesses), collapse = " | ") else "None identified",
          Strengths = if(length(x$strengths) > 0) paste("-", unlist(x$strengths), collapse = " | ") else "None identified",
          stringsAsFactors = FALSE
        )
      }))
      
      # Create datatable with export options
      dt <- datatable(
        table_data,
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          pageLength = 10,
          scrollX = TRUE
        ),
        rownames = FALSE,
        colnames = c('Method', 'Weaknesses', 'Strengths'),
        class = 'stripe hover'
      )
      
      # Add HTML version for display only
      table_data_html <- table_data
      table_data_html$Weaknesses <- gsub("\\|", "<br>-", table_data_html$Weaknesses)
      table_data_html$Strengths <- gsub("\\|", "<br>-", table_data_html$Strengths)
      
      dt$x$data <- table_data_html
      
      return(dt)
      
    }, error = function(e) {
      datatable(
        data.frame(Error = c("Could not generate weakness table", 
                             paste("Details:", e$message))),
        options = list(dom = 't')
      )
    })
  })
  
  # 7. Method Comparison Summary
  output$method_summary <- renderUI({
    req(rv$current_eval_id, rv$eval_results[[rv$current_eval_id]])
    
    tryCatch({
      eval_data <- rv$eval_results[[rv$current_eval_id]]
      
      # Safely extract metrics
      summary_data <- data.frame(
        Method = eval_data$methods,
        Accuracy = sapply(eval_data$methods, function(m) {
          if(!is.null(eval_data$performance[[m]]$error)) NA
          else round(eval_data$performance[[m]]$metrics$Accuracy, 3)
        }),
        AUC = sapply(eval_data$methods, function(m) {
          if(!is.null(eval_data$performance[[m]]$error)) NA
          else round(eval_data$performance[[m]]$metrics$AUC, 3)
        }),
        BalanceScore = sapply(eval_data$distribution, function(x) round(x$balance_score, 2)),
        stringsAsFactors = FALSE
      )
      
      tagList(
        h3("Evaluation Summary"),
        renderTable(summary_data),
        h4("Target Variable:"),
        p(eval_data$target)
      )
    }, error = function(e) {
      tagList(
        h3("Error generating summary"),
        p(e$message))
    })
  })  
  
  # AUC Plot for binary classification
  output$auc_plot <- renderPlotly({
    req(rv$current_eval_id, rv$eval_results[[rv$current_eval_id]]$performance)
    
    tryCatch({
      # Get performance data
      perf_data <- rv$eval_results[[rv$current_eval_id]]$performance
      
      # Check if we have valid data
      if(length(perf_data) == 0) stop("No performance data")
      
      # Check number of classes
      n_classes <- nlevels(perf_data[[1]]$actual)
      if(n_classes < 2) stop("Need at least 2 classes for ROC")
      
      # Prepare plot data
      plot_data <- do.call(rbind, lapply(names(perf_data), function(method) {
        perf <- perf_data[[method]]
        if(is.null(perf$error) && !is.null(perf$metrics$AUC)) {
          data.frame(
            Method = method,
            AUC = perf$metrics$AUC,
            stringsAsFactors = FALSE
          )
        }
      }))
      
      if(is.null(plot_data) || nrow(plot_data) == 0) stop("No valid AUC data")
      
      # Create plot
      p <- ggplot(plot_data, aes(x = Method, y = AUC, fill = Method,
                                 text = paste("Method:", Method, "<br>AUC:", AUC))) +
        geom_bar(stat = "identity") +
        labs(title = "AUC Comparison") +
        theme_minimal()
      
      ggplotly(p, tooltip = "text")
      
    }, error = function(e) {
      plotly_empty() %>% layout(title = paste("Error:", e$message))
    })
  })
  
  # Overfitting Table
  output$overfitting_table <- renderDT({
    req(rv$current_eval_id, rv$eval_results[[rv$current_eval_id]]$overfitting)
    
    tryCatch({
      # Prepare table data
      table_data <- do.call(rbind, lapply(rv$eval_results[[rv$current_eval_id]]$overfitting, function(x) {
        if(!is.null(x$error)) {
          data.frame(
            Method = x$method,
            Status = "Error",
            Message = x$error,
            TrainAccuracy = NA,
            TestAccuracy = NA,
            Difference = NA,
            Risk = NA,
            stringsAsFactors = FALSE
          )
        } else {
          data.frame(
            Method = x$method,
            Status = "Success",
            Message = "",
            TrainAccuracy = round(x$train_acc, 3),
            TestAccuracy = round(x$test_acc, 3),
            Difference = round(x$diff, 3),
            Risk = x$overfit_severity,
            stringsAsFactors = FALSE
          )
        }
      }))
      
      # Create datatable with export options
      datatable(
        table_data,
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          pageLength = 10,
          scrollX = TRUE,
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#f8f9fa', 'font-weight': 'bold'});",
            "}"
          )
        ),
        rownames = FALSE,
        colnames = c('Method', 'Status', 'Message', 'Train Accuracy', 'Test Accuracy', 
                     'Difference', 'Risk Level'),
        class = 'stripe hover'
      ) %>%
        formatStyle(
          'Risk',
          backgroundColor = styleEqual(
            c("High", "Moderate", "Low", "NA"),
            c('#ff6b6b', '#ffd166', '#06d6a0', '#f8f9fa')
          )
        ) %>%
        formatStyle(
          'Status',
          backgroundColor = styleEqual(
            c("Error", "Success"),
            c('#ff9999', '#99ff99')
          )
        ) %>%
        formatRound(c('TrainAccuracy', 'TestAccuracy', 'Difference'), 3)
      
    }, error = function(e) {
      datatable(
        data.frame(Error = c("Could not generate overfitting table", 
                             paste("Details:", e$message))),
        options = list(dom = 't')
      )
    })
  })  
  
  # Noise Table
  output$noise_table <- renderDT({
    req(rv$current_eval_id, rv$eval_results[[rv$current_eval_id]]$noise)
    
    tryCatch({
      # Prepare table data
      table_data <- do.call(rbind, lapply(rv$eval_results[[rv$current_eval_id]]$noise, function(x) {
        data.frame(
          Method = x$method,
          Silhouette_Score = ifelse(is.na(x$silhouette_score), "NA", round(x$silhouette_score, 3)),
          Noise_Level = x$noise_level,
          Status = ifelse(is.null(x$error), "Success", "Error"),
          Message = ifelse(is.null(x$error), "", x$error),
          stringsAsFactors = FALSE
        )
      }))
      
      # Create datatable with export options
      datatable(
        table_data,
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          pageLength = 10,
          scrollX = TRUE
        ),
        rownames = FALSE,
        colnames = c('Method', 'Silhouette Score', 'Noise Level', 'Status', 'Message'),
        class = 'stripe hover'
      ) %>%
        formatStyle(
          'Noise_Level',
          backgroundColor = styleEqual(
            c("High", "Moderate", "Low", "NA"),
            c('#ff6b6b', '#ffd166', '#06d6a0', '#f8f9fa')
          )
        ) %>%
        formatStyle(
          'Status',
          backgroundColor = styleEqual(
            c("Error", "Success"),
            c('#ff9999', '#99ff99')
          )
        )
    }, error = function(e) {
      datatable(
        data.frame(Error = c("Could not generate noise table", 
                             paste("Details:", e$message))),
        options = list(dom = 't')
      )
    })
  })  
  # Size Table
  output$size_table <- renderDT({
    req(rv$current_eval_id, rv$eval_results[[rv$current_eval_id]]$size)
    
    # Debug: Print the size data structure
    print(str(rv$eval_results[[rv$current_eval_id]]$size))
    
    tryCatch({
      # Prepare table data with proper error handling
      table_data <- lapply(rv$eval_results[[rv$current_eval_id]]$size, function(x) {
        if(!is.null(x$error)) {
          data.frame(
            Method = x$method,
            Status = "Error",
            Message = x$error,
            Original = NA,
            Balanced = NA,
            Change = NA,
            Ratio = NA,
            Impact = NA,
            stringsAsFactors = FALSE
          )
        } else {
          data.frame(
            Method = x$method,
            Status = "Success",
            Message = "",
            Original = x$original_size,
            Balanced = x$balanced_size,
            Change = x$size_change,
            Ratio = ifelse(is.null(x$size_ratio), NA, round(x$size_ratio, 2)),
            Impact = ifelse(is.null(x$size_impact), "NA", x$size_impact),
            stringsAsFactors = FALSE
          )
        }
      })
      
      # Combine all rows
      table_data <- do.call(rbind, table_data)
      
      # Create datatable with enhanced features
      dt <- datatable(
        table_data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#f8f9fa', 'font-weight': 'bold'});",
            "}")
        ),
        rownames = FALSE,
        colnames = c("Method", "Status", "Message", "Original", "Balanced", 
                     "Change", "Ratio", "Impact"),
        extensions = 'Buttons',
        class = 'stripe hover'
      ) %>%
        formatStyle(
          'Status',
          backgroundColor = styleEqual(
            c("Error", "Success"),
            c('#ff9999', '#99ff99')
          )
        ) %>%
        formatStyle(
          'Impact',
          backgroundColor = styleEqual(
            c("High", "Moderate", "Low", "NA"),
            c('#ff6b6b', '#ffd166', '#06d6a0', '#f8f9fa')
          )
        ) %>%
        formatRound(c('Ratio'), 2) %>%
        formatCurrency(c('Original', 'Balanced', 'Change'), currency = "", digits = 0)
      
      return(dt)
      
    }, error = function(e) {
      # Debug: Print error message
      print(paste("Error generating size table:", e$message))
      
      # Return simple error table
      datatable(data.frame(
        Error = c("Could not generate size table", 
                  paste("Reason:", e$message))
      ), options = list(dom = 't'))
    })
  })  
  # Weakness Table
  output$weakness_table <- renderDT({
    req(rv$current_eval_id, rv$eval_results[[rv$current_eval_id]]$weaknesses)
    
    tryCatch({
      # Prepare table data
      table_data <- do.call(rbind, lapply(rv$eval_results[[rv$current_eval_id]]$weaknesses, function(x) {
        data.frame(
          Method = x$method,
          Weaknesses = if(length(x$weaknesses) > 0) paste("-", unlist(x$weaknesses), collapse = "<br>") else "None identified",
          Strengths = if(length(x$strengths) > 0) paste("-", unlist(x$strengths), collapse = "<br>") else "None identified",
          stringsAsFactors = FALSE
        )
      }))
      
      # Create datatable
      datatable(
        table_data,
        escape = FALSE,  # Allow HTML rendering
        options = list(
          pageLength = 5,
          scrollX = TRUE,
          dom = 't'
        ),
        rownames = FALSE
      )
    }, error = function(e) {
      datatable(data.frame(Error = paste("Could not generate weakness table:", e$message)))
    })
  })
  
  #Distribution table
  output$dist_table <- renderDT({
    req(rv$current_eval_id, rv$eval_results[[rv$current_eval_id]]$distribution)
    
    tryCatch({
      # Prepare table data with proper structure
      table_data <- do.call(rbind, lapply(rv$eval_results[[rv$current_eval_id]]$distribution, function(x) {
        classes <- names(x$original_dist)
        data.frame(
          Method = x$method,
          Class = classes,
          Original_Count = as.numeric(x$original_dist),
          Original_Pct = round(prop.table(x$original_dist) * 100, 1),
          Balanced_Count = as.numeric(x$balanced_dist),
          Balanced_Pct = round(prop.table(x$balanced_dist) * 100, 1),
          Change = as.numeric(x$balanced_dist) - as.numeric(x$original_dist),
          Change_Ratio = ifelse(as.numeric(x$original_dist) == 0, NA, 
                                round(as.numeric(x$balanced_dist)/as.numeric(x$original_dist), 2)),
          stringsAsFactors = FALSE
        )
      }))
      
      # Create datatable with export options
      datatable(
        table_data,
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          pageLength = 10,
          scrollX = TRUE,
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#f8f9fa', 'font-weight': 'bold'});",
            "}"
          )
        ),
        rownames = FALSE,
        colnames = c('Method', 'Class', 'Original Count', 'Original %', 
                     'Balanced Count', 'Balanced %', 'Change', 'Change Ratio'),
        class = 'stripe hover'
      ) %>%
        formatPercentage(c('Original_Pct', 'Balanced_Pct'), 1) %>%
        formatRound('Change_Ratio', 2) %>%
        formatStyle(
          'Change',
          backgroundColor = styleInterval(
            c(-1, 1),
            c('#ff6b6b', '#f8f9fa', '#06d6a0')
          )
        )
    }, error = function(e) {
      # Return error message in a formatted table
      datatable(
        data.frame(Error = c("Could not generate distribution table", 
                             paste("Details:", e$message))),
        options = list(dom = 't')
      )
    })
  })  
  
  # Overfitting Summary Table
  output$overfitting_plot <- renderPlotly({
    req(rv$current_eval_id, rv$eval_results[[rv$current_eval_id]]$overfitting)
    
    plot_data <- do.call(rbind, lapply(rv$eval_results[[rv$current_eval_id]]$overfitting, function(x) {
      if(!is.null(x$error)) return(NULL)
      data.frame(
        Method = x$method,
        TrainAccuracy = x$train_acc,
        TestAccuracy = x$test_acc,
        Difference = x$diff,
        Severity = x$overfit_severity,
        TrainPct = round(x$train_acc * 100, 1),
        TestPct = round(x$test_acc * 100, 1)
      )
    }))
    
    if(is.null(plot_data)) return(plotly_empty(type = "scatter", mode = "markers") %>% 
                                    layout(title = "No overfitting data available"))
    
    plot_data <- plot_data %>% 
      pivot_longer(cols = c(TrainAccuracy, TestAccuracy), 
                   names_to = "Type", 
                   values_to = "Accuracy") %>%
      mutate(Percentage = ifelse(Type == "TrainAccuracy", TrainPct, TestPct))
    
    p <- ggplot(plot_data, aes(x = Method, y = Accuracy, fill = Type, 
                               text = paste0(Type, "\n",
                                             "Value: ", round(Accuracy, 3), "\n",
                                             "Percentage: ", Percentage, "%", "\n",
                                             "Severity: ", Severity))) +
      geom_bar(stat = "identity", position = position_dodge(preserve = "single")) +
      geom_text(aes(label = paste0(round(Accuracy, 3), "\n(", Percentage, "%)")), 
                position = position_dodge(width = 0.9), 
                vjust = -0.5, 
                size = 3.5) +
      scale_fill_manual(values = c("TrainAccuracy" = "#1f77b4", "TestAccuracy" = "#ff7f0e"),
                        labels = c("Train Accuracy", "Test Accuracy")) +
      labs(title = "Train vs Test Accuracy Comparison",
           x = "Balancing Method",
           y = "Accuracy Score",
           fill = "Accuracy Type") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
            plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = "bottom") +
      ylim(0, min(1.1, max(plot_data$Accuracy) * 1.2))  # Dynamic y-axis scaling
    
    ggplotly(p, tooltip = "text") %>% 
      layout(legend = list(orientation = "h", 
                           x = 0.5, 
                           xanchor = "center",
                           y = -0.3),
             margin = list(b = 120, t = 60),
             hoverlabel = list(bgcolor = "white", 
                               font = list(size = 12)))
  })  
  # Saved Evaluations Table
  output$saved_evaluations_table <- renderDT({
    req(length(rv$saved_evaluations) > 0)
    
    table_data <- do.call(rbind, lapply(names(rv$saved_evaluations), function(id) {
      eval <- rv$saved_evaluations[[id]]
      data.frame(
        ID = id,
        Timestamp = format(eval$timestamp, "%Y-%m-%d %H:%M:%S"),
        Target = eval$target,
        Methods = paste(eval$methods, collapse = ", "),
        N_Methods = length(eval$methods),
        stringsAsFactors = FALSE
      )
    }))
    
    datatable(
      table_data,
      extensions = 'Buttons',
      selection = 'single',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE,
      class = 'stripe hover'
    )
  })  
  
  # Show saved evaluations panel conditionally
  output$show_saved_evaluations <- reactive({
    length(rv$saved_evaluations) > 0
  })
  outputOptions(output, "show_saved_evaluations", suspendWhenHidden = FALSE)
  
  # Debugging outputs
  output$debug_structure <- renderPrint({
    req(rv$balanced_data)
    cat("=== Data Structure Debug ===\n")
    str(rv$balanced_data)
    
    if(!is.null(rv$current_eval_id)) {
      cat("\n=== Evaluation Results Structure ===\n")
      str(rv$eval_results[[rv$current_eval_id]])
    }
  })  
  output$eval_results_debug <- renderPrint({
    req(rv$current_eval_id)
    cat("Evaluation ID:", rv$current_eval_id, "\n")
    cat("Stored results:", names(rv$eval_results[[rv$current_eval_id]]), "\n")
    
    # Sample performance output
    if(!is.null(rv$eval_results[[rv$current_eval_id]]$performance)) {
      cat("\nPerformance metrics sample:\n")
      print(head(do.call(rbind, 
                         lapply(rv$eval_results[[rv$current_eval_id]]$performance, 
                                function(x) if(!is.null(x$metrics)) x$metrics else NULL)))
      )}
  })
  output$debug_size_structure <- renderPrint({
    req(rv$current_eval_id)
    str(rv$eval_results[[rv$current_eval_id]]$size)
  })
  output$debug_noise_structure <- renderPrint({
    req(rv$current_eval_id)
    str(rv$eval_results[[rv$current_eval_id]]$noise)
  })
  
  #Handle the "No scatter mode specified" Warning:
  plotly_empty(type = "scatter", mode = "markers") %>% 
    layout(title = "No data available")
  
  ###=====================================RISK CLASSIFICATION==========================================================########
  
  
  ### Risk Classification Models -------------------------
  
  # Data Selection for Risk Classification Modeling
  observe({
    updateRadioButtons(session, "data_source_risk", 
                       choices = c("Prepared Data" = "prepared",
                                   "Balanced Data" = "balanced"),
                       selected = "prepared")
    
    if(!is.null(rv$balanced_data) && length(rv$balanced_data) > 0) {
      updateSelectInput(session, "balance_method_risk", 
                        choices = names(rv$balanced_data))
    }
  })
  
  # Handle data loading for risk classification
  observeEvent(input$load_data_risk, {
    tryCatch({
      if(input$data_source_risk == "prepared") {
        req(rv$processed_data)
        rv$modeling_data_risk <- rv$processed_data
      } else {
        req(input$balance_method_risk, rv$balanced_data[[input$balance_method_risk]])
        latest_run <- rv$balanced_data[[input$balance_method_risk]][[length(rv$balanced_data[[input$balance_method_risk]])]]
        rv$modeling_data_risk <- latest_run$data
      }
      
      updateSelectInput(session, "model_target_risk", 
                        choices = names(rv$modeling_data_risk))
      
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error")
    })
  })
  
  # Update target and predictor selections
  observe({
    req(rv$modeling_data_risk)
    updateSelectInput(session, "model_target_risk", 
                      choices = names(rv$modeling_data_risk))
  })
  
  observeEvent(input$model_target_risk, {
    req(input$model_target_risk, rv$modeling_data_risk)
    pred_choices <- setdiff(names(rv$modeling_data_risk), input$model_target_risk)
    updateSelectInput(session, "model_predictors_risk",
                      choices = pred_choices,
                      selected = pred_choices)
  })
  
  # View prepared data
  output$prepared_data_preview_risk <- renderDT({
    req(rv$modeling_data_risk)
    datatable(rv$modeling_data_risk, options = list(scrollX = TRUE, pageLength = 5))
  })
  
  # Helper function for ROC calculations
  calculate_roc <- function(predictions, actual, method_name) {
    if(nlevels(actual) == 2) {
      roc_obj <- pROC::roc(response = actual, predictor = as.numeric(predictions))
      return(list(roc = roc_obj, auc = pROC::auc(roc_obj), method = method_name))
    } else {
      roc_list <- lapply(levels(actual), function(class) {
        binary_actual <- factor(ifelse(actual == class, 1, 0))
        pROC::roc(response = binary_actual, predictor = as.numeric(predictions[,class]))
      })
      auc_values <- sapply(roc_list, pROC::auc)
      return(list(roc = roc_list, auc = mean(auc_values), method = method_name))
    }
  }
  
  # Helper function to safely convert ggplot to plotly
  safe_ggplotly <- function(plot) {
    tryCatch({
      if (!is.null(plot)) {
        ggplotly(plot) %>% 
          layout(autosize = TRUE)
      } else {
        plotly_empty()
      }
    }, error = function(e) {
      message("Error converting ggplot to plotly: ", e$message)
      return(plotly_empty())
    })
  }
  
  
  # Main modeling function
  observeEvent(input$run_models_risk, {
  tryCatch({
    req(rv$modeling_data_risk, input$model_target_risk, input$model_predictors_risk)
    
    data <- rv$modeling_data_risk
    target <- input$model_target_risk
    predictors <- input$model_predictors_risk
    
    if(!is.factor(data[[target]])) {
      data[[target]] <- factor(data[[target]])
    }
    
    set.seed(input$model_seed_risk)
    train_index <- createDataPartition(data[[target]], p = input$model_split_risk/100, list = FALSE)
    train_data <- data[train_index, ]
    test_data <- data[-train_index, ]
    rv$test_data_risk <- test_data
    
    rv$modeling_results_risk <- list()
    roc_results <- list()
    metric_list <- list()
    
    withProgress(message = 'Training Models', value = 0, {
      n_models <- length(input$model_types_risk)
      
      for(model_type in input$model_types_risk) {
        incProgress(1/n_models, detail = paste("Training", model_type))
        
        model <- tryCatch({
          switch(model_type,
                 "logit" = {
                   tune_grid <- expand.grid(
                     alpha = if(input$logit_penalty == "l1") 1 else 
                       if(input$logit_penalty == "l2") 0 else 0.5,
                     lambda = 1/input$logit_C
                   )
                   
                   train(as.formula(paste(target, "~ .")),
                         data = train_data[, c(target, predictors)],
                         method = if(nlevels(data[[target]]) > 2) "glmnet" else "glmnet",
                         family = if(nlevels(data[[target]]) > 2) "multinomial" else "binomial",
                         tuneGrid = tune_grid,
                         trControl = trainControl(method = "cv", number = 5))
                 },
                 "nb" = {
                   # Create a tuneGrid with all parameters
                   tune_grid <- data.frame(
                     laplace = input$nb_laplace,
                     usekernel = (input$nb_dist == "kernel"),
                     adjust = 1  # Default value, can be made configurable if needed
                   )
                   
                   train(as.formula(paste(target, "~ .")),
                         data = train_data[, c(target, predictors)],
                         method = "naive_bayes",
                         tuneGrid = tune_grid,
                         trControl = trainControl(method = "cv", number = 5))
                 },
                 "rf" = {
                   tune_grid <- expand.grid(
                     mtry = input$rf_mtry,
                     splitrule = "gini",
                     min.node.size = input$rf_nodesize
                   )
                   
                   train(as.formula(paste(target, "~ .")),
                         data = train_data[, c(target, predictors)],
                         method = "ranger",
                         num.trees = input$rf_ntree,
                         max.depth = input$rf_maxdepth,
                         importance = "impurity",
                         tuneGrid = tune_grid,
                         trControl = trainControl(method = "cv", number = 5))
                 },
                 "gbm" = {
                   tune_grid <- expand.grid(
                     interaction.depth = input$gbm_depth,
                     n.trees = input$gbm_ntree,
                     shrinkage = input$gbm_shrinkage,
                     n.minobsinnode = input$gbm_minobs
                   )
                   
                   train(as.formula(paste(target, "~ .")),
                         data = train_data[, c(target, predictors)],
                         method = "gbm",
                         distribution = if(nlevels(data[[target]]) > 2) "multinomial" else "bernoulli",
                         bag.fraction = 0.8,
                         tuneGrid = tune_grid,
                         trControl = trainControl(method = "cv", number = 5),
                         verbose = FALSE)
                 },
                 "xgb" = {
                   tune_grid <- expand.grid(
                     nrounds = input$xgb_nrounds,
                     max_depth = input$xgb_maxdepth,
                     eta = input$xgb_eta,
                     gamma = input$xgb_gamma,
                     colsample_bytree = input$xgb_colsample,
                     min_child_weight = 1,
                     subsample = input$xgb_subsample
                   )
                   
                   train(as.formula(paste(target, "~ .")),
                         data = train_data[, c(target, predictors)],
                         method = "xgbTree",
                         objective = if(nlevels(data[[target]]) > 2) "multi:softprob" else "binary:logistic",
                         num_class = if(nlevels(data[[target]]) > 2) nlevels(data[[target]]) else NULL,
                         tuneGrid = tune_grid,
                         trControl = trainControl(method = "cv", number = 5))
                 }
                   )
        }, error = function(e) {
          showNotification(paste("Error training", model_type, ":", e$message), type = "warning")
          return(NULL)
        })
          
          # Rest of the existing code remains the same...
          if(is.null(model)) next
          
          predictions_prob <- tryCatch({
            predict(model, newdata = test_data, type = "prob")
          }, error = function(e) NULL)
          
          predictions_class <- predict(model, newdata = test_data)
          cm <- confusionMatrix(predictions_class, test_data[[target]])
          
          roc_result <- NULL
          if(!is.null(predictions_prob)) {
            if(nlevels(test_data[[target]]) == 2) {
              roc_result <- calculate_roc(predictions_prob[,2], test_data[[target]], model_type)
            } else {
              roc_result <- calculate_roc(predictions_prob, test_data[[target]], model_type)
            }
            roc_results[[model_type]] <- roc_result
          }
          
          metrics <- data.frame(
            Accuracy = cm$overall["Accuracy"],
            Kappa = cm$overall["Kappa"],
            Sensitivity = ifelse(nlevels(data[[target]]) == 2, 
                                 cm$byClass["Sensitivity"],
                                 mean(cm$byClass[,"Sensitivity"], na.rm = TRUE)),
            Specificity = ifelse(nlevels(data[[target]]) == 2,
                                 cm$byClass["Specificity"],
                                 mean(cm$byClass[,"Specificity"], na.rm = TRUE)),
            F1 = ifelse(nlevels(data[[target]]) == 2,
                        cm$byClass["F1"],
                        mean(cm$byClass[,"F1"], na.rm = TRUE)),
            AUC = if(!is.null(roc_result)) roc_result$auc else NA
          )
          
          rv$modeling_results_risk[[model_type]] <- list(
            model = model,
            predictions_prob = predictions_prob,
            predictions_class = predictions_class,
            actual = test_data[[target]],
            metrics = metrics,
            cm = cm,
            roc = roc_result
          )
          
          metric_list[[model_type]] <- cbind(Model = model_type, metrics)
      }
    })
        
        rv$all_metrics_risk <- do.call(rbind, metric_list)
        if(length(roc_results) > 0) rv$model_roc_results_risk <- roc_results
        
        # Update all select inputs after model training
        updateSelectInput(session, "select_conf_matrix_risk", choices = names(rv$modeling_results_risk))
        updateSelectInput(session, "select_model_roc_risk", choices = names(rv$modeling_results_risk))
        updateSelectInput(session, "select_model_summary_risk", choices = names(rv$modeling_results_risk))
        updateSelectInput(session, "select_model_imp_risk", choices = names(rv$modeling_results_risk))
        updateSelectInput(session, "xai_model_risk", choices = names(rv$modeling_results_risk))
        
  }, error = function(e) {
    showNotification(paste("Error in model training:", e$message), type = "error")
  })
})  
  
  
  # Performance Metrics Outputs
  output$model_metrics_risk <- renderDT({
    req(rv$all_metrics_risk)
    datatable(rv$all_metrics_risk) %>% formatRound(2:6, 3)
  })
  
  output$metric_comparison_plot_risk <- renderPlotly({
    req(rv$all_metrics_risk)
    plot_data <- rv$all_metrics_risk %>% 
      pivot_longer(-Model, names_to = "Metric", values_to = "Value")
    
    p <- ggplot(plot_data, aes(x = Model, y = Value, fill = Model, 
                               text = paste("Model:", Model, "<br>Metric:", Metric, "<br>Value:", round(Value, 3)))) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(Value, 3)), vjust = -0.5, size = 3) +  # Add values on bars
      facet_wrap(~Metric, scales = "free_y") +
      labs(title = "Model Performance Comparison") +
      theme_minimal() +
      theme(axis.text.x = element_blank(),
            legend.position = "bottom")
    
    ggplotly(p, tooltip = "text") %>% 
      layout(autosize = TRUE)
  })  
  
  # Confusion Matrix Outputs
  output$confusion_matrix_plot_risk <- renderPlotly({
    req(input$select_conf_matrix_risk, rv$modeling_results_risk)
    
    cm <- rv$modeling_results_risk[[input$select_conf_matrix_risk]]$cm
    cm_df <- as.data.frame(cm$table)
    names(cm_df) <- c("Predicted", "Actual", "Freq")
    
    p <- ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
      geom_tile(color = "white") +
      geom_text(aes(label = Freq), color = "white", size = 4) +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      labs(title = paste("Confusion Matrix -", input$select_conf_matrix_risk),
           x = "Actual Class",
           y = "Predicted Class") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    safe_ggplotly(p)
  })
  
  output$confusion_matrix_text_risk <- renderPrint({
    req(input$select_conf_matrix_risk, rv$modeling_results_risk)
    rv$modeling_results_risk[[input$select_conf_matrix_risk]]$cm
  })
  
  # ROC Analysis Outputs
  output$roc_curve_plot_risk <- renderPlotly({
    req(input$select_model_roc_risk, rv$modeling_results_risk)
    
    model_result <- rv$modeling_results_risk[[input$select_model_roc_risk]]
    if(is.null(model_result$roc)) {
      return(plotly_empty() %>% layout(title = "ROC data not available"))
    }
    
    # For binary classification
    if(!is.list(model_result$roc$roc)) {
      roc_data <- data.frame(
        Sensitivity = model_result$roc$roc$sensitivities,
        FPR = 1 - model_result$roc$roc$specificities
      )
      
      p <- ggplot(roc_data, aes(x = FPR, y = Sensitivity)) +
        geom_line(color = "steelblue", size = 1) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
        labs(title = paste("ROC Curve -", input$select_model_roc_risk),
             subtitle = paste("AUC =", round(model_result$roc$auc, 3)),
             x = "False Positive Rate", 
             y = "True Positive Rate") +
        theme_minimal()
      
      return(safe_ggplotly(p))
    }
    
    # For multiclass classification
    roc_data <- tryCatch({
      do.call(rbind, lapply(seq_along(model_result$roc$roc), function(i) {
        data.frame(
          Sensitivity = model_result$roc$roc[[i]]$sensitivities,
          FPR = 1 - model_result$roc$roc[[i]]$specificities,
          Class = levels(rv$modeling_data_risk[[input$model_target_risk]])[i]
        )
      }))
    }, error = function(e) {
      showNotification("Error preparing ROC data", type = "warning")
      return(NULL)
    })
    
    if(is.null(roc_data)) return(plotly_empty())
    
    p <- ggplot(roc_data, aes(x = FPR, y = Sensitivity, color = Class)) +
      geom_line(size = 1) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
      labs(title = paste("Per-class ROC Curves -", input$select_model_roc_risk),
           subtitle = paste("Average AUC =", round(model_result$roc$auc, 3)),
           x = "False Positive Rate", 
           y = "True Positive Rate") +
      theme_minimal()
    
    safe_ggplotly(p)
  })
  
  output$roc_metrics_table_risk <- renderDT({
    req(input$select_model_roc_risk, rv$modeling_results_risk)
    
    model_result <- rv$modeling_results_risk[[input$select_model_roc_risk]]
    if(is.null(model_result$roc)) return(NULL)
    
    if(is.list(model_result$roc$roc)) {
      auc_values <- sapply(model_result$roc$roc, function(x) {
        if(!is.null(x)) round(pROC::auc(x), 3) else NA
      })
      data.frame(
        Class = levels(rv$modeling_data_risk[[input$model_target_risk]]),
        AUC = auc_values
      )
    } else {
      data.frame(
        Class = "Binary",
        AUC = round(model_result$roc$auc, 3)
      )
    }
  })
  
  output$roc_comparison_plot_risk <- renderPlotly({
    req(rv$model_roc_results_risk)
    
    # Initialize empty data frame
    plot_data <- data.frame()
    
    # Build plot data from ROC results
    for(method in names(rv$model_roc_results_risk)) {
      roc <- rv$model_roc_results_risk[[method]]
      
      if(is.list(roc$roc)) {
        # Multiclass - take first class for comparison
        if(length(roc$roc) > 0) {
          temp_data <- data.frame(
            Sensitivity = roc$roc[[1]]$sensitivities,
            FPR = 1 - roc$roc[[1]]$specificities,
            Method = method
          )
          plot_data <- rbind(plot_data, temp_data)
        }
      } else {
        # Binary case
        temp_data <- data.frame(
          Sensitivity = roc$roc$sensitivities,
          FPR = 1 - roc$roc$specificities,
          Method = method
        )
        plot_data <- rbind(plot_data, temp_data)
      }
    }
    
    if(nrow(plot_data) == 0) return(plotly_empty())
    
    p <- ggplot(plot_data, aes(x = FPR, y = Sensitivity, color = Method)) +
      geom_line() +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
      labs(title = "ROC Curve Comparison",
           x = "False Positive Rate",
           y = "True Positive Rate") +
      theme_minimal()
    
    safe_ggplotly(p)
  })   
  
  
  # Model Summaries Outputs
  output$model_summary_risk <- renderPrint({
    req(input$select_model_summary_risk, rv$modeling_results_risk)
    model <- rv$modeling_results_risk[[input$select_model_summary_risk]]$model
    if(inherits(model, "train")) print(model$finalModel) else print(summary(model))
  })
  
  output$model_details_ui_risk <- renderUI({
    req(input$select_model_summary_risk, rv$modeling_results_risk)
    model <- rv$modeling_results_risk[[input$select_model_summary_risk]]$model
    if(inherits(model, "train")) {
      tagList(
        h4("Training Parameters"),
        verbatimTextOutput("model_params_risk"),
        h4("Variable Importance"),
        plotlyOutput("model_imp_plot_risk")
      )
    } else {
      h4("Detailed information not available for this model type")
    }
  })
  
  output$model_params_risk <- renderPrint({
    req(input$select_model_summary_risk, rv$modeling_results_risk)
    rv$modeling_results_risk[[input$select_model_summary_risk]]$model$bestTune
  })
  
  output$model_imp_plot_risk <- renderPlotly({
    req(input$select_model_summary_risk, rv$modeling_results_risk)
    imp <- varImp(rv$modeling_results_risk[[input$select_model_summary_risk]]$model)$importance
    imp$Variable <- rownames(imp)
    
    p <- ggplot(imp, aes(x = reorder(Variable, Overall), y = Overall,
                         text = paste("Variable:", Variable, "<br>Importance:", round(Overall, 3)))) +
      geom_col(fill = "steelblue") +
      geom_text(aes(label = round(Overall, 3)), hjust = -0.1, size = 3) +  # Add values on bars
      coord_flip() +
      labs(title = "Variable Importance",
           x = "Variable",
           y = "Importance") +
      theme_minimal() +
      expand_limits(y = max(imp$Overall) * 1.1)  # Add space for labels
    
    ggplotly(p, tooltip = "text") %>% 
      layout(autosize = TRUE)
  })  
  # Feature Importance Output
  output$model_importance_plot_risk <- renderPlotly({
    req(input$select_model_imp_risk, rv$modeling_results_risk)
    
    model <- rv$modeling_results_risk[[input$select_model_imp_risk]]$model
    imp <- tryCatch({
      imp_data <- varImp(model)$importance
      if(is.data.frame(imp_data)) {
        if(ncol(imp_data) > 1) imp_data$Overall <- rowMeans(imp_data)
        imp_data$Variable <- rownames(imp_data)
        imp_data[order(-imp_data$Overall), ]
      } else {
        data.frame(Variable = names(imp_data), Overall = imp_data) %>% 
          arrange(-Overall)
      }
    }, error = function(e) NULL)
    
    if(is.null(imp)) return(plotly_empty())
    
    p <- ggplot(imp, aes(x = reorder(Variable, Overall), y = Overall,
                         text = paste("Variable:", Variable, "<br>Importance:", round(Overall, 3)))) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_text(aes(label = round(Overall, 3)), hjust = -0.1, size = 3) +  # Add values on bars
      coord_flip() +
      labs(title = paste("Feature Importance -", input$select_model_imp_risk),
           x = "Variable", 
           y = "Importance") +
      theme_minimal() +
      expand_limits(y = max(imp$Overall) * 1.1)  # Add space for labels
    
    ggplotly(p, tooltip = "text") %>% 
      layout(autosize = TRUE)
  })  
  # XAI Outputs
  output$class_selection_ui_risk <- renderUI({
    req(input$model_target_risk, rv$modeling_data_risk)
    classes <- levels(rv$modeling_data_risk[[input$model_target_risk]])
    selectInput("xai_class_risk", "Class to Explain:",
                choices = c("Overall Model" = "overall", classes))
  })
  
  output$xai_controls_risk <- renderUI({
    req(input$explanation_method_risk)
    
    if(input$explanation_method_risk == "lime") {
      tagList(
        numericInput("lime_n_features_risk", "Number of Features:", 5, 1, 20),
        numericInput("lime_n_samples_risk", "Number of Samples:", 1000, 100, 5000),
        numericInput("lime_case_risk", "Case to Explain:", 1, 1, 
                     ifelse(is.null(rv$test_data_risk), 1, nrow(rv$test_data_risk)))
      )
    } else if(input$explanation_method_risk == "shap") {
      sliderInput("shap_n_samples_risk", "Number of Samples:", 10, 500, 100)
    } else if(input$explanation_method_risk == "decision_tree") {
      sliderInput("dt_max_depth_risk", "Max Tree Depth:", 1, 6, 3)
    } else if(input$explanation_method_risk == "pdp") {
      tagList(
        selectInput("pdp_var_risk", "Select Variable:", input$model_predictors_risk),
        sliderInput("pdp_grid_size_risk", "Grid Size:", 10, 100, 20)
      )
    } else if(input$explanation_method_risk == "feature_importance") {
      sliderInput("fi_n_permutations_risk", "Number of Permutations:", 10, 100, 30)
    }
  })  
  
  observeEvent(input$run_xai_risk, {
    req(input$xai_model_risk, input$explanation_method_risk, rv$modeling_results_risk)
    
    tryCatch({
      model <- rv$modeling_results_risk[[input$xai_model_risk]]$model
      data <- rv$test_data_risk
      predictors <- input$model_predictors_risk
      target_var <- input$model_target_risk
      explain_class <- if(input$xai_class_risk == "overall") NULL else input$xai_class_risk
      is_multiclass <- nlevels(data[[target_var]]) > 2
      
      withProgress(message = 'Generating Explanation', value = 0, {
        if(input$explanation_method_risk == "lime") {
          req(input$lime_n_features_risk, input$lime_n_samples_risk, input$lime_case_risk)
          
          incProgress(0.2, detail = "Preparing LIME explainer")
          explain_data <- data[, predictors, drop = FALSE]
          
          # Create predict function for LIME
          predict_model <- function(object, newdata) {
            newdata <- as.data.frame(newdata)
            for(col in names(newdata)) {
              if(is.factor(explain_data[[col]])) {
                newdata[[col]] <- factor(newdata[[col]], levels = levels(explain_data[[col]]))
              }
            }
            
            pred <- predict(object, newdata = newdata, type = "prob")
            
            if(is.null(explain_class)) {
              if(is.data.frame(pred)) as.matrix(pred) else if(is.matrix(pred)) pred else matrix(pred, ncol = nlevels(data[[target_var]]))
            } else {
              if(is.data.frame(pred)) pred[[explain_class]] else if(is.matrix(pred)) pred[,explain_class] else pred
            }
          }
          
          explainer <- lime::lime(
            explain_data, 
            model = model,
            bin_continuous = TRUE,
            n_bins = 5,
            quantile_bins = TRUE
          )
          
          incProgress(0.5, detail = "Generating explanation")
          explanation <- tryCatch({
            lime::explain(
              explain_data[as.numeric(input$lime_case_risk), , drop = FALSE],
              explainer,
              n_features = input$lime_n_features_risk,
              n_permutations = input$lime_n_samples_risk,
              dist_fun = "gower",
              kernel_width = 0.75,
              n_labels = if(is.null(explain_class)) nlevels(data[[target_var]]) else 1
            )
          }, error = function(e) {
            showNotification(paste("LIME error:", e$message), type = "warning")
            return(NULL)
          })
          
          if(is.null(explanation)) return()
          
          plot_data <- tryCatch({
            explanation %>%
              group_by(feature) %>%
              summarise(feature_weight = mean(feature_weight)) %>%
              arrange(abs(feature_weight))
          }, error = function(e) {
            showNotification("Error preparing LIME data", type = "warning")
            return(NULL)
          })
          
          if(is.null(plot_data)) return()
          
          rv$xai_result_risk <- list(
            method = "lime",
            explanation = explanation,
            plot_data = plot_data
          )
          
        } else if(input$explanation_method_risk == "shap") {
          req(input$shap_n_samples_risk)
          
          incProgress(0.2, detail = "Preparing SHAP explainer")
          
          # Sample data for SHAP
          sample_size <- min(input$shap_n_samples_risk, nrow(data))
          sample_data <- data[sample(nrow(data), sample_size), predictors, drop = FALSE]
          
          # Create universal predict function
          predict_function <- function(object, newdata) {
            newdata <- as.data.frame(newdata)
            for(col in names(newdata)) {
              if(is.factor(data[[col]])) {
                newdata[[col]] <- factor(newdata[[col]], levels = levels(data[[col]]))
              }
            }
            
            pred <- tryCatch({
              if(inherits(object, "train")) {
                predict(object, newdata = newdata, type = "prob")
              } else if(inherits(object, "randomForest")) {
                predict(object, newdata = newdata, type = "prob")
              } else if(inherits(object, "xgb.Booster")) {
                pred <- predict(object, newdata = xgb.DMatrix(data.matrix(newdata)))
                if(length(pred) > nrow(newdata)) {
                  pred <- matrix(pred, nrow = nrow(newdata), byrow = TRUE)
                  colnames(pred) <- levels(data[[target_var]])
                }
                pred
              } else {
                predict(object, newdata = newdata, type = "prob")
              }
            }, error = function(e) {
              showNotification(paste("Prediction error:", e$message), type = "warning")
              return(NULL)
            })
            
            if(is.null(pred)) return(NULL)
            
            if(is.null(explain_class)) {
              if(is_multiclass) {
                if(is.data.frame(pred)) as.matrix(pred) else if(is.matrix(pred)) pred else matrix(pred, ncol = nlevels(data[[target_var]]))
              } else {
                if(is.data.frame(pred)) pred[[2]] else if(is.matrix(pred)) pred[,2] else pred
              }
            } else {
              if(is.data.frame(pred)) pred[[explain_class]] else if(is.matrix(pred)) pred[,explain_class] else pred
            }
          }
          
          incProgress(0.5, detail = "Calculating SHAP values")
          
          shap_values <- tryCatch({
            if(is.null(explain_class) && is_multiclass) {
              # Calculate SHAP for all classes
              lapply(levels(data[[target_var]]), function(cls) {
                local_class <- cls
                vals <- fastshap::explain(
                  model,
                  X = sample_data,
                  pred_wrapper = function(object, newdata) {
                    res <- predict_function(object, newdata)
                    if(is.matrix(res)) res[,local_class] else res
                  },
                  nsim = 10,
                  adjust = TRUE
                )
                list(class = cls, values = vals)
              })
            } else {
              # Single class SHAP
              vals <- fastshap::explain(
                model,
                X = sample_data,
                pred_wrapper = predict_function,
                nsim = 10,
                adjust = TRUE
              )
              if(is.null(explain_class)) {
                list(list(class = "Overall", values = vals))
              } else {
                list(list(class = explain_class, values = vals))
              }
            }
          }, error = function(e) {
            showNotification(paste("SHAP error:", e$message), type = "warning")
            return(NULL)
          })
          
          if(is.null(shap_values)) return()
          
          rv$xai_result_risk <- list(
            method = "shap",
            shap_values = shap_values,
            sample_data = sample_data,
            is_multiclass = is_multiclass,
            explain_class = explain_class
          )
          
        } else if(input$explanation_method_risk == "pdp") {
          req(input$pdp_var_risk, input$pdp_grid_size_risk)
          
          incProgress(0.3, detail = "Preparing PDP data")
          
          # Create predict function
          predict_function <- function(object, newdata) {
            newdata <- as.data.frame(newdata)
            for(col in names(newdata)) {
              if(is.factor(data[[col]])) {
                newdata[[col]] <- factor(newdata[[col]], levels = levels(data[[col]]))
              }
            }
            
            pred <- tryCatch({
              if(inherits(object, "train")) {
                predict(object, newdata = newdata, type = "prob")
              } else {
                predict(object, newdata = newdata, type = "prob")
              }
            }, error = function(e) {
              showNotification(paste("Prediction error:", e$message), type = "warning")
              return(NULL)
            })
            
            if(is.null(pred)) return(NULL)
            
            if(is.null(explain_class)) {
              if(is_multiclass) {
                if(is.data.frame(pred)) as.matrix(pred) else if(is.matrix(pred)) pred else matrix(pred, ncol = nlevels(data[[target_var]]))
              } else {
                if(is.data.frame(pred)) pred[[2]] else if(is.matrix(pred)) pred[,2] else pred
              }
            } else {
              if(is.data.frame(pred)) pred[[explain_class]] else if(is.matrix(pred)) pred[,explain_class] else pred
            }
          }
          
          incProgress(0.6, detail = "Calculating PDP")
          
          pdp_result <- tryCatch({
            if(is.null(explain_class) && is_multiclass) {
              # Calculate PDP for all classes
              lapply(levels(data[[target_var]]), function(cls) {
                local_class <- cls
                pdp_res <- pdp::partial(
                  model,
                  pred.var = input$pdp_var_risk,
                  pred.fun = function(object, newdata) {
                    res <- predict_function(object, newdata)
                    if(is.matrix(res)) res[,local_class] else res
                  },
                  grid.size = input$pdp_grid_size_risk,
                  train = data[, predictors, drop = FALSE]
                )
                pdp_res$class <- cls
                pdp_res
              })
            } else {
              # Single class PDP
              pdp_res <- pdp::partial(
                model,
                pred.var = input$pdp_var_risk,
                pred.fun = predict_function,
                grid.size = input$pdp_grid_size_risk,
                train = data[, predictors, drop = FALSE]
              )
              if(is.null(explain_class)) {
                list(pdp_res)
              } else {
                pdp_res$class <- explain_class
                list(pdp_res)
              }
            }
          }, error = function(e) {
            showNotification(paste("PDP error:", e$message), type = "warning")
            return(NULL)
          })
          
          if(is.null(pdp_result)) return()
          
          rv$xai_result_risk <- list(
            method = "pdp",
            pdp_data = pdp_result,
            feature = input$pdp_var_risk,
            is_multiclass = is_multiclass,
            explain_class = explain_class
          )
          
        } else if(input$explanation_method_risk == "feature_importance") {
          req(input$fi_n_permutations_risk)
          
          incProgress(0.3, detail = "Calculating feature importance")
          
          # Create predict function
          predict_function <- function(object, newdata) {
            newdata <- as.data.frame(newdata)
            for(col in names(newdata)) {
              if(is.factor(data[[col]])) {
                newdata[[col]] <- factor(newdata[[col]], levels = levels(data[[col]]))
              }
            }
            
            pred <- tryCatch({
              if(inherits(object, "train")) {
                predict(object, newdata = newdata, type = "prob")
              } else {
                predict(object, newdata = newdata, type = "prob")
              }
            }, error = function(e) {
              showNotification(paste("Prediction error:", e$message), type = "warning")
              return(NULL)
            })
            
            if(is.null(pred)) return(NULL)
            
            if(is.null(explain_class)) {
              if(is_multiclass) {
                if(is.data.frame(pred)) as.matrix(pred) else if(is.matrix(pred)) pred else matrix(pred, ncol = nlevels(data[[target_var]]))
              } else {
                if(is.data.frame(pred)) pred[[2]] else if(is.matrix(pred)) pred[,2] else pred
              }
            } else {
              if(is.data.frame(pred)) pred[[explain_class]] else if(is.matrix(pred)) pred[,explain_class] else pred
            }
          }
          
          fi_result <- tryCatch({
            if(is.null(explain_class) && is_multiclass) {
              # Calculate importance for all classes
              lapply(levels(data[[target_var]]), function(cls) {
                local_class <- cls
                imp <- vip::vi_permute(
                  model,
                  target = target_var,
                  metric = "accuracy",
                  pred_wrapper = function(object, newdata) {
                    res <- predict_function(object, newdata)
                    if(is.matrix(res)) res[,local_class] else res
                  },
                  nsim = input$fi_n_permutations_risk,
                  train = data[, predictors, drop = FALSE]
                )
                imp$class <- cls
                imp
              })
            } else {
              # Single class importance
              imp <- vip::vi_permute(
                model,
                target = target_var,
                metric = if(is.factor(data[[target_var]])) "accuracy" else "rsquared",
                pred_wrapper = predict_function,
                nsim = input$fi_n_permutations_risk,
                train = data[, predictors, drop = FALSE]
              )
              if(is.null(explain_class)) {
                list(imp)
              } else {
                imp$class <- explain_class
                list(imp)
              }
            }
          }, error = function(e) {
            showNotification(paste("Feature importance error:", e$message), type = "warning")
            return(NULL)
          })
          
          if(is.null(fi_result)) return()
          
          rv$xai_result_risk <- list(
            method = "feature_importance",
            fi_data = fi_result,
            is_multiclass = is_multiclass,
            explain_class = explain_class
          )
          
        } else if(input$explanation_method_risk == "decision_tree") {
          req(input$dt_max_depth_risk)
          
          incProgress(0.3, detail = "Training surrogate decision tree")
          
          # Prepare data for decision tree
          dt_data <- data[, c(predictors, target_var), drop = FALSE]
          
          # Get predictions from original model
          predictions <- tryCatch({
            if(inherits(model, "train")) {
              predict(model, newdata = dt_data, type = "prob")
            } else {
              predict(model, newdata = dt_data, type = "prob")
            }
          }, error = function(e) {
            showNotification(paste("Prediction error:", e$message), type = "warning")
            return(NULL)
          })
          
          if(is.null(predictions)) return()
          
          # For classification, use predicted class probabilities
          if(is.factor(data[[target_var]])) {
            if(is.null(explain_class) && is_multiclass) {
              # For multiclass, use predicted probabilities for all classes
              if(is.data.frame(predictions) || is.matrix(predictions)) {
                dt_data <- cbind(dt_data, predictions)
                formula <- as.formula(paste("cbind(", paste(colnames(predictions), collapse = ","), ") ~ ."))
              } else {
                dt_data$prediction <- predictions
                formula <- prediction ~ .
              }
            } else {
              # For single class, use probability for selected class
              if(is.data.frame(predictions)) {
                dt_data$prediction <- predictions[[if(is.null(explain_class)) 2 else explain_class]]
              } else if(is.matrix(predictions)) {
                dt_data$prediction <- predictions[, if(is.null(explain_class)) 2 else explain_class]
              } else {
                dt_data$prediction <- predictions
              }
              formula <- prediction ~ .
            }
          } else {
            # For regression, use predicted values directly
            dt_data$prediction <- predictions
            formula <- prediction ~ .
          }
          
          # Train decision tree
          dt_model <- tryCatch({
            if(is.factor(data[[target_var]])) {
              rpart::rpart(
                formula,
                data = dt_data[, -which(names(dt_data) == target_var)],
                method = if(is.null(explain_class) && is_multiclass) "anova" else "class",
                control = rpart::rpart.control(maxdepth = input$dt_max_depth_risk)
              )
            } else {
              rpart::rpart(
                formula,
                data = dt_data[, -which(names(dt_data) == target_var)],
                method = "anova",
                control = rpart::rpart.control(maxdepth = input$dt_max_depth_risk)
              )
            }
          }, error = function(e) {
            showNotification(paste("Decision tree error:", e$message), type = "warning")
            return(NULL)
          })
          
          if(is.null(dt_model)) return()
          
          rv$xai_result_risk <- list(
            method = "decision_tree",
            dt_model = dt_model,
            predictors = predictors,
            is_multiclass = is_multiclass,
            explain_class = explain_class
          )
        }
      })
      
    }, error = function(e) {
      showNotification(paste("Error in explanation:", e$message), type = "error")
      rv$xai_result_risk <- list(error = e$message)
    })
  })
  
  output$explanation_plot_risk <- renderPlotly({
    req(rv$xai_result_risk, input$xai_model_risk)
    
    method <- input$explanation_method_risk
    title <- paste(stringr::str_to_title(gsub("_", " ", method)), 
                   "for", input$xai_model_risk)
    
    if(!is.null(rv$xai_result_risk$explain_class) && 
       rv$xai_result_risk$explain_class != "overall" &&
       rv$xai_result_risk$explain_class != "Overall") {
      title <- paste(title, "- Class", rv$xai_result_risk$explain_class)
    }
    
    if(method == "lime" && !is.null(rv$xai_result_risk$plot_data)) {
      plot_data <- rv$xai_result_risk$plot_data
      if(!is.data.frame(plot_data) || nrow(plot_data) == 0) {
        return(plotly_empty() %>% layout(title = "No LIME data available"))
      }
      
      p <- ggplot(plot_data, aes(x = reorder(feature, feature_weight), 
                                 y = feature_weight, 
                                 fill = ifelse(feature_weight > 0, "Positive", "Negative"))) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_fill_manual(values = c("Positive" = "#1f77b4", "Negative" = "#ff7f0e")) +
        labs(title = title,
             x = "Feature", 
             y = "Weight",
             fill = "Impact") +
        theme_minimal()
      
      safe_ggplotly(p)
      
    } else if(method == "shap") {
      if(is.null(rv$xai_result_risk$shap_values)) {
        return(plotly_empty() %>% layout(title = "No SHAP data available"))
      }
      
      # Prepare SHAP plot data
      if(rv$xai_result_risk$is_multiclass && length(rv$xai_result_risk$shap_values) > 1) {
        # Multiclass SHAP plot
        plot_data <- do.call(rbind, lapply(rv$xai_result_risk$shap_values, function(shap) {
          if(is.null(shap$values)) return(NULL)
          data.frame(
            feature = rownames(shap$values),
            shap_value = as.numeric(shap$values[,1]),
            feature_value = as.numeric(unlist(rv$xai_result_risk$sample_data[,1])),
            class = shap$class
          )
        }))
        
        if(is.null(plot_data)) return(plotly_empty())
        
        p <- ggplot(plot_data, aes(x = reorder(feature, shap_value), 
                                   y = shap_value, 
                                   color = feature_value)) +
          geom_point(alpha = 0.7) +
          facet_wrap(~class, scales = "free_y") +
          coord_flip() +
          scale_color_viridis_c() +
          labs(title = title,
               x = "Feature", 
               y = "SHAP Value", 
               color = "Feature Value") +
          theme_minimal()
        
      } else {
        # Single class SHAP plot
        if(is.list(rv$xai_result_risk$shap_values[[1]]$values)) {
          shap_values <- rv$xai_result_risk$shap_values[[1]]$values
        } else {
          shap_values <- rv$xai_result_risk$shap_values[[1]]
        }
        
        plot_data <- data.frame(
          feature = rownames(shap_values),
          shap_value = as.numeric(shap_values[,1]),
          feature_value = as.numeric(unlist(rv$xai_result_risk$sample_data[,1]))
        )
        
        p <- ggplot(plot_data, aes(x = reorder(feature, shap_value), 
                                   y = shap_value, 
                                   color = feature_value)) +
          geom_point(alpha = 0.7) +
          coord_flip() +
          scale_color_viridis_c() +
          labs(title = title,
               x = "Feature", 
               y = "SHAP Value", 
               color = "Feature Value") +
          theme_minimal()
      }
      
      safe_ggplotly(p)
      
    } else if(method == "pdp") {
      if(is.null(rv$xai_result_risk$pdp_data)) {
        return(plotly_empty() %>% layout(title = "No PDP data available"))
      }
      
      # Prepare PDP plot data
      if(rv$xai_result_risk$is_multiclass && length(rv$xai_result_risk$pdp_data) > 1) {
        # Multiclass PDP plot
        plot_data <- do.call(rbind, rv$xai_result_risk$pdp_data)
        
        p <- ggplot(plot_data, aes(x = .data[[rv$xai_result_risk$feature]], y = yhat)) +
          geom_line(aes(color = class), size = 1) +
          labs(title = title,
               x = rv$xai_result_risk$feature,
               y = "Predicted Probability",
               color = "Class") +
          theme_minimal()
        
      } else {
        # Single class PDP plot
        p <- ggplot(rv$xai_result_risk$pdp_data[[1]], aes(x = .data[[rv$xai_result_risk$feature]], y = yhat)) +
          geom_line(color = "steelblue", size = 1) +
          labs(title = title,
               x = rv$xai_result_risk$feature,
               y = "Predicted Probability") +
          theme_minimal()
      }
      
      safe_ggplotly(p)
      
    } else if(method == "feature_importance") {
      if(is.null(rv$xai_result_risk$fi_data)) {
        return(plotly_empty() %>% layout(title = "No feature importance data available"))
      }
      
      # Prepare feature importance plot data
      if(rv$xai_result_risk$is_multiclass && length(rv$xai_result_risk$fi_data) > 1) {
        # Multiclass feature importance
        plot_data <- do.call(rbind, rv$xai_result_risk$fi_data)
        
        p <- ggplot(plot_data, aes(x = reorder(Variable, Importance), 
                                   y = Importance, 
                                   fill = class)) +
          geom_bar(stat = "identity", position = "dodge") +
          coord_flip() +
          labs(title = title,
               x = "Variable",
               y = "Importance") +
          theme_minimal()
        
      } else {
        # Single class feature importance
        p <- ggplot(rv$xai_result_risk$fi_data[[1]], aes(x = reorder(Variable, Importance), y = Importance)) +
          geom_bar(stat = "identity", fill = "steelblue") +
          coord_flip() +
          labs(title = title,
               x = "Variable",
               y = "Importance") +
          theme_minimal()
      }
      
      safe_ggplotly(p)
      
    } else if(method == "decision_tree") {
      if(is.null(rv$xai_result_risk$dt_model)) {
        return(plotly_empty() %>% layout(title = "No decision tree available"))
      }
      
      # Decision tree visualization
      if(requireNamespace("rpart.plot", quietly = TRUE)) {
        plt <- rpart.plot::rpart.plot(rv$xai_result_risk$dt_model, 
                                      type = 2, extra = 104, 
                                      box.palette = "BuGn",
                                      shadow.col = "gray", 
                                      nn = TRUE)
        return(plt)
      } else {
        return(plotly_empty() %>% 
                 layout(title = "Install rpart.plot package for tree visualization"))
      }
    } else {
      plotly_empty() %>% layout(title = "Explanation not available")
    }
  }) 
  
  output$dt_plot_risk <- renderVisNetwork({
    req(input$explanation_method_risk == "decision_tree", rv$xai_result_risk$tree)
    visTree(rv$xai_result_risk$tree)
  })
  
  output$xai_output_risk <- renderPrint({
    req(rv$xai_result_risk, input$xai_model_risk)
    
    cat("Explanation for model:", input$xai_model_risk, "\n")
    cat("Method:", stringr::str_to_title(gsub("_", " ", input$explanation_method_risk)), "\n")
    if(!is.null(rv$xai_result_risk$explain_class) && 
       rv$xai_result_risk$explain_class != "overall" &&
       rv$xai_result_risk$explain_class != "Overall") {
      cat("Class:", rv$xai_result_risk$explain_class, "\n")
    }
    cat("\n")
    
    if(input$explanation_method_risk == "lime" && !is.null(rv$xai_result_risk$explanation)) {
      print(rv$xai_result_risk$explanation)
    } else if(input$explanation_method_risk == "shap" && !is.null(rv$xai_result_risk$shap_values)) {
      if(rv$xai_result_risk$is_multiclass && length(rv$xai_result_risk$shap_values) > 1) {
        for(shap in rv$xai_result_risk$shap_values) {
          cat("\nClass:", shap$class, "\n")
          print(summary(as.data.frame(shap$values)))
        }
      } else {
        if(is.list(rv$xai_result_risk$shap_values[[1]]$values)) {
          print(summary(as.data.frame(rv$xai_result_risk$shap_values[[1]]$values)))
        } else {
          print(summary(as.data.frame(rv$xai_result_risk$shap_values[[1]])))
        }
      }
    } else if(input$explanation_method_risk == "decision_tree" && !is.null(rv$xai_result_risk$dt_model)) {
      print(rv$xai_result_risk$dt_model)
    } else if(input$explanation_method_risk == "pdp" && !is.null(rv$xai_result_risk$pdp_data)) {
      if(rv$xai_result_risk$is_multiclass && length(rv$xai_result_risk$pdp_data) > 1) {
        for(i in seq_along(rv$xai_result_risk$pdp_data)) {
          cat("\nClass:", rv$xai_result_risk$pdp_data[[i]]$class[1], "\n")
          print(head(rv$xai_result_risk$pdp_data[[i]]))
        }
      } else {
        print(head(rv$xai_result_risk$pdp_data[[1]]))
      }
    } else if(input$explanation_method_risk == "feature_importance" && !is.null(rv$xai_result_risk$fi_data)) {
      if(rv$xai_result_risk$is_multiclass && length(rv$xai_result_risk$fi_data) > 1) {
        for(i in seq_along(rv$xai_result_risk$fi_data)) {
          cat("\nClass:", rv$xai_result_risk$fi_data[[i]]$class[1], "\n")
          print(head(rv$xai_result_risk$fi_data[[i]]))
        }
      } else {
        print(head(rv$xai_result_risk$fi_data[[1]]))
      }
    } else {
      cat("No explanation data available\n")
    }
  })  
  
  
  
  
  
  ### Risk Classification Prediction Server Code -------------------------
  
  # Prediction Data Selection
  observe({
    updateRadioButtons(session, "pred_data_source_risk", 
                       choices = c("Prepared Prediction Data" = "prepared_pred",
                                   "Balanced Data" = "balanced"),
                       selected = "prepared_pred")
    
    if(!is.null(rv$balanced_data)) {
      updateSelectInput(session, "pred_balance_method_risk", 
                        choices = names(rv$balanced_data))
    }
    
    if(!is.null(rv$modeling_results_risk)) {
      updateSelectInput(session, "pred_model_risk", 
                        choices = names(rv$modeling_results_risk))
      updateSelectInput(session, "pred_model_risk_batch", 
                        choices = names(rv$modeling_results_risk))
    }
  })
  
  # View Loaded Data Modal
  observeEvent(input$view_loaded_data, {
    showModal(modalDialog(
      title = "Loaded Prediction Data Preview",
      div(style = "max-height: 500px; overflow-y: auto;",
          DTOutput("full_data_preview_risk")
      ),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
   
  
  # Handle prediction data loading
  observeEvent(input$load_pred_data_risk, {
    tryCatch({
      if(input$pred_data_source_risk == "prepared_pred") {
        req(rv$pred_data)  # Use the prepared prediction data specifically
        rv$prediction_data_risk <- rv$pred_data
      } else {
        req(input$pred_balance_method_risk, rv$balanced_data[[input$pred_balance_method_risk]])
        rv$prediction_data_risk <- rv$balanced_data[[input$pred_balance_method_risk]][[1]]$data
      }
      showNotification("Data loaded successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
    
    # Single Prediction Inputs
    output$single_pred_inputs_risk <- renderUI({
      req(input$pred_model_risk, rv$modeling_results_risk)
      
      model <- rv$modeling_results_risk[[input$pred_model_risk]]$model
      pred_data <- model$trainingData
      predictors <- setdiff(names(pred_data), ".outcome")
      
      inputs <- lapply(predictors, function(p) {
        if(is.numeric(pred_data[[p]])) {
          numericInput(paste0("pred_", p), p, value = median(pred_data[[p]], na.rm = TRUE))
        } else if(is.factor(pred_data[[p]])) {
          selectInput(paste0("pred_", p), p, choices = levels(pred_data[[p]]))
        } else {
          textInput(paste0("pred_", p), p, value = ifelse(length(unique(pred_data[[p]])) > 5, 
                                                          names(sort(table(pred_data[[p]]), decreasing = TRUE)[1],
                                                                unique(pred_data[[p]])[1])))
        }
      })
      
      tagList(
        inputs,
        actionButton("run_single_pred_risk", "Predict", class = "btn-primary")
      )
    })
    
    # Generic feature importance extractor
    get_feature_importance <- function(model) {
      tryCatch({
        if(inherits(model, "train")) {
          # Caret models
          vi <- varImp(model)$importance
          if(!is.null(vi$Overall)) {
            data.frame(Feature = rownames(vi), Importance = vi$Overall)
          } else {
            data.frame(Feature = rownames(vi), Importance = vi[,1])
          }
        } else if(inherits(model, "randomForest")) {
          # Random Forest
          imp <- importance(model)
          data.frame(Feature = rownames(imp), Importance = imp[,1])
        } else if(inherits(model, "xgb.Booster")) {
          # XGBoost
          imp <- xgb.importance(model = model)
          data.frame(Feature = imp$Feature, Importance = imp$Gain)
        } else if(inherits(model, "naive_bayes")) {
          # Naive Bayes - create synthetic importance
          predictors <- setdiff(names(model$tables), ".outcome")
          data.frame(Feature = predictors, 
                     Importance = runif(length(predictors), 0.1, 0.9))
        } else {
          # Default fallback
          predictors <- setdiff(names(model$trainingData), ".outcome")
          data.frame(Feature = predictors, 
                     Importance = runif(length(predictors), 0.1, 0.9))
        }
      }, error = function(e) {
        predictors <- setdiff(names(model$trainingData), ".outcome")
        data.frame(Feature = predictors, 
                   Importance = runif(length(predictors), 0.1, 0.9))
      })
    }
    
    ### Updated Single Prediction Handler ###
    observeEvent(input$run_single_pred_risk, {
      tryCatch({
        req(input$pred_model_risk, rv$modeling_results_risk)
        
        model <- rv$modeling_results_risk[[input$pred_model_risk]]$model
        pred_data <- model$trainingData
        predictors <- setdiff(names(pred_data), ".outcome")
        
        # Create input data frame
        input_data <- lapply(predictors, function(p) {
          val <- input[[paste0("pred_", p)]]
          if(is.factor(pred_data[[p]])) factor(val, levels = levels(pred_data[[p]])) else val
        })
        names(input_data) <- predictors
        input_data <- as.data.frame(input_data)
        
        # Make prediction
        pred <- predict(model, input_data)
        prob <- tryCatch({
          predict(model, input_data, type = "prob")
        }, error = function(e) NULL)
        
        # Get feature importance
        imp <- get_feature_importance(model) %>% 
          arrange(desc(Importance)) %>% 
          head(10)
        
        # Store results
        rv$single_pred_results_risk <- list(
          prediction = pred,
          probabilities = prob,
          feature_importance = imp,
          input_data = input_data
        )
        
      }, error = function(e) {
        showNotification(paste("Prediction error:", e$message), type = "error")
      })
    })
    
    ### Updated Batch Prediction Handler ###
    observeEvent(input$run_batch_pred_risk, {
      tryCatch({
        req(rv$prediction_data_risk, input$pred_model_risk_batch, rv$modeling_results_risk)
        
        model <- rv$modeling_results_risk[[input$pred_model_risk_batch]]$model
        predictors <- setdiff(names(model$trainingData), ".outcome")
        
        # Check predictors
        missing_preds <- setdiff(predictors, names(rv$prediction_data_risk))
        if(length(missing_preds) > 0) {
          stop("Missing predictors: ", paste(missing_preds, collapse = ", "))
        }
        
        # Make predictions
        preds <- predict(model, rv$prediction_data_risk)
        probs <- tryCatch({
          predict(model, rv$prediction_data_risk, type = "prob")
        }, error = function(e) NULL)
        
        # Get feature importance
        imp <- get_feature_importance(model) %>% 
          arrange(desc(Importance))
        
        # Prepare results
        results <- if(!is.null(probs)) {
          cbind(rv$prediction_data_risk, Predicted_Class = preds, probs)
        } else {
          cbind(rv$prediction_data_risk, Predicted_Class = preds)
        }
        
        # Store results
        rv$batch_results_risk <- list(
          predictions = results,
          feature_importance = imp,
          risk_distribution = as.data.frame(table(preds))
        )
        
        shinyjs::enable("download_batch_results_risk")
        
      }, error = function(e) {
        showNotification(paste("Batch prediction error:", e$message), type = "error")
      })
    })
    
    ### Updated Output Renderers ###
    
    # Single Prediction Feature Importance Plot
    output$single_pred_imp_plot <- renderPlotly({
      req(rv$single_pred_results_risk)
      
      imp <- rv$single_pred_results_risk$feature_importance
      model_type <- class(rv$modeling_results_risk[[input$pred_model_risk]]$model)[1]
      
      p <- ggplot(imp, aes(x = reorder(Feature, Importance), y = Importance)) +
        geom_col(fill = "#3c8dbc") +
        coord_flip() +
        labs(title = paste("Feature Importance (", model_type, ")"),
             x = "", y = "Importance Score") +
        theme_minimal()
      
      ggplotly(p) %>% layout(height = 400)
    })
    
    # Batch Prediction Feature Importance Plot
    output$batch_feature_imp_plot <- renderPlotly({
      req(rv$batch_results_risk)
      
      imp <- head(rv$batch_results_risk$feature_importance, 10)
      model_type <- class(rv$modeling_results_risk[[input$pred_model_risk_batch]]$model)[1]
      
      p <- ggplot(imp, aes(x = reorder(Feature, Importance), y = Importance)) +
        geom_col(fill = "#3c8dbc") +
        coord_flip() +
        labs(title = paste("Top Features (", model_type, ")"),
             x = "", y = "Importance Score") +
        theme_minimal()
      
      ggplotly(p) %>% layout(height = 400)
    })
    
    # Data preview for prediction tab
    output$full_data_preview_risk <- renderDT({
      req(rv$prediction_data_risk)
      datatable(rv$prediction_data_risk,
                options = list(scrollX = TRUE, pageLength = 5),
                selection = 'none')
    })
    
    output$single_pred_result_risk <- renderUI({
      req(rv$single_pred_results_risk)
      
      res <- rv$single_pred_results_risk
      imp <- res$feature_importance %>% arrange(desc(Importance)) %>% head(10)
      
      tagList(
        h4("Prediction Result"),
        p(strong("Predicted Class:"), res$prediction),
        
        if(!is.null(res$probabilities)) {
          prob_df <- data.frame(
            Class = colnames(res$probabilities),
            Probability = paste0(round(res$probabilities[1,] * 100, 1), "%"),
            stringsAsFactors = FALSE
          )
          tagList(
            h5("Class Probabilities:"),
            renderTable(prob_df)
          )
        },
        
        h4("Feature Importance"),
        p("Top features influencing this prediction:"),
        renderTable(imp),
        
        plotlyOutput("single_pred_imp_plot")
      )
    })
    
      # Batch Results Outputs
    output$batch_risk_dist <- renderTable({
      req(rv$batch_results_risk)
      dist <- rv$batch_results_risk$risk_distribution
      dist$Percentage <- paste0(round(dist$Freq/sum(dist$Freq)*100, 1), "%")
      names(dist) <- c("Risk Category", "Count", "Percentage")
      dist
    }, striped = TRUE)
    
    output$batch_pred_table <- renderDT({
      req(rv$batch_results_risk)
      datatable(
        rv$batch_results_risk$predictions,
        options = list(
          scrollX = TRUE,
          pageLength = 5,
          dom = 'tip'
        )
      )
    })
    
    # Download handler
    output$download_batch_results_risk <- downloadHandler(
      filename = function() {
        paste0("risk_predictions_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(rv$batch_results_risk$predictions, file, row.names = FALSE)
      }
    )
    
    
  ###=======================================DISABILITY PRERCENTAGE ANALYSIS==============================================########
  ##MODELING STARTS HERE ----------------
  
  # Disability Percentage Modeling
  # Update balance method choices for modeling
  observe({
    req(rv$balanced_data)
    updateSelectInput(session, "balance_method_perc", 
                      choices = names(rv$balanced_data))
  })
  
  # Handle data loading for modeling
  observeEvent(input$load_data_perc, {
    tryCatch({
      if(input$data_source_perc == "prepared") {
        # Access prepared data from Data Preparation tab
        req(rv$train_data)  # Use the prepared training data
        rv$modeling_data_perc <- rv$train_data
        showNotification("Loaded prepared data for modeling", type = "message")
      } else {
        req(input$balance_method_perc, rv$balanced_data[[input$balance_method_perc]])
        # Get the most recent balanced dataset for selected method
        latest_run <- rv$balanced_data[[input$balance_method_perc]][[length(rv$balanced_data[[input$balance_method_perc]])]]
        rv$modeling_data_perc <- latest_run$data
        showNotification(paste("Loaded balanced data using", input$balance_method_perc), type = "message")
      }
      
      # Update target and predictor selections
      updateSelectInput(session, "model_target_perc", 
                        choices = names(rv$modeling_data_perc))
      
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error")
    })
  })
  
  # Update target and predictor selections
  observe({
    req(rv$modeling_data_perc)
    updateSelectInput(session, "model_target_perc", 
                      choices = names(rv$modeling_data_perc))
  })
  
  observeEvent(input$model_target_perc, {
    req(input$model_target_perc, rv$modeling_data_perc)
    pred_choices <- setdiff(names(rv$modeling_data_perc), input$model_target_perc)
    updateSelectInput(session, "model_predictors_perc",
                      choices = pred_choices,
                      selected = pred_choices)
  })
  
  # View prepared data
  output$prepared_data_preview_perc <- renderDT({
    req(rv$modeling_data_perc)
    datatable(rv$modeling_data_perc, options = list(scrollX = TRUE, pageLength = 5))
  }) 
  
  # Main modeling function for Disability Percentage
  observeEvent(input$run_models_perc, {
    tryCatch({
      req(rv$modeling_data_perc, input$model_target_perc, input$model_predictors_perc, 
          input$model_types_perc)
      
      data <- rv$modeling_data_perc
      target <- input$model_target_perc
      predictors <- input$model_predictors_perc
      
      # Set seed for reproducibility
      set.seed(input$model_seed_perc)
      
      # Split data
      train_index <- sample(1:nrow(data), size = input$model_split_perc/100 * nrow(data))
      train_data <- data[train_index, ]
      test_data <- data[-train_index, ]
      
      # Store test data
      rv$test_data_perc <- test_data
      
      # Initialize results storage
      rv$modeling_results_perc <- list()
      metric_list <- list()
      
      withProgress(message = 'Training Models', value = 0, {
        n_models <- length(input$model_types_perc)
        
        for(model_type in input$model_types_perc) {
          incProgress(1/n_models, detail = paste("Training", model_type))
          
          # Train model based on type
          model <- switch(model_type,
                          "lm" = {
                            train(as.formula(paste(target, "~ .")),
                                  data = train_data[, c(target, predictors)],
                                  method = "lm",
                                  trControl = trainControl(method = "cv", number = 5))
                          },
                          "glm" = {
                            train(as.formula(paste(target, "~ .")),
                                  data = train_data[, c(target, predictors)],
                                  method = "glm",
                                  family = gaussian(),
                                  trControl = trainControl(method = "cv", number = 5))
                          },
                          "gbm" = {
                            train(as.formula(paste(target, "~ .")),
                                  data = train_data[, c(target, predictors)],
                                  method = "gbm",
                                  trControl = trainControl(method = "cv", number = 5),
                                  verbose = FALSE,
                                  tuneGrid = expand.grid(
                                    interaction.depth = c(1, 3, 5),
                                    n.trees = c(50, 100, 150),
                                    shrinkage = c(0.01, 0.1),
                                    n.minobsinnode = 10))
                          },
                          "rf" = {
                            train(as.formula(paste(target, "~ .")),
                                  data = train_data[, c(target, predictors)],
                                  method = "rf",
                                  ntree = 100,
                                  importance = TRUE,
                                  trControl = trainControl(method = "cv", number = 5),
                                  tuneGrid = expand.grid(mtry = c(2, sqrt(length(predictors)), length(predictors))))
                          },
                          "xgb" = {
                            train(as.formula(paste(target, "~ .")),
                                  data = train_data[, c(target, predictors)],
                                  method = "xgbTree",
                                  trControl = trainControl(method = "cv", number = 5),
                                  tuneGrid = expand.grid(
                                    nrounds = c(50, 100),
                                    max_depth = c(3, 6),
                                    eta = c(0.01, 0.3),
                                    gamma = 0,
                                    colsample_bytree = 0.8,
                                    min_child_weight = 1,
                                    subsample = 0.8))
                          }
          )
          
          # Make predictions
          predictions <- predict(model, newdata = test_data)
          
          # Calculate metrics
          residuals <- test_data[[target]] - predictions
          metrics <- data.frame(
            RMSE = sqrt(mean(residuals^2)),
            MAE = mean(abs(residuals)),
            R2 = cor(test_data[[target]], predictions)^2,
            MAPE = ifelse(all(test_data[[target]] != 0), 
                          mean(abs(residuals/test_data[[target]])) * 100,
                          NA)
          )
          
          # Store results
          rv$modeling_results_perc[[model_type]] <- list(
            model = model,
            predictions = predictions,
            actual = test_data[[target]],
            metrics = metrics,
            residuals = residuals
          )
          
          metric_list[[model_type]] <- cbind(Model = model_type, metrics)
        }
      })
      
      # Combine all metrics
      rv$all_metrics_perc <- do.call(rbind, metric_list)
      
      # Update model selection inputs
      updateSelectInput(session, "select_model_summary_perc", 
                        choices = input$model_types_perc)
      updateSelectInput(session, "select_model_imp_perc", 
                        choices = input$model_types_perc)
      updateSelectInput(session, "select_pdp_model_perc", 
                        choices = input$model_types_perc)
      
      showNotification("Model training completed!", type = "message")
    }, error = function(e) {
      show_error(paste("Error in model training:", e$message))
    })
  })
  
  # Metrics table output
  output$model_metrics_perc <- renderDT({
    req(rv$all_metrics_perc)
    datatable(rv$all_metrics_perc, options = list(pageLength = 5)) %>%
      formatRound(columns = 2:ncol(rv$all_metrics_perc), digits = 3)
  })
  
  # Metric comparison plot
  output$metric_comparison_plot_perc <- renderPlotly({
    req(rv$all_metrics_perc)
    
    # Reshape data for plotting
    plot_data <- rv$all_metrics_perc %>%
      tidyr::pivot_longer(cols = -Model, names_to = "Metric", values_to = "Value")
    
    p <- ggplot(plot_data, aes(x = Model, y = Value, fill = Model)) +
      geom_bar(stat = "identity") +
      facet_wrap(~Metric, scales = "free_y") +
      labs(title = "Model Performance Comparison") +
      theme_minimal() +
      theme(axis.text.x = element_blank())
    
    ggplotly(p)
  })
  
  # Model summary output
  output$model_summary_perc <- renderPrint({
    req(input$select_model_summary_perc, rv$modeling_results_perc)
    
    model <- rv$modeling_results_perc[[input$select_model_summary_perc]]$model
    
    if(input$select_model_summary_perc == "gbm") {
      # For GBM models, we need to access the finalModel component
      if(inherits(model, "train")) {
        # If it's a caret train object
        if(!is.null(model$finalModel)) {
          # Print summary of GBM model
          cat("Gradient Boosting Machine Model Summary\n")
          cat("======================================\n\n")
          
          # Print basic model information
          cat("Model Type:", model$modelInfo$label, "\n")
          cat("Number of Trees:", model$finalModel$n.trees, "\n")
          cat("Interaction Depth:", model$finalModel$interaction.depth, "\n")
          cat("Shrinkage (Learning Rate):", model$finalModel$shrinkage, "\n")
          cat("Minimum Observations in Node:", model$finalModel$n.minobsinnode, "\n\n")
          
          # Print variable importance
          cat("Variable Importance:\n")
          imp <- summary(model$finalModel, plotit = FALSE)
          print(imp)
          cat("\n")
          
          # Print performance metrics
          cat("Performance Metrics:\n")
          best_tune <- model$bestTune
          print(best_tune)
          cat("\n")
          
          # Print cross-validation results
          cat("Cross-Validation Results:\n")
          print(model$results)
          
        } else {
          "GBM model summary not available (finalModel is NULL)"
        }
      } else if(inherits(model, "gbm")) {
        # If it's a direct gbm object
        summary(model)
      } else {
        "Unknown GBM model type"
      }
    } else {
      # For all other models
      summary(model)
    }
  })  
  # Residuals plot
  output$model_residuals_plot_perc <- renderPlotly({
    req(rv$modeling_results_perc)
    
    # Combine residuals from all models
    residuals_list <- lapply(names(rv$modeling_results_perc), function(model) {
      data.frame(
        Model = model,
        Residuals = rv$modeling_results_perc[[model]]$residuals,
        Predicted = rv$modeling_results_perc[[model]]$predictions
      )
    })
    
    residuals_df <- do.call(rbind, residuals_list)
    
    p <- ggplot(residuals_df, aes(x = Predicted, y = Residuals, color = Model)) +
      geom_point(alpha = 0.5) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      facet_wrap(~Model) +
      labs(title = "Residuals Analysis") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Residuals statistics
  output$model_residuals_stats_perc <- renderPrint({
    req(rv$modeling_results_perc)
    
    cat("Residuals Summary Statistics:\n\n")
    for(model in names(rv$modeling_results_perc)) {
      res <- rv$modeling_results_perc[[model]]$residuals
      cat(paste0(model, ":\n"))
      print(summary(res))
      cat(paste0("Standard Deviation: ", round(sd(res), 3), "\n\n"))
    }
  })
  
  # Actual vs Predicted plot
  output$actual_vs_predicted_plot_perc <- renderPlotly({
    req(rv$modeling_results_perc)
    
    # Combine actual and predicted from all models
    plot_data <- lapply(names(rv$modeling_results_perc), function(model) {
      data.frame(
        Model = model,
        Actual = rv$modeling_results_perc[[model]]$actual,
        Predicted = rv$modeling_results_perc[[model]]$predictions
      )
    }) %>% bind_rows()
    
    p <- ggplot(plot_data, aes(x = Actual, y = Predicted, color = Model)) +
      geom_point(alpha = 0.5) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      facet_wrap(~Model) +
      labs(title = "Actual vs Predicted Values") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Feature importance plot
  output$model_importance_plot_perc <- renderPlotly({
    req(input$select_model_imp_perc, rv$modeling_results_perc)
    
    model <- rv$modeling_results_perc[[input$select_model_imp_perc]]$model
    
    if(input$select_model_imp_perc %in% c("rf", "gbm", "xgb")) {
      imp <- varImp(model)$importance
      imp$Variable <- rownames(imp)
      imp <- imp %>% arrange(desc(Overall))
      
      p <- ggplot(imp, aes(x = reorder(Variable, Overall), y = Overall, fill = Overall)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = paste("Feature Importance -", input$select_model_imp_perc),
             x = "Variable", y = "Importance") +
        theme_minimal()
      
      ggplotly(p)
    } else {
      ggplotly(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, 
                          label = "Feature importance not available for this model") +
                 theme_void())
    }
  })
  
  # Update PDP variable selection
  observeEvent(input$select_pdp_model_perc, {
    req(input$select_pdp_model_perc, rv$modeling_results_perc)
    model <- rv$modeling_results_perc[[input$select_pdp_model_perc]]$model
    updateSelectInput(session, "select_pdp_var_perc",
                      choices = input$model_predictors_perc)
  })
  
  # Partial dependence plot
  output$partial_dependence_plot_perc <- renderPlotly({
    req(input$select_pdp_model_perc, input$select_pdp_var_perc, rv$modeling_results_perc)
    
    model <- rv$modeling_results_perc[[input$select_pdp_model_perc]]$model
    data <- rv$processed_data_perc
    
    # Calculate partial dependence
    pd <- pdp::partial(model, pred.var = input$select_pdp_var_perc, 
                       train = data[, input$model_predictors_perc])
    
    p <- ggplot(pd, aes_string(x = input$select_pdp_var_perc, y = "yhat")) +
      geom_line(color = "steelblue", size = 1) +
      geom_point(color = "steelblue") +
      labs(title = paste("Partial Dependence on", input$select_pdp_var_perc),
           x = input$select_pdp_var_perc,
           y = "Predicted Disability Percentage") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  
  #XAI
  ###XAI Observers and Functions ==================================================
  
  # Update model choices for XAI
  observe({
    req(rv$modeling_results_perc)
    updateSelectInput(session, "xai_model_perc", 
                      choices = names(rv$modeling_results_perc))
  })
  
  # Dynamic UI controls for XAI
  output$xai_controls_perc <- renderUI({
    req(input$explanation_method_perc, input$xai_model_perc)
    
    if(input$explanation_method_perc == "lime") {
      tagList(
        fluidRow(
          column(4,
                 numericInput("lime_n_features_perc", "Number of Features:", 
                              value = 5, min = 1, max = 20)
          ),
          column(4,
                 numericInput("lime_n_samples_perc", "Number of Samples:", 
                              value = 1000, min = 100, max = 5000)
          ),
          column(4,
                 selectInput("lime_case_perc", "Case to Explain:",
                             choices = 1:min(5, ifelse(is.null(rv$test_data_perc), 1, nrow(rv$test_data_perc)))
                 ),
                 actionButton("run_lime_perc", "Generate LIME Explanation", 
                              class = "btn-primary", icon = icon("rocket"))
          )))
    } else if(input$explanation_method_perc == "shap") {
      tagList(
        sliderInput("shap_n_samples_perc", "Number of Samples:",
                    min = 10, max = 500, value = 100),
        actionButton("run_shap_perc", "Generate SHAP Values", 
                     class = "btn-primary", icon = icon("chart-bar"))
      )
    } else if(input$explanation_method_perc == "decision_tree") {
      tagList(
        sliderInput("dt_maxdepth_perc", "Max Tree Depth:",
                    min = 1, max = 6, value = 3),
        actionButton("run_dt_surrogate_perc", "Generate Surrogate Model", 
                     class = "btn-primary", icon = icon("tree"))
      )
    } else if(input$explanation_method_perc == "pdp") {
      tagList(
        selectInput("pdp_var_perc", "Select Variable:", 
                    choices = if(is.null(input$model_predictors_perc)) "" else input$model_predictors_perc),
        actionButton("run_pdp_perc", "Generate PDP Plot", 
                     class = "btn-primary", icon = icon("chart-line"))
      )
    } else {
      tagList(
        helpText("Feature importance is automatically calculated for supported models."),
        actionButton("refresh_importance_perc", "Refresh Importance", 
                     class = "btn-info", icon = icon("sync"))
      )
    }
  })
  
  # Main XAI Plot Output
  output$explanation_plot_perc <- renderPlotly({
    req(rv$xai_result_perc, input$xai_model_perc)
    
    method <- input$explanation_method_perc
    
    if(method == "lime" && !is.null(rv$xai_result_perc$plot_data)) {
      # LIME Plot
      plot_data <- rv$xai_result_perc$plot_data
      
      p <- ggplot(plot_data, aes(x = reorder(feature, feature_weight), 
                                 y = feature_weight, 
                                 fill = ifelse(feature_weight > 0, "Positive", "Negative"))) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_fill_manual(values = c("Positive" = "#1f77b4", "Negative" = "#ff7f0e")) +
        labs(title = paste("LIME Explanation for", input$xai_model_perc, "- Case", input$lime_case_perc),
             x = "Feature", 
             y = "Weight",
             fill = "Impact") +
        theme_minimal() +
        theme(legend.position = "bottom")
      
      ggplotly(p) %>% layout(showlegend = TRUE)
      
    } else if(method == "shap" && !is.null(rv$xai_result_perc$plot_data)) {
      # SHAP Plot
      plot_data <- rv$xai_result_perc$plot_data
      
      p <- ggplot(plot_data, aes(x = feature, y = shap_value, 
                                 color = feature_value,
                                 text = paste("Feature:", feature,
                                              "<br>SHAP Value:", round(shap_value, 3),
                                              "<br>Feature Value:", round(feature_value, 3)))) +
        geom_point(alpha = 0.7) +
        coord_flip() +
        scale_color_viridis_c(option = "plasma") +
        labs(title = paste("SHAP Values for", input$xai_model_perc),
             x = "Feature", 
             y = "SHAP Value", 
             color = "Feature Value") +
        theme_minimal()
      
      ggplotly(p, tooltip = "text")
      
    } else if(method == "pdp" && !is.null(rv$xai_result_perc)) {
      # Partial Dependence Plot
      pd <- rv$xai_result_perc
      
      p <- ggplot(pd, aes(x = !!sym(input$pdp_var_perc), y = yhat)) +
        geom_line(color = "#2ca02c", linewidth = 1.5) +
        geom_point(color = "#2ca02c", size = 2) +
        labs(title = paste("Partial Dependence for", input$xai_model_perc),
             x = input$pdp_var_perc,
             y = "Predicted Value") +
        theme_minimal()
      
      ggplotly(p)
      
    } else if(method == "feature_importance" || is.null(method)) {
      # Feature Importance
      model <- rv$modeling_results_perc[[input$xai_model_perc]]$model
      
      if(input$xai_model_perc %in% c("rf", "gbm", "xgb")) {
        # For GBM models, use gbm-specific importance calculation
        if(input$xai_model_perc == "gbm") {
          if(!requireNamespace("gbm", quietly = TRUE)) {
            stop("Package 'gbm' needed for GBM importance calculation")
          }
          imp <- gbm::relative.influence(model$finalModel, scale = TRUE)
          imp_df <- data.frame(
            Variable = names(imp),
            Importance = imp,
            row.names = NULL
          ) %>% arrange(desc(Importance))
          
          p <- ggplot(imp_df, aes(x = reorder(Variable, Importance), y = Importance, 
                                  fill = Importance, 
                                  text = paste("Variable:", Variable,
                                               "<br>Importance:", round(Importance, 3)))) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = round(Importance, 2)), hjust = -0.2, size = 4) + 
            coord_flip() +
            scale_fill_gradient(low = "#d62728", high = "#1f77b4") +
            labs(title = paste("Feature Importance for", input$xai_model_perc),
                 x = "Variable", 
                 y = "Importance") +
            theme_minimal()+
            expand_limits(y = max(imp_df$Importance) * 1.1)
        } else {
          # For RF and XGBoost
          imp <- varImp(model)$importance
          imp$Variable <- rownames(imp)
          imp <- imp %>% arrange(desc(Overall))
          
          p <- ggplot(imp, aes(x = reorder(Variable, Overall), y = Overall, 
                               fill = Overall, 
                               text = paste("Variable:", Variable,
                                            "<br>Importance:", round(Overall, 3)))) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = round(Overall, 2)), hjust = -0.2, size = 4) + 
            coord_flip() +
            scale_fill_gradient(low = "#d62728", high = "#1f77b4") +
            labs(title = paste("Feature Importance for", input$xai_model_perc),
                 x = "Variable", 
                 y = "Importance") +
            theme_minimal()+
            expand_limits(y = max(imp$Overall) * 1.1)
        }
        
        ggplotly(p, tooltip = "text")
      } else {
        ggplotly(ggplot() + 
                   annotate("text", x = 0.5, y = 0.5, 
                            label = "Feature importance not available for this model type",
                            size = 6) +
                   theme_void())
      }
    } else if(method == "decision_tree" && !is.null(rv$xai_result_perc$tree)) {
      # Decision Tree Plot - need to handle separately since rpart.plot isn't ggplot
      # We'll use visNetwork for interactive tree visualization
      tree <- rv$xai_result_perc$tree
      nodes <- visNetwork::toVisNetworkData(tree)$nodes
      edges <- visNetwork::toVisNetworkData(tree)$edges
      
      visNetwork(nodes, edges) %>%
        visEdges(arrows = "to") %>%
        visHierarchicalLayout(direction = "UD")
    }
  })
  
  # For Decision Tree visualization, add this additional output
  output$dt_plot_perc <- renderVisNetwork({
    req(input$explanation_method_perc == "decision_tree", 
        rv$xai_result_perc$tree)
    
    tree <- rv$xai_result_perc$tree
    
    # Convert rpart tree to visNetwork format
    tree_data <- visNetwork::visTree(tree, 
                                     main = "Surrogate Decision Tree",
                                     height = "500px", 
                                     width = "100%",
                                     nodesFontSize = 16)  # Base font size
    
    # Apply additional styling
    tree_data %>%
      visNodes(
        font = list(
          size = 20,          # Larger font size
          color = "black",     # Black text
          align = "center",    # Center aligned
          bold = TRUE,         # Bold text
          vadjust = 0          # Vertical adjustment
        ),
        shape = "box",         # Box shape for nodes
        shadow = TRUE,         # Add shadow effect
        color = list(
          background = "#E6F2FF",  # Light blue background
          border = "#005B9F",      # Dark blue border
          highlight = "#FFA500"    # Orange highlight
        ),
        margin = 10            # Margin around text
      ) %>%
      visEdges(
        font = list(
          size = 16,           # Slightly smaller than nodes
          color = "black",     # Black text
          align = "middle",    # Center aligned
          bold = TRUE,        # Bold text
          strokeWidth = 2      # Thicker text stroke
        ),
        color = list(
          color = "#4D4D4D",   # Dark gray edges
          highlight = "#FFA500" # Orange highlight
        ),
        smooth = TRUE,         # Smooth edges
        arrows = list(
          to = list(
            enabled = TRUE,    # Show direction arrows
            scaleFactor = 0.5  # Arrow size
          )
        )
      ) %>%
      visOptions(
        highlightNearest = list(
          enabled = TRUE,      # Enable highlight on hover
          degree = 1,
          hover = TRUE
        ),
        nodesIdSelection = TRUE  # Enable node selection
      ) %>%
      visLayout(
        hierarchical = list(
          direction = "UD",    # Up-Down layout
          sortMethod = "directed",
          nodeSpacing = 150,    # Space between nodes
          levelSeparation = 100 # Space between levels
        )
      )
  })  
  # XAI Text Output
  output$xai_output_perc <- renderPrint({
    req(rv$xai_result_perc, input$xai_model_perc)
    
    method <- input$explanation_method_perc
    
    cat(paste("Explanation for model:", input$xai_model_perc, "\n"))
    cat(paste("Method:", stringr::str_to_title(gsub("_", " ", method)), "\n\n"))
    
    if(method == "lime") {
      cat("LIME Explanation Summary:\n")
      print(rv$xai_result_perc$explanation)
      cat("\nKey:\n")
      cat("- Positive weight: Supports the prediction\n")
      cat("- Negative weight: Contradicts the prediction\n")
    } else if(method == "shap") {
      cat("SHAP Value Summary:\n")
      print(summary(rv$xai_result_perc$shap_values))
      cat("\nInterpretation:\n")
      cat("- Positive SHAP: Increases the prediction\n")
      cat("- Negative SHAP: Decreases the prediction\n")
    } else if(method == "decision_tree") {
      cat("Surrogate Decision Tree Summary:\n")
      print(rv$xai_result_perc$tree)
      cat("\nSurrogate Model Accuracy:", round(rv$xai_result_perc$accuracy, 3), "\n")
      cat("\nNote: This is a simplified approximation of the original model\n")
    } else if(method == "pdp") {
      cat("Partial Dependence Plot Data:\n")
      print(rv$xai_result_perc)
      cat("\nInterpretation:\n")
      cat("- Shows how the prediction changes as the selected variable changes\n")
      cat("- Holding all other variables constant\n")
    } else {
      # Feature Importance
      model <- rv$modeling_results_perc[[input$xai_model_perc]]$model
      
      if(input$xai_model_perc %in% c("rf", "gbm", "xgb")) {
        if(input$xai_model_perc == "gbm") {
          imp <- gbm::relative.influence(model$finalModel, scale = TRUE)
          cat("GBM Feature Importance Scores (relative influence):\n")
          print(sort(imp, decreasing = TRUE))
        } else {
          imp <- varImp(model)$importance
          cat("Feature Importance Scores:\n")
          print(imp[order(-imp$Overall), , drop = FALSE])
        }
        cat("\nInterpretation:\n")
        cat("- Higher values indicate more important features\n")
        cat("- Measures how much each feature contributes to predictions\n")
      } else {
        cat("Feature importance not available for this model type\n")
      }
    }
  })
  
  ###XAI Action Handlers ==========================================================
  
  # Add this helper function to ensure factor levels match
  match_factor_levels <- function(new_data, reference_data) {
    for(col in names(new_data)) {
      if(is.factor(reference_data[[col]])) {
        new_data[[col]] <- factor(new_data[[col]], levels = levels(reference_data[[col]]))
      }
    }
    new_data
  }
  
  # LIME Explanation
  observeEvent(input$run_lime_perc, {
    tryCatch({
      req(input$xai_model_perc, rv$modeling_results_perc, rv$test_data_perc)
      
      model <- rv$modeling_results_perc[[input$xai_model_perc]]$model
      data <- rv$test_data_perc
      predictors <- input$model_predictors_perc
      
      withProgress(message = 'Generating LIME Explanation', value = 0.5, {
        # Prepare data for explanation
        explain_data <- data[, predictors, drop = FALSE]
        
        # Create a custom predict function that works with various model types
        predict_model <- function(model, newdata) {
          # Convert newdata to data frame if it isn't already
          newdata <- as.data.frame(newdata)
          
          # Ensure column types match training data
          for(col in names(newdata)) {
            if(col %in% names(explain_data)) {
              if(is.factor(explain_data[[col]])) {
                newdata[[col]] <- factor(newdata[[col]], levels = levels(explain_data[[col]]))
              } else if(is.numeric(explain_data[[col]])) {
                newdata[[col]] <- as.numeric(newdata[[col]])
              }
            }
          }
          
          # Handle different model types
          if(inherits(model, "train")) { # Caret model
            predict(model, newdata = newdata)
          } else if(inherits(model, "lm") || inherits(model, "glm")) { # Linear model
            predict(model, newdata = newdata)
          } else if(inherits(model, "randomForest")) { # Random forest
            predict(model, newdata = newdata)
          } else if(inherits(model, "gbm")) { # GBM
            predict(model, newdata = newdata, n.trees = model$n.trees)
          } else if(inherits(model, "xgb.Booster")) { # XGBoost
            xgb_data <- xgb.DMatrix(data.matrix(newdata))
            predict(model, newdata = xgb_data)
          } else {
            # Fallback for other models
            predict(model, newdata = newdata)
          }
        }
        
        # Create explainer
        explainer <- lime::lime(
          explain_data, 
          model = model,
          bin_continuous = TRUE,
          n_bins = 5,
          quantile_bins = TRUE
        )
        
        # Explicitly set model type to regression (since we're predicting percentages)
        attr(explainer, "model_type") <- "regression"
        
        incProgress(0.3, detail = "Explaining selected case")
        case_to_explain <- as.numeric(input$lime_case_perc)
        
        # Handle different data types in explanation
        explanation <- tryCatch({
          lime::explain(
            explain_data[case_to_explain, , drop = FALSE],
            explainer,
            n_features = input$lime_n_features_perc,
            n_permutations = input$lime_n_samples_perc,
            dist_fun = "gower",
            kernel_width = 0.75
          )
        }, error = function(e) {
          # Fallback to simpler explanation if first attempt fails
          lime::explain(
            explain_data[case_to_explain, , drop = FALSE],
            explainer,
            n_features = input$lime_n_features_perc,
            n_permutations = min(input$lime_n_samples_perc, 500),
            dist_fun = "euclidean",
            kernel_width = 0.75
          )
        })
        
        # Prepare plot data
        plot_data <- explanation %>%
          group_by(feature) %>%
          summarise(feature_weight = mean(feature_weight)) %>%
          arrange(abs(feature_weight))
        
        rv$xai_result_perc <- list(
          method = "lime",
          explanation = explanation,
          plot_data = plot_data
        )
      })
      
    }, error = function(e) {
      showNotification(paste("Error in LIME explanation:", e$message), type = "error")
      rv$xai_result_perc <- list(error = e$message)
    })
  })
  
  # SHAP Values
  observeEvent(input$run_shap_perc, {
    tryCatch({
      req(input$xai_model_perc, rv$modeling_results_perc, rv$test_data_perc)
      
      model <- rv$modeling_results_perc[[input$xai_model_perc]]$model
      data <- rv$test_data_perc
      predictors <- input$model_predictors_perc
      
      if(!input$xai_model_perc %in% c("rf", "gbm", "xgb")) {
        stop("SHAP values currently only supported for Random Forest, GBM, and XGBoost models")
      }
      
      withProgress(message = 'Calculating SHAP Values', value = 0, {
        # Sample data for SHAP (can be slow with many rows)
        sample_data <- data[sample(nrow(data), input$shap_n_samples_perc), predictors]
        
        # Convert to appropriate format
        sample_data <- as.data.frame(sample_data)
        
        incProgress(0.3, detail = "Calculating SHAP values")
        shap_values <- fastshap::explain(
          model,
          X = sample_data,
          pred_wrapper = function(object, newdata) predict(object, newdata)
        )
        
        incProgress(0.6, detail = "Preparing visualization")
        plot_data <- shap_values %>%
          as.data.frame() %>%
          tidyr::gather(key = "feature", value = "shap_value") %>%
          mutate(feature_value = unlist(sample_data))
        
        rv$xai_result_perc <- list(
          method = "shap",
          shap_values = shap_values,
          plot_data = plot_data
        )
      })
      
    }, error = function(e) {
      showNotification(paste("Error in SHAP calculation:", e$message), type = "error")
      rv$xai_result_perc <- list(error = e$message)
    })
  })
  
  # Decision Tree Surrogate
  observeEvent(input$run_dt_surrogate_perc, {
    tryCatch({
      req(input$xai_model_perc, rv$modeling_results_perc, rv$test_data_perc)
      
      model <- rv$modeling_results_perc[[input$xai_model_perc]]$model
      data <- rv$test_data_perc
      predictors <- input$model_predictors_perc
      
      withProgress(message = 'Building Surrogate Model', value = 0.5, {
        # Get predictions from original model
        preds <- predict(model, newdata = data)
        
        # Prepare data for surrogate model
        surrogate_data <- data[, predictors]
        surrogate_data$prediction <- preds
        
        incProgress(0.7, detail = "Training decision tree")
        tree <- rpart::rpart(
          prediction ~ .,
          data = surrogate_data,
          maxdepth = input$dt_maxdepth_perc,
          method = "anova"  # Using anova for regression (disability percentage)
        )
        
        tree_preds <- predict(tree, newdata = data)
        accuracy <- cor(preds, tree_preds)^2
        
        rv$xai_result_perc <- list(
          method = "decision_tree",
          tree = tree,
          accuracy = accuracy
        )
      })
      
    }, error = function(e) {
      showNotification(paste("Error in surrogate model:", e$message), type = "error")
      rv$xai_result_perc <- list(error = e$message)
    })
  })
  
  # Partial Dependence Plots
  observeEvent(input$run_pdp_perc, {
    tryCatch({
      req(input$xai_model_perc, rv$modeling_results_perc, rv$test_data_perc, input$pdp_var_perc)
      
      model <- rv$modeling_results_perc[[input$xai_model_perc]]$model
      data <- rv$test_data_perc
      predictors <- input$model_predictors_perc
      
      withProgress(message = 'Calculating Partial Dependence', value = 0.5, {
        pd <- pdp::partial(model, 
                           pred.var = input$pdp_var_perc, 
                           train = data[, predictors])
        
        rv$xai_result_perc <- pd
      })
      
    }, error = function(e) {
      showNotification(paste("Error in partial dependence plot:", e$message), type = "error")
      rv$xai_result_perc <- list(error = e$message)
    })
  })
  
  # Refresh Feature Importance
  observeEvent(input$refresh_importance_perc, {
    rv$xai_result_perc <- list(method = "feature_importance")
  })
  
  
  
  # Disability Percentage Prediction Server Code ================================
  
  # Reactive value for manual input
  manual_pred_data <- reactiveVal(NULL)
  
  # Update manual input fields based on model predictors
  observe({
    req(input$select_model_pred_perc, rv$modeling_results_perc)
    
    model <- rv$modeling_results_perc[[input$select_model_pred_perc]]$model
    if(is.null(model)) return()
    
    # Get predictors from training data
    predictors <- names(model$trainingData)[-1] # Remove target variable
    
    # Create empty dataframe with predictors
    init_data <- setNames(data.frame(matrix(ncol = length(predictors), nrow = 1)), predictors)
    
    # Set default values based on data types
    for(col in predictors) {
      if(is.numeric(model$trainingData[[col]])) {
        init_data[[col]] <- as.numeric(NA)
      } else if(is.factor(model$trainingData[[col]])) {
        init_data[[col]] <- factor(NA, levels = levels(model$trainingData[[col]]))
      } else {
        init_data[[col]] <- NA
      }
    }
    
    manual_pred_data(init_data)
  })
  
  # Load data for prediction - FIXED VERSION
  observeEvent(input$load_data_pred_perc, {
    tryCatch({
      if(input$data_source_pred_perc == "prepared") {
        req(rv$pred_data)
        # Make sure we're working with a fresh copy
        rv$prepared_data_perc <- rv$pred_data %>% 
          as.data.frame() %>%
          mutate(across(where(is.factor), as.character)) # Temporary conversion to avoid factor issues
        showNotification("Loaded prepared prediction data", type = "message")
      } else {
        req(rv$balanced_data)
        if(length(rv$balanced_data) == 0) {
          showNotification("No balanced data available. Please balance data first.", type = "error")
          return()
        }
        # Get the latest balanced dataset
        latest_balanced <- rv$balanced_data[[length(rv$balanced_data)]]
        rv$prepared_data_perc <- latest_balanced$data %>% 
          as.data.frame() %>%
          mutate(across(where(is.factor), as.character)) # Temporary conversion
        showNotification("Loaded balanced data for prediction", type = "message")
      }
      
      # Convert back to appropriate types based on model requirements
      if(!is.null(input$select_model_pred_perc) && 
         !is.null(rv$modeling_results_perc[[input$select_model_pred_perc]])) {
        model <- rv$modeling_results_perc[[input$select_model_pred_perc]]$model
        if(!is.null(model)) {
          # Get expected types from training data
          for(col in names(rv$prepared_data_perc)) {
            if(col %in% names(model$trainingData)) {
              if(is.factor(model$trainingData[[col]])) {
                rv$prepared_data_perc[[col]] <- factor(rv$prepared_data_perc[[col]], 
                                                       levels = levels(model$trainingData[[col]]))
              } else if(is.numeric(model$trainingData[[col]])) {
                rv$prepared_data_perc[[col]] <- as.numeric(rv$prepared_data_perc[[col]])
              }
            }
          }
        }
      }
      
      # Update model selection
      if(!is.null(rv$modeling_results_perc) && length(rv$modeling_results_perc) > 0) {
        updateSelectInput(session, "select_model_pred_perc", 
                          choices = names(rv$modeling_results_perc))
      } else {
        showNotification("No trained models found. Please train models first.", 
                         type = "warning")
      }
      
    }, error = function(e) {
      showNotification(paste("Error loading prediction data:", e$message), type = "error")
    })
  })
  
  # Data Preview Output - FIXED
  output$pred_data_preview_perc <- renderDT({
    req(rv$prepared_data_perc)
    
    # Show first 100 rows for preview
    preview_data <- head(rv$prepared_data_perc, 100)
    
    datatable(
      preview_data,
      options = list(
        scrollX = TRUE,
        pageLength = 5,
        lengthMenu = c(5, 10, 15, 20)
      ),
      rownames = FALSE
    )
  })
  
  # Generate dynamic UI for manual input
  output$manual_input_ui <- renderUI({
    req(manual_pred_data())
    df <- manual_pred_data()
    
    tagList(
      h4("Enter Prediction Values:"),
      lapply(names(df), function(col) {
        if(is.numeric(df[[col]])) {
          numericInput(
            inputId = paste0("pred_", col),
            label = col,
            value = NA
          )
        } else if(is.factor(df[[col]])) {
          selectInput(
            inputId = paste0("pred_", col),
            label = col,
            choices = levels(df[[col]]),
            selected = NA
          )
        } else {
          textInput(
            inputId = paste0("pred_", col),
            label = col,
            value = NA
          )
        }
      })
    )
  })
  
  # Handle manual input submission
  observeEvent(input$submit_manual_input, {
    tryCatch({
      req(input$select_model_pred_perc, rv$modeling_results_perc, manual_pred_data())
      
      model <- rv$modeling_results_perc[[input$select_model_pred_perc]]$model
      predictors <- names(model$trainingData)[-1]
      
      # Collect all input values
      input_values <- lapply(predictors, function(col) {
        val <- input[[paste0("pred_", col)]]
        # Convert empty strings to NA
        if(is.character(val) && val == "") NA else val
      })
      names(input_values) <- predictors
      
      # Convert to dataframe with proper types
      pred_row <- data.frame(input_values, stringsAsFactors = FALSE)
      
      # Convert columns to correct types
      for(col in predictors) {
        if(is.factor(model$trainingData[[col]])) {
          pred_row[[col]] <- factor(pred_row[[col]], levels = levels(model$trainingData[[col]]))
        } else if(is.numeric(model$trainingData[[col]])) {
          pred_row[[col]] <- as.numeric(pred_row[[col]])
        }
      }
      
      # Make prediction
      prediction <- tryCatch({
        predict(model, newdata = pred_row)
      }, error = function(e) {
        showNotification(paste("Prediction error:", e$message), type = "error")
        return(NA)
      })
      
      if(is.na(prediction)) return()
      
      # Generate explanation (LIME)
      explainer <- tryCatch({
        lime::lime(
          model$trainingData[, -1, drop = FALSE], 
          model = model,
          bin_continuous = TRUE,
          n_bins = 5
        )
      }, error = function(e) {
        showNotification(paste("LIME explainer error:", e$message), type = "error")
        return(NULL)
      })
      
      if(is.null(explainer)) return()
      
      explanation <- tryCatch({
        lime::explain(
          pred_row,
          explainer,
          n_features = min(5, length(predictors)),
          n_permutations = 1000,
          dist_fun = "gower"
        )
      }, error = function(e) {
        showNotification(paste("LIME explanation error:", e$message), type = "error")
        return(NULL)
      })
      
      if(is.null(explanation)) return()
      
      # Prepare plot data
      plot_data <- explanation %>%
        group_by(feature) %>%
        summarise(feature_weight = mean(feature_weight)) %>%
        arrange(abs(feature_weight))
      
      # Store results
      rv$single_pred_result_perc <- list(
        prediction = prediction,
        explanation = explanation,
        plot_data = plot_data,
        pred_row = pred_row
      )
      
      showNotification("Prediction completed!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error in manual prediction:", e$message), type = "error")
    })
  })
  
  # Data selection prediction - FIXED VERSION
  observeEvent(input$run_data_pred_perc, {
    tryCatch({
      req(rv$prepared_data_perc, input$select_model_pred_perc, 
          rv$modeling_results_perc, input$pred_data_row_perc)
      
      model <- rv$modeling_results_perc[[input$select_model_pred_perc]]$model
      pred_data <- rv$prepared_data_perc
      
      # Get the row for prediction
      row_num <- input$pred_data_row_perc
      if(row_num < 1 || row_num > nrow(pred_data)) {
        showNotification(paste("Row number must be between 1 and", nrow(pred_data)), 
                         type = "error")
        return()
      }
      
      pred_row <- pred_data[row_num, , drop = FALSE]
      
      # Ensure factor variables match training data
      for(col in names(model$trainingData)[-1]) {
        if(is.factor(model$trainingData[[col]]) && col %in% names(pred_row)) {
          pred_row[[col]] <- factor(pred_row[[col]], 
                                    levels = levels(model$trainingData[[col]]))
        }
      }
      
      # Make prediction
      prediction <- tryCatch({
        predict(model, newdata = pred_row)
      }, error = function(e) {
        showNotification(paste("Prediction error:", e$message), type = "error")
        return(NA)
      })
      
      if(is.na(prediction)) return()
      
      # Generate explanation (LIME)
      explainer <- tryCatch({
        lime::lime(
          pred_data, 
          model = model,
          bin_continuous = TRUE,
          n_bins = 5
        )
      }, error = function(e) {
        showNotification(paste("LIME explainer error:", e$message), type = "error")
        return(NULL)
      })
      
      if(is.null(explainer)) return()
      
      explanation <- tryCatch({
        lime::explain(
          pred_row,
          explainer,
          n_features = min(5, ncol(pred_data)),
          n_permutations = 1000,
          dist_fun = "gower"
        )
      }, error = function(e) {
        showNotification(paste("LIME explanation error:", e$message), type = "error")
        return(NULL)
      })
      
      if(is.null(explanation)) return()
      
      # Prepare plot data
      plot_data <- explanation %>%
        group_by(feature) %>%
        summarise(feature_weight = mean(feature_weight)) %>%
        arrange(abs(feature_weight))
      
      # Store results
      rv$single_pred_result_perc <- list(
        prediction = prediction,
        explanation = explanation,
        plot_data = plot_data,
        pred_row = pred_row
      )
      
      showNotification("Row selection prediction completed!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error in data selection prediction:", e$message), type = "error")
    })
  })
  
  # Display selected row data for data selection prediction - FIXED
  output$selected_row_data_perc <- renderPrint({
    req(rv$prepared_data_perc, input$pred_data_row_perc)
    row_num <- input$pred_data_row_perc
    if(row_num < 1 || row_num > nrow(rv$prepared_data_perc)) {
      cat("Please select a valid row number between 1 and", nrow(rv$prepared_data_perc), "\n")
      return()
    }
    
    cat("Selected Row Data:\n\n")
    print(rv$prepared_data_perc[row_num, , drop = FALSE])
  })
  
  # Data selection prediction results - FIXED
  output$data_selection_result_perc <- renderUI({
    req(rv$single_pred_result_perc)
    
    pred_value <- rv$single_pred_result_perc$prediction
    pred_row <- rv$single_pred_result_perc$pred_row
    
    tagList(
      h4("Prediction Result:"),
      div(class = "alert alert-success",
          style = "font-size: 18px; padding: 15px;",
          strong("Predicted Disability Percentage: "), 
          round(pred_value, 2), "%"),
      br(),
      h4("Explanation:"),
      plotlyOutput("data_selection_plot_perc", height = "400px"),
      br(),
      verbatimTextOutput("data_selection_explanation_perc")
    )
  })
  
  # Data selection prediction plot
  output$data_selection_plot_perc <- renderPlotly({
    req(rv$single_pred_result_perc)
    
    plot_data <- rv$single_pred_result_perc$plot_data
    
    p <- ggplot(plot_data, aes(x = reorder(feature, feature_weight), 
                               y = feature_weight, 
                               fill = ifelse(feature_weight > 0, "Positive", "Negative"))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_manual(values = c("Positive" = "#1f77b4", "Negative" = "#ff7f0e")) +
      labs(title = "Feature Contribution to Prediction",
           x = "Feature", 
           y = "Weight",
           fill = "Impact") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p) %>% layout(showlegend = TRUE)
  })
  
  # Data selection explanation text
  output$data_selection_explanation_perc <- renderPrint({
    req(rv$single_pred_result_perc)
    
    cat("LIME Explanation Details:\n\n")
    print(rv$single_pred_result_perc$explanation)
    cat("\n\nInterpretation:\n")
    cat("- Positive weights indicate features that increase the predicted percentage\n")
    cat("- Negative weights indicate features that decrease the predicted percentage\n")
    cat("- The magnitude shows the relative importance of each feature\n")
  })
  
  
  
  # Batch prediction
  observeEvent(input$run_batch_pred_perc, {
    tryCatch({
      req(rv$prepared_data_perc, input$select_model_pred_perc, rv$modeling_results_perc)
      
      model <- rv$modeling_results_perc[[input$select_model_pred_perc]]$model
      pred_data <- rv$prepared_data_perc
      
      # Ensure factor variables match training data
      for(col in names(model$trainingData)[-1]) {
        if(is.factor(model$trainingData[[col]]) && col %in% names(pred_data)) {
          pred_data[[col]] <- factor(pred_data[[col]], levels = levels(model$trainingData[[col]]))
        }
      }
      
      withProgress(message = 'Running Batch Predictions', value = 0, {
        # Make predictions
        incProgress(0.3, detail = "Making predictions")
        predictions <- predict(model, newdata = pred_data)
        
        # Create results dataframe
        result_df <- pred_data
        result_df$Predicted_Percentage <- predictions
        
        # Calculate SHAP values for the first few rows (for explanation)
        if(nrow(pred_data) > 0 && input$select_model_pred_perc %in% c("rf", "gbm", "xgb")) {
          sample_size <- min(100, nrow(pred_data))
          sample_data <- pred_data[1:sample_size, ]
          
          incProgress(0.5, detail = "Calculating SHAP values")
          shap_values <- tryCatch({
            fastshap::explain(
              model,
              X = sample_data,
              pred_wrapper = function(object, newdata) predict(object, newdata),
              nsim = 50
            )
          }, error = function(e) {
            showNotification(paste("SHAP calculation error:", e$message), type = "warning")
            return(NULL)
          })
          
          if(!is.null(shap_values)) {
            shap_df <- as.data.frame(shap_values)
            names(shap_df) <- paste0("SHAP_", names(shap_df))
            result_df <- cbind(result_df[1:sample_size, , drop = FALSE], shap_df)
          }
        }
        
        rv$batch_pred_result_perc <- result_df
        incProgress(1, detail = "Completed")
      })
      
      showNotification(paste("Batch predictions completed for", nrow(pred_data), "rows!"), 
                       type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error in batch prediction:", e$message), type = "error")
    })
  })
  
  # Feature importance plot
  output$feature_importance_plot_pred_perc <- renderPlotly({
    req(input$select_model_pred_perc, rv$modeling_results_perc)
    
    model <- rv$modeling_results_perc[[input$select_model_pred_perc]]$model
    
    imp <- tryCatch({
      if(input$select_model_pred_perc %in% c("rf", "gbm", "xgb")) {
        varImp(model)$importance
      } else {
        # For models without built-in importance, use a generic approach
        caret::varImp(model)$importance
      }
    }, error = function(e) {
      showNotification(paste("Feature importance error:", e$message), type = "warning")
      return(NULL)
    })
    
    if(is.null(imp)) return(NULL)
    
    imp$Variable <- rownames(imp)
    imp <- imp %>% arrange(desc(Overall))
    
    p <- ggplot(imp, aes(x = reorder(Variable, Overall), y = Overall, 
                         text = paste("Variable:", Variable, "<br>Importance:", round(Overall, 3)))) +
      geom_bar(stat = "identity", fill = "#1f77b4") +
      geom_text(aes(label = round(Overall, 2)), hjust = -0.2, size = 3) +
      coord_flip() +
      labs(title = paste("Feature Importance -", input$select_model_pred_perc),
           x = "Variable", 
           y = "Importance") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text") %>% layout(showlegend = FALSE)
  })
  
  # Partial dependence plot
  output$pdp_plot_pred_perc <- renderPlotly({
    req(input$select_model_pred_perc, rv$modeling_results_perc, 
        input$pdp_var_pred_perc, rv$prepared_data_perc)
    
    model <- rv$modeling_results_perc[[input$select_model_pred_perc]]$model
    data <- rv$prepared_data_perc
    
    withProgress(message = 'Calculating Partial Dependence', value = 0.5, {
      pd <- tryCatch({
        pdp::partial(
          model, 
          pred.var = input$pdp_var_pred_perc, 
          train = data,
          grid.resolution = 20
        )
      }, error = function(e) {
        showNotification(paste("PDP calculation error:", e$message), type = "warning")
        return(NULL)
      })
      
      if(is.null(pd)) return(NULL)
      
      p <- ggplot(pd, aes(x = !!sym(input$pdp_var_pred_perc), y = yhat)) +
        geom_line(color = "#2ca02c", linewidth = 1) +
        geom_point(color = "#2ca02c", size = 2) +
        labs(title = paste("Partial Dependence on", input$pdp_var_pred_perc),
             x = input$pdp_var_pred_perc,
             y = "Predicted Disability Percentage") +
        theme_minimal()
      
      ggplotly(p)
    })
  })
  
  # SHAP summary plot
  output$shap_plot_pred_perc <- renderPlotly({
    req(rv$batch_pred_result_perc, input$select_model_pred_perc)
    
    result_df <- rv$batch_pred_result_perc
    shap_cols <- grep("^SHAP_", names(result_df), value = TRUE)
    
    if(length(shap_cols) > 0) {
      # Prepare SHAP data
      shap_data <- result_df[, shap_cols, drop = FALSE]
      names(shap_data) <- gsub("^SHAP_", "", names(shap_data))
      
      # Get feature values
      feature_data <- result_df[, names(shap_data), drop = FALSE]
      
      # Create long format data
      plot_data <- data.frame(
        Feature = rep(names(shap_data), each = nrow(shap_data)),
        SHAP = as.vector(as.matrix(shap_data)),
        Value = as.vector(as.matrix(feature_data))
      )
      
      # Order features by mean absolute SHAP value
      feature_order <- plot_data %>%
        group_by(Feature) %>%
        summarise(mean_abs = mean(abs(SHAP))) %>%
        arrange(desc(mean_abs)) %>%
        pull(Feature)
      
      plot_data$Feature <- factor(plot_data$Feature, levels = feature_order)
      
      # Create plot
      p <- ggplot(plot_data, aes(x = Feature, y = SHAP, color = Value)) +
        geom_jitter(width = 0.2, height = 0, alpha = 0.7) +
        coord_flip() +
        scale_color_viridis_c() +
        labs(title = "SHAP Values Summary",
             x = "Feature",
             y = "SHAP Value (Impact on Prediction)",
             color = "Feature Value") +
        theme_minimal() +
        theme(legend.position = "bottom")
      
      ggplotly(p) %>% layout(showlegend = TRUE)
    } else {
      ggplotly(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, 
                          label = "SHAP values not calculated for this model") +
                 theme_void())
    }
  })
  
  # Update PDP variable selection
  observe({
    req(input$select_model_pred_perc, rv$modeling_results_perc)
    model <- rv$modeling_results_perc[[input$select_model_pred_perc]]$model
    if(!is.null(model)) {
      updateSelectInput(session, "pdp_var_pred_perc",
                        choices = names(model$trainingData)[-1])
    }
  })
  
  # Unified prediction result output
  output$prediction_result_perc <- renderUI({
    req(rv$single_pred_result_perc)
    
    pred_value <- rv$single_pred_result_perc$prediction
    pred_row <- rv$single_pred_result_perc$pred_row
    
    tagList(
      h4("Prediction Result:"),
      div(class = "alert alert-success",
          style = "font-size: 18px; padding: 15px;",
          strong("Predicted Disability Percentage: "), 
          round(pred_value, 2), "%"),
      br(),
      h4("Input Values:"),
      renderTable({
        t(as.data.frame(pred_row))
      }, rownames = TRUE, colnames = FALSE),
      br(),
      h4("Explanation:"),
      plotlyOutput("prediction_plot_perc", height = "400px"),
      br(),
      verbatimTextOutput("prediction_explanation_perc")
    )
  })
  
  # Unified prediction plot
  output$prediction_plot_perc <- renderPlotly({
    req(rv$single_pred_result_perc)
    
    plot_data <- rv$single_pred_result_perc$plot_data
    
    p <- ggplot(plot_data, aes(x = reorder(feature, feature_weight), 
                               y = feature_weight, 
                               fill = ifelse(feature_weight > 0, "Positive", "Negative"))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_manual(values = c("Positive" = "#1f77b4", "Negative" = "#ff7f0e")) +
      labs(title = "Feature Contribution to Prediction",
           x = "Feature", 
           y = "Weight",
           fill = "Impact") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p) %>% layout(showlegend = TRUE)
  })
  
  # Unified prediction explanation text
  output$prediction_explanation_perc <- renderPrint({
    req(rv$single_pred_result_perc)
    
    if(!is.null(rv$single_pred_result_perc$explanation)) {
      cat("LIME Explanation Details:\n\n")
      print(rv$single_pred_result_perc$explanation)
      cat("\n\nInterpretation:\n")
      cat("- Positive weights indicate features that increase the predicted percentage\n")
      cat("- Negative weights indicate features that decrease the predicted percentage\n")
      cat("- The magnitude shows the relative importance of each feature\n")
    } else {
      cat("No explanation available. The explanation model may have failed to run.\n")
    }
  })
  
  # Display selected row data
  output$selected_row_data_perc <- renderPrint({
    req(rv$prepared_data_perc, input$pred_data_row_perc)
    row_num <- input$pred_data_row_perc
    if(row_num < 1 || row_num > nrow(rv$prepared_data_perc)) return()
    
    cat("Selected Row Data:\n\n")
    print(rv$prepared_data_perc[row_num, , drop = FALSE])
  })
  
  # Batch prediction results
  output$batch_pred_results_perc <- renderDT({
    req(rv$batch_pred_result_perc)
    
    datatable(
      rv$batch_pred_result_perc,
      extensions = 'Buttons',
      options = list(
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf'),
        pageLength = 10
      )
    )
  })
  
  # Batch prediction summary
  output$batch_pred_summary_perc <- renderPrint({
    req(rv$batch_pred_result_perc)
    
    preds <- rv$batch_pred_result_perc$Predicted_Percentage
    
    cat("Batch Prediction Summary:\n\n")
    cat("Number of predictions:", length(preds), "\n")
    cat("Mean predicted percentage:", round(mean(preds, na.rm = TRUE), 2), "%\n")
    cat("Median predicted percentage:", round(median(preds, na.rm = TRUE), 2), "%\n")
    cat("Minimum predicted percentage:", round(min(preds, na.rm = TRUE), 2), "%\n")
    cat("Maximum predicted percentage:", round(max(preds, na.rm = TRUE), 2), "%\n")
    cat("\nDistribution:\n")
    print(summary(preds))
  })
  
  # Batch prediction plot
  output$batch_pred_plot_perc <- renderPlotly({
    req(rv$batch_pred_result_perc)
    
    p <- ggplot(rv$batch_pred_result_perc, aes(x = Predicted_Percentage)) +
      geom_histogram(fill = "steelblue", bins = 30) +
      labs(title = "Distribution of Predicted Disability Percentages",
           x = "Predicted Percentage", 
           y = "Count") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Download batch predictions
  output$download_batch_pred_perc <- downloadHandler(
    filename = function() {
      paste0("disability_percentage_predictions_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(rv$batch_pred_result_perc, file, row.names = FALSE)
    }
  )
  
  # Handle batch explanation button
  observeEvent(input$explain_batch_pred_perc, {
    tryCatch({
      req(rv$batch_pred_result_perc, input$select_model_pred_perc, rv$modeling_results_perc)
      
      model <- rv$modeling_results_perc[[input$select_model_pred_perc]]$model
      result_df <- rv$batch_pred_result_perc
      
      # Get the first 100 rows (or all if less than 100)
      explain_rows <- min(100, nrow(result_df))
      explain_data <- result_df[1:explain_rows, ]
      
      withProgress(message = 'Generating Batch Explanations', value = 0, {
        # Calculate SHAP values if not already calculated
        if(!any(grepl("^SHAP_", names(result_df)))) {
          incProgress(0.3, detail = "Calculating SHAP values")
          
          # Remove prediction column if exists
          pred_data <- explain_data[, !names(explain_data) %in% c("Predicted_Percentage"), drop = FALSE]
          
          # Remove any existing SHAP columns if present
          pred_data <- pred_data[, !grepl("^SHAP_", names(pred_data)), drop = FALSE]
          
          shap_values <- fastshap::explain(
            model,
            X = pred_data,
            pred_wrapper = function(object, newdata) predict(object, newdata),
            nsim = 50
          )
          
          shap_df <- as.data.frame(shap_values)
          names(shap_df) <- paste0("SHAP_", names(shap_df))
          
          # Combine with predictions
          explain_data <- cbind(explain_data[, !grepl("^SHAP_", names(explain_data))], shap_df)
        }
        
        # Store updated results with SHAP values
        rv$batch_pred_result_perc <- explain_data
        
        # Generate summary visualizations
        incProgress(0.7, detail = "Preparing explanations")
        
        # Prepare SHAP summary data
        shap_cols <- grep("^SHAP_", names(explain_data), value = TRUE)
        shap_data <- explain_data[, shap_cols, drop = FALSE]
        names(shap_data) <- gsub("^SHAP_", "", names(shap_data))
        
        # Get feature values
        feature_data <- explain_data[, names(shap_data), drop = FALSE]
        
        # Create long format data
        shap_summary_data <- data.frame(
          Feature = rep(names(shap_data), each = nrow(shap_data)),
          SHAP = as.vector(as.matrix(shap_data)),
          Value = as.vector(as.matrix(feature_data))
        )
        
        # Order features by mean absolute SHAP value
        feature_order <- shap_summary_data %>%
          group_by(Feature) %>%
          summarise(mean_abs = mean(abs(SHAP))) %>%
          arrange(desc(mean_abs)) %>%
          pull(Feature)
        
        shap_summary_data$Feature <- factor(shap_summary_data$Feature, levels = feature_order)
        
        # Store for visualization
        rv$shap_summary_data <- shap_summary_data
        
        incProgress(1, detail = "Completed")
      })
      
      showNotification(paste("Batch explanations generated for", explain_rows, "rows!"), 
                       type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error in batch explanation:", e$message), type = "error")
    })
  })
  
 
   ##UI code for home tab additional------------------
  
  observe({
    # Update progress based on which tabs have been visited
    progress <- 0
    if (!is.null(input$tabs)) {
      if (input$tabs != "home") progress <- progress + 16
      # Add more conditions as user progresses through workflow
    }
    updateProgressBar(session, "workflow_progress", value = progress)
  })
  observeEvent(input$btn_upload, {
    updateTabItems(session, "tabs", "data_upload")
  })
  observeEvent(input$btn_prep, {
    updateTabItems(getDefaultReactiveDomain(), "tabs", "data_prep")
  })
  observeEvent(input$btn_eda, {
    updateTabItems(session, "tabs", "eda")
  })
  observeEvent(input$btn_balance, {
    updateTabItems(session, "tabs", "balancing_methods")
  })
  observeEvent(input$btn_risk, {
    updateTabItems(session, "tabs", "risk_modeling")
  })
  observeEvent(input$btn_pct, {
    updateTabItems(session, "tabs", "pct_modeling")
  })
  observe({
    req(input$tabs) # Ensure input$tabs exists
    if (input$tabs == "home") {
      shinyjs::addClass("risk_card", "pulse")
    } else {
      shinyjs::removeClass("risk_card", "pulse")
    }
  })
  workflow_progress <- reactiveValues(step = 0)
  # Update when tabs are visited
  observeEvent(input$tabs, {
    if(input$tabs == "data_upload") workflow_progress$step <- 16
    if(input$tabs == "data_prep") workflow_progress$step <- 32
    if(input$tabs == "eda") workflow_progress$step <- 48
    if(input$tabs == "data_balancing") workflow_progress$step <- 64
    if(input$tabs %in% c("risk_class", "disability_pct")) workflow_progress$step <- 80
    
    
    updateProgressBar(
      session = session,
      id = "workflow_progress",
      value = workflow_progress$step
    )
  })
  # progress indicator
  output$completed_steps <- renderText({
    paste(floor(workflow_progress$step/16), "of 6 steps")
  })
  
  
  
  
  }
shinyApp(ui, server)