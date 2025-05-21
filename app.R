#Loading required packages
library(shiny)
library(htmltools)
library(shinyjs)
library(readr)
library(readxl)
library(openxlsx)
library(ggplot2)
library(plotly)
library(DT)
library(lubridate)
library(dplyr)
library(shinyBS)
library(tidyr)

library(purrr)
# Loading the dataset
pcrs_data <- readRDS("PCRS.rds")
#pcrs_data <- read_csv("PCRS.csv")
#pcrs_data <- readxl::read_xlsx("PCRS.xlsx")  

# rendering the dates intoto required format
pcrs_data$mth_yr <- as.Date(paste0(pcrs_data$mth_yr, "01"), format = "%Y%m%d")
pcrs_data$mth_yr <- ymd(pcrs_data$mth_yr)
pcrs_data$year <- year(pcrs_data$mth_yr)
pcrs_data$mth_yr_display <- format(pcrs_data$mth_yr, "%b %Y")

# UI Logic
ui <- fluidPage(
  useShinyjs(),  
  
  #customizing fonts size and colour and positions of ui elements
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Great+Vibes&display=swap"),
    tags$style(HTML("
      .selectize-input::placeholder {
        font-style: italic;
        color: #999999;
      }
      .selectize-input.items.not-full.has-options > input::placeholder {
        font-style: italic;
        color: #999999;
      }
      .hover-tooltip {
        position: relative;
        display: inline-block;
        z-index: 10; 
      }
      .hover-tooltip .tooltip-text {
        visibility: hidden;
        width: 300px; 
        background-color: #f9f9f9;
        color: #000000; 
        text-align: left;
        border-radius: 6px;
        padding: 10px;
        position: absolute;
        z-index: 1000; 
        bottom: 103%; 
        left: 33%;
        transform: translateX(-50%);
        opacity: 0;
        transition: opacity 0.3s;
        box-shadow: 0px 0px 10px rgba(0, 0, 0, 0.2);
        white-space: normal; 
        border: 1px solid #333333; 
        pointer-events: none; 
      }
      .hover-tooltip:hover .tooltip-text {
        visibility: visible;
        opacity: 1;
        pointer-events: auto; 
      }
      

#toggleCost_tooltip,
#toggleUnitCost_tooltip,
#toggleCostRate_tooltip {
  bottom: 110%; 
  right: 0; 
  width: 300px; 
  transform: translateX(20px); 
  background-color: #ffffff; 
  border: 1px solid #333333;
  opacity: 1; 
  z-index: 10000; 
  background-clip: border-box; 
}


  
      .hover-tooltip .info-icon {
        font-family: 'Great Vibes', cursive; 
        font-size: 14px; 
        font-weight: bold; 
        font-style: italic; 
        cursor: pointer; 
        color: black; 
        position: relative;
        top: -5px; 
        margin-left: 5px; 
      }
    "))
  ),
  
  # App header with description
  div(
    class = "app-header",
    style = "margin-bottom: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 5px;",
    fluidRow(
      column(10,
             titlePanel("RxTrends 2.0"),
      ),
      column(2,
             div(style = "text-align: right;",
                 actionButton("showHelp", "Help & Documentation", 
                              class = "btn-info",
                              style = "margin-top: 20px;")
             )
      )
    )
  ),
  
  # help and documentation
  bsModal("helpModal", "RxTrends Help & Documentation", "showHelp", size = "large",
          tabsetPanel(
            tabPanel("Overview",
                     h4("About RxTrends 2.0"),
                     p("RxTrends 2.0 is designed to provide interactive visualisation and analysis of open data from Ireland on primary care prescribing. The tool uses data from the HSE-PCRS monthly datasets of Ireland's General Medical Services (GMS) scheme, Drugs Payment Scheme (DPS), Long-Term Illness (LTI) scheme and High-Tech Drug Scheme (HTS) (available at https://www.sspcrs.ie/portal/annual-reporting/report/pharmacy) to enable dynamic exploration of prescribing patterns."),   
                     h4("Key Features"),
                     tags$ul(
                       tags$li("Interactive visualisation of prescribing trends"),
                       tags$li("Cost analysis with inflation adjustment"),
                       tags$li("Scheme eligible population-adjusted prescribing and cost rates for GMS scheme"),
                       tags$li("Flexible time period selection"),
                       tags$li("Comparison of metrics across medications, therapeutic groups, and systems")
                     )
            ),
            tabPanel("Using the Tool",
                     h4("Step-by-Step Guide"),
                     tags$ol(
                       tags$li("Choose Scheme, medications, therapeutic groups, and/or physiological systems to analyse"),
                       tags$li("Select the time period (monthly or yearly)"),
                       tags$li("Use the comparison feature to analyse relationships between medications/therapeutic groups and therapeutic groups/physiological systems"),
                       tags$li("Export of selected items data using the download button")
                     ),
                     
                     h4("Tips for Effective Use"),
                     tags$ul(
                       tags$li("Begin typing the name of a medication or therapeutic/physiological group in dropdown menus to search and allow for quick selection"),
                       tags$li("Compare related items to explore proportions"),
                       tags$li("Switch between original and adjusted costs for different perspectives"),
                       tags$li("Use the monthly view for detailed patterns and the yearly view for less granular visualisation")
                     ),
                     h4("Citing this tool"),
                     tags$ul(
                       tags$li("If citing this tool in your work, please include the following reference: Hassan Ali A, Moriarty F. RxTrends: An R-based Shiny Application for Visualising Open Data on Prescribed Medications in Ireland. Zenodo; 2025. https://doi.org/10.5281/zenodo.14726890"),
                       tags$li("If you would like to get in contact, please feel free to get in touch with us at ahmedhassanali@rcsi.ie and frankmoriarty@rcsi.ie.")
                     ),
                     
            ),
            tabPanel("Data Structure",
                     h4("Raw Data"),
                     p("This data reports on medications (which feature in the top 100 agents reported in the data source per month), and therapeutic groups (i.e. drug classes, categorised by the WHO Anatomical Therapeutic Chemical (ATC) second-level code. In addition, monthly data on the number of persons eligible for the GMS scheme are available from the same source)."),  
                     h4("Output Specifications"),
                     p("The tool generates the following types of outputs, with up to 5 tabs and statistical summaries. Each tab includes analysis of a different metric: prescribing frequency, cost, cost per prescribing, prescribing rate per 1,000 GMS eligible persons (if GMS scheme is selected), and cost rate per 1,000 GMS eligible persons (if GMS scheme is selected):"),
                     tags$ul(
                       tags$li(strong("Prescribing Frequency:"), " It is the number of times a medicine was dispensed per month, and corresponds to the variable “Prescribing Frequency” in the source data."),
                       tags$li(strong("Cost Analysis:"), " It is the total ingredient costs of dispensing of a medicine in euro and corresponds to the variable “Ingredient Cost” in the source file."),
                       tags$li(strong("Cost per prescribing:"), " It is the mean ingredient cost per dispensing of a medicine, and is derived by dividing the ”Ingredient Cost” variable by the ”Prescribing Frequency” variable in the source data."),
                       tags$li(strong("Prescribing rate and Cost rate:"), " are the number of dispensing/ingredient cost of a medicine per 1,000 GMS eligible persons. They are derived by dividing the “Prescribing Frequency” or “Ingredient Cost” variables by the number of GMS eligible persons and multiplying by 1,000. These are only available for the GMS scheme data, as other schemes’ eligibility data do not allow reliable interpretation of rates."),
                       tags$li(strong("Statistical Measures:"), " Maximum, minimum, median, and IQR for selected metrics")
                     ),
                     p("The downloadable datasets are provided in Comma Separated Values (.csv) format, including the following fields:"),
                     tags$ul(
                       tags$li(strong("name (String):"), "The name of the selected medication(s), therapeutic group(s), or system(s) with corresponding ATC code between brackets (e.g., Ranitidine (A02BA02))."),
                       tags$li(strong("Prescribing_Frequency (Float):"), "The number of prescription items dispensed during the selected period."),
                       tags$li(strong("Ingredient Cost € (Float):"), "The total ingredient cost (€) associated with the dispensed prescriptions."),
                       tags$li(strong("Prescribing Unit Cost € (Float):"), "The average ingredient cost per prescribing event (€)."),
                       tags$li(strong("dispensing_rate_per_1000 (Float):"), "The rate of prescribing frequency normalized per 1,000 GMS eligible patients in the selected period."),
                       tags$li(strong("cost_rate_per_1000 (Float):"), "The rate of ingredient cost normalized per 1,000 GMS eligible patients in the selected period."),
                       tags$li(strong("Date (YYYY-MM format):"), "The month or year associated with the data point."),
                       tags$li(strong("CPIallitems (Float):"), "The Consumer Price Index (CPI) for all items during the selected period relative to October 2024, representing general inflation at that time (baseline = 100)."),
                       tags$li(strong("Adjusted_Ingredient Cost € (Float):"), "The total ingredient cost (€) adjusted for inflation using the CPI."),
                       tags$li(strong("adjusted_Prescribing Unit Cost € (Float):"), "The average ingredient cost per prescribing event (€) after adjusting for inflation using the CPI."),
                       tags$li(strong("adjusted_cost_rate_per_1000 (Float):"), "The inflation-adjusted rate of ingredient cost normalized per 1,000 GMS eligible patients in the selected period.")
                       
                     ),    
                     
            )
            
            
            
            
          )
  ),
  
  
  fluidRow(
    column(1,  
           selectizeInput("selectedScheme", "Scheme:",
                          choices = c("GMS", "LTI", "DPS", "HTS"),  
                          selected = "GMS",
                          multiple = FALSE,
                          options = list(placeholder = "Select a Scheme")
           )),
    column(3, 
           selectizeInput("selectedMedication", "Medication:",
                          choices = NULL, 
                          selected = NULL,
                          multiple = TRUE,
                          options = list(placeholder = "Type or select")
           )
    ),
    column(3,  
           selectizeInput("selectedATC", "Therapeutic Group:",
                          choices = NULL,  
                          selected = NULL,
                          multiple = TRUE,
                          options = list(placeholder = "Type or select")
           )
    ),
    column(3,  
           selectizeInput("selectedSystem", "System:",
                          choices = NULL,  
                          selected = NULL,
                          multiple = TRUE,
                          options = list(placeholder = "Type or select")
           )
    ),
    column(2,
           div(class = "hover-tooltip",
               actionButton("compareButton", "Compare Selected", 
                            class = "btn-primary", 
                            style = "width:100%; margin-top: 20px; padding: 8px 12px;"), 
               span(class = "tooltip-text", textOutput("infoText"))
           ),
           div(style = "margin-top: 10px;",
               actionButton("clearButton", "Clear Selected", 
                            class = "btn-warning", 
                            style = "width:69%; padding: 8px 12px;")
           )
    )
  ),
  # Input of Date 
  fluidRow(
    column(3,
           radioButtons("dateInputType", "Choose Trend Interval:",
                        choices = c("Monthly Trends" = "slider", "Yearly Trends" = "sliderYearly"),
                        selected = "slider", inline = TRUE)
    ),
    column(6,
           conditionalPanel(
             condition = "input.dateInputType == 'slider'",
             sliderInput("dateRange", "",
                         min = as.Date("2016-01-01"),
                         max = as.Date("2024-12-01"),
                         value = c(as.Date("2016-01-01"), as.Date("2024-12-01")),
                         timeFormat = "%b %Y",  
                         step = 1,  
                         width = '70%')  
           ),
           conditionalPanel(
             condition = "input.dateInputType == 'sliderYearly'",
             sliderInput("yearRange", "Select Year Range:",
                         min = 2016,
                         max = 2024,
                         value = c(2016, 2024),
                         sep = "",  
                         step = 1,
                         ticks = FALSE, 
                         #animate = TRUE, 
                         width = '70%')  
           )
    )
    
  ),
  
  
  # Output Tab panels
  tabsetPanel(
    type = "tabs",
    id = "mainTabs",  
    tabPanel(
      HTML('<div style="display: flex; align-items: center;">Prescribing Frequency <div class="hover-tooltip"><span class="info-icon">i</span><span class="tooltip-text">Prescribing frequency is the number of times a medicine was dispensed per month or year (i.e. based on your selection). Max, Min, Median, and IQR are the statistical measures across the selected period.</span></div></div>'),
      plotlyOutput("DispensingPlot"), 
      dataTableOutput("DispensingStats"),
      uiOutput("dispensingDesc")
    ),
    tabPanel(
      HTML('<div style="display: flex; align-items: center;">Cost <div class="hover-tooltip"><span class="info-icon">i</span><span class="tooltip-text">Cost refers to the cost in euros (€) for the prescribed items per month or year (i.e., based on your selection). Use the toggle button above to switch to cost data adjusted for inflation, based on the consumer price index of December 2024. Max, Min, Median, and IQR are the statistical measures across the selected period.</span></div></div>'),
      div(class = "hover-tooltip",
          actionButton("toggleCost", "Switch to Adjusted Cost", style = "background-color: #dc3545; color: white;"),
          span(class = "tooltip-text", 
               id = "toggleCost_tooltip",  
               "Costs are adjusted for inflation using the monthly Consumer Price Index (CPI), normalised to the most recent month in the data (i.e., December 2024). For more information, visit: https://visual.cso.ie/?body=entity/cpicalculator")
      ),
      plotlyOutput("costPlot"), 
      dataTableOutput("costStats"),
      uiOutput("costDesc")
    ),
    tabPanel(
      HTML('<div style="display: flex; align-items: center;">Cost Per Prescribing <div class="hover-tooltip"><span class="info-icon">i</span><span class="tooltip-text">Cost Per Prescribing is the average cost in euros (€) per dispensing of an item per month or year (i.e., based on your selection). Use the toggle button above to switch to cost per prescribing data adjusted for inflation, based on the consumer price index of December 2024. Max, Min, Median, and IQR are the statistical measures across the selected period.</span></div></div>'),
      div(class = "hover-tooltip",
          actionButton("toggleUnitCost", "Switch to Adjusted Cost Per Prescribing", style = "background-color: #dc3545; color: white;"),
          span(class = "tooltip-text", 
               id = "toggleUnitCost_tooltip",  
               "Costs are adjusted for inflation using the monthly Consumer Price Index (CPI), normalised to the most recent month in the data (i.e., December 2024). For more information, visit: https://visual.cso.ie/?body=entity/cpicalculator")
      ),
      plotlyOutput("unitcostPlot"), 
      dataTableOutput("unitcostStats"),
      uiOutput("unitcostDesc")
    ),
    
    tabPanel(
      HTML('<div style="display: flex; align-items: center;">Prescribing rate <div class="hover-tooltip"><span class="info-icon">i</span><span class="tooltip-text">Prescribing rate is the rate of prescriptions of an item per 1000 eligible persons per month or year (i.e. based on your selection). Max, Min, Median, and IQR are the statistical measures across the selected period.</span></div></div>'),
      plotlyOutput("freqratetPlot"), 
      dataTableOutput("freqratetStats"),
      uiOutput("freqratetDesc")
    ),
    tabPanel(
      HTML('<div style="display: flex; align-items: center;">Cost Rate <div class="hover-tooltip"><span class="info-icon">i</span><span class="tooltip-text">Cost rate is the cost rate of an item per 1000 eligible persons in euros (€). Use the toggle button above to switch to cost rate data adjusted for inflation, based on the consumer price index of December 2024. Max, Min, Median, and IQR are the statistical measures across the selected period.</span></div></div>'),
      div(class = "hover-tooltip",
          actionButton("toggleCostRate", "Switch to Adjusted Cost Rate", style = "background-color: #dc3545; color: white;"),
          span(class = "tooltip-text", 
               id = "toggleCostRate_tooltip",  
               "Costs are adjusted for inflation using the monthly Consumer Price Index (CPI), normalised to the most recent month in the data (i.e., December 2024). For more information, visit: https://visual.cso.ie/?body=entity/cpicalculator")
      ),
      plotlyOutput("costratetPlot"), 
      dataTableOutput("costratetStats"),
      uiOutput("costratetDesc")
      
    )
  ),
  
  
  
  # Download button for filtered dataset
  fluidRow(
    column(12,
           downloadButton("downloadData", "Download Filtered Data", class = "btn-success", style = "width:100%;"))
  ),
  
  
  fluidRow(
    column(12, uiOutput("plotControlsDesc"))
  )
)
# debuging 
textOutput("debugInfo")



# Server logic
server <- function(input, output, session) {
  
  # show & hide tab panel based on seleceted schemes 
  shinyjs::hide(selector = "#mainTabs li:nth-child(4)")  
  shinyjs::hide(selector = "#mainTabs li:nth-child(5)")  
  
  observeEvent(input$selectedScheme, {
    if (input$selectedScheme == "GMS") {
      
      shinyjs::show(selector = "#mainTabs li:nth-child(4)") 
      shinyjs::show(selector = "#mainTabs li:nth-child(5)") 
    } else {
      
      shinyjs::hide(selector = "#mainTabs li:nth-child(4)")  
      shinyjs::hide(selector = "#mainTabs li:nth-child(5)")  
      
      
      if (input$mainTabs %in% c("Prescribing rate", "Cost Rate")) {
        updateTabsetPanel(session, "mainTabs", selected = "Prescribing Frequency")
      }
    }
  }, ignoreInit = FALSE)  # Ensure this runs when the app starts
  
  
  
  # Reactive values to store the current selections
  storedSelections <- reactiveValues(selectedMedication = NULL, selectedATC = NULL, selectedSystem = NULL, selectedScheme = NULL, dateRange = NULL, yearRange = c(2016, 2024))
  
  
  
  
  # Update selected inputs based on current data sorting and filtering
  observe({
    filtered_data <- pcrs_data %>%
      filter(Scheme == input$selectedScheme) %>%
      arrange(`ATC code`)
    
    med_choices <- unique(filtered_data$name[filtered_data$Type == "Medication"])
    atc_choices <- unique(filtered_data$name[filtered_data$Type == "ATC"])
    system_choices <- unique(filtered_data$name[filtered_data$Type == "System"])
    
    isolate({
      updateSelectizeInput(session, "selectedMedication", choices = med_choices, selected = storedSelections$selectedMedication)
      updateSelectizeInput(session, "selectedATC", choices = atc_choices, selected = storedSelections$selectedATC)
      updateSelectizeInput(session, "selectedSystem", choices = system_choices, selected = storedSelections$selectedSystem)
    })
  })
  
  # Respond to Scheme selection changes
  observeEvent(input$selectedScheme, {
    updateSelectizeInput(session, "selectedScheme", selected = input$selectedScheme)
    storedSelections$selectedScheme <- input$selectedScheme
  })
  
  # Toggle cost, unit cost & cost rates and adjust UI based on the selection
  observeEvent(input$toggleCost, {
    shinyjs::toggle(input$toggleCost %% 2 == 0, "toggleCost_tooltip")
    new_label <- ifelse(input$toggleCost %% 2 == 0, "Switch to Adjusted Cost", "Switch to Original Cost")
    updateActionButton(session, "toggleCost", label = new_label)
  })
  
  
  observeEvent(input$toggleUnitCost, {
    shinyjs::toggle(input$toggleUnitCost %% 2 == 0, "toggleUnitCost_tooltip")
    new_label <- ifelse(input$toggleUnitCost %% 2 == 0, "Switch to Adjusted Cost Per Prescribing", "Switch to Original Cost Per Prescribing")
    updateActionButton(session, "toggleUnitCost", label = new_label)
  })
  
  
  observeEvent(input$toggleCostRate, {
    shinyjs::toggle(input$toggleCostRate %% 2 == 0, "toggleCostRate_tooltip")
    new_label <- ifelse(input$toggleCostRate %% 2 == 0, "Switch to Adjusted Cost Rate", "Switch to Original Cost Rate")
    updateActionButton(session, "toggleCostRate", label = new_label)
  })
  
  # Clear all selections
  observeEvent(input$clearButton, {
    storedSelections$selectedMedication <- NULL
    storedSelections$selectedATC <- NULL
    storedSelections$selectedSystem <- NULL
    storedSelections$selectedScheme <- NULL
    storedSelections$dateRange <- c(min(pcrs_data$mth_yr), max(pcrs_data$mth_yr))
    storedSelections$yearRange <- c(2016, 2024)
    
    isolate({
      updateSelectizeInput(session, "selectedMedication", selected = character(0))
      updateSelectizeInput(session, "selectedATC", selected = character(0))
      updateSelectizeInput(session, "selectedSystem", selected = character(0))
      updateSelectizeInput(session, "selectedScheme", selected = NULL)
    })
    
    updateRadioButtons(session, "dateInputType", selected = "slider")
    updateSliderInput(session, "dateRange", min = min(pcrs_data$mth_yr), max = max(pcrs_data$mth_yr), value = c(min(pcrs_data$mth_yr), max(pcrs_data$mth_yr)), timeFormat = "%b %Y")
    updateSliderInput(session, "yearRange", value = c(2016, 2024))
  })
  
  # Filter data based on Scheme and other criteria
  filteredData <- reactive({
    data <- pcrs_data %>%
      filter(Scheme == input$selectedScheme)
    
    
    if (input$dateInputType == "slider") {
      start_date <- floor_date(input$dateRange[1], "month")
      end_date <- ceiling_date(input$dateRange[2], "month") - days(1)
      
      data_filtered <- data %>%
        filter(mth_yr >= start_date & mth_yr <= end_date)
      
    } else {
      start_year <- as.numeric(input$yearRange[1])
      end_year <- as.numeric(input$yearRange[2])
      
      data_filtered <- data %>%
        filter(year >= start_year & year <= end_year) %>%
        group_by(name, Type, year) %>%
        summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = 'drop')
    }
    
    
    data_filtered <- data_filtered %>%
      filter(
        (name %in% input$selectedMedication & Type == "Medication") |
          (name %in% input$selectedATC & Type == "ATC") |
          (name %in% input$selectedSystem & Type == "System")
      )
    
    return(data_filtered)
  })
  
  
  
  #logic for "Compare" option
  comparisonTrigger <- reactiveVal(FALSE)
  costToggle <- reactiveVal(FALSE)
  unitCostToggle <- reactiveVal(FALSE)
  costRateToggle <- reactiveVal(FALSE)
  
  observe({
    medCount <- length(input$selectedMedication)
    atcCount <- length(input$selectedATC)
    systemCount <- length(input$selectedSystem)
    totalSelections <- medCount + atcCount + systemCount
    
    
    isButtonActive <- (totalSelections == 2) && (medCount <= 1) && (atcCount <= 1) && (systemCount <= 1)
    shinyjs::toggleState("compareButton", isButtonActive)
    
    
    if (totalSelections > 2 || medCount > 1 || atcCount > 1 || systemCount > 1) {
      updateActionButton(session, "compareButton", label = "Compare Selected")
    } else if (comparisonTrigger()) {
      updateActionButton(session, "compareButton", label = "Return to Individual Data")
    }
  })
  
  observeEvent(input$compareButton, {
    if (comparisonTrigger()) {
      comparisonTrigger(FALSE)  
      updateActionButton(session, "compareButton", label = "Compare Selected")
    } else {
      comparisonTrigger(TRUE)  
      updateActionButton(session, "compareButton", label = "Return to Individual Data")
    }
  })
  
  observeEvent(input$clearButton, {
    updateSelectizeInput(session, "selectedMedication", selected = character(0))
    updateSelectizeInput(session, "selectedATC", selected = character(0))
    updateSelectizeInput(session, "selectedSystem", selected = character(0))
    updateRadioButtons(session, "dateInputType", selected = "slider")
    updateSliderInput(session, "dateRange",
                      min = min(pcrs_data$mth_yr),
                      max = max(pcrs_data$mth_yr),
                      value = c(min(pcrs_data$mth_yr), max(pcrs_data$mth_yr)),
                      timeFormat = "%b %Y")  
    updateSliderInput(session, "yearRange", value = c(2016, 2024))
    comparisonTrigger(FALSE)
    costToggle(FALSE)
    unitCostToggle(FALSE)
    costRateToggle(FALSE)
    updateActionButton(session, "compareButton", label = "Compare Selected")
  })
  
  output$debugInfo <- renderText({
    paste("Selected scheme:", input$selectedScheme)
  })
  
  output$infoText <- renderText({
    medCount <- length(input$selectedMedication)
    atcCount <- length(input$selectedATC)
    systemCount <- length(input$selectedSystem)
    totalSelections <- medCount + atcCount + systemCount
    
    if (totalSelections > 2) {
      "More than two selections made. Compare function is disabled."
    } else if (comparisonTrigger()) {
      "Deactivate Compare option and return to individual plots of selected items."
    } else if (totalSelections == 2 && (medCount <= 1) && (atcCount <= 1) && (systemCount <= 1)) {
      "Click 'Compare Selected' to compare the selected categories."
    } else if (medCount >= 2 || atcCount >= 2 || systemCount >= 2) {
      "Two selections made within the same category. Compare function is disabled."
    } else {
      "Select exactly one option from two different categories to enable comparison."
    }
  })
  
  observeEvent(input$toggleCost, {
    costToggle(!costToggle())
    if (costToggle()) {
      runjs("$('#toggleCost').css('background-color', '#28a745').css('color', white);")
      updateActionButton(session, "toggleCost", label = "Switch to Original Cost")
    } else {
      runjs("$('#toggleCost').css('background-color', '#dc3545').css('color', 'white');")
      updateActionButton(session, "toggleCost", label = "Switch to Adjusted Cost")
    }
  })
  
  observeEvent(input$toggleUnitCost, {
    unitCostToggle(!unitCostToggle())
    if (unitCostToggle()) {
      runjs("$('#toggleUnitCost').css('background-color', '#28a745').css('color', white);")
      updateActionButton(session, "toggleUnitCost", label = "Switch to Original Cost Per Prescribing")
    } else {
      runjs("$('#toggleUnitCost').css('background-color', '#dc3545').css('color', 'white');")
      updateActionButton(session, "toggleUnitCost", label = "Switch to Adjusted Cost Per Prescribing")
    }
  })
  
  observeEvent(input$toggleCostRate, {
    costRateToggle(!costRateToggle())
    if (costRateToggle()) {
      runjs("$('#toggleCostRate').css('background-color', '#28a745').css('color', white);")
      updateActionButton(session, "toggleCostRate", label = "Switch to Original Cost Rate")
    } else {
      runjs("$('#toggleCostRate').css('background-color', '#dc3545').css('color', 'white');")
      updateActionButton(session, "toggleCostRate", label = "Switch to Adjusted Cost Rate")
    }
  })
  
  #Staticics for figures 
  calculateStatsPerMedication <- function(data, column, yAxisTitle) {
    stats <- data %>%
      group_by(name) %>%
      summarise(
        Max = if (all(is.na(.data[[column]]))) NA else round(max(.data[[column]], na.rm = TRUE), 2),
        Min = if (all(is.na(.data[[column]]))) NA else round(min(.data[[column]], na.rm = TRUE), 2),
        Median = if (all(is.na(.data[[column]]))) NA else round(median(.data[[column]], na.rm = TRUE), 2),
        IQR = if (all(is.na(.data[[column]]))) NA else round(IQR(.data[[column]], na.rm = TRUE), 2))
    
    # Format the euro columns
    if (grepl("Cost", yAxisTitle, ignore.case = TRUE)) {
      stats <- stats %>%
        mutate(
          Max = ifelse(is.na(Max), Max, paste0(Max, " €")),
          Min = ifelse(is.na(Min), Min, paste0(Min, " €")),
          Median = ifelse(is.na(Median), Median, paste0(Median, " €")),
          IQR = ifelse(is.na(IQR), IQR, paste0(IQR, " €"))
        )
    }
    
    return(stats)
  }
  
  
  # PlotS generation and characterisrics
  
  outputList <- list(
    DispensingPlot = list(column = "Prescribing_Frequency", title = "<interval> Prescribing Frequency"),
    costPlot = list(column = "Ingredient Cost €", title = "<interval> Prescribing Cost (€)"),
    unitcostPlot = list(column = "Prescribing Unit Cost €", title = "<interval> Cost Per Prescribing (€)"),
    freqratetPlot = list(column = "dispensing_rate_per_1000", title = "<interval> Prescribing Rate (Per 1000 eligible persons)"),
    costratetPlot = list(column = "cost_rate_per_1000", title = "<interval> Cost Rate (Per 1000 eligible persons) (€)")
  )
  
  
  for (name in names(outputList)) {
    local({
      plotDetails <- outputList[[name]]
      columnName <- plotDetails$column
      yAxisTitle <- plotDetails$title
      plotName <- name
      statsName <- gsub("Plot", "Stats", name)
      
      output[[plotName]] <- renderPlotly({
        
        intervalType <- if (!is.null(input$dateInputType) && input$dateInputType == "slider") "Monthly" else "Yearly"
        yAxisTitle <- gsub("<interval>", intervalType, yAxisTitle)
        
        
        if (is.null(input$dateInputType) || is.null(input$dateRange) || is.null(input$yearRange)) {
          return(plotly_empty())  
        }
        
        
        medCount <- length(input$selectedMedication)
        atcCount <- length(input$selectedATC)
        systemCount <- length(input$selectedSystem)
        totalSelections <- medCount + atcCount + systemCount
        
        
        if (input$dateInputType == "slider") {
          start_date <- input$dateRange[1]
          end_date <- input$dateRange[2]
          single_date <- !is.null(start_date) && start_date == end_date
          date_label <- "Date"
        } else {
          start_year <- input$yearRange[1]
          end_year <- input$yearRange[2]
          single_date <- !is.null(start_year) && start_year == end_year
          date_label <- "Year"
        }
        
        
        if ((input$dateInputType == "slider" && (is.null(start_date) || is.null(end_date))) ||
            (input$dateInputType == "sliderYearly" && (is.null(start_year) || is.null(end_year)))) {
          return(plotly_empty()) 
        }
        
        # choose btw a bar or line
        plot_type <- if (single_date) "bar" else "line"
        
        # warning showing logic
        show_warning <- FALSE
        warning_message <- ""
        
        # Handle comparison logic
        if (comparisonTrigger() && totalSelections == 2 && (medCount <= 1) && (atcCount <= 1) && (systemCount <= 1)) {
          merge_by <- if (input$dateInputType == "slider") "mth_yr" else "year"
          
          if (medCount == 1 && atcCount == 1 && systemCount == 0) {
            data1 <- filteredData() %>% filter(name == input$selectedMedication & Type == "Medication")
            data2 <- filteredData() %>% filter(name == input$selectedATC & Type == "ATC")
            title_suffix <- "a Medication's with a Therapeutic Group's Metrics"
          } else if (medCount == 1 && systemCount == 1 && atcCount == 0) {
            data1 <- filteredData() %>% filter(name == input$selectedMedication & Type == "Medication")
            data2 <- filteredData() %>% filter(name == input$selectedSystem & Type == "System")
            title_suffix <- "a Medication's with a System's Metrics"
          } else if (atcCount == 1 && systemCount == 1 && medCount == 0) {
            data1 <- filteredData() %>% filter(name == input$selectedATC & Type == "ATC")
            data2 <- filteredData() %>% filter(name == input$selectedSystem & Type == "System")
            title_suffix <- "a Therapeutic Group's with a System's Metrics"
          } else {
            return(plotly_empty())  
          }
          
          if (nrow(data1) == 0 || nrow(data2) == 0) {
            empty_data_name <- if (nrow(data1) == 0) input$selectedMedication else if (nrow(data2) == 0) input$selectedATC else input$selectedSystem
            
            message <- if (input$dateInputType == "slider") {
              sprintf("No data is available for the selected item: <b><i>%s</i></b> for the period <b><i>%s to %s</i></b>.",
                      empty_data_name, format(start_date, "%b %Y"), format(end_date, "%b %Y"))
            } else {
              sprintf("No data is available for the selected item: <b><i>%s</i></b> for the period <b><i>%d to %d</i></b>.",
                      empty_data_name, start_year, end_year)
            }
            
            return(plotly_empty() %>%
                     layout(
                       shapes = list(
                         list(
                           type = "rect",
                           fillcolor = "orange",  
                           line = list(color = "darkorange"), 
                           x0 = 0.1, x1 = 0.9,  
                           y0 = 0.45, y1 = 0.55,  
                           xref = "paper", yref = "paper"
                         )
                       ),
                       title = list(
                         text = message,
                         x = 0.5,
                         y = 0.5,
                         xanchor = 'center',
                         yanchor = 'middle',
                         font = list(size = 14, color = 'black'),
                         xref = "paper", yref = "paper"
                       ),
                       xaxis = list(visible = FALSE),
                       yaxis = list(visible = FALSE),
                       margin = list(t = 100)  
                     ))
          }
          
          if (input$dateInputType != "slider") {
            # Get all years in the selected range
            all_years <- seq(input$yearRange[1], input$yearRange[2])
            
            # Determine selected items
            med_selected <- input$selectedMedication
            atc_selected <- input$selectedATC
            system_selected <- input$selectedSystem
            
            # Load data for selected categories
            original_data1 <- NULL
            original_data2 <- NULL
            original_data3 <- NULL
            
            if (!is.null(med_selected) && !is.null(atc_selected)) {
              original_data1 <- pcrs_data %>%
                filter(Scheme == input$selectedScheme,
                       name == med_selected,
                       Type == "Medication",
                       year >= input$yearRange[1],
                       year <= input$yearRange[2])
              
              original_data2 <- pcrs_data %>%
                filter(Scheme == input$selectedScheme,
                       name == atc_selected,
                       Type == "ATC",
                       year >= input$yearRange[1],
                       year <= input$yearRange[2])
              
            } else if (!is.null(med_selected) && !is.null(system_selected)) {
              original_data1 <- pcrs_data %>%
                filter(Scheme == input$selectedScheme,
                       name == med_selected,
                       Type == "Medication",
                       year >= input$yearRange[1],
                       year <= input$yearRange[2])
              
              original_data2 <- pcrs_data %>%
                filter(Scheme == input$selectedScheme,
                       name == system_selected,
                       Type == "System",
                       year >= input$yearRange[1],
                       year <= input$yearRange[2])
              
            } else if (!is.null(atc_selected) && !is.null(system_selected)) {
              original_data1 <- pcrs_data %>%
                filter(Scheme == input$selectedScheme,
                       name == atc_selected,
                       Type == "ATC",
                       year >= input$yearRange[1],
                       year <= input$yearRange[2])
              
              original_data2 <- pcrs_data %>%
                filter(Scheme == input$selectedScheme,
                       name == system_selected,
                       Type == "System",
                       year >= input$yearRange[1],
                       year <= input$yearRange[2])
            }
            
            # Count data per year
            count_data1 <- original_data1 %>% count(year, name = "count1")
            count_data2 <- original_data2 %>% count(year, name = "count2")
            
            # Create base comparison
            comparison <- tibble(year = all_years) %>%
              left_join(count_data1, by = "year") %>%
              left_join(count_data2, by = "year") %>%
              mutate(
                count1 = ifelse(is.na(count1), 0, count1),
                count2 = ifelse(is.na(count2), 0, count2),
                mismatch = (count1 != count2) & (count1 > 0) & (count2 > 0),
                lower_count_item = case_when(
                  count1 < count2 ~ if (!is.null(med_selected)) med_selected else atc_selected,
                  count2 < count1 ~ if (!is.null(atc_selected)) atc_selected else system_selected,
                  TRUE ~ NA_character_
                )
              )
            
            # Collect mismatch info
            mismatch_info <- comparison %>%
              filter(mismatch) %>%
              group_by(lower_count_item) %>%
              summarize(years = list(year), .groups = "drop")
            
            if (nrow(mismatch_info) > 0) {
              show_warning <- TRUE
              years_list <- mismatch_info %>%
                mutate(years_str = map_chr(years, ~paste(sort(.), collapse = ", "))) %>%
                pull(years_str) %>%
                paste(collapse = "; ")
              
              warning_message <- sprintf(
                "Check monthly trends; '%s' number of records in year(s) %s don't match the comparator's.",
                mismatch_info$lower_count_item[1],
                years_list
              )
            }
            
          }
          combined_data <- merge(data1, data2, by = merge_by, suffixes = c(".1", ".2"))
          combined_data$Proportion <- combined_data[[paste(columnName, "1", sep = ".")]] / combined_data[[paste(columnName, "2", sep = ".")]]
          
          
          x_axis <- if (merge_by == "year") as.factor(combined_data$year) else combined_data$mth_yr
          
          if (plot_type == "bar") {
            p <- ggplot(combined_data, aes(
              x = x_axis,
              y = Proportion,
              fill = name,
              text = paste(
                date_label, ":", if (merge_by == "mth_yr") format(!!sym(merge_by), "%b %Y") else !!sym(merge_by),
                "<br>Proportion:", round(Proportion, 3)
              )
            )) +
              geom_bar(stat = "identity", width = 0.2) +
              labs(
                title = "",
                x = date_label,
                y = sprintf("Proportion of %s", paste(strwrap(title_suffix, width = 40), collapse = "\n"))
              ) +
              theme_minimal() +
              scale_y_continuous(labels = scales::comma, expand = c(0, 0)) +
              expand_limits(y = 0)
            theme(aspect.Proportion = 1/2)  
          } else {
            p <- ggplot(combined_data, aes(
              x = x_axis,
              y = Proportion,
              group = 1,
              text = paste(
                date_label, ":", if (merge_by == "mth_yr") format(!!sym(merge_by), "%b %Y") else !!sym(merge_by),
                "<br>Proportion:", round(Proportion, 3)
              )
            )) +
              geom_line(linewidth = 0.5) +
              geom_point(size = 0.5) +
              labs(
                title = "",
                x = date_label,
                y = sprintf("Proportion of %s", paste(strwrap(title_suffix, width = 40), collapse = "\n"))
              ) +
              theme_minimal() +
              scale_y_continuous(labels = scales::comma, expand = c(0, 0)) +
              expand_limits(y = 0)
          }
          
          
          if (merge_by == "year") {
            p <- p + scale_x_discrete(name = "Year")  # Ensure that each year is shown correctly on the x-axis
          } else {
            p <- p + scale_x_date(date_labels = "%Y", date_breaks = "1 year")
          }
          
          plotly_plot <- ggplotly(p, tooltip = "text")
          
          if (show_warning) {
            plotly_plot <- plotly_plot %>%
              layout(
                annotations = list(
                  list(
                    x = 0.45,
                    y = 1.07,
                    xref = "paper",
                    yref = "paper",
                    text = paste("<b>", warning_message, "</b>"),
                    showarrow = FALSE,
                    font = list(size = 10, color = "black"),
                    align = "center",
                    bgcolor = "orange",
                    bordercolor = "darkorange",
                    borderwidth = 3
                  )
                )
              )
          }
          
          
          if (single_date && plot_type == "bar") {
            plotly_plot <- plotly_plot %>% layout(showlegend = FALSE)
          }
          
          return(plotly_plot)
          
        } else {
          
          data <- filteredData()
          
          
          plot_type <- if (single_date) "bar" else "line"
          
          if (plotName == "costPlot" && costToggle()) {
            columnName <- "Adjusted_Ingredient Cost €"
            yAxisTitle <- gsub("<interval>", if (input$dateInputType == "slider") "Monthly" else "Yearly", "<interval> Adjusted Prescribing Cost (€)")
          } else if (plotName == "unitcostPlot" && unitCostToggle()) {
            columnName <- "adjusted_Prescribing Unit Cost €"
            yAxisTitle <- gsub("<interval>", if (input$dateInputType == "slider") "Monthly" else "Yearly", "<interval> Adjusted Cost Per Prescribing (€)")
          } else if (plotName == "costratetPlot" && costRateToggle()) {
            columnName <- "adjusted_cost_rate_per_1000"
            yAxisTitle <- gsub("<interval>", if (input$dateInputType == "slider") "Monthly" else "Yearly", "<interval> Adjusted Cost Rate (€)")
          } else {
            
            yAxisTitle <- gsub("<interval>", if (input$dateInputType == "slider") "Monthly" else "Yearly", plotDetails$title)
          }
          
          if (nrow(data) > 0) {
            
            insert_line_breaks <- function(name, max_length = 20) {
              words <- unlist(strsplit(name, " "))
              new_name <- words[1]
              line_length <- nchar(new_name)
              
              for (i in 2:length(words)) {
                if (line_length + nchar(words[i]) + 1 > max_length) {
                  new_name <- paste(new_name, "<br>", words[i], sep = "")
                  line_length <- nchar(words[i])
                } else {
                  new_name <- paste(new_name, words[i], sep = " ")
                  line_length <- line_length + nchar(words[i]) + 1
                }
              }
              
              return(new_name)
            }
            
            
            data$name_for_legend <- sapply(data$name, insert_line_breaks)
            
            x_axis <- if (input$dateInputType == "slider") data$mth_yr else as.factor(data$year)
            
            if (plot_type == "bar") {
              p <- ggplot(data, aes(
                x = x_axis,
                y = !!sym(columnName),
                fill = name_for_legend,
                text = paste(
                  paste(date_label, ": ", if (input$dateInputType == "slider") format(x_axis, "%b %Y") else x_axis, sep = ""),
                  paste("<br>Name: ", name, sep = ""),
                  paste("<br>", yAxisTitle, ": ", round(!!sym(columnName), 2), sep = "")
                ),
                group = name
              )) +
                geom_bar(stat = "identity", width = 0.2) +
                labs(x = date_label, y = yAxisTitle) +
                theme_minimal() +
                scale_y_continuous(labels = scales::comma, expand = c(0, 0)) +
                expand_limits(y = 0)
              
            } else {
              p <- ggplot(data, aes(
                x = x_axis,
                y = !!sym(columnName),
                color = name_for_legend,
                text = paste(
                  paste(date_label, ": ", if (input$dateInputType == "slider") format(x_axis, "%b %Y") else x_axis, sep = ""),
                  paste("<br>Name: ", name, sep = ""),
                  paste("<br>", yAxisTitle, ": ", round(!!sym(columnName), 2), sep = "")
                ),
                group = name
              )) +
                geom_line(linewidth = 0.5) +
                geom_point(size = 0.5) +
                labs(x = date_label, y = yAxisTitle) +
                theme_minimal() +
                scale_y_continuous(labels = scales::comma, expand = c(0, 0)) +
                expand_limits(y = 0)
            }
            
            if (input$dateInputType == "slider") {
              p <- p + scale_x_date(date_labels = "%Y", date_breaks = "1 year")
            } else {
              p <- p + scale_x_discrete()
            }
            
            plotly_plot <- ggplotly(p, tooltip = "text") %>%
              layout(
                legend = list(
                  x = 1.05,
                  y = 1,
                  xanchor = "left",
                  yanchor = "top",
                  orientation = "v",
                  title = list(text = ""),  
                  font = list(size = 12),
                  itemwidth = 20,  
                  tracegroupgap = 5,  
                  valign = "top",  
                  width = 200  
                ),
                hoverlabel = list(
                  align = "left"  
                ),
                margin = list(t = 30)  
              )
            
            return(plotly_plot)
          } else {
            return(NULL)
          }
          
          
        }
      })
      
      # Generating statistical table
      output[[statsName]] <- renderDataTable({
        # Ensure intervalType is defined here
        intervalType <- if (input$dateInputType == "slider") "Monthly" else "Yearly"
        
        adjustedTitle <- if (plotName == "costPlot" && costToggle()) {
          paste(intervalType, "Adjusted Prescribing Cost (€)")
        } else if (plotName == "unitcostPlot" && unitCostToggle()) {
          paste(intervalType, "Adjusted Cost Per Prescribing (€)")
        } else if (plotName == "costratetPlot" && costRateToggle()) {
          paste(intervalType, "Adjusted Cost Rate (€)")
        } else {
          gsub("<interval>", intervalType, plotDetails$title)
        }
        
        
        intervalType <- if (input$dateInputType == "slider") "Monthly" else "Yearly"
        adjustedTitle <- gsub("<interval>", intervalType, adjustedTitle)
        
        
        currentColumn <- if (plotName == "costPlot" && costToggle()) {
          "Adjusted_Ingredient Cost €"
        } else if (plotName == "unitcostPlot" && unitCostToggle()) {
          "adjusted_Prescribing Unit Cost €"
        } else if (plotName == "costratetPlot" && costRateToggle()) {
          "adjusted_cost_rate_per_1000"
        } else {
          columnName
        }
        
        
        stats <- calculateStatsPerMedication(filteredData(), currentColumn, adjustedTitle)
        
        datatable(stats,
                  caption = htmltools::tags$caption(
                    style = 'caption-side: top; text-align: center; font-weight: bold;',
                    paste(adjustedTitle, "Statistics")
                  ),
                  options = list(
                    dom = 'tip',
                    language = list(search = 'Filter Results:')
                  )
        )
      })
      
      
      
    })
  }
  
  
  # download option
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered-data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if (comparisonTrigger()) {
        merge_by <- if ("mth_yr" %in% colnames(filteredData())) "mth_yr" else "year"
        
        combined_data <- merge(filteredData() %>% filter(name == input$selectedMedication & Type == "Medication"),
                               filteredData() %>% filter(name == input$selectedATC & Type == "ATC"), 
                               by = merge_by, suffixes = c(".1", ".2"))
        
        
        combined_data$`Proportion of Prescribing Frequencies` <- combined_data[[paste("Prescribing_Frequency", "1", sep = ".")]] / combined_data[[paste("Prescribing_Frequency", "2", sep = ".")]]
        combined_data$`Proportion of Costs` <- combined_data[[paste("Ingredient Cost €", "1", sep = ".")]] / combined_data[[paste("Ingredient Cost €", "2", sep = ".")]]
        combined_data$`Proportion of Costs Per Prescribing` <- combined_data[[paste("Prescribing Unit Cost €", "1", sep = ".")]] / combined_data[[paste("Prescribing Unit Cost €", "2", sep = ".")]]
        combined_data$`Proportion of Prescribing rates` <- combined_data[[paste("dispensing_rate_per_1000", "1", sep = ".")]] / combined_data[[paste("dispensing_rate_per_1000", "2", sep = ".")]]
        combined_data$`Proportion of Cost rates` <- combined_data[[paste("cost_rate_per_1000", "1", sep = ".")]] / combined_data[[paste("cost_rate_per_1000", "2", sep = ".")]]
        
        # Save as CSV
        write.csv(combined_data, file, row.names = FALSE)
      } else {
        # Save the filtered data as CSV
        write.csv(filteredData(), file, row.names = FALSE)
      }
    }
  )
}


# Run the application
shinyApp(ui = ui, server = server)
