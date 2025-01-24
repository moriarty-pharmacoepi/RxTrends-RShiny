
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

# reading dataset
pcrs_data <- readxl::read_xlsx("PCRS.xlsx")  

# Transfer dates to required format
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
        width: 350px; 
        background-color: #f9f9f9;
        color: #000000; 
        text-align: left;
        border-radius: 6px;
        padding: 10px;
        position: absolute;
        z-index: 1000; 
        bottom: 125%; 
        left: 50%;
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
  
  titlePanel("RxTrends"),
  
  # Inputs selection dropdown lits
  fluidRow(
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
    column(3,
           div(class = "hover-tooltip",
               actionButton("compareButton", "Compare Selected", class = "btn-primary", style = "width:100%; margin-top: 20px;"),
               span(class = "tooltip-text", textOutput("infoText"))
           ),
           div(style = "margin-top: 10px;",
               actionButton("clearButton", "Clear Selected", class = "btn-warning", style = "width:55%; font-size:15px; padding:5px 10px;"))
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
                         max = as.Date("2024-10-01"),
                         value = c(as.Date("2016-01-01"), as.Date("2024-10-01")),
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
  
  # Output Tabs
  tabsetPanel(type = "tabs",
              tabPanel(
                HTML('<div style="display: flex; align-items: center;">Prescribing Frequency <div class="hover-tooltip"><span class="info-icon">i</span><span class="tooltip-text">Prescribing frequency is the number of times a medicine was dispensed per month or year (i.e. based on your selection). Max, Min, Median, and IQR are the statistical measures across the selected period.</span></div></div>'),
                plotlyOutput("DispensingPlot"), 
                dataTableOutput("DispensingStats"),
                uiOutput("dispensingDesc")
              ),
              tabPanel(
                HTML('<div style="display: flex; align-items: center;">Cost <div class="hover-tooltip"><span class="info-icon">i</span><span class="tooltip-text">Cost refers to the cost in euros (€) for the prescribed items per month or year (i.e., based on your selection). Use the toggle button above to switch to cost data adjusted for inflation, based on the consumer price index of April 2024. Max, Min, Median, and IQR are the statistical measures across the selected period.</span></div></div>'),
                
                div(class = "hover-tooltip",
                    actionButton("toggleCost", "Switch to Adjusted Cost", style = "background-color: #dc3545; color: white;"),
                    span(class = "tooltip-text", 
                         id = "toggleCost_tooltip",  
                         "Costs are adjusted for inflation using the monthly Consumer Price Index (CPI), normalised to the most recent month in the data (i.e., July 2024). For more information, visit: https://visual.cso.ie/?body=entity/cpicalculator")
                ),
                
                plotlyOutput("costPlot"), 
                dataTableOutput("costStats"),
                uiOutput("costDesc")
              ),
              tabPanel(
                HTML('<div style="display: flex; align-items: center;">Cost Per Prescribing <div class="hover-tooltip"><span class="info-icon">i</span><span class="tooltip-text">Cost Per Prescribing is the average cost in euros (€) per dispensing of an item per month or year (i.e., based on your selection). Use the toggle button above to switch to cost per prescribing data adjusted for inflation, based on the consumer price index of April 2024. Max, Min, Median, and IQR are the statistical measures across the selected period.</span></div></div>'),
                
                div(class = "hover-tooltip",
                    actionButton("toggleUnitCost", "Switch to Adjusted Cost Per Prescribing", style = "background-color: #dc3545; color: white;"),
                    span(class = "tooltip-text", 
                         id = "toggleUnitCost_tooltip",  
                         "Costs are adjusted for inflation using the monthly Consumer Price Index (CPI), normalised to the most recent month in the data (i.e., July 2024). For more information, visit: https://visual.cso.ie/?body=entity/cpicalculator")
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
                HTML('<div style="display: flex; align-items: center;">Cost Rate <div class="hover-tooltip"><span class="info-icon">i</span><span class="tooltip-text">Cost rate is the cost rate of an item per 1000 eligible persons in euros (€). Use the toggle button above to switch to cost rate data adjusted for inflation, based on the consumer price index of April 2024. Max, Min, Median, and IQR are the statistical measures across the selected period.</span></div></div>'),
                
                div(class = "hover-tooltip",
                    actionButton("toggleCostRate", "Switch to Adjusted Cost Rate", style = "background-color: #dc3545; color: white;"),
                    span(class = "tooltip-text", 
                         id = "toggleCostRate_tooltip",  
                         "Costs are adjusted for inflation using the monthly Consumer Price Index (CPI), normalised to the most recent month in the data (i.e., July 2024). For more information, visit: https://visual.cso.ie/?body=entity/cpicalculator")
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
  
  # Zoom and Filter Descriptions
  fluidRow(
    column(12, uiOutput("plotControlsDesc"))
  )
)
# Server logic
server <- function(input, output, session) {
  
  # Reactive values to store the current selections
  storedSelections <- reactiveValues(selectedMedication = NULL, selectedATC = NULL, selectedSystem = NULL, dateRange = NULL, yearRange = c(2016, 2024))
  
  
  # Prevent unncessary updates of selectize inputs
  observe({
    
    data_sorted <- pcrs_data %>% arrange(`ATC code`)
    
    
    med_choices <- unique(data_sorted$name[data_sorted$Type == "Medication"])
    atc_choices <- unique(data_sorted$name[data_sorted$Type == "ATC"])
    system_choices <- unique(data_sorted$name[data_sorted$Type == "System"])
    
    
    isolate({
      updateSelectizeInput(session, "selectedMedication", choices = med_choices, selected = storedSelections$selectedMedication)
      updateSelectizeInput(session, "selectedATC", choices = atc_choices, selected = storedSelections$selectedATC)
      updateSelectizeInput(session, "selectedSystem", choices = system_choices, selected = storedSelections$selectedSystem)
    })
  })
  
  # Handling the toggleCost option 
  observeEvent(input$toggleCost, {
    
    if (input$toggleCost %% 2 == 1) {  
      runjs("$('#toggleCost').text('Switch to Original Cost').css({'background-color': '#28a745', 'color': 'white'});")
      shinyjs::show("toggleCost_tooltip")  
    } else { 
      runjs("$('#toggleCost').text('Switch to Adjusted Cost').css({'background-color': '#dc3545', 'color': 'white'});")
      shinyjs::hide("toggleCost_tooltip") 
    }
  })
  #  Handling the "Cost Per Prescribing" option
  observeEvent(input$toggleUnitCost, {
    
    if (input$toggleUnitCost %% 2 == 1) {  
      runjs("$('#toggleUnitCost').text('Switch to Original Cost Per Prescribing').css({'background-color': '#28a745', 'color': 'white'});")
      shinyjs::show("toggleUnitCost_tooltip") 
    } else { 
      runjs("$('#toggleUnitCost').text('Switch to Adjusted Cost Per Prescribing').css({'background-color': '#dc3545', 'color': 'white'});")
      shinyjs::hide("toggleUnitCost_tooltip") 
    }
  })
  
  #  Handling the "Cost Rate" option
  observeEvent(input$toggleCostRate, {
    
    if (input$toggleCostRate %% 2 == 1) {  
      runjs("$('#toggleCostRate').text('Switch to Original Cost Rate').css({'background-color': '#28a745', 'color': 'white'});")
      shinyjs::show("toggleCostRate_tooltip")  
    } else {  
      runjs("$('#toggleCostRate').text('Switch to Adjusted Cost Rate').css({'background-color': '#dc3545', 'color': 'white'});")
      shinyjs::hide("toggleCostRate_tooltip")  
    }
  })
  
  #Improving triggering on initialization and reasctivity 
  
  observeEvent(input$selectedMedication, { storedSelections$selectedMedication <- input$selectedMedication }, ignoreInit = TRUE)
  observeEvent(input$selectedATC, { storedSelections$selectedATC <- input$selectedATC }, ignoreInit = TRUE)
  observeEvent(input$selectedSystem, { storedSelections$selectedSystem <- input$selectedSystem }, ignoreInit = TRUE)
  observeEvent(input$dateRange, { storedSelections$dateRange <- input$dateRange }, ignoreInit = TRUE)
  observeEvent(input$yearRange, { storedSelections$yearRange <- input$yearRange }, ignoreInit = TRUE)
  
  
  observeEvent(input$clearButton, {
    storedSelections$selectedMedication <- NULL
    storedSelections$selectedATC <- NULL
    storedSelections$selectedSystem <- NULL
    storedSelections$dateRange <- c(min(pcrs_data$mth_yr), max(pcrs_data$mth_yr))
    storedSelections$yearRange <- c(2016, 2024)
    
    isolate({
      updateSelectizeInput(session, "selectedMedication", selected = character(0))
      updateSelectizeInput(session, "selectedATC", selected = character(0))
      updateSelectizeInput(session, "selectedSystem", selected = character(0))
    })
    
    #Handling dates on curves 
    updateRadioButtons(session, "dateInputType", selected = "slider")
    updateSliderInput(session, "dateRange", min = min(pcrs_data$mth_yr), max = max(pcrs_data$mth_yr), value = c(min(pcrs_data$mth_yr), max(pcrs_data$mth_yr)), timeFormat = "%b %Y")
    updateSliderInput(session, "yearRange", value = c(2016, 2024))
  })
  
  
  filteredData <- reactive({
    data <- pcrs_data
    
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
            count_data1 <- data1 %>% 
              group_by(year) %>% 
              summarize(n = n())
            
            count_data2 <- data2 %>% 
              group_by(year) %>% 
              summarize(n = n())
            
            
            combined_counts <- full_join(count_data1, count_data2, by = "year", suffix = c("_1", "_2"))
            combined_counts[is.na(combined_counts)] <- 0  
            
            
            if (any(combined_counts$n_1 != combined_counts$n_2)) {
              show_warning <- TRUE
              differing_years <- combined_counts$year[combined_counts$n_1 != combined_counts$n_2]
              
              
              correct_year <- differing_years[1]
              
              
              if (input$dateInputType != "slider") {
                correct_year <- correct_year - 1
              }
              
              warning_message <- sprintf(
                "Check monthly trends; '%s' number of records in year %d don’t match the comparator's.",
                ifelse(any(combined_counts$n_1 != combined_counts$n_2), input$selectedMedication, input$selectedATC),
                correct_year
              )
            }
          }
          
          
          combined_data <- merge(data1, data2, by = merge_by, suffixes = c(".1", ".2"))
          combined_data$ratio <- combined_data[[paste(columnName, "1", sep = ".")]] / combined_data[[paste(columnName, "2", sep = ".")]]
          
          
          x_axis <- if (merge_by == "year") as.factor(combined_data$year) else combined_data$mth_yr
          
          if (plot_type == "bar") {
            p <- ggplot(combined_data, aes(
              x = x_axis,
              y = ratio,
              fill = name,
              text = paste(
                date_label, ":", if (merge_by == "mth_yr") format(!!sym(merge_by), "%b %Y") else !!sym(merge_by),
                "<br>Ratio:", round(ratio, 3)
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
            theme(aspect.ratio = 1/2)  
          } else {
            p <- ggplot(combined_data, aes(
              x = x_axis,
              y = ratio,
              group = 1,
              text = paste(
                date_label, ":", if (merge_by == "mth_yr") format(!!sym(merge_by), "%b %Y") else !!sym(merge_by),
                "<br>Ratio:", round(ratio, 3)
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
  
  #executing file based on download option
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered-data-", Sys.Date(), ".xlsx", sep = "")
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
        
        
        openxlsx::write.xlsx(combined_data, file)
      } else {
        
        openxlsx::write.xlsx(filteredData(), file)
      }
    }
  )
}


# Run the application
shinyApp(ui = ui, server = server)