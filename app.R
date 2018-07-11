#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(ggExtra)
library(ggcorrplot)
library(dplyr)
library(gridExtra)
library(reshape2)
library(data.table)
library(RColorBrewer)
library(shinyWidgets)
library(ggrepel)

source("./shiny_common.R")

# Define UI for application that draws a histogram
ui <- pageWithSidebar(
   
  # Application title
  headerPanel("Variable mean viewer"),
  
  # Sidebar with a slider input for number of bins 
  sidebarPanel(
    fileInput('datafile', 
              'Choose CSV file',
              accept=c("text/csv", 
                       "text/comma-separated-values,text/plain",
                       ".csv")),
    
    uiOutput("cbTreatmentSelection"),
    
    uiOutput("visualizedVariable"),
    uiOutput("secondaryVariable"),
    uiOutput("dotSize"),
    uiOutput("cbMarginal"),
    
    uiOutput("chkShowPlantName"),
    
    uiOutput("chkSplitByTime"),
    uiOutput("cbDateTimeSelector"),
    
    tags$head(tags$style("#plant_plots{height:80vh !important;}"))
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Plots", 
               plotOutput(outputId = "plant_plots",
                          inline = FALSE, 
                          height = "800")),
      tabPanel("CSV File", tableOutput("filetable"))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  #This function is repsonsible for loading in the selected file
  filedata <- reactive({
    load_experience_csv(input)
  })
  
  # Populate plants selector
  output$cbTreatmentSelection <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    fill_treatment_selection(df,
                             "cbTreatmentSelection",
                             "Select treatments to be displayed",
                             "count > 3")
  })
  
  #The following set of functions populate the x axis selectors
  output$visualizedVariable <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    new_df <- df[sapply(df,is.numeric)]
    dsnames <- names(new_df)
    cb_options <- list()
    cb_options[ dsnames] <- dsnames
    selectInput("visualizedVariable", "Selected variable:", choices = cb_options, selected = "area")
  })
  
  output$secondaryVariable <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    new_df <- df[sapply(df,is.numeric)]
    dsnames <- names(new_df)
    cb_options <- list()
    cb_options[ dsnames] <- dsnames
    selectInput("secondaryVariable", 
                "Secondary (y axis) variable:", 
                choices = c("-----" = "none", cb_options), 
                selected = "none")
  })
  
  #The following set of functions populate the dot size selectors
  output$dotSize <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    new_df <- df[sapply(df,is.numeric)]
    dsnames <- names(new_df)
    cb_options <- list()
    cb_options[ dsnames] <- dsnames
    if("disease_index" %in% colnames(new_df)) {
      selectedOption <- "disease_index"
    } else {
      selectedOption <- "shape_solidity"
    }
    selectInput("dotSize", "Dot Size:", choices = cb_options, selected = selectedOption)
  })
  
  output$chkShowPlantName <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    checkboxInput("chkShowPlantName", "Show plant name (if all dots are displayed graph will become cluttered", FALSE)
  })
  
  output$chkSplitByTime <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    checkboxInput("chkSplitByTime", "Split plots by datetime (Does not work with marginals)", FALSE)
  })
  
  output$cbDateTimeSelector <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    df <- df %>% select(trunc_day_after_start)
    df <- df[!(duplicated(df) | duplicated(df, fromLast = FALSE)), ]
    
    cb_options = as.list(levels(df))
    if(length(cb_options) == 0) {
      cb_options <- as.list(df$trunc_day_after_start)
    }
    pickerInput(
      inputId = "cbDateTimeSelector", 
      label = "Select days to be displayed",
      choices = cb_options,
      options = list(
        `selected-text-format` = "count > 3",
        `count-selected-text` = "{0} attributes selelcted",
        `actions-box` = TRUE,
        `deselect-all-text` = "Select none",
        `select-all-text` = "Select all"
      ), 
      selected = cb_options,
      multiple = TRUE
    )
  })
  
  output$cbMarginal <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    fill_marginal_cb()
  })
  
  
  output$timePointSelector <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    tags$hr()
    max_time <- trunc(max(df$trunc_day_after_start))
    sliderInput(inputId =  "timePointSelector",
                label = NULL,
                min = 0,
                max = max_time,
                value = max_time / 2,
                step = 1)
  })
  
  # Here it renders
  output$plant_plots = renderPlot({
    req(input$cbTreatmentSelection, input$visualizedVariable, input$dotSize)
    
    df <-filedata()
    if (is.null(df)) return(NULL)
    mainVar <- input$visualizedVariable
    if (is.null(mainVar)) return(NULL)
    secVar <- input$secondaryVariable
    if (is.null(secVar)) return(NULL)
    dotSize <- input$dotSize
    if (is.null(dotSize)) return(NULL)
    
    # Filter by selected plants
    plants_to_plot <- df %>% filter(treatment %in% input$cbTreatmentSelection)
    
    # Filter by time
    plants_to_plot <- filter(plants_to_plot, trunc_day_after_start %in% input$cbDateTimeSelector)

    if (secVar == "none") {
      # Get the means
      gd <- plants_to_plot %>%
        group_by(treatment) %>%
        summarise_at(.vars = mainVar, .funs = mean)
      
      # Plot
      gg <- ggplot(plants_to_plot, 
                   aes_string(x = "treatment", 
                              y = mainVar, 
                              # color="treatment", 
                              fill="treatment"))
      
      if (input$chkSplitByTime) {
        gg <- gg + geom_boxplot()
      } else {
        gg <- gg + geom_violin(alpha = 0.2)
        gg <- gg + geom_boxplot(width = 0.2)
      }
      
      gg <- gg + geom_jitter(aes_string(size = dotSize), width = 0.3, alpha = 0.3)
      
      if (input$chkShowPlantName) {
        gg <- gg + geom_text_repel(data = plants_to_plot,
                                   aes(label = plant),
                                   color = "black",
                                   size = 3.5,
                                   segment.color = "grey")
      }

      if (input$chkSplitByTime) {
        gg <- gg + facet_wrap("trunc_day_after_start")      
      }
      
      gg 
    } else {
      gd <- plants_to_plot %>% 
        group_by(treatment) %>% 
        summarise_at(.vars = c(mainVar, secVar), .funs = mean)
      gg <- ggplot(plants_to_plot, aes_string(x = mainVar, 
                                              y = secVar, 
                                              color="treatment"))
      gg <- gg + geom_point(alpha = .3, aes_string(size = dotSize))
      if (!input$chkSplitByTime) {
        gg <- gg + geom_point(data = gd, size = 16)
        gg <- gg + geom_label_repel(data = gd, 
                                    aes(label = treatment, color=treatment),
                                    size = 5,
                                    alpha = 0.8,
                                    segment.color = "grey")
      }
      
      if (input$chkShowPlantName) {
        gg <- gg + geom_text_repel(data = plants_to_plot,
                                   aes(label = plant, color=treatment),
                                   # color = "black",
                                   size = 3.5,
                                   segment.color = "grey")
      }
      
      if (input$cbMarginal == 'none') {
        if (input$chkSplitByTime) {
          gg <- gg + facet_wrap("trunc_day_after_start")      
        }
        gg  
      } else {
        if (input$cbMarginal == 'histogram') {
          ggMarginal(gg, type = "histogram", fill="transparent")
        } else {
          if (input$cbMarginal == 'boxplot') {
            ggMarginal(gg, type = "boxplot", fill="transparent")
          }
        }
      }
    }
  })
  
  #This previews the CSV data file
  output$filetable <- renderTable({
    filedata()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

