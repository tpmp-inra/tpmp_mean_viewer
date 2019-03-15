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
library(ggfortify)
library(ggrepel)
library(RColorBrewer)
library(shinyWidgets)
library(tidyverse)
library(broom)
library(Rtsne)
library(factoextra)
library(cluster)
library(gtools)

source('./shiny_common_all.R')

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
    uiOutput("cbPlantSelection"),
    uiOutput("chkShowOutliers"),
    
    uiOutput("visualizedVariable"),
    uiOutput("secondaryVariable"),
    uiOutput("dotSize"),
    uiOutput("groupMeansBy"),
    uiOutput("colorDotsBy"),
    uiOutput("cbMarginal"),
    
    uiOutput("chkShowPlantName"),
    
    uiOutput("cbSplitScatter"),
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
   
  options(shiny.maxRequestSize=30*1024^2) # Still not sure it's a good idea
  
  #This function is repsonsible for loading in the selected file
  filedata <- reactive({
    load_experience_csv(input)
  })

  group_column_name <- reactive({
    group_seaprator <- input$groupMeansBy
    if (group_seaprator == 'none') {
      'no_color'
    } else {
      paste('group', group_seaprator, 'xyz', sep = '_')
    }
  })
  
  dotColor_column_name <- reactive({
    dotColor <- input$colorDotsBy
    if (dotColor == 'none') {
      'no_color'
    } else {
      paste('color', dotColor, 'xyz', sep = '_')
    }
  })
  
  # Print observation details
  output$observationInfo <- renderTable({
    df <-filteredData()
    if (is.null(df)) return(NULL)
    
    nearPoints(df$df, input$plot_hover)
  })
  
  filteredData <- reactive({
    req(input$cbTreatmentSelection, input$visualizedVariable, input$dotSize)
    
    df <-filedata()
    if (is.null(df)) return(NULL)
    mainVar <- input$visualizedVariable
    if (is.null(mainVar)) return(NULL)
    secVar <- input$secondaryVariable
    if (is.null(secVar)) return(NULL)
    dotSize <- input$dotSize
    if (is.null(dotSize)) return(NULL)
    selPlant <- input$cbPlantSelection
    if (is.null(selPlant)) return(NULL)
    groupMeansBy <- input$groupMeansBy
    if (is.null(groupMeansBy)) return(NULL)
    colorDotsBy <- input$colorDotsBy
    if (is.null(colorDotsBy)) return(NULL)
    
    plants_to_plot <- 
      df %>%
      filter(treatment %in% input$cbTreatmentSelection) %>%
      filter(trunc_day_after_start %in% input$cbDateTimeSelector)%>%
      filter(plant %in% selPlant)
    
    if (!input$chkShowOutliers & ("outlier" %in% colnames(df))) {
      plants_to_plot <- plants_to_plot %>% filter(outlier == 0)
    }
    
    # Create color columns
    gcn = group_column_name()
    if (groupMeansBy != 'none') {
      gcn.vector <- as.factor(plants_to_plot[,groupMeansBy][[1]])
      if (length(unique(gcn.vector)) > 20) {
        gcn.vector <- plants_to_plot[,groupMeansBy][[1]]
      }
      plants_to_plot <- 
        plants_to_plot %>%
        mutate(!!gcn := gcn.vector)
    } else {
      plants_to_plot <- 
        plants_to_plot %>%
        mutate(!!gcn := as.factor(1))
    }
    ccn = dotColor_column_name()
    if (colorDotsBy != 'none') {
      ccn.vector <- as.factor(plants_to_plot[,colorDotsBy][[1]])
      if (length(unique(ccn.vector)) > 20) {
        ccn.vector <- plants_to_plot[,colorDotsBy][[1]]
      }
      plants_to_plot <- 
        plants_to_plot %>%
        mutate(!!ccn := ccn.vector)
    } else {
      plants_to_plot <- 
        plants_to_plot %>%
        mutate(!!ccn := as.factor(1))
    }
    
    if (dotSize == 'none') dotSize <- 4
    
    return(list(df = plants_to_plot,
                mainVar = mainVar,
                secVar = secVar,
                dotSize = dotSize,
                selPlant = selPlant))
  })
  
  # Populate treatment selector
  output$cbTreatmentSelection <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    fill_treatment_selection(df,
                             "cbTreatmentSelection",
                             "Select treatments to be displayed",
                             "count > 3")
  })
  
  output$groupMeansBy <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    dsnames <- names(df)
    cb_options <- list()
    cb_options[ dsnames] <- dsnames
    cb_options <- cb_options[mixedorder(unlist(cb_options),decreasing=F)]
    if ('treatment' %in% cb_options) {
      selected_choice <- 'treatment'
    } else {
      selected_choice <- 'none'
    }
    selectInput("groupMeansBy", "Group means by:", choices = c('none', cb_options), selected = selected_choice)
  })
  
  output$colorDotsBy <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    dsnames <- names(df)
    cb_options <- list()
    cb_options[ dsnames] <- dsnames
    cb_options <- cb_options[mixedorder(unlist(cb_options),decreasing=F)]
    if ('treatment' %in% cb_options) {
      selected_choice <- 'treatment'
    } else {
      selected_choice <- 'none'
    }
    selectInput("colorDotsBy", "Color dots by:", choices = c('none', cb_options), selected = selected_choice)
  })
  
  # Populate plants selector
  output$cbPlantSelection <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    fill_plant_selection(df)
  })
  
  output$chkShowOutliers <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    if ("outlier" %in% colnames(df)) {
      checkboxInput("chkShowOutliers", paste('Show outliers (', length(which(df$outlier==1)), ')', sep=''), TRUE)
    } else {
      checkboxInput("chkShowOutliers", 'No outliers detected, option ignored', FALSE)
    }
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
    
    build_numeric_selectImput(df, "dotSize", "Dot Size:", "none")
  })
  
  output$chkShowPlantName <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    checkboxInput("chkShowPlantName", "Show plant name (if all dots are displayed graph will become cluttered", FALSE)
  })
  
  output$cbDateTimeSelector <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    fill_time_selection(df)
  })
  
  output$cbMarginal <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    fill_marginal_cb()
  })
  
  # Populate scatter selector
  output$cbSplitScatter <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    build_string_selectImput(df, "cbSplitScatter",  "Separate graphs using:", "treament", c("none", "trunc_day_after_start"))
  })
  
  # Here it renders
  output$plant_plots = renderPlot({
    req(input$cbTreatmentSelection, input$visualizedVariable, input$dotSize)
    
    ptp <- filteredData()
    
    if (ptp$secVar == "none") {
      # Get the means
      gd <- ptp$df %>%
        group_by_(group_column_name()) %>%
        summarise_at(.vars = ptp$mainVar, .funs = mean)
      
      # Plot
      gg <- ggplot(ptp$df, 
                   aes_string(x = group_column_name(), 
                              y = ptp$mainVar, 
                              # color="treatment", 
                              fill= group_column_name()))
      
      if (input$cbSplitScatter != "none"){
        gg <- gg + geom_boxplot()
      } else {
        gg <- gg + geom_violin(alpha = 0.2)
        gg <- gg + geom_boxplot(width = 0.2)
      }
      
      gg <- gg + geom_jitter(aes_string(size = ptp$dotSize, color=dotColor_column_name()), width = 0.3, alpha = 0.3)
      
      if (input$chkShowPlantName) {
        gg <- gg + geom_text_repel(data = ptp$df,
                                   aes(label = plant),
                                   color = "black",
                                   size = 3.5,
                                   segment.color = "grey")
      }
      
      # Scatter the PCA
      if (input$cbSplitScatter != "none"){
        gg <- gg +  facet_wrap(input$cbSplitScatter)
      }
      
      # gg <- gg + theme(legend.title = element_text(size=32, face = "bold"),
      #                  legend.text=element_text(size=30),
      #                  axis.text=element_text(size=10),
      #                  axis.title=element_text(size=22,face="bold"),
      #                  title = element_text(size=20))
      
      gg 
    } else {
      gd <- ptp$df %>% 
        drop_na() %>%
        group_by_(group_column_name()) %>%
        summarise_at(.vars = c(ptp$mainVar, ptp$secVar), .funs = mean)
      gg <- ggplot(ptp$df, aes_string(x = ptp$mainVar, 
                                      y = ptp$secVar, 
                                      color = group_column_name()))
      gg <- gg + geom_point(alpha = .3, aes_string(size = ptp$dotSize, color=dotColor_column_name()))
      if (input$cbSplitScatter == "none"){
        gg <- gg + geom_point(data = gd, size = 16)
        gg <- gg + geom_label_repel(data = gd, 
                                    aes_string(label = group_column_name(), color = group_column_name()),
                                    size = 5,
                                    alpha = 0.8,
                                    segment.color = "grey")
      }
      
      if (input$chkShowPlantName) {
        gg <- gg + geom_text_repel(data = ptp$df,
                                   aes_string(label = "plant", color = group_column_name()),
                                   size = 3.5,
                                   segment.color = "grey")
      }
      
      # gg <- gg + theme(legend.title = element_text(size=32, face = "bold"),
      #                  legend.text=element_text(size=30),
      #                  axis.text=element_text(size=20),
      #                  axis.title=element_text(size=22,face="bold"),
      #                  title = element_text(size=20))
      
      if (input$cbMarginal == 'none') {
        if (input$cbSplitScatter != "none"){
          gg <- gg +  facet_wrap(input$cbSplitScatter)
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

