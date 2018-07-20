library(tidyverse)
library(shiny)
library(gtools)

load_experience_csv <- function(input) {
  
  infile <- input$datafile
  if (is.null(infile)) {
    # User has not uploaded a file yet
    return(NULL)
  }
  tmp <- read_csv(infile$datapath)
  
  # Factorize all and set all strings to lower case
  
  
  # If file is really raw use the median
  if ("series_id" %in% colnames(tmp)) {
    tmp <- 
      tmp %>%
      select(-c(view_option, experiment, camera)) %>%
      mutate_if(is.character, str_to_lower) %>%
      mutate_if(is.character, as.factor) %>%
      group_by(series_id) %>%
      mutate_if(is.numeric, median) %>%
      ungroup() %>%
      select(-c(series_id)) %>%
      distinct()
  }
  
  if (("treatment" %in% colnames(tmp)) & 
      ("genotype" %in% colnames(tmp)) &
      (!"condition" %in% colnames(tmp))) {
    tmp <- 
      tmp %>% 
      mutate(condition = treatment) %>%
      mutate(treatment = sprintf("%s - %s", genotype, condition))
  } else {
    # If needed ad a treatment column
    if (!"treatment" %in% colnames(tmp)) {
      tmp <- tmp %>% mutate(treatment = "No Data")
    }
  }
  
  # Build day after start if not present
  if (!("day_after_start" %in% colnames(tmp))) {
    if("date_time" %in% colnames(tmp)) {
      tmp <- tmp %>% mutate(day_after_start = as.numeric(date_time - min(date_time)) / (60*60*24))
    } else {
      if ("date" %in% colnames(tmp)) {
        tmp <- tmp %>% mutate(day_after_start = date - min(date))
      }
    }
  }
  
  # If treatment exists create foctor
  if ("treatment" %in% colnames(tmp)) {
    if (!("treatment_value" %in% colnames(tmp))) {
      tmp <- tmp %>% mutate(treatment_value = as.numeric(as.factor(treatment)))
    }
  }
  
  # If disease index exists create natural version
  if ("disease_index" %in% colnames(tmp)) {
    if (!("trunc_disease_index" %in% colnames(tmp))) {
      tmp <- tmp %>% mutate(trunc_disease_index = as.character(trunc(disease_index)))
    }
    
  } else {
    tmp <- tmp %>% 
           mutate(disease_index = 0) %>%
           mutate(trunc_disease_index = as.character(trunc(disease_index)))
  }
  
  # Remove experiment if only one is present
  if ("experiment" %in% colnames(tmp)) {
    df <- tmp %>% select(experiment)
    df <- df[!(duplicated(df) | duplicated(df, fromLast = FALSE)), ]
    cb_options = as.list(levels(df))
    if(length(cb_options) == 1) {
      tmp <- tmp %>% select(-c(experiment))
    }
  }
  
  # Create natural version of day after start
  tmp <- tmp %>%
    mutate(trunc_day_after_start = round(day_after_start))
}

fill_treatment_selection <- function(df, 
                                     input_id="cbTreatmentSelection", 
                                     label_id="Select treatments to be displayed", 
                                     selected_text_format="count > 3") {
  df <- df %>% 
    select(treatment)
  df <- df[!(duplicated(df) | duplicated(df, fromLast = FALSE)), ]
  
  cb_options = as.list(levels(df))
  if(length(cb_options) == 0) {
    cb_options <- as.list(df$treatment)
  }
  cb_options <- cb_options[mixedorder(unlist(cb_options),decreasing=F)]
  pickerInput(
    inputId = input_id, 
    label = label_id,
    choices = cb_options,
    options = list(
      `selected-text-format` = selected_text_format,
      `count-selected-text` = "{0} attributes selelcted",
      `actions-box` = TRUE,
      `deselect-all-text` = "Select none",
      `select-all-text` = "Select all"
    ), 
    selected = cb_options,
    multiple = TRUE
  )
  
}

fill_time_selection <- function(df, input_id = "cbDateTimeSelector", label_id = "Select days to be displayed") {
  df <- df %>% select(trunc_day_after_start)
  df <- df[!(duplicated(df) | duplicated(df, fromLast = FALSE)), ]
  
  cb_options = as.list(levels(df))
  if(length(cb_options) == 0) {
    cb_options <- as.list(df$trunc_day_after_start)
  }
  cb_options <- cb_options[mixedorder(unlist(cb_options),decreasing=F)]
  pickerInput(
    inputId = input_id, 
    label = label_id,
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
}

fill_plant_selection <- function(df, 
                                 input_id="cbPlantSelection", 
                                 label_id="Select plants to be displayed", 
                                 selected_text_format="count > 3",
                                 preseselct_all=T) {
  if ("treatment" %in% colnames(df)) {
    df <- df %>% 
      select(plant, treatment)
    df <- df[!(duplicated(df) | duplicated(df, fromLast = FALSE)), ]
    df <- df %>% mutate(desc = sprintf("Plant: %s, Treatment: %s", plant, treatment))
  } else {
    df <- df %>%  select(plant)
    df <- df[!(duplicated(df) | duplicated(df, fromLast = FALSE)), ]
    df <- df %>% mutate(desc = sprintf("Plant: %s", plant))
  }
  
  cb_options = setNames(as.list(df$plant), as.list(df$desc))
  cb_options <- cb_options[mixedorder(unlist(cb_options),decreasing=F)]
  if (preseselct_all) {
    selected_set <- cb_options
  } else {
    selected_set <- cb_options[1]
  }
  
  pickerInput(
    inputId = input_id, 
    label = label_id,
    choices = cb_options,
    options = list(
      `selected-text-format` = selected_text_format,
      `count-selected-text` = "{0} attributes selelcted",
      `actions-box` = TRUE,
      `deselect-all-text` = "Select none",
      `select-all-text` = "Select all"
    ), 
    selected = selected_set,
    multiple = TRUE
  )
}

fill_normalization_cb <- function(input_id="cbNormalizationMethod"){
  selectInput(input_id,
              "Data normalization method:",
              c("Normalization (default)" = "normalization",
                "Scale" = "scale",
                "None (not recommended)" = "none"), 
              selected = "normalization")
}

fill_marginal_cb <- function(input_id="cbMarginal") {
  selectInput(input_id,
              "Marginal display mode:",
              c("None (default)" = "none",
                "Histogram" = "histogram",
                "Boxplot" = "boxplot"), 
              selected = "none")
}

fill_palette_selector <- function(input_id="cbPaletteSelector") {
  selectInput(input_id,
              "Color Palette:",
              c("Accent" = "Accent",
                "Spectral" = "Spectral",
                "RdYlGn" = "RdYlGn",
                "RdYlBu" = "RdYlBu",
                "Set1" = "Set1",
                "Set2" = "Set2",
                "Set3" = "Set3",
                "Pastel1" = "Pastel1",
                "Pastel2" = "Pastel2",
                "Paired" = "Paired",
                "Dark2" = "Dark2",
                "Blues" = "Blues"
              ))
}

build_string_selectImput <- function(df,  
                                     input_id,
                                     label_id, 
                                     selected_item, 
                                     add_items = c("none")) {
  stringDf <- df[sapply(df,is.character)]
  stringDf <- stringDf[, !(colnames(stringDf) %in% c("date_time"))]
  dsnames <- names(stringDf)
  if (length(add_items) > 0) {
    cb_choices <- c(add_items, as.list(dsnames))
  } else {
    cb_choices <- as.list(dsnames)
  }
  cb_options <- cb_choices[mixedorder(unlist(cb_choices),decreasing=F)]
  selectInput(inputId =  input_id, 
              label = label_id, 
              choices = cb_choices, 
              selected = selected_item)
}
