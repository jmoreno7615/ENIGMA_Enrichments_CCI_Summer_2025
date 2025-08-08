# Load packages ----------------------------------------------------------------
# PROTOTYPE
# Interrupt is shift+ctrl+F10
# Jennifer Moreno 7/3/25 CCI DOE
# must call these every time
# library calls the packages after manual install (install.packages("packagename"))
# These packages are for general data base(db) and app-building
library(duckdb)
library(shiny)
library(DBI)
library(here)
library(httr)
library(jsonlite)

#These packages are for app appearance
library(shinydashboard)
library(shinyWidgets)
library(bslib)
library(bsplus)
library(shinythemes)
library(RColorBrewer)
library(colourpicker)
library(rcartocolor)
library(fontawesome)
library(shinycssloaders)
library(htmltools)
library(tibble)

#These are for statistical analysis
library(RSQLite)
library(tidyverse) # This package has tidyr, dplyr, and ggplot2 (among others)
library(phyloseq)
library(DT)
library(forcats)
#___________________________________________
#user interface (front end)
ui <- navbarPage(
  # Change the title in quotes here
  "MEET",
  theme = shinytheme("journal"),
  tabPanel("About", icon=icon("circle-info"),
             mainPanel(
               tags$img(src = "ENIGMA Enrichments Logo.png", width = "180px",),
               tags$img(src = "ENIGMA long logo.png", width = "600px",),
               h1("Welcome!"),
               p("The Ecosystems & Networks Integrated with Genes & Molecular Assemblies (ENIGMA) team, composed of researchers at Lawrence Berkeley National Laboratory and other institutions, 
                 studies subsurface microbial ecology at a nuclear waste-contaminated site. To address ENIGMA research goals, experimentalists in the lab produce microbial enrichment communities under various field-like conditions. 
                 The goal of this project was to improve the data microbial enrichment pipelines for ENIGMA researchers with varying programming abilities and preferences, due to a need for standardization across datasets and individual data processing. 
                 The ENIGMA Microbial Enrichment Exploration Tool (MEET), a custom computational pipeline within a web-application, empowers ENIGMA researchers with efficient data processing, visualizations, and reproducible results for better microbial community data exploration. 
                 "),
               p("- Author, Jennifer Moreno-Ramirez (CCI Intern Summer 2025)"),
               h2("Where To?"),
               p("Metadata Overview:"),
               p("-Filter and view the data in the ENIGMA data base."),
               p("Total Counts:"),
               p("-View bar chart and accompanying tables for total counts (reads) for each ASV of a data subset you choose."),
               p("Community Overview:"),
               p("-View stacked bar charts for every taxanomic level of a data subset you choose."),
               p("Community Bar chart:"),
               p("-View and customize a stacked barchart for a single taxonomic level of a data subset you choose."),
               p("Flagging Tool:"),
               p("-View the top 3 samples containing the highest relative abundance of ASVs of interest in a data subset you choose.")
             )
  ),
  tabPanel("Metadata Overview", icon=icon("database"),
           sidebarLayout(
             sidebarPanel(
               p("For the whole data set, press Filter with no selections."),
               uiOutput("DO_metadata_selection_ui"),
               uiOutput("DO_submetadata_selection_ui"),
               uiOutput("DO_threshold_selection_ui"),
               actionButton("DO_filter_button", "Filter")
             ),
             mainPanel(
               DTOutput("filtered_DO_metadata")
             )
           )
  ),
  tabPanel("Total Counts", icon=icon("calculator"),
           sidebarLayout(
             sidebarPanel(
               p("For the whole data set, press Filter with no selections."),
               uiOutput("TC_metadata_selection_ui"),
               uiOutput("TC_submetadata_selection_ui"),
               actionButton("filter_button", "Filter"),
               #for spacing 
               tags$br(), 
               tags$br(),
               bs_accordion(
                 id = "accordion"
               ) %>%
                 bs_set_opts(
                   use_heading_link = TRUE
                 ) %>%
                 bs_append(
                   title = "Download Options",
                   content = tagList(
                     downloadButton("total_counts_barchart_download_button_svg", "Download the total counts plot (.svg)"),
                     tags$br(),
                     tags$br(),
                     downloadButton("total_counts_barchart_download_button_png", "Download the total counts plot (.png)"),
                     tags$br(),
                     tags$br(),
                     downloadButton("total_counts_download_button", "Download the filtered total counts table (.csv)"),
                     tags$br(),
                     tags$br(),
                     downloadButton("metadownload_button", "Download the filtered meta data table (.csv)")
                   )
                 )
             ),
             mainPanel(
               # Outputted plot for Total Counts tabs
               plotOutput("TC_metadata_barchart"),
               tags$br(),
               tags$br(),
               DTOutput("filtered_totalcounts"),
               tags$br(),
               tags$br(),
               DTOutput("filtered_TC_metadata")
             )
           )
  ), 
  tabPanel("Community Overview", icon=icon("chart-column"),
           sidebarLayout(
             sidebarPanel(
               p("For the whole data set, press Filter with no selections."),
               uiOutput("OV_metadata_selection_ui"),
               uiOutput("OV_submetadata_selection_ui"),
               uiOutput("OV_threshold_selection_ui"),
               actionButton("OV_filter_button", "Filter")
             ),
             mainPanel(
               withSpinner(uiOutput("relabund_barchart_overview_ui"), type = 5, color = "black")
             )
           )
  ),
  tabPanel("Community Bar Chart", icon=icon("dna"),
           sidebarLayout(
             sidebarPanel(
               p("For the whole data set, press Filter with no selections."),
               uiOutput("RB_metadata_selection_ui"),
               uiOutput("RB_submetadata_selection_ui"),
               uiOutput("RB_threshold_selection_ui"),
               uiOutput("taxadata_selection_ui"),
               actionButton("RB_filter_button", "Filter"),
               tags$br(), 
               tags$br(),
               bs_accordion(
                 id = "download_accordion"
               ) %>%
                 bs_set_opts(
                   use_heading_link = TRUE
                 ) %>%
                 bs_append(
                   title = "Download Options",
                   content = tagList(
                     downloadButton("enrichmentplotdownload_button_svg", "Download the Enrichment Plot (.svg)"),
                     tags$br(), 
                     tags$br(),
                     downloadButton("enrichmentplotdownload_button_png", "Download the Enrichment Plot (.png)"),
                     tags$br(), 
                     tags$br(),
                     downloadButton("RB_relabunddownload_button", "Download the Filtered Relative Abundance table (.csv)"),
                     tags$br(), 
                     tags$br(),
                     downloadButton("RB_metadownload_button", "Download the Filtered Meta table (.csv)")
                   )
                 ),
               tags$br(),
               bs_accordion(
                 id = "custom_accordion"
               ) %>%
                 bs_set_opts(
                   use_heading_link = TRUE
                 ) %>%
                 bs_append(
                   title = "Graph Customization",
                   content = tagList(
                     p("To execute customizations, press Filter."),
                     uiOutput("stackedbarchart_header"),
                     uiOutput("stackedbarchart_xlabel"),
                     htmlOutput("colorsafe_info"),
                     uiOutput("color_picker_ui")
                   )
                 )
             ),
             mainPanel(
               plotOutput("relabund_barchart"),
               tags$br(),
               tags$br(),
               DTOutput("filtered_relabund"),
               tags$br(),
               tags$br(),
               DTOutput("RB_filtered_metadata")
             )
           )
  ),
  tabPanel(
    "Flagging Tool", icon=icon("flag"),
    sidebarLayout(
      sidebarPanel(
        p("For the whole data set, press Filter with no selections."),
        uiOutput("FT_metadata_selection_ui"),
        uiOutput("FT_submetadata_selection_ui"),
        uiOutput("FT_threshold_selection_ui"),
        actionButton("FT_filter_button", "Filter"),
        downloadButton("flagged_enrichment_list_download_button_csv", "Download the flagged enrichments table (.csv)")
      ),
      mainPanel(
        # where one DT table per ASV will appear
        uiOutput("FT_filtered_relabund")
      )
    )
  )
)
#____________________________________________________________
#back-end
server <- function(input, output, session) {
  # Connect to the database
  con <- dbConnect(duckdb(), dbdir = "exampleduckbyjenTEST.db")
  on.exit({dbDisconnect(con)})
  #_____________________________________________________________
  # Any/All manipulation for meta data mass selection -> TOTAL ASV counts / Sample ID bar chart
  #______________________________________________________
  # Getting user input for meta data tab
  # Query the db to get the data from the meta data table
  DO_metadata <-dbGetQuery(con, "SELECT * FROM meta_data")
  TC_metadata <-dbGetQuery(con, "SELECT * FROM meta_data")
  countsums <-dbGetQuery(con, "SELECT * FROM countsums_data")
  RB_metadata <-dbGetQuery(con, "SELECT * FROM meta_data")
  RB_taxadata <-dbGetQuery(con, "SELECT * FROM taxa_data")
  OV_metadata <- dbGetQuery(con, "SELECT * FROM meta_data")
  OV_taxadata <- dbGetQuery(con, "SELECT * FROM taxa_data")
  relabunddata <- dbGetQuery(con, "SELECT * FROM relabund_data")
  FT_metadata     <- dbGetQuery(con, "SELECT * FROM meta_data")
  FT_taxadata     <- dbGetQuery(con, "SELECT * FROM taxa_data")
  FT_relabunddata <- dbGetQuery(con, "SELECT * FROM relabund_data")
  #REPLACE with updated ASV matches (note for Jenn K.)
  ASV_file <- read.csv(here("Data Input", "CulturedASVs_vs_FieldASVs.csv"), row.names = NULL)
  colnames(ASV_file)[1] <- "ASV"
  ASV_to_match <- ASV_file$ASV
  
  
  #1DO Get the column(s) name input
  output$DO_metadata_selection_ui <- renderUI({
    selectInput("DO_metadata_selection", "Select meta-data type(s) of interest:",
                choices = colnames(DO_metadata),
                multiple = TRUE)
  })
  
  #2DO Get the selected column(s) value(s) input
  output$DO_submetadata_selection_ui <- renderUI({
    # Requires a selection from the meta data headers
    req(input$DO_metadata_selection)
    # Combines the elements for sub selection into one UI element
    tagList(
      # Creating the function to apply to all sub meta selections (using headers as vectors)
      lapply(input$DO_metadata_selection, function(x) {
        # Subsets the DO_metadata values by header + removes duplicate options
        values <- unique(DO_metadata[[x]])
        # Creates elements for each metadata type
        # paste0 makes unique element IDs for each element variant (diff header choices)
        selectInput(paste0("DO_submetadata_selection_", x),
                    paste0("Select Value(s) from ", x, ":"),
                    choices = values,
                    multiple = TRUE)
      })
    )
  })
  
  #1TC Get the column(s) name input
  output$TC_metadata_selection_ui <- renderUI({
    selectInput("TC_metadata_selection", "Select meta-data type(s) of interest:",
                choices = colnames(TC_metadata),
                multiple = TRUE)
  })
  
  #2TC Get the selected column(s) value(s) input
  output$TC_submetadata_selection_ui <- renderUI({
    # Requires a selection from the meta data headers
    req(input$TC_metadata_selection)
    # Combines the elements for sub selection into one UI element
    tagList(
      # Creating the function to apply to all sub meta selections (using headers as vectors)
      lapply(input$TC_metadata_selection, function(x) {
        # Subsets the TC_metadata values by header + removes duplicate options
        values <- unique(TC_metadata[[x]])
        # Creates elements for each metadata type
        # paste0 makes unique element IDs for each element variant (diff header choices)
        selectInput(paste0("TC_submetadata_selection_", x),
                    paste0("Select Value(s) from ", x, ":"),
                    choices = values,
                    multiple = TRUE)
      })
    )
  })
  
  #1RB Get the column(s) name input
  output$RB_metadata_selection_ui <- renderUI({
    selectInput("RB_metadata_selection", "Select meta-data type(s) of interest:",
                choices = colnames(RB_metadata),
                multiple = TRUE)
  })
  
  #2RB Get the selected column(s) value(s) input
  output$RB_submetadata_selection_ui <- renderUI({
    # requires a selection from the meta data headers
    req(input$RB_metadata_selection)
    # Combines the elements for sub selection into one UI element
    tagList(
      # creating the function to apply to all sub meta selections (using headers as vectors)
      lapply(input$RB_metadata_selection, function(x) { # lapply function start
        # subsets the metadata values by header+ removes duplicate options
        metavalues <- unique(RB_metadata[[x]])
        # creates elements for each metadata type
        # paste0 makes unique element IDs for each element variant (diff header choices)
        selectInput(paste0("RB_submetadata_selection_", x),
                    paste0("Select Value(s) from ", x, ":"),
                    choices = metavalues,
                    multiple = TRUE)
      })
    )
  })
  
  #3RB Get the taxonomic selection
  output$taxadata_selection_ui <- renderUI ({
    choices<-colnames(RB_taxadata)
    #moving ASV last
    ASV<- choices[1]
    ASVlast<-c(choices[-1], ASV)
    selectInput("taxadata_selection", "Select taxonomic level of interest:",
                choices = ASVlast)
  })
  
  #4RB Threshold selection
  output$RB_threshold_selection_ui <- renderUI ({
    #this means it shouldn't appear unless a non kingdom taxa is selected
    req(input$taxadata_selection != "Kingdom")
    #default threshold to 10%
    numericInput("threshold_selection", "Select Relative Abundance Threshold:", value = 0.10, min = 0, max = 1.0, step=0.01)
  })
  
  #5RB Customization - chart headers and x axis label
  output$stackedbarchart_header <- renderUI ({
    textInput("stackedbarchart_header", "Chart header:", value = "Relative Abundance at Taxa Level per Sample")
  })
  
  output$stackedbarchart_xlabel <- renderUI ({
    textInput("stackedbarchart_xlabel", "X-axis Label:", value = "Sample ID")
  })
  
  #6RB outputs the color palette info
  output$colorsafe_info <- renderText({
    HTML("Need help? Find the color blind safe palette guide <a href='https://drive.google.com/file/d/1RE3Tk63tO2h5_pijxOoK4khH0sw9DGHa/view?usp=drive_link' target='_blank'>HERE</a>")
  })
  
  #7RB color picker of taxa
  output$color_picker_ui <- renderUI({
    #require some input from relative abundance
    req(relabunddatasubset())
    #Get the subset
    relabundsubset <- relabunddatasubset()
    selected_taxa <- input$taxadata_selection
    
    # Pivot to long format to get unique taxa (after lumping)
    relabunddata_long_for_colors <- pivot_longer(
      relabundsubset,
      cols = -all_of(selected_taxa),
      names_to = "SampleID",
      values_to = "RelativeAbundance"
    )
    
    # lists the unique taxa on the barchart AFTER pooling the "other" category
    unique_taxa_for_colors <- unique(relabunddata_long_for_colors[[selected_taxa]])
    # gets quantity of unique taxa
    num_unique_taxa <- length(unique_taxa_for_colors)
    
    # Generate initial default colors
    default_colors <- if (num_unique_taxa > 0) {
      # The 'Safe' palette from rcartocolor has 12 colors.
      # Use it if the number of unique taxa is 12 or less.
      if (num_unique_taxa <= 12) {
        carto_pal(n = num_unique_taxa, name = "Safe")
      } else {
        safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                                     "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")
        set.seed(007)
        colourCount <- num_unique_taxa
        getPalette <- colorRampPalette(safe_colorblind_palette)
        org_palette = sample(getPalette(colourCount), replace=FALSE)
        org_palette
      }
    } else {
      # Return an empty vector if no taxa to color
      character(0) 
    }
    
    #creates a selector for each unique taxa
    color_inputs <- lapply(seq_along(unique_taxa_for_colors), function(i) {
      taxon <- unique_taxa_for_colors[i]
      color_id <- paste0("color_taxon_", gsub(" ", "_", taxon))
      color_label <- paste0("Color for ", taxon)
      default_color <- default_colors[i]
      
      #applies the color picker to each taxa
      colourInput(
        inputId = color_id,
        label = color_label,
        value = default_color
      )
    }) # lapply function end
    tagList(color_inputs)
  })
  
  #1OV Get the column(s) name input
  output$OV_metadata_selection_ui <- renderUI({
    selectInput("OV_metadata_selection", "Select meta-data type(s) of interest:",
                choices = colnames(OV_metadata),
                multiple = TRUE)
  })
  
  #2OV Get the selected column(s) value(s) input
  output$OV_submetadata_selection_ui <- renderUI({
    req(input$OV_metadata_selection)
    tagList(
      lapply(input$OV_metadata_selection, function(x) {
        metavalues <- unique(OV_metadata[[x]])
        selectInput(paste0("OV_submetadata_selection_", x),
                    paste0("Select Value(s) from ", x, ":"),
                    choices = metavalues,
                    multiple = TRUE)
      })
    )
  })
  
  #3OV Threshold selection
  output$OV_threshold_selection_ui <- renderUI({
    numericInput("OV_threshold_selection", "Select Relative Abundance Threshold:", value = 0.10, min = 0, max = 1.0, step = 0.01)
  })
  
  #1FT Get the column(s) name input
  output$FT_metadata_selection_ui <- renderUI({
    selectInput(
      inputId = "FT_metadata_selection",
      label   = "Select meta-data type(s) of interest:",
      choices = colnames(FT_metadata),
      multiple = TRUE
    )
  })
  
  #2FT Get the selected column(s) value(s) input
  output$FT_submetadata_selection_ui <- renderUI({
    req(input$FT_metadata_selection)
    tagList(
      lapply(input$FT_metadata_selection, function(col) {
        vals <- unique(FT_metadata[[col]])
        selectInput(
          inputId = paste0("FT_submetadata_selection_", col),
          label   = paste0("Select Value(s) from ", col, ":"),
          choices = vals,
          multiple = TRUE
        )
      })
    )
  })
  
  #3FT Threshold selection
  output$FT_threshold_selection_ui <- renderUI({
    numericInput(
      inputId = "FT_threshold_selection",
      label   = "Select Relative Abundance Threshold:", value   = 0.10, min = 0, max = 1.0, step = 0.01)
  })
  
  
  #________________________________________________________
  # Getting the filtered sample IDs into a data subset
  # Reactive expression to get the filtered sample IDs into a data subset
  # Getting the filtered sample IDs into a data subset
  
  DO_metadatasubset <- eventReactive(input$DO_filter_button, {
    # No selections made -> return all the sample IDs from the meta data
    if (is.null(input$DO_metadata_selection) || length(input$DO_metadata_selection) == 0) {
      return(DO_metadata)
    } else {
      # Initialized to all the meta data
      DO_subset <- DO_metadata
      # for each ID in the meta data we selected
      for (meta in input$DO_metadata_selection) {
        submeta_input <- input[[paste0("DO_submetadata_selection_", meta)]]
        # Ensuring the selected data exists
        if (!is.null(submeta_input) && length(submeta_input) > 0) {
          # Gets the sample IDs column that matches the selections
          DO_subset <- DO_subset[DO_subset[[meta]] %in% submeta_input, ]
        }
      }
      # the final subset from meta data
      return(DO_subset)
    }
  })
  
  TC_metadatasubset <- eventReactive(input$filter_button, {
    # If no selections are made:
    if (is.null(input$TC_metadata_selection) == TRUE || length(input$TC_metadata_selection) == 0) {
      # Returns metadata (non-manipulated values)
      return(TC_metadata)
    } else {
      # Initializes to non-manip metadata
      TC_subset <- TC_metadata
      # For the column(s) selected:
      for (meta in input$TC_metadata_selection) {
        # Grabs the value(s) from selected column
        submeta_input <- input[[paste0("TC_submetadata_selection_", meta)]]
        # If it exists and is valid in the table, grab the rows that they belong to
        if (!is.null(submeta_input) && length(submeta_input) > 0) {
          TC_subset <- TC_subset[TC_subset[[meta]] %in% submeta_input, ]
        }
      }
      # If the filters don't have matching rows, let the user know
      if (nrow(TC_subset) == 0) {
        return(data.frame(ncol = ncol(TC_metadata), nrow = 0))
      } else {
        return(TC_subset)
      }
    }
  })
  
  totalcounts_subset <- eventReactive(input$filter_button, {
    req(TC_metadatasubset())
    TC_subset <- TC_metadatasubset()
    # If subset has no matches:
    if (nrow(TC_subset) == 0) {
      # Returns an empty data frame
      return(data.frame(matrix(ncol = 2, nrow = 0, dimnames = list(NULL, c("SampleID", "Sum")))))
    } else {
      # Otherwise find the matching vals and output
      return(sample_sums <- countsums[countsums$SampleID %in% TC_subset$SampleID, ])
    }
  })
  
  totalcounts_barchart <- eventReactive(input$filter_button, {
    # Require some data to make the bar plot
    req(TC_metadatasubset())
    TC_subset <- TC_metadatasubset()
    # If the subset is a string (no data matches the filters)
    if (nrow(TC_subset) == 0) {
      # This doesn't actually output anything
      validate(need(nrow(TC_subset) != 0, "There are no matches in the dataset."))
    } else {
      # Generate the plot if there is data
      # Find the matching columns for SampleID in the countsums data frame as is present in the subset
      sample_sums <- countsums[countsums$SampleID %in% TC_subset$SampleID, ]
      ggplot(sample_sums, aes(x = SampleID, y = Sum)) +
        geom_bar(stat = "identity") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
        labs(title = "Total Counts by Sample ID", x = "Sample ID", y = "Total Counts")
    }
  })
  
  # Getting the filtered sample IDs into a data subset
  RB_metadatasubset <- eventReactive(input$RB_filter_button, {
    # If no selections are made:
    if (is.null(input$RB_metadata_selection) || length(input$RB_metadata_selection) == 0) {
      # returns RB_metadata (non maniped values)
      return(RB_metadata)
    } else {
      # initializes to non manip metadata
      RB_subset <- RB_metadata
      # for the column(s) selected:
      for (meta in input$RB_metadata_selection) {
        # grabs the value(s) from selected column
        submeta_input <- input[[paste0("RB_submetadata_selection_", meta)]]
        # if it exists and is valid in the table, grab the rows that they belong to
        if (!is.null(submeta_input) && length(submeta_input) > 0) {
          RB_subset <- RB_subset[RB_subset[[meta]] %in% submeta_input, ]
        }
      }
      # if the filters don't have matching rows, let the user know
      if (nrow(RB_subset) == 0) {
        return(data.frame(ncol=ncol(RB_metadata), nrow = 0))
      } else {
        return(RB_subset)
      }
    }
  })
  
  # Getting the subset for relative abundance based on taxa level
  relabunddatasubset <- eventReactive(input$RB_filter_button, {
    # extract the filtered sample IDs from metadata
    meta_selected_ID <- RB_metadatasubset()$SampleID
    meta_to_match <- c("ASV", meta_selected_ID)
    
    #!!This will only output the MATCHING columns so disclaimer, not all the observations in a data set may be displayed if they don't have BOTH relabund and metadata !!
    common_columns <- intersect(names(relabunddata), meta_to_match)
    #filter only the samples of interest
    relabund_data_selected <- relabunddata[, common_columns]
    # Getting the selected taxa level
    selected_taxa <- input$taxadata_selection
    #for when selected taxa IS ASV, the manipulations for table output are done here
    if (selected_taxa == "ASV") {
      modded_relabund <- relabund_data_selected
      #otherwise ASV is used to combine tables at the needed taxa level
    } else {
      # removes extra selected ASV column
      merged_relabund <- merge(relabund_data_selected, RB_taxadata[, c("ASV", selected_taxa)], by = "ASV")
      modded_relabund <- merged_relabund[, !(names(merged_relabund) %in% c("ASV"))]
    }
    
    #longer table for manip
    long_table <- modded_relabund %>%
      pivot_longer(cols = -all_of(selected_taxa), names_to = "SampleID", values_to = "value", values_drop_na = TRUE)
    
    # If it's kingdom, ignore the "other" column threshold thing since theres only two possibilities
    if (selected_taxa == "Kingdom") {
      # Group and summarize the lumped data
      grouped_table <- long_table %>%
        group_by(!!sym(selected_taxa), SampleID) %>%
        summarise(total_value = sum(value), .groups = 'drop')
    }else{
      selected_threshold<- input$threshold_selection
      # This is the threshold code block, creates an "other" taxa for when the relabund is below a certain proportion of each sample
      relabunddata_lumped <- long_table %>%
        group_by(SampleID) %>%
        mutate(!!sym(selected_taxa) := fct_lump_prop(f=!!sym(selected_taxa), prop = selected_threshold, other_level = "Other", w = value)) %>%
        ungroup()
      
      # Group and summarize the lumped data
      grouped_table <- relabunddata_lumped %>%
        group_by(!!sym(selected_taxa), SampleID) %>%
        summarise(total_value = sum(value), .groups = 'drop')
    }
    
    #resets table to normal format
    reverted_relabundtable <- grouped_table %>%
      pivot_wider(
        # Use sampleID as column names
        names_from = SampleID,
        # Use total_value as the values
        values_from = total_value,
        values_fill = 0
      ) 
    
    #puts the values in the columns in order from greatest to least
    cols <- rev(names(reverted_relabundtable))
    final_relabundtable <- reverted_relabundtable %>%
      arrange(across(all_of(cols), desc))
    
    return(final_relabundtable)
  })
  
  enrichmenttaxaplot <- eventReactive(input$RB_filter_button, {
    # require some data to make the bar plot
    req(RB_metadatasubset())
    req(relabunddatasubset())
    selected_taxa <- input$taxadata_selection
    header <- input$stackedbarchart_header
    xlabel <- input$stackedbarchart_xlabel
    relabundsubset <- relabunddatasubset()
    relabunddata_long <- pivot_longer(
      relabundsubset,
      cols = -all_of(selected_taxa),
      names_to = "SampleID",
      values_to = "RelativeAbundance"
    )
    
    # Get the unique taxa that will be plotted
    unique_taxa_in_plot <- unique(relabunddata_long[[selected_taxa]])
    
    # Create a named vector of colors from user input, or use defaults
    plot_colors <- sapply(unique_taxa_in_plot, function(taxon) {
      input_id <- paste0("color_taxon_", gsub(" ", "_", taxon))
      if (!is.null(input[[input_id]])) {
        input[[input_id]]
      } else {
        if (length(unique_taxa_in_plot) <= 12) {
          # Use rcartocolor if within the 12 color limit
          carto_pal(n = length(unique_taxa_in_plot), name = "Safe")[which(unique_taxa_in_plot == taxon)]
        } else {
          safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                                       "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")
          # Use custom safe colorblind palette for more than 12 unique taxa
          set.seed(007)  # Set seed for reproducibility
          colourCount <- length(unique_taxa_in_plot)
          getPalette <- colorRampPalette(safe_colorblind_palette)
          org_palette <- sample(getPalette(colourCount), replace = FALSE)
          org_palette[which(unique_taxa_in_plot == taxon)]
        }
      }
    }, USE.NAMES = TRUE)
    
    ggplot(relabunddata_long, aes(x = SampleID, y = RelativeAbundance, fill = .data[[selected_taxa]])) +
      geom_bar(stat = "identity") +
      labs(title = header,
           x = xlabel,
           y = "Relative Abundance",
           fill = "Taxon") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
      # where the dynamic colors are applied
      scale_fill_manual(values = plot_colors)
  })
  
  # Getting the filtered sample IDs into a data subset
  OV_metadatasubset <- eventReactive(input$OV_filter_button, {
    # No selections made -> return all the sample IDs from the meta data
    if (is.null(input$OV_metadata_selection) || length(input$OV_metadata_selection) == 0) {
      return(OV_metadata)
    } else {
      # Initialized to all the meta data
      OV_subset <- OV_metadata
      # for each ID in the meta data we selected
      for (meta in input$OV_metadata_selection) {
        submeta_input <- input[[paste0("OV_submetadata_selection_", meta)]]
        # Ensuring the selected data exists
        if (!is.null(submeta_input) && length(submeta_input) > 0) {
          # Gets the sample IDs column that matches the selections
          OV_subset <- OV_subset[OV_subset[[meta]] %in% submeta_input, ]
        }
      }
      if (nrow(OV_subset) == 0) {
        # Returns empty df when no selections made
        return(data.frame(ncol = ncol(OV_metadata), nrow = 0))
      } else {
        # the final subset from meta data
        return(OV_subset)
      }
    }
  })
  
  # Function to filter and process data for a given taxonomic level
  process_taxonomic_level <- function(selected_taxa) {
    # Require these to prevent premature loading (need the defaults to initialize)
    req(OV_metadatasubset())
    req(input$OV_threshold_selection)
    # Get the IDs we filtered
    meta_selected_ID <- OV_metadatasubset()$SampleID
    # Getting all the IDs and the ASVs
    meta_to_match <- c("ASV", meta_selected_ID)
    # getting the columns in relabund data that have the ASVs and ONLY the matching SampleIDs
    common_columns <- intersect(names(relabunddata), meta_to_match)
    relabund_data_selected <- relabunddata[, common_columns]
    
    # If the selected level is taxa, we're there, other wise use ASV to get the level
    if (selected_taxa == "ASV") {
      modded_relabund <- relabund_data_selected
    } else {
      merged_relabund <- merge(relabund_data_selected, OV_taxadata[, c("ASV", selected_taxa)], by = "ASV")
      modded_relabund <- merged_relabund[, !(names(merged_relabund) %in% c("ASV"))]
    }
    
    # Format mods for easy grabbing
    long_table <- modded_relabund %>%
      pivot_longer(cols = -all_of(selected_taxa), names_to = "SampleID", values_to = "value", values_drop_na = TRUE)
    
    # Read in the threshold (starts at 0.10)
    OV_selected_threshold <- input$OV_threshold_selection
    
    #kingdom doesn't get threshold applied want to show both Bacteria and archaea
    if (selected_taxa == "Kingdom") {
      grouped_table <- long_table %>%
        group_by(!!sym(selected_taxa), SampleID) %>%
        summarise(total_value = sum(value), .groups = 'drop')
    } else {
      relabunddata_lumped <- long_table %>%
        group_by(SampleID) %>%
        #mutate the groups based on the threshold to make a new column called other for those below it using there relabund values to sum
        mutate(!!sym(selected_taxa) := fct_lump_prop(f = !!sym(selected_taxa), prop = OV_selected_threshold, other_level = "Other", w = value)) %>%
        ungroup()
      
      # Get the groups for EACH sample and sum them
      grouped_table <- relabunddata_lumped %>%
        group_by(!!sym(selected_taxa), SampleID) %>%
        summarise(total_value = sum(value), .groups = 'drop')
    }
    
    # Reformatting again for outut, set null values to zero atp
    reverted_relabundtable <- grouped_table %>%
      pivot_wider(
        names_from = SampleID,
        values_from = total_value,
        values_fill= 0
      )
    
    #sorting the values
    cols <- names(reverted_relabundtable)
    cols <- rev(names(reverted_relabundtable))
    final_relabundtable <- reverted_relabundtable %>%
      arrange(across(all_of(cols), desc))
    
    return(final_relabundtable)
  }
  
  # Function to generate stacked bar chart for a given taxonomic level
  generate_stacked_bar_chart <- function(data, level) {
    req(OV_metadatasubset())
    
    # Reformatting the data table
    pivot_table <- data %>%
      pivot_longer(cols = -all_of(level), names_to = "SampleID", values_to = "RelativeAbundance")
    
    # Getting # of colors needed for fill
    unique_taxa_for_colors <- unique(pivot_table[[level]])
    num_unique_taxa <- length(unique_taxa_for_colors)
    
    # Getting default colors
    default_colors <- if (num_unique_taxa > 0) {
      if (num_unique_taxa <= 12) {
        carto_pal(n = num_unique_taxa, name = "Safe")
      } else {
        safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                                     "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")
        set.seed(007)
        colourCount <- num_unique_taxa
        getPalette <- colorRampPalette(safe_colorblind_palette)
        org_palette = sample(getPalette(colourCount), replace = FALSE)
        org_palette
      }
    } else {
      character(0)
    }
    
    # Graphing!
    ggplot(pivot_table, aes(x = SampleID, y = RelativeAbundance, fill = .data[[level]])) +
      geom_bar(stat = "identity") +
      labs(title = level,
           x = "Sample ID",
           y = "Relative Abundance",
           fill = "Taxon") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
      scale_fill_manual(values = default_colors)
  }
  
  # Reactive expression to store all plots
  all_plots <- eventReactive(input$OV_filter_button, {
    req(OV_metadatasubset())
    taxonomic_levels <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "ASV")
    # Gets the list of plots (for all the taxa levels)
    plots_list <- list()
    for (level in taxonomic_levels) {
      processed_data <- process_taxonomic_level(level)
      if (nrow(processed_data) > 0) {
        plot <- generate_stacked_bar_chart(processed_data, level)
        plots_list[[level]] <- plot
      }
    }
    return(plots_list)
  })
  
  # Getting the filtered sample IDs into a data subset
  FT_metadatasubset <- eventReactive(input$FT_filter_button, {
    # no selections → keep everything
    if (is.null(input$FT_metadata_selection) ||
        length(input$FT_metadata_selection) == 0) {
      return(FT_metadata)
    }
    
    tmp <- FT_metadata
    for (col in input$FT_metadata_selection) {
      sel <- input[[paste0("FT_submetadata_selection_", col)]]
      if (!is.null(sel) && length(sel) > 0) {
        tmp <- tmp[tmp[[col]] %in% sel, ]
      }
    }
    
    # if nothing survives the filter, return an empty data.frame with same cols
    if (nrow(tmp) == 0) {
      data.frame(
        matrix(
          ncol = ncol(FT_metadata), nrow = 0,
          dimnames = list(NULL, colnames(FT_metadata))
        )
      )
    } else {
      tmp
    }
  })
  
  # build the wide ASV‑by‑Sample relative‑abundance table
  FT_process_relabund_data <- eventReactive(input$FT_filter_button, {
    req(FT_metadatasubset())
    req(input$FT_threshold_selection)
    
    # Sample IDs that passed the metadata filter
    meta_selected_ID <- FT_metadatasubset()$SampleID
    
    # Keep only the ASV column and the selected samples from the raw relabund table
    needed_cols <- c("ASV", meta_selected_ID)
    common_cols  <- intersect(names(FT_relabunddata), needed_cols)
    if (length(common_cols) < 2) {
      # Create a data frame with a single row (SampleID = NA) and no ASV columns
      # This will survive the later pivot_longer() calls without error.
      empty_wide <- tibble(SampleID = character(0))
      return(empty_wide)
    }
    
    relabund_sel <- FT_relabunddata[, common_cols]
    
    # Long format: ASV | SampleID | value
    long_tbl <- relabund_sel %>%
      pivot_longer(
        cols = -ASV,
        names_to = "SampleID",
        values_to = "value",
        values_drop_na = TRUE
      )
    
    # Apply the relative‑abundance threshold (lumping low‑abundance taxa into "Other")
    FT_thresh <- input$FT_threshold_selection
    lumped_tbl <- long_tbl %>%
      group_by(SampleID) %>%
      mutate(
        ASV = fct_lump_prop(
          f = ASV,
          prop = FT_thresh,
          other_level = "Other",
          w = value
        )
      ) %>%
      ungroup() %>%
      filter(ASV %in% ASV_to_match)
    
    # Sum per ASV‑Sample (gives the numbers we will later reshape)
    summed_tbl <- lumped_tbl %>%
      group_by(ASV, SampleID) %>%
      summarise(total_value = sum(value), .groups = "drop")
    
    # Wide format: rows = SampleID, columns = ASVs
    wide_tbl <- summed_tbl %>%
      pivot_wider(
        names_from = ASV,
        values_from = total_value,
        values_fill = list(total_value = 0)
      )
    
    # Optional cosmetic ordering of columns
    col_order <- rev(names(wide_tbl))
    wide_tbl <- wide_tbl %>% arrange(across(all_of(col_order), desc))
    
    return(wide_tbl)
  })
  
  # extract the top‑3 NON‑ZERO samples for each ASV
  FT_top3_per_ASV <- eventReactive(input$FT_filter_button, {
    req(FT_process_relabund_data())
    wide_tbl <- FT_process_relabund_data()
    
    if (ncol(wide_tbl) <= 1 || nrow(wide_tbl) == 0) {
      return(list())
    }
    
    # Convert wide → long (keep SampleID, melt all ASV columns)
    long_tbl <- wide_tbl %>%
      pivot_longer(
        cols = -SampleID,
        names_to = "ASV",
        values_to = "rel_abund",
        values_drop_na = TRUE
      )
    
    # Keep only the ASVs that are in the master list
    long_tbl <- long_tbl %>% filter(ASV %in% ASV_to_match)
    
    # Discard rows with a zero relative abundance
    long_tbl <- long_tbl %>% filter(rel_abund > 0)
    
    # For each ASV keep the three samples with the highest abundance
    top3_tbl <- long_tbl %>%
      group_by(ASV) %>%
      slice_max(order_by = rel_abund, n = 3, with_ties = FALSE) %>%
      ungroup() %>%
      arrange(ASV, desc(rel_abund))
    
    # Return a named list – one tiny data.frame per ASV
    split(top3_tbl, top3_tbl$ASV)
  })
  
  #____________________________________________________
  
  # Render the filtered metadata table
  output$filtered_DO_metadata <- renderDT({
    req(DO_metadatasubset())
    datatable(DO_metadatasubset(),
              # Title
              caption = "Filtered Metadata Matches",
              # These options allow for the scroll bar and fitting to screen
              options = list(
                scrollX = TRUE,
                autoWidth = TRUE
              ))
  })
  
  # Making a bar chart for the filtered data
  output$TC_metadata_barchart <- renderPlot({
    totalcounts_barchart()
  })
  

  # Render the filtered metadata table
  output$filtered_TC_metadata <- renderDT({
    req(TC_metadatasubset())
    datatable(TC_metadatasubset(),
              # Title
              caption = "Filtered Metadata Matches",
              # These options allow for the scroll bar and fitting to screen
              options = list(
                scrollX = TRUE,
                autoWidth = TRUE
              ))
  })
  
  # Render the filtered total counts table
  output$filtered_totalcounts <- renderDT({
    req(totalcounts_subset())
    datatable(totalcounts_subset(), caption = "Filtered Total Count Matches", rownames = FALSE)
  })
  
  # Render the filtered metadata table
  output$RB_filtered_metadata <- renderDT({
    req(RB_metadatasubset())
    datatable(RB_metadatasubset(),
              # title
              caption = "Filtered Metadata Matches",
              # these options allow for the scroll bar and fitting to screen
              options = list(
                scrollX = TRUE,
                autoWidth = TRUE
              ))
  })
  
  # Render the filtered RELABUND table
  output$filtered_relabund <- renderDT({
    req(relabunddatasubset())
    datatable(relabunddatasubset(),
              caption = "Filtered Relative Abundance Values",
              options = list (
                scrollX = TRUE,
                autoWidth = TRUE
              ))
  })
  
  
  # Making a bar chart for the filtered data
  output$relabund_barchart <- renderPlot({
    enrichmenttaxaplot()
  })
  
  # Making a bar chart for the filtered data
  # Render UI for all plots
  output$relabund_barchart_overview_ui <- renderUI({
    req(all_plots())
    plots_list <- all_plots()
    plot_outputs <- lapply(names(plots_list), function(level) {
      plotOutput(paste0("plot_", level))
    })
    do.call(tagList, plot_outputs)
  })  
  # Render each plot
  observe({
    req(all_plots())
    plots_list <- all_plots()
    for (level in names(plots_list)) {
      local({
        lev <- level
        output[[paste0("plot_", lev)]] <- renderPlot({
          plots_list[[lev]]
        })
      })
    }
  })
  
  #create a DT output for every ASV that has a top‑3 table
  output$FT_filtered_relabund <- renderUI({
    req(FT_top3_per_ASV())
    asv_list <- FT_top3_per_ASV()
    
    if (length(asv_list) == 0) {
      return(tags$div(
        "No ASVs passed the filter (or all remaining values are zero)."
      ))
    }
    
    # For each ASV, create a tagList with two DT outputs
    tbl_outputs <- lapply(names(asv_list), function(asv) {
      tagList(
        DTOutput(outputId = paste0("table_", asv)),
        DTOutput(outputId = paste0("taxa_table_", asv)),
        DTOutput(outputId = paste0("asv_file_table_", asv)),
        tags$br(),
        tags$br(),
        tags$br()
      )
    })
    
    do.call(tagList, tbl_outputs)
  })
  
  observe({
    req(FT_top3_per_ASV())
    asv_list <- FT_top3_per_ASV()
    
    lapply(names(asv_list), function(asv) {
      local({
        my_asv <- asv
        
        # Output 1: Sample and Metadata table
        output[[paste0("table_", my_asv)]] <- renderDT({
          df <- asv_list[[my_asv]] %>%
            left_join(FT_metadata, by = "SampleID") %>%
            select(SampleID, rel_abund, everything(), -ASV) %>%
            arrange(desc(rel_abund))
          
          datatable(
            df,
            caption = tags$caption(
              style = "caption-side: top; text-align: left;",
              tags$b(style = "color: black;", paste0("Top 3 (non-zero) samples for ", my_asv, " within your subset"))
            ),
            rownames = FALSE,
            options = list(pageLEngth = 3, scrollX = TRUE, autoWidth = TRUE, searching = FALSE)
          )
        })
        
        # Output 2: Taxonomic information table
        output[[paste0("taxa_table_", my_asv)]] <- renderDT({
          taxa_df <- FT_taxadata %>%
            filter(ASV == my_asv) %>%
            select(-ASV) # Remove the ASV column from the output table
          
          datatable(
            taxa_df,
            caption = tags$caption(
              style = "caption-side: top; text-align: left;",
              paste0("Taxonomy for ", my_asv)
            ),
            rownames = FALSE,
            options = list(pageLenghth = 1, ordering = FALSE, dom = 't', scrollX = TRUE, searching = FALSE)
          )
        })
        
        # Output 3: Cultured vs. Field ASVs table
        output[[paste0("asv_file_table_", my_asv)]] <- renderDT({
          asv_data <- ASV_file %>%
            filter(ASV == my_asv)
          colnames(asv_data)[1] <- "Cultured_ASV_ID" 
          datatable(
            asv_data,
            caption = tags$caption(
              style = "caption-side: top; text-align: left;",
              paste0("Field ASV Information for ", my_asv)
            ),
            rownames = FALSE,
            options = list(dom = 't', ordering = FALSE, searching = FALSE, scrollx = TRUE)
          )
        })
      })
    })
  })
  
  
  #_______________________________________________________________________________
  # Exporting buttons
  output$total_counts_download_button <- downloadHandler(
    # The placeholder name for the file
    filename = function() {
      paste("metadata_", Sys.Date(), ".csv", sep="")
    },
    # The content of the file will be the contents of the total counts data
    content = function(file) {
      write.csv(totalcounts_subset(), file, quote = FALSE)
    }
  )
  
  output$metadownload_button <- downloadHandler(
    # The placeholder name for the file
    filename = function() {
      paste("totalcounts_data_", Sys.Date(), ".csv", sep="")
    },
    # The content of the file will be the contents of the meta reactive data
    content = function(file) {
      write.csv(TC_metadatasubset(), file, quote = FALSE)
    }
  )
  
  output$total_counts_barchart_download_button_svg <- downloadHandler(
    filename = function() {
      paste("totalcounts_barchart_", Sys.Date(), ".svg", sep="")
    },
    # The content of the file will be the contents of the mtcars_plot() reactive expression
    # Note how we need to encapsulate the plot in svg() and dev.off() functions
    # The syntax also demands that we put print() around our plot
    content = function(file) {
      svg(file, width = 1000, height = 600)
      print(totalcounts_barchart())
      dev.off()
    }
  )
  
  output$total_counts_barchart_download_button_png <- downloadHandler(
    filename = function() {
      paste("totalcounts_barchart_", Sys.Date(), ".png", sep="")
    },
    # The content of the file will be the contents of the mtcars_plot() reactive expression
    # Note how we need to encapsulate the plot in svg() and dev.off() functions
    # The syntax also demands that we put print() around our plot
    content = function(file) {
      png(file, width = 1000, height = 600)
      print(totalcounts_barchart())
      dev.off()
    }
  )
  
  output$RB_relabunddownload_button <- downloadHandler(
    # The placeholder name for the file
    filename = function() {
      paste("filtered_enrichment_abundance_data.csv", Sys.Date(), ".svg", sep="")
    },
    # The content of the file will be the contents of the relabund reactive data
    content = function(file) {
      write.csv(relabunddatasubset(), file, quote = FALSE)
    }
  )
  
  output$RB_metadownload_button <- downloadHandler(
    # The placeholder name for the file
    filename = function() {
      paste("filtered_enrichment_meta_data.csv", Sys.Date(), ".svg", sep="")
    },
    # The content of the file will be the contents of the meta reactive data
    content = function(file) {
      write.csv(RB_metadatasubset(), file, quote = FALSE)
    }
  )
  
  output$enrichmentplotdownload_button_svg <- downloadHandler(
    filename = function() {
      paste("enrichment_taxa.svg", Sys.Date(), ".svg", sep="")
    },
    # The content of the file will be the contents of the mtcars_plot() reactive expression
    # Note how we need to encapsulate the plot in png() and dev.off() functions
    # The syntax also demands that we put print() around our plot
    content = function(file) {
      svg(file, width = 1000, height = 600)
      print(enrichmenttaxaplot())
      dev.off()
    }
  )
  output$enrichmentplotdownload_button_png <- downloadHandler(
    filename = function() {
      paste("enrichment_taxa.png", Sys.Date(), ".svg", sep="")
    },
    # The content of the file will be the contents of the mtcars_plot() reactive expression
    # Note how we need to encapsulate the plot in png() and dev.off() functions
    # The syntax also demands that we put print() around our plot
    content = function(file) {
      png(file, width = 1000, height = 600)
      print(enrichmenttaxaplot())
      dev.off()
    }
  )
  output$flagged_enrichment_list_download_button_csv <- downloadHandler(
    filename = function() {
      paste("Flagged_Enrichments_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(FT_top3_per_ASV())
      asv_list <- FT_top3_per_ASV()
      
      # If no data is available, create an empty data frame to prevent errors
      if (length(asv_list) == 0) {
        combined_df <- data.frame("Message" = "No data passed the filters.")
      } else {
        # Combine the list of data frames into a single data frame
        combined_df <- do.call(rbind, lapply(names(asv_list), function(asv) {
          # Get the top 3 samples and metadata
          df <- asv_list[[asv]] %>%
            left_join(FT_metadata, by = "SampleID") %>%
            select(ASV, SampleID, rel_abund, everything())
          
          # Get the taxonomic data
          taxa_df <- FT_taxadata %>%
            filter(ASV == asv) %>%
            select(-ASV)
          
          # Combine the two data frames by duplicating the taxonomy for each sample
          if (nrow(taxa_df) > 0) {
            # Combine taxonomic data with the samples data
            df <- cbind(df, taxa_df[rep(1, nrow(df)), ])
          }
          
          return(df)
        }))
      }
      
      # Write the combined data frame to the CSV file
      write.csv(combined_df, file, row.names = FALSE)
    }
  )
}
#___________________________________________________________________
# Compiling all the components
shinyApp(ui, server)