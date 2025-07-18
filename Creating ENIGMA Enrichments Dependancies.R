# This script is for making the actual data base, tables, and viewing db contents
# Jennifer Moreno 6/5/25
# Load in needed packages
library(duckdb)
library(dplyr)
library(DT)
library(RSQLite)
library(tidyverse) # This package has tidyr, dplyr, and ggplot2 (among others)
library(phyloseq)
library(DT)
library(here)
library(DBI)
library(janitor)

#_________________________________________________
#CREATING AND CONNECTING TO DB
  # Define the directory path and database file name
  # REPLACE with your own directory path
  db_dir <- here("Data Output")
  # REPLACE with your own name.db for the data base
  db_file <- "exampleduckbyjenTEST.db"
  # Create the directory if it doesn't exist
  dir.create(db_dir, recursive = TRUE, showWarnings = FALSE)
  # Connect to the database - no need to replace anything here
  con <- dbConnect(duckdb(file.path(db_dir, db_file)))

#_____________________________
#CREATING DB TABLES
  # Getting the data from the csv (raw data)
  # Headers set to true if the first row has the headers
  #METADATA
    # REPLACE with your own filename
    # My suggestion is to keep a constantly accumulating "existing_metadata.csv" file that you can reference when merging
    existing_metadata <- read.csv(here("Data Input", "Copy of Meta_data_test - Sheet4(1).csv"), row.names = NULL)
    existing_metadata <- existing_metadata[-c(1)]
    metadata_adding <- read.csv(here("Data Input", "meta_data - meta.csv"))
    # Excludes exact duplicate rows
    final_metadata <- unique(bind_rows(existing_metadata, metadata_adding))
    #check the data before overwriting OG
    #print(datatable((tail(final_metadata))))
    # Writing the data into the database
    # The words in the Quotes are what this table of data will be named in the database (for future referencing)
    dbWriteTable(con, "meta_data", final_metadata, overwrite = TRUE)
    # Saving the existing metadata in the table NOW (updated)
    write.csv(final_metadata, paste("existing_metadata_", Sys.Date(), ".csv", sep=""))
  
  #COUNTSDATA
    # REPLACE with your own filename
    existing_countsdata <-read.csv(here("Data Input", "existing_countsdata_2025-07-15.csv"), row.names = NULL)%>%
      mutate(across(where(is.numeric), ~replace_na(., 0)))
    existing_countsdata <- existing_countsdata[-c(1)]
    colnames(existing_countsdata) <- gsub("X", "", colnames(existing_countsdata))
    # REPLACE with your own filename
    countsdata_adding <-read.csv(here("Data Input", "counts_data - OR_enrich_wide2.csv"))
    colnames(countsdata_adding) <- gsub("X", "", colnames(countsdata_adding))
    #in case first column isn't labeled ASV
    colnames(countsdata_adding)[1] <- "ASV"
    final_countsdata <- unique(bind_rows(existing_countsdata, countsdata_adding))%>%
      mutate(across(where(is.numeric), ~replace_na(., 0)))
    # Finding duplicate ASVs
    duplicated_asv <- duplicated(final_countsdata$ASV)
    # Grab duplicate entire rows
    duplicate_rows <- final_countsdata[duplicated_asv, ]
    # Combine rows based on ASV (add sums)
    # May take a few minutes, pls be patient !!
    final_countsdata <- aggregate(.~ASV, data = final_countsdata, sum)
    # Upload back onto the db
    dbWriteTable(con, "counts_data", final_countsdata, overwrite = TRUE) 
    # Saving the existing countsdata in the table NOW (updated)
    write.csv(final_countsdata, paste("existing_countsdata_", Sys.Date(), ".csv", sep=""))
  
  #TAXADATA
    # REPLACE with your own filename
    existing_taxadata <- read.csv(here("Data Input", "tax_data - Sheet1.csv"), row.names = NULL)
    existing_taxadata <- existing_taxadata[-c(1)]
    #filling in missing values of the table
    # REPLACE with your own filename
    taxadata_adding <- read.csv(here("Data Input", "tax_data - Sheet1.csv"), row.names = 1)%>%
      filter(Kingdom == "Bacteria" | Kingdom == "Archaea")%>%
      mutate(across(!c(Kingdom, Phylum, Class, Order, Family, Genus), ~na_if(., "")))%>%
      remove_empty("cols")%>%
      rownames_to_column("ASV")
    # Function to find the most specific taxonomic designation
    fill_missing_taxa <- function(row) {
      # Find the indices of filled cells in each row
      filled_indices <- which(row != "")
      # If there are no filled cells, return the row as totally empty
      if (length(filled_indices) == 0) {
        return(row)
        #if there are filled cells
      } else {
        # Get the most specific taxonomic designation
        most_specific_taxa <- row[filled_indices[length(filled_indices)]]
        # Fill in the missing values with the most specific taxonomic designation available
        row[row == ""] <- most_specific_taxa
        return(row)
      }
    }
    
    filled_taxadata <- apply(taxadata_adding, 1, fill_missing_taxa)
    # Convert the result back to a data frame, ensuring rows and columns are correct
    filled_taxadata <- as.data.frame((filled_taxadata), stringsAsFactors = FALSE)
    # fixing the formatting of columns and headers
    taxadata_adding <- data.frame(t(filled_taxadata[-1]))
    row.names(taxadata_adding) <- NULL
    final_taxadata <- unique(bind_rows(existing_taxadata, taxadata_adding))
    # Upload back onto the db
    dbWriteTable(con, "taxa_data", final_taxadata, overwrite = TRUE) 
    # Saving the existing countsdata in the table NOW (updated)
    write.csv(final_taxadata, paste("existing_taxadata_", Sys.Date(), ".csv", sep=""))
   
  #TOTAL COUNTS  
    # Get the data
    countsdata <- dbGetQuery(con,"SELECT * FROM counts_data")
    # Ensures the correct row names
    rownames(countsdata) <- countsdata[, 1]
    countsdata <- countsdata[, -1]
    # Ensures all values in the table are number
    countsdatanumeric<- countsdata[, sapply(countsdata, is.numeric)]
    # takes the sums of the columns
    countsums <- colSums(countsdatanumeric, na.rm=FALSE)
    # makes a data frame of the table for manip
    # naming the columns in parens
    countsums_df <- data.frame(SampleID = names(countsdata), Sum=countsums)
    countsums_df$SampleID <- gsub("X", "", countsums_df$SampleID)
    
    #For debugging/verification before appending
    #datatable(countsums_df)
    dbWriteTable(con, "countsums_data", countsums_df, overwrite = TRUE)
    
    
  #RELATIVE ABUNDANCE
    # Get data from Counts 
    countsdata <- dbGetQuery(con,"SELECT * FROM counts_data")
    
    # Getting correct row names
    rownames(countsdata) <- countsdata[, 1]
    countsdata <- countsdata[, -1]
    countsdatanumeric<- countsdata[, sapply(countsdata, is.numeric)]
    countsums <- colSums(countsdatanumeric, na.rm=TRUE)
    countsums_df <- data.frame(SampleID = names(countsdata), Sum=countsums)
    countsums_df$SampleID <- gsub("X", "", countsums_df$SampleID)
    colnames(countsdata) <- gsub("X", "", colnames(countsdata))
    
    # Assuming countsums_df has columns 'SampleID' and 'Sum'
    # Assuming countsdata has ASV rownames and SampleIDs as column headers
    # Ensure SampleID is a character vector in both data frames
    countsums_df$SampleID <- as.character(countsums_df$SampleID)
    colnames(countsdata) <- as.character(colnames(countsdata))
    
    # Create a named vector of sums for quick lookup
    sums_lookup <- setNames(countsums_df$Sum, countsums_df$SampleID)
    # Initialize relative_abundance with the same dimensions as countsdata
    relative_abundance <- matrix(0, nrow=nrow(countsdata), ncol=ncol(countsdata))
    
    # Get column and row names
    sample_ids <- colnames(countsdata)
    asvs <- rownames(countsdata)
    
    # Extract sum values for all sample IDs at once
    sum_values <- sapply(sample_ids, function(id) sums_lookup[[id]])
    for (i in seq_along(sample_ids)) {
      sample_id <- sample_ids[i]
      sum_value <- sum_values[i]
      
      # Check if the sum is not NA and not zero
      if (!is.na(sum_value) && sum_value != 0) {
        # Vectorized operation for all ASVs
        counts <- countsdata[, sample_id]
        non_zero_counts <- counts > 0
        relative_abundance[non_zero_counts, i] <- counts[non_zero_counts] / sum_value
      }else {
        # If sum is zero or NA, set relative abundance to 0 for all ASVs
        relative_abundance[, i] <- 0
      }
    }
    relative_abundance_df <- as.data.frame(relative_abundance)
    colnames(relative_abundance_df) <- sample_ids
    rownames(relative_abundance_df) <- asvs
    relative_abundance_df <- rownames_to_column(relative_abundance_df, "ASV")
    dbWriteTable(con, "relabund_data", relative_abundance_df, overwrite = TRUE)
    
#_____________________________
#VIEWING DB CONTENTS
  #TABLE NAMES
  tables <- dbListTables(con)
  for (table in tables) {
    print(table)
  }
  #TABLE CONTENTS
  #change the FROM table to the table name desired
  data <- dbGetQuery(con, "SELECT * FROM taxa_data")
  datatable(head(data))
  
  #ALTERATIONS:
  #if you need to change a column name
  #dbExecute(con, "ALTER TABLE relabund_data RENAME COLUMN row_names TO ASV;")
#___________________
dbDisconnect(con)
