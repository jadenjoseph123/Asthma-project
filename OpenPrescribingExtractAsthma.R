library(openxlsx)
library(httr)
library(jsonlite)
library(dplyr)
library(readxl)
library(writexl)
library(ggplot2)
library(lubridate)
library(readr)

df <- read_excel("BNFcodesasthma.xlsx")
#mapping <- read_excel("GetUBetterMapping.xlsx")

get_data_for_code <- function(BNFcode, medication, category, chemical) {
  print(paste("Fetching data for BNF Code:", code))  
  url <- paste0("https://openprescribing.net/api/1.0/spending_by_org/?org_type=icb&code=", code, "&format=csv")
  response <- GET(url)
  
  if (status_code(response) == 200) {
    content <- content(response, "text")
    
    if(nchar(content) == 0) {
      print(paste("No data available for BNF Code:", code))
      return(NULL)
    }
    
    df <- read.csv(text = content)
    df$medication <- medication
    df$category <- drug_type
    df$chemical <- descriptor
    return(df)
    
  } else if (status_code(response) == 429) {
    print(paste("Rate limit exceeded for BNF Code:", code))
    Sys.sleep(5)  
    return(get_data_for_code(code, medication, drug_type, descriptor)) 
  } else {
    print(paste("Error:", status_code(response), "for BNF Code:", code)) 
    return(NULL)
  }
}

# Initialise an empty list to store dataframes
data_list <- list()

# Loop through each row in the filtered dataframe
for (i in 1:nrow(df)) {
  code <- df$BNFcode[i]
  medication <- df$medication[i]
  drug_type <- df$category[i]
  descriptor <- df$chemical[i]
  
  # Get data for the current code and add to the list
  temp_df <- get_data_for_code(code, medication, drug_type, descriptor)
  if (!is.null(df)) {
    data_list <- append(data_list, list(temp_df))
  }
}

# Combine all dataframes into one
combined_df <- bind_rows(data_list)

combined_df$date <- format(as.Date(combined_df$date), "%Y-%m")

ics_size_df <- read_csv("ICSsize.csv")

#combined_df$date <- format(as.Date(combined_df$date), "%Y-%m")
ics_size_df$date <- format(as.Date(ics_size_df$date), "%Y-%m")

combined_df <- left_join(combined_df, ics_size_df, by = c("row_id", "date"))

write_csv(combined_df, "inhaler_output.csv")

