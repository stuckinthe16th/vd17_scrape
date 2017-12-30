### FUNCTIONS TO PROCESS RESULTS FROM GBV-VD17 DATABASE ###
### Developed by Russ Gasdia ###
### All questions and bugs should be directed to russell.gasdia@yale.edu ###

## Load Libraries
sink(tempfile())
if (!require("pacman")) install.packages("pacman")
pacman::p_load("rvest", "httr", "dplyr")
sink()

##Function: Scrape GBV Page
     #Returns data frame
gbv_scrape <- function(url){
     
     #Load in Page
     cur_page <- read_html(url)
     
     #Capture Table
     result_table <- cur_page %>%
               html_nodes("span div:nth-child(2) table") %>%
               html_table()
     result_table <- as.data.frame(result_table)
     result_table[1,2] <- "Normnummer"
     
     #Remove Extraneous Results
     trip <- 0
     for(x in 1:nrow(result_table)){
          if(result_table[x,3]=="" && trip!=1){
               result_table_clean <- result_table[1:x-1, ]
               trip <- 1
          }
     }
     result_table_clean <- result_table_clean[,2:3]
     
     #Clean Labels and Apply
     for(x in 1:nrow(result_table_clean)){
          result_table_clean[x,1] <- gsub(":", "", result_table_clean[x,1]) 
     }
     results_clean <- list()
     for(x in 1:nrow(result_table_clean)){
          results_clean[[result_table_clean[x,1]]] <- result_table_clean[x,2]
     }
     results_df <- as.data.frame(results_clean)
     return(results_df)
}

##Example
#test <- gbv_scrape("https://gso.gbv.de/DB=1.28/CMD?ACT=SRCHA&IKT=8002&TRM=%2712:117245T%27")