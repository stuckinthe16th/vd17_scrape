##### A Scrape for the VD17 Catalogue @ http://www.vd17.de###
##### Developed by Russ Gasdia #####
##### All questions and bugs should be directed to russell.gasdia@yale.edu #####

###CURRENTLY CHOOSES 100 SAMPLE RESULTS FROM 3 RANDOM YEARS###

### Load Libraries and Other Set-up
     
     sink(file="./scrape_reporting/test_scrape_output.txt", append=FALSE, split=TRUE)
     cat("\n\n--------------------------------------------\n")
     cat("--------------------------------------------\n")
     cat("Scrape of the VD17 Catalogue of German Books\n")
     cat("--------------------------------------------\n")
     cat("--------------------------------------------\n\n")
     saveRDS(format(Sys.time(), '%B %d, %Y @ %I:%M %p'), "./scrape_reporting/start_time.rds")
     cat("Scrape started: ", format(Sys.time()), "\n", sep="")
     cat("Results saved to ", getwd(), "/output \n", sep="")

     ## Clear Environment
     rm(list = ls())

     ## Load Functions
     source("gbv_functions.R")
     source("gb_functions.R")
     source("year_selection.R")

     
     

### Initial Scrape: Gateway-Bayern
     #https://opacplus.bib-bvb.de/TouchPoint_touchpoint/start.do?SearchProfile=Altbestand&SearchType=2
     
     cat("\n\n-----------------------------------\n")
     cat("Processing Data from Gateway-Bayern\n")
     cat("-----------------------------------\n")
     
     ##For Working: Subset Results
     years = year_selection()

     ##Perform Search, Scrape Each Result, Loop by Year
     for(year_loop in years){
          temp_df <- gb_search_gen(year_loop)
          temp_df$year_gb <- year_loop #NOTE: NEED TO ACTUALLY EXTRACT YEAR LATER
          if(year_loop==years[1]){
               vd17 <- temp_df
          } else{
               vd17 <- bind_rows(vd17, temp_df)
          }
     }
     
     ##Cleaning Results
     cat("Cleaning results...\n")
     vd17 <- vd17[, c("Normnummer", "Verfasser", "Titel", "year_gb", "Impressum", "Kollation", "Verf..Ang.", "Weitere.Pers.", "Weitere.Informationen", "permlink")]
     names(vd17) <- c("vdn", "author_gb", "title_gb", "year_gb", "imprint_gb", "collation_gb", "author_information_gb", "other_people_gb", "other_information_gb", "permlink_gb")
     vd17$vdn <- sapply(vd17$vdn, function(x){gsub("VD17 ", "", x)})
     cat("Completed\n\n")
          
      
     
     
### Supplementary Scrape: 
     #https://gso.gbv.de/DB=1.28/
     
     cat("\n\n------------------------\n")
     cat("Processing Data from GBV\n")
     cat("------------------------\n\n")
     
     ##For Working: Subset Results
     #vd17_sample = sample(1:nrow(vd17), 500, replace=F)
     #vd17 <- vd17[vd17_sample,]
     
     ##Set-up Progress Tracker
     total <- nrow(vd17)
     cat("Using VDN from G-B to Scrape GBV...\n")
     
     ##Loop Over Each G-B Result Based on VDN
     for(x in 1:nrow(vd17)){
          cat("\rProgress: ")
          for(y in 1:trunc(x/(total/10))){
               cat("*")
          }
          cat(" Scraping ", x, " of ", total, sep="")
          
          
          url <- paste("https://gso.gbv.de/DB=1.28/CMD?ACT=SRCHA&IKT=8002&TRM=%27", vd17$vdn[x], "%27/LNG=EN/", sep="")
          temp_df <- gbv_scrape(url)
          temp_df$permlink_gbv <- url
          if(x==1){
               vd17_gbv <- temp_df
          } else{
               vd17_gbv <- bind_rows(vd17_gbv, temp_df)
          }
     }
     
     ##Clean Results
     cat("\n\nCleaning results...\n")
     gbv_names <- c("Normnummer", "Fingerprint", "Author", "Title", "Published", "Place.s.", "Collation", "Corporate.bodies", "Collaborator", "Other.persons", "Editor.Printer", "Language.s", "Language.of.original", "Category.Subject", "Notes", "permlink_gbv")
     gbv_names_checked <- c()
     gbv_names_clean <- c("vdn", "fingerprint_gbv", "author_gbv", "title_gbv", "imprint_gbv", "places_gbv", "collation_gb", "corporate_gbv", "collaborator_gbv", "other_people_gbv", "editor_printer_gbv", "languages_gbv", "orig_language.gbv", "category_gbv", "other_information_gb", "permlink_gbv")
     gbv_names_clean_checked <- c()
     for(x in 1:length(gbv_names)){
          if(gbv_names[x] %in% names(vd17_gbv)){
               gbv_names_checked <- c(gbv_names_checked, gbv_names[x])
               gbv_names_clean_checked <- c(gbv_names_clean_checked, gbv_names_clean[x])
          }
     }
     vd17_gbv <- vd17_gbv[, gbv_names_checked]
     names(vd17_gbv) <- gbv_names_clean_checked
     vd17_gbv$vdn <- sapply(vd17_gbv$vdn, function(x){gsub("VD17 ", "", x)})
     
### Clean Output
     
     cat("\n\n----------------------\n")
     cat("Producing Final Results\n")
     cat("-----------------------\n\n")
     
     ##Bring Together Data
     vd17 <- merge(vd17, vd17_gbv, by=c("vdn"))
     
     ##Save Ouput and Clean Workspace
     rm(list=setdiff(ls(), "vd17"))
     file_name <- paste("./output/vd17_scrape_", years[[1]], ".csv", sep="")
     write.csv(vd17, file_name, row.names=F)
     write.csv(vd17, "/var/www/html/data/test_scrape_results.csv", row.names=F)
     
     cat("Scrape ended: ", format(Sys.time()), "\n", sep="")
     saveRDS(format(Sys.time(), '%B %d, %Y @ %I:%M %p'), "./scrape_reporting/end_time.rds")
     sink()
