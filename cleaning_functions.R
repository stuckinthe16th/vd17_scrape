###Load Libraries
sink(tempfile())
if (!require("pacman"))
     install.packages("pacman")
pacman::p_load("readr", "stringr", "plyr", "xlsx")
sink()

######################################################
######################################################

###Function to Clean Raw Scrape File

clean_scrape_function <- function(file){
     ##Load in File and Set-up Basic Columns
     cat("*")
     raw_scrape <- read_csv(file, trim_ws = FALSE, col_types = cols())
     
     clean_scrape <- as.data.frame(raw_scrape$vdn)
     clean_scrape$year <- raw_scrape$year_gb
     clean_scrape$title <- raw_scrape$title_gb
     clean_scrape$place <- substring(raw_scrape$places_gbv, 3)
     
     ##Clean Author Names and Information
     cat("*")
     max_authors <- max(lengths(strsplit(as.character(raw_scrape$author_gb), ";")))
     for(x in 1:max_authors){
          col_name <- paste("author_", x, sep="")
          col_name2 <- paste("author_", x, "_pnd", sep="")
          clean_scrape[[col_name]]  <- lapply(strsplit(as.character(raw_scrape$author_gb), ";"), "[", x)
          suppressWarnings(
               clean_scrape[[col_name2]] <- as.numeric(substring(clean_scrape[[col_name]], regexpr("- PND", clean_scrape[[col_name]]) + 6))
          )
          clean_scrape[[col_name]] <- trimws(gsub("- PND.*", "", clean_scrape[[col_name]]))
     }
     
     
     ##Clean Languages
     cat("*")
     max_lang <- max(lengths(strsplit(as.character(raw_scrape$languages_gbv), ";")))
     for(x in 1:max_lang){
          col_name <- paste("language_", x, sep="")
          clean_scrape[[col_name]]  <- lapply(strsplit(as.character(raw_scrape$languages_gbv), ";"), "[", x)
          clean_scrape[[col_name]] <- trimws(clean_scrape[[col_name]])
     }
     
     ##Clean Category
     cat("*")
     max_cat <- max(lengths(strsplit(as.character(raw_scrape$category_gbv), "\n|\n\n|\n\n\n")))
     for(x in 1:max_cat){
          col_name <- paste("category_", x, sep="")
          clean_scrape[[col_name]]  <- lapply(strsplit(as.character(raw_scrape$category_gbv), "\n|\n\n|\n\n\n"), "[", x)
          clean_scrape[[col_name]] <- trimws(gsub("\\*", "", clean_scrape[[col_name]]))
     }
     #Create "simple" variable seperation (ex. "Dissertation", "Ordensliteratur", etc.)
     
     ##Return Data Table
     return(clean_scrape)
}

##############################################
##############################################

### Load, Clean, Join Tables

cat("\n\n------------------------\n")
cat("Cleaning Raw Scrape Data\n")
cat("------------------------\n")

     ##Loop Through Files and Run Cleaning
     incomplete <- 0
     incomplete_list <- list()
     for(x in 1600:1610){
          tryCatch({
               cat("\n Currently Processing: ", x, "  ", sep="")
               file <- paste("output/vd17_scrape_", x, ".csv", sep="")
               if(x==1600){
                    clean_table_running <- clean_scrape_function(file)
               } else {
                    new_table <- clean_scrape_function(file)
                    clean_table_running <- rbind.fill(clean_table_running, new_table)
                    clean_table_running[clean_table_running=="NULL"] <- NA
               }
          }, error=function(err){
             cat("\r File for", x, "does not exist...", sep=" ")  
             incomplete <<- incomplete + 1
             incomplete_list[incomplete] <<- x
          })
     }

     ##Reorganize Columns
     names(clean_table_running)[names(clean_table_running)=="raw_scrape$vdn"] <- "vdn"
     clean_table_running <- clean_table_running[c(grep("vdn", names(clean_table_running)), grep("author", names(clean_table_running)), grep("title", names(clean_table_running)), grep("year", names(clean_table_running)), grep("place", names(clean_table_running)), grep("category", names(clean_table_running)), grep("language", names(clean_table_running)))]
     
     
     ##Save File
     if(incomplete>0){
          cat("\n\n WARNING: Series is incomplete, missing years ")
          cat(sapply(incomplete_list, toString), sep=", ")
     }
     cat("\n\n Saving output/cleaned_scrape_all.RData")
     save(clean_table_running, file="output/cleaned_scrape_all.RData")
     cat("\n Saving output/cleaned_scrape_all.csv")
     write.csv(clean_table_running, file="output/cleaned_scrape_all.csv", row.names=FALSE, na="")
     #write.xlsx(clean_table_running, "output/cleaned_scrape_all.xlsx", row.names=FALSE, showNA=FALSE)
     
     
     