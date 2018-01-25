### Load Libraries
sink(tempfile())
if (!require("pacman"))
     install.packages("pacman")
pacman::p_load("readr", "stringr", "plyr", "reshape2", "ggplot2")
sink()

######################################################
######################################################

### Function to Clean Raw Scrape File

clean_scrape_function <- function(file){
     ## Load in File and Set-up Basic Columns
     cat("*")
     raw_scrape <- read_csv(file, trim_ws = FALSE, col_types = cols())
     
     clean_scrape <- as.data.frame(raw_scrape$vdn)
     clean_scrape$year <- raw_scrape$year_gb
     clean_scrape$title <- raw_scrape$title_gb
     clean_scrape$place <- substring(raw_scrape$places_gbv, 3)
     
     ## Clean Author Names and Information
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
     
     
     ## Clean Languages
     cat("*")
     max_lang <- max(lengths(strsplit(as.character(raw_scrape$languages_gbv), ";")))
     for(x in 1:max_lang){
          col_name <- paste("language_", x, sep="")
          clean_scrape[[col_name]]  <- lapply(strsplit(as.character(raw_scrape$languages_gbv), ";"), "[", x)
          clean_scrape[[col_name]] <- trimws(clean_scrape[[col_name]])
     }
     
     ## Clean Category
     cat("*")
     max_cat <- max(lengths(strsplit(as.character(raw_scrape$category_gbv), "\n|\n\n|\n\n\n")))
     for(x in 1:max_cat){
          col_name <- paste("category_", x, sep="")
          clean_scrape[[col_name]]  <- lapply(strsplit(as.character(raw_scrape$category_gbv), "\n|\n\n|\n\n\n"), "[", x)
          clean_scrape[[col_name]] <- trimws(gsub("\\*", "", clean_scrape[[col_name]]))
     }
     #Create "simple" variable seperation (ex. "Dissertation", "Ordensliteratur", etc.)
     
     ## Return Data Table
     return(clean_scrape)
}

##############################################
##############################################

### Load, Clean, Join Tables

cat("\n\n------------------------\n")
cat("Cleaning Raw Scrape Data\n")
cat("------------------------\n")

     ## Loop Through Files and Run Cleaning
     incomplete <- 0
     incomplete_list <- list()
     for(x in 1600:1699){
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

     ## Reorganize Columns
     names(clean_table_running)[names(clean_table_running)=="raw_scrape$vdn"] <- "vdn"
     clean_table_running <- clean_table_running[c(grep("vdn", names(clean_table_running)), grep("author", names(clean_table_running)), grep("title", names(clean_table_running)), grep("year", names(clean_table_running)), grep("place", names(clean_table_running)), grep("category", names(clean_table_running)), grep("language", names(clean_table_running)))]
     
     
     ## Save File
     if(incomplete>0){
          cat("\n\n WARNING: Series is incomplete, missing years ")
          cat(sapply(incomplete_list, toString), sep=", ")
     }
     cat("\n\n Saving cleaned data to output/cleaned_output/cleaned_scrape_all.RData...")
     save(clean_table_running, file="output/cleaned_output/cleaned_scrape_all.RData")
     cat("\n Saving cleaned data to output/cleaned_output/cleaned_scrape_all.csv...")
     write.csv(clean_table_running, file="output/cleaned_output/cleaned_scrape_all.csv", row.names=FALSE, na="")
     #write.xlsx(clean_table_running, "output/cleaned_scrape_all.xlsx", row.names=FALSE, showNA=FALSE)
     
##############################################
##############################################
     
### Create Time Series For Subjects
     
cat("\n\n-----------------------------\n")
cat("Generating Subject Statistics\n")
cat("-----------------------------\n")
     
     ## Load File
     rm(list=ls())
     load(file="output/cleaned_output/cleaned_scrape_all.RData")
     
     ## Select Only Relevant Columns
     cat("\n Cleaning and reformatting data...\n")
     subject_table_year <- clean_table_running[c("year", "category_1", "category_2", "category_3", "category_4", "category_5")]
     
     ## Wide to Long
     subject_table_year <- melt(subject_table_year, id.vars=c("year"))
     names(subject_table_year)[names(subject_table_year)=="value"] <- "subject"
     subject_table_year <- subset(subject_table_year, select = c(year, subject))
     
     ## Generate Count and Collapse
     subject_table_year$count <- 1
     subject_table_year <- aggregate(subject_table_year$count,list(year = subject_table_year$year, subject = subject_table_year$subject),sum)
     names(subject_table_year)[names(subject_table_year)=="x"] <- "sum_count"
     
     ## Save Tables
     cat("\n Saving subject data to output/subject_output/cleaned_scrape_all.RData...")
     save(subject_table_year, file="output/subject_output/cleaned_scrape_all.RData")
     cat("\n Saving subject data to output/subject_output/cleaned_scrape_all.csv...\n")
     write.csv(subject_table_year, file="output/subject_output/cleaned_scrape_all.csv", row.names=FALSE, na="")
     
     ## Find Top Five Subjects
     cat("\n Finding top twelve subjects...")
     total_books_year <- aggregate(subject_table_year$sum_count,list(subject_table_year$year), sum)
     names(total_books_year) <- c("year", "total_books")
     subject_table_year <- merge(subject_table_year, total_books_year, ID="year")
     subject_table_year$avg <- subject_table_year$sum_count/subject_table_year$total_books
     top_subjects <- aggregate(subject_table_year$avg,list(subject_table_year$subject), mean)
     top_subjects <- top_subjects[order(-top_subjects$x), ]
     top_subjects_5 <- top_subjects$Group.1[1:5]
     top_subjects_12 <- top_subjects$Group.1[1:12]
     cat("\n They are: ")
     cat(top_subjects_12, sep=", ")
     top_subject_table_year_5 <- subject_table_year[subject_table_year$subject %in% top_subjects_5, ]
     top_subject_table_year_12 <- subject_table_year[subject_table_year$subject %in% top_subjects_12, ]
     
     ## Generate Charts
     cat("\n\n Generating charts on subject popularity over time...")
     scaleFUN <- function(x) sprintf("%.0f", x)
     gg <- ggplot(top_subject_table_year_5, aes(x=year, y=avg*100, color=subject)) + 
          geom_line(size =1) + 
          scale_colour_manual(values=c("#d7191c", "#fdae61", "#ffffbf", "#abdda4", "#2b83ba")) + 
          scale_x_continuous(breaks=seq(1600, 1699, 5)) + theme(legend.position="bottom", legend.box = "horizontal", axis.title.x=element_blank(), legend.title.align=0.5, title=element_text(face="bold", margin=c(4,4,4,4)), plot.title=element_text(size=24)) + 
          guides(color = guide_legend(title = "Subjects", title.position = "bottom")) + 
          labs(y="% of Books") + ggtitle("Popularity of Subjects Over Time") +
          geom_vline(xintercept = 1618, linetype="dotted", color = "red") + geom_vline(xintercept = 1648, linetype="dotted", color = "red")
     cat("\n Saving color graph to output/subject_output/subjects_over_time_color.png...")
     ggsave("output/subject_output/subjects_over_time_color.png", plot = gg, width= 9.42, height=6.81)
     gg <- ggplot(top_subject_table_year_5, aes(x=year, y=avg*100, shape=subject)) + geom_point(size=1) + scale_x_continuous(breaks=seq(1600, 1699, 5)) + theme(legend.position="bottom", legend.box = "horizontal", axis.title.x=element_blank(), legend.title.align=0.5, title=element_text(face="bold", margin=c(4,4,4,4)), plot.title=element_text(size=24)) + 
          guides(shape = guide_legend(title = "Subjects", title.position = "bottom")) + 
          labs(y="% of Books") + ggtitle("Popularity of Subjects Over Time") +
          geom_vline(xintercept = 1618, linetype="dotted") + geom_vline(xintercept = 1648, linetype="dotted")
     cat("\n Saving b/w graph to output/subject_output/subjects_over_time_bw.png...")
     ggsave("output/subject_output/subjects_over_time_bw.png", plot = gg, width= 9.42, height=6.81)
     gg<- ggplot(top_subject_table_year_12, aes(x=year, y=avg*100)) + geom_line(size =2) + facet_wrap(~subject, ncol=3) + scale_x_continuous(breaks=seq(1600, 1699, 20)) + theme(axis.title.x=element_blank(), title=element_text(face="bold", margin=c(4,4,4,4))) + labs(y="% of Books") + ggtitle("Popularity of Subjects Over Time") + geom_vline(xintercept = 1618, linetype="dotted", color = "red") + geom_vline(xintercept = 1648, linetype="dotted", color = "red")
     cat("\n Saving panel graph to output/subject_output/subjects_over_time_panel.png...")
     ggsave("output/subject_output/subjects_over_time_panel.png", plot = gg, width= 9.42, height=6.81)