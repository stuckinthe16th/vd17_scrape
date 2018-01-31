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
     cat("\n Saving subject data to output/subject_output/subjects_all.RData...")
     save(subject_table_year, file="output/subject_output/subjects_all.RData")
     cat("\n Saving subject data to output/subject_output/subjects_all.csv...\n")
     write.csv(subject_table_year, file="output/subject_output/subjects_all.csv", row.names=FALSE, na="")
     
     ## Find Top Subjects & Change
     cat("\n Finding top twelve subjects...")
     
     # Calculate Total Books
     total_books_year <- aggregate(subject_table_year$sum_count,list(subject_table_year$year), sum)
     names(total_books_year) <- c("year", "total_books")
     subject_table_year <- merge(subject_table_year, total_books_year, ID="year")
     
     # Calculate Percentage of Total
     subject_table_year$avg <- subject_table_year$sum_count/subject_table_year$total_books
     
     
     # Calculate Change +/-
     early <- aggregate(subject_table_year$avg[subject_table_year$year<1618],list(subject_table_year$subject[subject_table_year$year<1618]), mean)
     late <- aggregate(subject_table_year$avg[subject_table_year$year>1648],list(subject_table_year$subject[subject_table_year$year>1648]), mean)
     names(early) <- c("subject", "early_avg")
     names(late) <- c("subject", "late_avg")
     change_table <- merge(early, late, ID="subject")
     subject_table_year <- merge(subject_table_year, change_table, ID="subject")
     subject_table_year$change_avg <- subject_table_year$late_avg-subject_table_year$early_avg
     subject_table_year$change_avg_desc <- "none"
     subject_table_year$change_avg_desc[subject_table_year$change_avg>0] <- "More After War"
     subject_table_year$change_avg_desc[subject_table_year$change_avg<0] <- "Less After War"
     
     # Calculate Top Subjects
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
     
          # Top Five Color Line
          gg <- ggplot(top_subject_table_year_5, aes(x=year, y=avg*100, color=subject)) + 
               geom_line(size =1) + 
               scale_colour_manual(values=c("#d7191c", "#fdae61", "#ffffbf", "#abdda4", "#2b83ba")) + 
               scale_x_continuous(breaks=seq(1600, 1699, 5)) + theme(legend.position="bottom", legend.box = "horizontal", axis.title.x=element_blank(), legend.title.align=0.5, title=element_text(face="bold", margin=c(4,4,4,4)), plot.title=element_text(size=24)) + 
               guides(color = guide_legend(title = "Subjects", title.position = "bottom")) + 
               labs(y="% of Books") + ggtitle("Popularity of Subjects Over Time") +
               geom_vline(xintercept = 1618, linetype="dotted", color = "red") + geom_vline(xintercept = 1648, linetype="dotted", color = "red")
          cat("\n Saving color graph to output/subject_output/subjects_over_time_color.png...")
          ggsave("output/subject_output/subjects_over_time_color.png", plot = gg, width=8, height=4, dpi=600, units="in")
          
          # Top Five B/W
          gg <- ggplot(top_subject_table_year_5, aes(x=year, y=avg*100, shape=subject)) + geom_point(size=1) + scale_x_continuous(breaks=seq(1600, 1699, 5)) + theme(legend.position="bottom", legend.box = "horizontal", axis.title.x=element_blank(), legend.title.align=0.5, title=element_text(face="bold", margin=c(4,4,4,4)), plot.title=element_text(size=24)) + 
               guides(shape = guide_legend(title = "Subjects", title.position = "bottom")) + 
               labs(y="% of Books") + ggtitle("Popularity of VD17 Categories Over Time") +
               geom_vline(xintercept = 1618, linetype="dotted") + geom_vline(xintercept = 1648, linetype="dotted")
          cat("\n Saving b/w graph to output/subject_output/subjects_over_time_bw.png...")
          ggsave("output/subject_output/subjects_over_time_bw.png", plot = gg, width=8, height=4, dpi=600, units="in")
          
          # Top Twelve Panel Line
          gg<- ggplot(top_subject_table_year_12, aes(x=year, y=avg*100)) + geom_line(size =2) + facet_wrap(~subject, ncol=3) + scale_x_continuous(breaks=seq(1600, 1699, 20)) + theme(axis.title.x=element_blank(), title=element_text(face="bold", margin=c(4,4,4,4))) + labs(y="% of Books") + ggtitle("Popularity of VD17 Categories Over Time") + geom_vline(xintercept = 1618, linetype="dotted", color = "red") + geom_vline(xintercept = 1648, linetype="dotted", color = "red")
          cat("\n Saving panel graph to output/subject_output/subjects_over_time_panel_line.png...")
          ggsave("output/subject_output/subjects_over_time_panel_line.png", plot = gg, width=8, height=4, dpi=600, units="in")
          
          # Top Twelve Dot Panel
          gg<- ggplot(top_subject_table_year_12, aes(x=year, y=avg*100)) + geom_point(size=0.5) + geom_smooth(aes(year, avg*100, color=factor(change_avg_desc))) + facet_wrap(~subject, ncol=3) + scale_x_continuous(breaks=seq(1600, 1699, 20)) + theme(axis.title.x=element_blank(), legend.title = element_blank(), title=element_text(face="bold", margin=c(4,4,4,4))) + labs(y="% of Books") + ggtitle("Popularity of VD17 Categories Over Time") + geom_vline(xintercept = 1618, linetype="dotted", color = "red") + geom_vline(xintercept = 1648, linetype="dotted", color = "red")
          cat("\n Saving panel graph to output/subject_output/subjects_over_time_panel_dot.png...")
          ggsave("output/subject_output/subjects_over_time_panel_dot.png", plot = gg, width=8, height=4, dpi=600, units="in")
          
     ## Religious Patterns Over Time
     cat("\n Plotting trends in religious books...")     
          
          # Load and Merge Religious Categories
          religious_subjects <- read.csv("religious_subjects.csv")
          subject_table_year <- merge(subject_table_year, religious_subjects, ID="subject")
          religious_subject_time <- subject_table_year[, c("year", "religious_likeliehood", "sum_count")]
          
          # Summarize Likeliehood by Year
          religious_subject_time <- aggregate(religious_subject_time$sum_count, list(year= religious_subject_time$year, likeliehood=religious_subject_time$religious_likeliehood), sum)
          names(religious_subject_time)[names(religious_subject_time)=="x"] <- "sum_count"
          
          # Change to Average by Year
          religious_subject_time <- merge(religious_subject_time, total_books_year, ID="year")
          religious_subject_time$avg <- religious_subject_time$sum_count/religious_subject_time$total_books
          
          # Combine Categorizations
          religious_subject_time_comb <- religious_subject_time
          religious_subject_time_comb$likeliehood_combined[religious_subject_time_comb$likeliehood=="Likely" | religious_subject_time_comb$likeliehood=="Very Likely"] <- "Likely or Very Likely"
          religious_subject_time_comb$likeliehood_combined[religious_subject_time_comb$likeliehood=="Unlikely" | religious_subject_time_comb$likeliehood=="Very Unlikely"] <- "Unlikely or Very Unlikely"
          religious_subject_time_comb$likeliehood_combined[religious_subject_time_comb$likeliehood=="Unknown"] <- "Unknown"
          religious_subject_time_comb <- aggregate(religious_subject_time_comb$avg, list(year=religious_subject_time_comb$year, likeliehood_combined=religious_subject_time_comb$likeliehood_combined), sum)
          names(religious_subject_time_comb)[names(religious_subject_time_comb)=="x"] <- "avg"
          
          # Plot All Likeliehood
          gg <- ggplot(religious_subject_time_comb, aes(x=year, y=avg*100, color=likeliehood_combined)) + geom_point() + geom_smooth(aes(color=likeliehood_combined)) + scale_x_continuous(breaks=seq(1600, 1699, 20)) + theme(axis.title.x=element_blank(), title=element_text(face="bold", margin=c(4,4,4,4))) + labs(y="% of Books") + ggtitle("VD17 Religious vs. Non-Religious Categories Over Time") + geom_vline(xintercept = 1618, linetype="dotted", color = "red") + geom_vline(xintercept = 1648, linetype="dotted", color = "red") + labs(color="Likeliehood of\nBeing Relgious") 
          
          
          cat("\n Saving graph to output/subject_output/religious_subjects_over_time_vd17.png... \n")
          ggsave("output/subject_output/religious_subjects_over_time_vd17.png", plot = gg, width=8, height=4, dpi=600, units="in")
          
     ## Intra-Category Patterns
     cat("\n Plotting intra-category patterns over time...")
          
          # Dissertation
          cat("\n ...Dissertations...")
          dissertation_table_year <- subject_table_year[grepl("Dissertation:", subject_table_year$subject),]
          dissertation_year_sums <- aggregate(dissertation_table_year$sum_count, list(year=dissertation_table_year$year), sum)
          names(dissertation_year_sums)[2] <- "dissertations_total"
          dissertation_table_year <- merge(dissertation_table_year, dissertation_year_sums, ID="year")
          dissertation_table_year$diss_avg <- dissertation_table_year$sum_count/dissertation_table_year$dissertations_total
          dissertation_table_year$dissertation_type <- trimws(gsub("Dissertation:", "", dissertation_table_year$subject))
          gg <- ggplot(dissertation_table_year, aes(x=year, y=diss_avg*100, color=dissertation_type)) + geom_point() + geom_smooth(aes(color=dissertation_type)) + scale_x_continuous(breaks=seq(1600, 1699, 20)) + theme(axis.title.x=element_blank(), title=element_text(face="bold", margin=c(4,4,4,4))) + labs(y="% of Dissertations") + ggtitle("VD17 Category 'Dissertation' Over Time") + geom_vline(xintercept = 1618, linetype="dotted", color = "red") + geom_vline(xintercept = 1648, linetype="dotted", color = "red") + labs(color="Type of\nDissertation") 
          suppressMessages(
               ggsave("output/subject_output/vd17_dissertations_over_time.png", plot = gg, width=8, height=4, dpi=600, units="in")
          )
          
          # Gelegenheitsschrift
          cat("\n ...Gelegenheitsschrift...")
          gelegenheitschrift_table_year <- subject_table_year[grepl("Gelegenheitsschrift:", subject_table_year$subject),]
          gelegenheitschrift_table_year$gelegenheitschrift_type <- trimws(gsub("Gelegenheitsschrift:", "", gelegenheitschrift_table_year$subject))
          gelegenheitschrift_year_sums <- aggregate(gelegenheitschrift_table_year$sum_count, list(year=gelegenheitschrift_table_year$year), sum)
          names(gelegenheitschrift_year_sums)[2] <- "gelegenheitschrift_total"
          gelegenheitschrift_table_year <- merge(gelegenheitschrift_table_year, gelegenheitschrift_year_sums, ID="year")
          gelegenheitschrift_table_year$gel_avg <- gelegenheitschrift_table_year$sum_count/gelegenheitschrift_table_year$gelegenheitschrift_total
          gelegenheitschrift_table_year$likeliehood_combined[gelegenheitschrift_table_year$religious_likeliehood=="Likely" | gelegenheitschrift_table_year$religious_likeliehood=="Very Likely"] <- "Likely or Very Likely"
          gelegenheitschrift_table_year$likeliehood_combined[gelegenheitschrift_table_year$religious_likeliehood=="Unlikely" | gelegenheitschrift_table_year$religious_likeliehood=="Very Unlikely"] <- "Unlikely or Very Unlikely"
          gelegenheitschrift_table_year$likeliehood_combined[gelegenheitschrift_table_year$religious_likeliehood=="Unknown"] <- "Unknown"
          gelegenheitschrift_table_year <- aggregate(gelegenheitschrift_table_year$gel_avg, list(year=gelegenheitschrift_table_year$year, likeliehood_combined=gelegenheitschrift_table_year$likeliehood_combined), sum)
          names(gelegenheitschrift_table_year)[names(gelegenheitschrift_table_year)=="x"] <- "avg"
          gg <- ggplot(gelegenheitschrift_table_year, aes(x=year, y=avg*100, color=likeliehood_combined)) + geom_point() + geom_smooth(aes(color=likeliehood_combined)) + scale_x_continuous(breaks=seq(1600, 1699, 20)) + theme(axis.title.x=element_blank(), title=element_text(face="bold", margin=c(4,4,4,4))) + labs(y="% of Gelegenheitschrift") + ggtitle("VD17 Category 'Gelegenheitsschrift' Over Time") + geom_vline(xintercept = 1618, linetype="dotted", color = "red") + geom_vline(xintercept = 1648, linetype="dotted", color = "red") + labs(color="Type of Gelegenheitschrift\nby Likelihood of\nBeing Religious") 
          suppressMessages(
               ggsave("output/subject_output/vd17_gelegenheitschrift_over_time.png", plot = gg, width=8, height=4, dpi=600, units="in")
          )
          
          # Kommentar
          cat("\n ...Kommentar...")
          kommentar_table_year <- subject_table_year[grepl("Kommentar:", subject_table_year$subject),]
          kommentar_year_sums <- aggregate(kommentar_table_year$sum_count, list(year=kommentar_table_year$year), sum)
          names(kommentar_year_sums)[2] <- "kommentar_total"
          kommentar_table_year <- merge(kommentar_table_year, kommentar_year_sums, ID="year")
          kommentar_table_year$komm_avg <- kommentar_table_year$sum_count/kommentar_table_year$kommentar_total
          kommentar_table_year$kommentar_type <- trimws(gsub("Kommentar:", "", kommentar_table_year$subject))
          gg <- ggplot(kommentar_table_year, aes(x=year, y=komm_avg*100, color=kommentar_type)) + geom_point() + geom_smooth(aes(color=kommentar_type)) + scale_x_continuous(breaks=seq(1600, 1699, 20)) + theme(axis.title.x=element_blank(), title=element_text(face="bold", margin=c(4,4,4,4))) + labs(y="% of Kommentar") + ggtitle("VD17 Category 'Kommentar' Over Time") + geom_vline(xintercept = 1618, linetype="dotted", color = "red") + geom_vline(xintercept = 1648, linetype="dotted", color = "red") + labs(color="Type of\nKommentar") 
          suppressMessages(
               ggsave("output/subject_output/vd17_kommentar_over_time.png", plot = gg, width=8, height=4, dpi=600, units="in")
          )
          
          # Streitschrift
          cat("\n ...Streitschrift...")
          streitschrift_table_year <- subject_table_year[grepl("Streitschrift:", subject_table_year$subject),]
          streitschrift_year_sums <- aggregate(streitschrift_table_year$sum_count, list(year=streitschrift_table_year$year), sum)
          names(streitschrift_year_sums)[2] <- "streitschrift_total"
          streitschrift_table_year <- merge(streitschrift_table_year, streitschrift_year_sums, ID="year")
          streitschrift_table_year$streit_avg <- streitschrift_table_year$sum_count/streitschrift_table_year$streitschrift_total
          streitschrift_table_year$streitschrift_type <- trimws(gsub("Streitschrift:", "", streitschrift_table_year$subject))
          gg <- ggplot(streitschrift_table_year, aes(x=year, y=streit_avg*100, color=streitschrift_type)) + geom_point() + geom_smooth(aes(color=streitschrift_type)) + scale_x_continuous(breaks=seq(1600, 1699, 20)) + theme(axis.title.x=element_blank(), title=element_text(face="bold", margin=c(4,4,4,4))) + labs(y="% of Streitschrift") + ggtitle("VD17 Category 'Streitschrift' Over Time") + geom_vline(xintercept = 1618, linetype="dotted", color = "red") + geom_vline(xintercept = 1648, linetype="dotted", color = "red") + labs(color="Type of\nStreitschrift") 
          suppressMessages(
               ggsave("output/subject_output/vd17_streitschrift_over_time.png", plot = gg, width=8, height=4, dpi=600, units="in")
          )