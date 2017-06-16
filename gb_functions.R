### FUNCTIONS TO PROCESS RESULTS FROM GB-VD17 DATABASE ###
### Developed by Russ Gasdia ###
### All questions and bugs should be directed to russell.gasdia@yale.edu ###

## Load Libraries
sink(tempfile())
if (!require("pacman"))
     install.packages("pacman")
pacman::p_load("rvest", "httr", "dplyr")
sink()

##Function: Scrape G-B Page
#Returns named character vector
gb_scrape <- function(url) {
     cur_page <- read_html(url)
          #Load Data Labels
          data_labels <- cur_page %>%
               html_nodes(".titleinfo .c2") %>%
               html_text()
          #Load Full Data
          data_full <- cur_page %>%
               html_nodes(".titleinfo li") %>%
               html_text()
          if (length(data_full > 1)) {
               #Produce Clean Data
               data_clean <- character()
               for (x in 1:length(data_full)) {
                    data_clean[x] <- gsub(data_labels[x], "", data_full[x])
               }
               #Drop Missing Data
               for (x in 1:length(data_clean)) {
                    if (data_clean[x] == "" | is.na(data_clean[x])) {
                         data_clean <- data_clean[-x]
                         data_labels <- data_labels[-x]
                    }
               }
               #Clean Data Labels
               for (x in 1:length(data_labels)) {
                    data_labels[x] <-
                         substr(data_labels[x],
                                start = 1,
                                stop = nchar(data_labels[x]) - 2)
               }
               #Apply Labels to Data and Add Permlink
               names(data_clean) <- data_labels
               data_clean$permlink <- url
               return(data_clean)
          } else{
               return(data.frame())
          }
}

##Function: Process Search Page
#Returns data frame
gb_search_process <- function(list_vdn, year, cur_pos_page) {
     #Create URLs for Each Result from VDNs
     list_urls <- list()
     for (x in 1:length(list_vdn)) {
          url_temp <- substr(list_vdn[x],
                             start = 6,
                             stop = nchar(list_vdn[x]))
          url_temp <- gsub("\\:", "%3A", url_temp)
          url_temp <-
               paste("http://gateway-bayern.de/VD17+", url_temp, sep = "")
          list_urls[x] <- url_temp
     }
     
     #Scrape Each Page from Search
     for (y in 1:length(list_urls)) {
          for(z in 1:10){
               temp_df <- try(data.frame(as.list(gb_scrape(list_urls[[y]]))), silent=TRUE)
               if(inherits(temp_df, "try-error")) {
                    cat("\r", "502 Error Retrying Scrape for ", year, " Page: ", cur_pos_page, " ", sep="")
                    Sys.sleep(30)
               } else{
                         break
                    }
          
          }
          if (y == 1) {
               running_df <- temp_df
          } else{
               running_df <- bind_rows(running_df, temp_df)
          }
     }
     return(running_df)
}

##Function: Loop Through Search Pages in Given Year
#Returns data frame
gb_search_gen <- function(year) {
     #Load Initial Year Search Page
     cur_pos <- 1
     cur_pos_page <- 1
     url <-
          "https://opacplus.bib-bvb.de/TouchPoint_touchpoint/start.do?SearchProfile=Altbestand&SearchType=2"
     pgsession <- html_session(url)
     pgform <- html_form(pgsession)[[1]]
     filled_form <- set_values(
          pgform,
          "searchRestrictionValue1[0]" = year,
          "searchRestrictionValue2[0]" = year
     )
     suppressMessages(page_result <- submit_form(session = pgsession, form = filled_form))
     list_vdn <- page_result %>%
          html_nodes("b") %>%
          html_text()
     
     
     
     #Data Frame of Initial Year Results
     cat("\n", "Scraping ", year, " Page: ", cur_pos_page, " ", sep="")
     assign(paste(year, "_data", sep = ""),
            gb_search_process(list_vdn, year, cur_pos_page))
     
     
     #Loop Over "Next" Pages
     next_loop <- "go"
     page_result <- html(page_result$url)
     
     while (next_loop == "go") {
          cur_pos <- cur_pos + 10
          cur_pos_page <- cur_pos_page + 1
          cat("\r", "Scraping ", year, " Page: ", cur_pos_page, " ", sep="")
          pagination <- page_result %>%
               html_nodes(".anchor+ .navigation a") %>%
               html_attr("href")
          next_url <- ""
          for (x in 1:length(pagination)) {
               if (regexpr(paste("curPos=", cur_pos, "#", sep = ""),
                           pagination[x])[1] != -1) {
                    next_url <-
                         paste("https://opacplus.bib-bvb.de",
                               pagination[x],
                               sep = "")
               }
          }
          if (next_url != "") {
               for(z in 1:10){
                    page_result <- try(read_html(next_url), silent=TRUE)
                    if(inherits(page_result, "try-error")) {
                         cat("\r", "502 Error Retrying Scrape for ", year, " Page: ", cur_pos_page, " ", sep="")
                         Sys.sleep(30)
                    } else{
                         break
                    }
                    
               }
               list_vdn <- page_result %>%
                    html_nodes("b") %>%
                    html_text()
               temp_df <- gb_search_process(list_vdn, year, cur_pos_page)
               assign(paste(year, "_data", sep = ""),
                      bind_rows(get(
                           paste(year, "_data", sep = "")
                      ), temp_df))
          } else {
               next_loop <- "stop"
          }
     }
     cat("\r", "Scraped ", cur_pos_page, " Pages from ", year, sep="")
     return(get(paste(year, "_data", sep = "")))
}
