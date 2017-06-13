##### A SCRAPE FOR "Bibliography of the Hebrew Book" #####
##### Developed by Russ Gasdia #####
## All questions and bugs should be directed to russell.gasdia@yale.edu ##
## Test of Commits 2 ##

#Set-up
rm(list = ls())
require("rvest")
require("xlsx")
Sys.setlocale("LC_CTYPE","hebrew")

#Scrape
loop = seq(1, 10796, by=1)
for (loop_place in loop) {
        if (loop_place<10){
                place_text<-paste("00000",loop_place,sep="")
        } 
        if (loop_place<100 & loop_place>9){
                place_text<-paste("0000",loop_place,sep="")
        }
        if (loop_place<1000 & loop_place>99){
                place_text<-paste("000",loop_place,sep="")
        }
        if (loop_place<10000 & loop_place>999){
                place_text<-paste("00",loop_place,sep="")
        }
        if (loop_place<100000 & loop_place>9999){
                place_text<-paste("0",loop_place,sep="")
        }
        print(paste(place_text, " of 010796",sep=""))
        url <- paste("http://aleph.nli.org.il/F/NJRLDKHMMXVK7FFB22GQBUSETKKKMSEVSS7HX1NX8AJ2LYPBJA-25502?func=full-set-set&set_number=005548&set_entry=",place_text,"&format=999", sep="")
        cur_page <- read_html(url, encoding="UTF-8")
        col2<- tryCatch({cur_page %>%
                        html_nodes("td") %>%
                        html_text()
                }, error = function(e){""})
                Encoding(col2) <- "UTF-8"
        col1<- tryCatch({cur_page %>%
                        html_nodes(".nowrap") %>%
                        html_text()
                }, error = function(e){""})
                Encoding(col1) <- "UTF-8"
        temp <- data.frame(col1, col2)
        temp$col1 <- as.character(temp$col1)
        temp$col2 <- as.character(temp$col2)
        norows <- nrow(temp)
        count <- norows
        for (rowval in seq(2,norows, by=1)) {
                if ((temp[count,1]==temp[count-1,1])| nchar(temp[count,1])<=2) {
                        test <- 1 
                } else {
                        test <- 0
                }
                if (test==1) {
                        temp[count-1,2] <- paste(temp[count-1,2], temp[count,2], sep = "// ")
                }
                count <- count-1
        }
        temp<-temp[(temp$col1=="Main Entry ?"|temp$col1=="Title ?"|temp$col1=="Imprint ?"|temp$col1=="Descr. ?"|temp$col1=="Language ?"|temp$col1=="Gen. Note ?"|temp$col1=="Permanent Link ?"|temp$col1=="Book Number ?"|temp$col1=="Record Format ?"),]
        Book.Num <- temp[temp$col1=="Book Number ?",2]
        Record.Format <- temp[temp$col1=="Record Format ?",2]
        if (length(temp[temp$col1=="Main Entry ?",2])>0){
                Main.Entry <- temp[temp$col1=="Main Entry ?",2]
        }else{
                Main.Entry <- ""
        }
        Title <- temp[temp$col1=="Title ?",2]
        Imprint <- temp[temp$col1=="Imprint ?",2]
        if (length(temp[temp$col1=="Descr. ?",2])>0){
                Description <- temp[temp$col1=="Descr. ?",2]
        } else {
                Description <- ""
        }
        Language <- temp[temp$col1=="Language ?",2]
        if (length(temp[temp$col1=="Gen. Note ?",2])>0){
                Gen.Note <- temp[temp$col1=="Gen. Note ?",2]  
        } else {
                Gen.Note <- ""
        }
        Perm.Link <- temp[temp$col1=="Permanent Link ?",2]  
        temp <- data.frame(Book.Num, Record.Format, Main.Entry, Title, Imprint, Description, Language, Gen.Note, Perm.Link)
        if (loop_place==1) {
                total <- temp
        } else {
                total <- rbind(total, temp)
        }
}

#write.xlsx(total, "C:/Users/Russ/Dropbox/Currently Working/Hebrew Paper/bibliography of the hebrew book.xlsx", row.names=FALSE)