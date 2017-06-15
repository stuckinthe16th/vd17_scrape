##### A Scrape for the VD17 Catalogue @ http://www.vd17.de###
##### Developed by Russ Gasdia #####
##### All questions and bugs should be directed to russell.gasdia@yale.edu #####

### Load Libraries and Other Set-up

     ## Clear Environment
     rm(list = ls())

     ## Load Functions
     source("gbv_functions.R")
     source("gb_functions.R")

     
     

### Initial Scrape: Gateway-Bayern
     #https://opacplus.bib-bvb.de/TouchPoint_touchpoint/start.do?SearchProfile=Altbestand&SearchType=2
     #Can iterate by year and gather VDN Numbers
     

     ##Perform Search, Scrape Each Result, Loop by Year
     for(year_loop in 1601:1606){
          temp_df <- gb_search_gen(year_loop)
          if(year_loop==1601){
               vd17 <- temp_df
          } else{
               vd17 <- bind_rows(vd17, temp_df)
          }
     }
          
      


### Supplementary Scrape: 
     #https://gso.gbv.de/DB=1.28/
     #Can iterate using VDN Numbers to gather more information on printers etc.
     
### Clean Output
     rm(list=setdiff(ls(), "vd17"))
     file_name <- paste("./output/vd17_scrape_", Sys.Date(), ".csv", sep="")
     write.csv(vd17, file_name, row.names=F)

