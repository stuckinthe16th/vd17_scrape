year_selection <- function(){
     #Load List of Years Left to Process
     load(file="years_left_to_process.RData")

     #Select Random Year
     year_index <- sample(1:length(years_left_to_process), 1, replace=F)
     year_temp <- years_left_to_process[year_index]
     
     #Delete Year from Master List
     years_left_to_process <- years_left_to_process[-year_index]
     save(years_left_to_process, file="years_left_to_process.RData")
     
     #Return Selected Year
     return(year_temp[[1]])
     
}