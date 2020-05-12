pollutantmean <- function(directory, pollutant , id= 1:332){
  filelist<-list.files(path = directory , pattern = ".csv" , full.names = TRUE)
  #print(filelist)
  values <- numeric()
  for (i in id ){
    
    data <- read.csv(filelist[i])
    # values <- c(values , data[[pollutant]])
    values <- c(values , data[[pollutant]])
  }
  print(mean(values , na.rm = TRUE))
}

pollutantmean ("/Users/MAHE/Desktop/programming/Data Science/Swirl/Swirl_excercises/specdata","sulfate")



complete <- function(directory, id=1:332){
  filelist<-list.files(path = directory , pattern = ".csv" , full.names = TRUE)
  results<- data.frame(id=numeric(),nobs= numeric(0))
  for(i in id){
    monitor_data <- read.csv(filelist[i])
    interested_data <- monitor_data[(!is.na(monitor_data$sulfate)), ]
    interested_data <- interested_data[(!is.na(interested_data$nitrate)), ]
    nobs <- nrow(interested_data)
    results <- rbind(results, data.frame(id=i,nobs=nobs))
    
  }
  print(results)
}

complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))



corr <- function(directory, threshold = 0){
  cor_results <- numeric(0)
  
  complete_cases <- complete(directory)
  complete_cases <- complete_cases[complete_cases$nobs>=threshold, ]
  
  if(nrow(complete_cases)>0){
    for(monitor in complete_cases$id){
      path <- paste(getwd(), "/", directory, "/", sprintf("%03d", monitor), ".csv", sep = "")
      #print(path)
      monitor_data <- read.csv(path)
      #print(monitor_data)
      interested_data <- monitor_data[(!is.na(monitor_data$sulfate)), ]
      interested_data <- interested_data[(!is.na(interested_data$nitrate)), ]
      sulfate_data <- interested_data["sulfate"]
      nitrate_data <- interested_data["nitrate"]
      cor_results <- c(cor_results, cor(sulfate_data, nitrate_data))
    }
  }
  cor_results
}


cc <- complete("specdata", 54)
print(cc$nobs)




