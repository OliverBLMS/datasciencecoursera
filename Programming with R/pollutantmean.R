
pollutantmean <- function(directory, pollutant = "sulfate", id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  # Get full path to the directory to read the files
  path <- "Programming with R/"
  full_path <- paste0(path, directory, "/")
  
  # Read names of all the monitor files
  files <- list.files(full_path)
  # Make full paths of files -> this is needed to read files properly instead of switching to another workdirectory
  files_full_path <- paste0(full_path, files)
  
  # make empty vector to store the means
  pollutant_vector <- c()
  # interate over the different monitor files and add pollutant to pollutant vector 
  for(i in id){
    file_select <- read.csv(files_full_path[[i]], header = TRUE, sep = ",") 
    # Remove NAN/NA
    pollutant_select <-  na.omit(file_select[,pollutant])
    # For each monitor file, add the pollutant column to the pollutant vector
    pollutant_vector <- c(pollutant_vector, pollutant_select)
  }
  # calculate mean and return
  return(mean(pollutant_vector, na.rm = T))
  
}

pollutantmean("specdata", "sulfate", 1:10)
## [1] 4.064128
pollutantmean("specdata", "nitrate", 70:72)
## [1] 1.706047
round(pollutantmean("specdata", "sulfate", 34), 3)
pollutantmean("specdata", "nitrate")

