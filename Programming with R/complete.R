complete <- function(directory, id = 1:332){
  # Get full path to the directory to read the files
  path <- "Programming with R/"
  full_path <- paste0(path, directory, "/")
  
  # Read names of all the monitor files
  files <- list.files(full_path)
  # Make full paths of files -> this is needed to read files properly instead of switching to another workdirectory
  files_full_path <- paste0(full_path, files)
  
  # make empty vector to store the means
  complete_vector <- c()
  # interate over the different monitor files and add pollutant to pollutant vector 
  for(i in id){
    file_select <- read.csv(files_full_path[[i]], header = TRUE, sep = ",") 
    # Remove NAN/NA
    complete_cases <-  sum(complete.cases(file_select))
    # For each monitor file, add the pollutant column to the pollutant vector
    complete_vector <- c(complete_vector, complete_cases)
  }
  # calculate mean and return
  result <- data.frame(id = id, nobs = complete_vector)
  return(result)
  
  
  
}

cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete("specdata", 54)
print(cc$nobs)

set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

