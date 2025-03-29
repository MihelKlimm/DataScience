pollutantmean <- function(source, pollutantname, ids) 
##   INPUT
  ## source         = string with data source where files can be found
  ## pollutantname  = string with the pollutant to examine ('nitrate' or 'sulfate')
  ## ids            = vector with ids representing monitor locations
  
##   DESCRIPTION
  ## Function calculates the mean for all pollutant values across monitor locations.
  
##   OUTPUT
  ## Mean of pollutant values accross all stations. 
  
##   Created 18-02-2019 by Fraukje Coopmans
{
        ## Load stringi library: needed for stri_pad_left function
        library(stringi)
        
        ## Only allow sulfate or nitrate calculations
        if (!xor(pollutantname == 'sulfate', pollutantname == 'nitrate'))
                {stop(paste('Warning: Pollutant name should be sulfate or nitrate'))}   
  
        ## Initialize empty vector that will contain the measurements
        pollutant_values_all <- vector()      
  
        for(i in ids)
        {
                ## Create full datafile name and read data
                datafile <- paste(source, "/", stri_pad_left(i, 3, 0), ".csv", sep = "")
                data <- read.csv(datafile)
        
                ## Extract pollutant values and concatenate to data vector
                pollutant_values_all <- c(pollutant_values_all, data[[pollutantname]])
      
        }
        ## Return mean of data without NA values
        mean(pollutant_values_all, na.rm = TRUE)
}
source <- "D:/Coursera/specdata"

omplete <- function(directory, ids)
  ##   INPUT
  ## directory      = string with data source where files can be found
  ## ids            = vector with ids representing monitor locations
  
  ##   DESCRIPTION
  ## Function finds for each location the number of complete observations.
  
  ##   OUTPUT
  ## Data frame with columns id, nobs
  
##   Created 19-02-2019 by Fraukje Coopmans
{
        ## Initialize data frame containing each id and corresponding observation #
        output <- as.data.frame(matrix(ncol = 2, nrow = length(ids)))      
        names(output) <- c("id", "nobs")
        
        for (i in 1:length(ids)) 
        {
                ## Create full datafile name and read data      
                datafile <- paste(directory, "/", stri_pad_left(ids[i], 3, 0), ".csv", sep = "")
                data <- read.csv(datafile)
        
                ## For each location id (i), find the number of complete observations
                output[i,] <- c(ids[i], sum(complete.cases(data)))
        }
        ## Return output
        output
}

corr <- function(directory, threshold = 0)
  ##   INPUT
  ## directory      = string with data source where files can be found
  ## threshold      = number of complete cases a location must have to be    ##                  included
  
  ##   DESCRIPTION
  ## Function goed through all files within the directory and finds those    ## locations that meet the threshold, and for these the correlation        ## between sulfate and nitrate levels is calculated.
  
  ##   OUTPUT
  ## Numberic vector of correlations
  
  ##   Created 04-03-2019 by Fraukje Coopmans
{
  monitor_cor <- vector()
  
  all_files = list.files(directory)
  ## For each file in the directory
  for (i in all_files) {
    # Read file and find number of complete cases
    data <- read.csv(file.path(directory, i))
    completes_vector <- complete.cases(data)
    
    ## If the threshold is met, calculate correlation
    if (sum(completes_vector) >= threshold) {
      monitor_cor <- c(monitor_cor, cor(data$nitrate[completes_vector], data$sulfate[completes_vector]))
    }
  }
  
  ## Return output
  monitor_cor
}
