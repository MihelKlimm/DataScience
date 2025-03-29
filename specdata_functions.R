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
