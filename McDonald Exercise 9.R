####Anna McDonald Biocomputing Exercise 9####

#create function as a new variable
calculate_coefficient <- function(dir,column_number){
  #set the directory name as the working directory
  setwd(dir)
  #compile all of the files in the directory into one variable
  allFiles <- list.files(pattern="*.csv")
  #make the results vector 
  finalResults <- c()
  #check number of columns (part of extra credit)
  #if equal to the input column_number, continue with calculations
  if(column_number=nrow(allFiles)){
    #check to make sure no row has an "NA" (part of extra credit)
    if(allFiles[allFiles=="NA",]){
      print("One or more row contains NA")
      #if no NA, move on to calculations
    }else{
      #check to make sure 50 observations
      #if there are 50 or more, do the calculations
      if(length(allFiles[,column_number])>49){
        coefficient=(sd(allFiles[,column_number])/mean(allFiles[,column_number]))
        #if there aren't 50, print a warning, but still do the calculation
      }else{
        print("Too few observations. 50 or more is recommended.")
        coefficient=(sd(allFiles[,column_number])/mean(allFiles[,column_number]))
      }
      #put the coefficient results into the empty vector from above 
      finalResults=c(coefficient, column_number)
    }
  #if columns not equal to the column number input, no calculations, and message 
  }else{
    print("File does not have the correct number of columns")
    break
  }
#print the actual final results as the return of the function
  return(finalResults)
}