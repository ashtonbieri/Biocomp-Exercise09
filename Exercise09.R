### Biocomputing -- Exercise 09
### Ashton Bieri | github @ashtonbieri

### Problem: """akes a directory name as an argument called dir plus any other arguments requried to
#accomplish the specified task.
#The function should read data from each file in the specified directory and calculate the coefficient of variation
#(standard deviation divided by the mean) for a user specified column. These values should be returned as a
#vector.
#To calculate a reliable coefficient of variation we would like to have 50 observations, but we also don't want
#to force the user to use our high standard for the data. Make your function, by default, report an error if any
#file has less than 50 observations, but allow the user to override this behavior and only receive a warningif 50
#observations are not present in a file.
#For an extra credit point, add arguments and associated code to your function to situations where a file
#doesn't have the correct number of columns or the provided data includes NA's.

Exercise09 <- function (dir=".",column=1,separator=",") {
   #Usage: Exercise09("./data",column=3,sep="")
   #dir is the directory to search for files; column is the column within each file; separator is the delimiter between elements in the same row
   
   # find files
   files <- list.files("./test",full.names=TRUE)
   output <- numeric(length = length(files))
   # read in each file
   for (i in 1:length(files)) {
      data <- read.table(file=files[i],sep=separator)
      if(length(data[,column])<50) {
         response<-readline("Your data file does not have 50 observations; do you wish to continue? (Y/N)")
         if(response=="Y"){
            output[i] <- sd(data[,column])/length(data[,column])
         }
      }else{
      output[i] <- sd(data[,column])/length(data[,column])
      }
   }
   # do calculation
   return(output)
}

Exercise09(dir="./test",separator='')
