### Biocomputing -- Exercise 09
### Ashton Bieri | github @ashtonbieri

### Function: takes a directory to search all files within; calculating standard of variance for a specific column

Exercise09 <- function (dir=".",column=1,separator=",") {
   #Usage: Exercise09("./data",column=3,sep="")
   #dir is the directory to search for files; column is the column within each file; separator is the delimiter between elements in the same row
   
   # find files
   files <- list.files("./test",full.names=TRUE)
   #set up output vector
   output <- numeric(length = length(files))
   # read in each file
   for (i in 1:length(files)) {
      data <- read.table(file=files[i],sep=separator)
      #check to see if there are 50 observations
      if(length(data[,column])<50) {
         #check to see if user wants to continue with <50
         response<-readline("Your data file does not have 50 observations; do you wish to continue? (Y/N)")
         if(response=="Y"){
            output[i] <- sd(data[,column])/length(data[,column])
         }
      }else{ #there ARE >=50; continue
      output[i] <- sd(data[,column])/length(data[,column])
      }
   }
   # return the output
   return(output)
}

Exercise09(dir="./test",separator='')
