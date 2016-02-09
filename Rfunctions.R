# this function takes a dataset as an argument and generates a data frame whose first column 
# shows the dataset's variables and second column shows the class of each variable

whatClass <- function(data){
  varClass <- character()
  for(i in 1:ncol(data)){
    varClass <- append(varClass, class(data[,i]))
  }
  result <- data.frame(names(data), varClass)
  names(result) <- c("variables", "class")
  return (result)
}


# this function takes a dataset as an argument and generates a data frame whose first column shows the dataset's 
# variables and second column shows the number of NA of each variable

NAnum <- function(data){
  y <- integer()
  for(i in 1:ncol(data)){
    y <- append(y, sum(is.na(data[,i])))
  }
  result <- data.frame(names(data), y)
  names(result) <- c("variables", "num of NA")
  return (result)
}

# the function takes data frame and column number as arguments and return data frame with 
# unique values on the specified column and its number of occurrence.
NofRow <- function(data, c){
    list1 <- unique(data[,c])
    list2 <- list()
    for(i in 1:length(list1)){
        list2 <- c(list2, sum(data[,2] %in% list1[i]))
    }
    result <- data.frame(uniqueV = list1, count = unlist(list2))
    return(result)
}
