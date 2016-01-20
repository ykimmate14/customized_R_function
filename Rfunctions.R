# this function takes a dataset as an argument and generates a data frame whose first column shows the dataset's variables and second column shows the class of each variable

whatClass <- function(data){
  varClass <- character()
  for(i in 1:ncol(data)){
    varClass <- append(varClass, class(data[,i]))
  }
  result <- data.frame(names(data), varClass)
  names(result) <- c("variables", "class")
  return (result)
}


# this function takes a dataset as an argument and generates a data frame whose first column shows the dataset's variables and second column shows the number of NA of each variable

NAnum <- function(data){
  y <- integer()
  for(i in 1:ncol(data)){
    y <- append(y, sum(is.na(data[,i])))
  }
  result <- data.frame(names(data), y)
  names(result) <- c("variables", "num of NA")
  return (result)
}

