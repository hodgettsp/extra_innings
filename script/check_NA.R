# function to identify missing values by column and row
# takes two parameters .data, a dataset, and .var, a character string
check_NA <- function(.data, .var){
  # returns an array index for missing values
  assign(.var, which(is.na(.data), arr.ind = TRUE), envir = .GlobalEnv)
  # creates dataframe with given character string
  assign(.var, as.data.frame(get(.var)), envir = .GlobalEnv)
  # removes duplicate values
  assign(.var, unique(get(.var)), envir = .GlobalEnv)
}
