library(easyr)

# Reverse dataframe values 
re_arrang <- function(data){
  data_dims <- dim(data)
  data_vec <- as.vector(data)
  ind = 1
  if(length(data_dims) == 2){
    for(a in 1:data_dims[1]){
      for(e in 1:data_dims[2]){
        data[a,e] <- data_vec[ind]
        ind = ind + 1
      }
    }
  }
  if(length(data_dims) == 3){
    for(a in 1:data_dims[1]){
      for(e in 1:data_dims[2]){
        for(i in 1:data_dims[3]){
          data[a,e,i] <- data_vec[ind]
          ind = ind + 1
        }
      }
    }
  }
  return(data)
}

message <- function(m) {
  require(shiny)
  shinyjs::html(id = "text", html = m, add = TRUE)
}

ty_message <- function(m){
  require(shiny)
  shinyjs::html(id = "ty_text", html = m, add = TRUE)
}

ul_message <- function(m){
  require(shiny)
  shinyjs::html(id = "ul_text","html" = m, add = TRUE)
}

lib_search <- function(...){
  libraries <- unlist(list(...))
  loaded_libraries <- rownames(install.packages())
  return(!any(!(libraries %in% loaded_libraries)))
}

# Check if the excel file that is going to be opened is already open in the program
excelCheck <- function(path) {
  file <- read_excel(path)
  tryCatch(is.na(tobool(write_xlsx(file,path), ifna = "return-na", verbose = FALSE)),
           error = function(e) {
              FALSE
           }) 
}


get_current_years <- function(){
  return(as.integer(colnames(read_excel(paste(dirname(getwd()),"/Data/SA/Input/Em.xlsx", sep = '')))))
}

array_to_list <- function(...) {
  col_names <- unlist(list(...))
  list_to_return <- data.frame(matrix(col_names,nr = 1))
  names(list_to_return) <- col_names
  list_to_return <- as.list(list_to_return)
  return(list_to_return)
}

## Return longest word
longest_word <- function(...){
  words <- unlist(list(...))
  if(length(words) == 0){
    return(NA)
  }
  return(words[which.max(nchar(words))])
}

#' Change serotype names
#'
#' @param serotypes renames serotypes
#' @param file_name file where serotypes are located
#' @param print_function 
#' @param ignore_error 
#'
#' @return
#' @export
#'
#' @examples
serotype_changes <- function(serotypes, file_name, print_function, ignore_error){
  
  unique_serotypes <- read_excel(paste(dirname(getwd()),"/Data/Typing_information/serotype_list.xlsx", sep = ''))
  unique_serotypes <- unname(unlist(unique_serotypes))
  unique_serotypes <- str_remove_all(unique_serotypes ,"\\s$") 
  

  new_serotype_names <- unlist(lapply(serotypes, function(x) longest_word(unique_serotypes[str_detect(x,fixed(unique_serotypes, ignore_case=TRUE))])))
  
  if(any(is.na(new_serotype_names))){
    if(ignore_error){
      print_function(paste("These Serotype(s) are filtered out in datafile ",file_name, "\n"))
    }
    else{
      print_function(paste("Found these illegal Serotype(s) in datafile ",file_name, " (Error code 5) :\n"))
      return(NULL)
    }
    illegal_serotypes <- serotypes[is.na(new_serotype_names)]
    print_function(paste(unique(illegal_serotypes),sep = "", end = "\n"))
  }
  
  if(ignore_error | all(! is.na(new_serotype_names))) {

    if(ignore_error == TRUE ){
      serotypes <- serotypes[!is.na(new_serotype_names)]
      new_serotype_names <- new_serotype_names[!is.na(new_serotype_names)]
    }
    # Check if there is made any Serotype changes
    is_serotype_name_same <- new_serotype_names %in% serotypes
    if(any(! is_serotype_name_same)){
      print_function(paste("Made these Serotype name changes to the file ", file_name,  ":\n"))
      name_changes <- paste(unique(serotypes[!is_serotype_name_same]), "-->" ,unique(new_serotype_names[!is_serotype_name_same]), "\n")
      lapply(name_changes,print_function)
    }
  }
  return(new_serotype_names)
}

# Check if array/list/vector includes integers >= 0
is_natural_numbers <- function(data){
  if(any(unlist(lapply(data,is.na))) | any(unlist(lapply(data,is.null)))){
    return(FALSE)
  }
  if(all(unlist(lapply(data,is.numeric)))){
      return(all(data >= 0))
  }
  return(FALSE)
}

get_time_difference <- function(end_time,start_time){
  compile_time <- difftime(end_time,start_time,"secs")
  
  # Convert time to correct form
  if(as.numeric(compile_time,units = "secs") <= 60){
      sec_info = if(as.integer(compile_time,units = "secs") == 1) "sec" else "secs"
    return(paste(as.integer(compile_time,units = "secs"), sec_info))
  }
  else if(1  < as.numeric(compile_time,units = "mins") & as.numeric(compile_time,units = "mins") <= 60){
    min_info = if(as.integer(compile_time,units = "mins") == 1) "min" else "mins"
    return(paste(as.integer(compile_time,units = "mins"), min_info))
  }
  else{
    hour_info = if(as.integer(compile_time,units = "hours") == 1) "hour" else "hours"
    return(paste(as.integer(compile_time,units = "hours"), hour_info))
  }
}

#' Title
#'
#' @param parameter 
#'
#' @return
#' @export
#'
#' @examples
convert_parameter_name <- function(parameter){
  if(is.null(parameter)){
    return("")
  }
  parameter_names = list("slaughpreva" = paste("Slaughter prevalence before",
                                                "cross-contamination"),
                         "p.cc" = paste("Resulting prevalence after cross",
                                        "contamination at meat cutting"),
                         "cont.DC.total" = "Total amount of contaminated meat",
                         "p.cont.DC" = paste("Prevalence of total domestic",
                                             "meat production") ,
                         "Ep" = "Expected proportion for each source",
                         "Epp" = "Expected proportion for each source",
                         "Ep2" = "Expected proportion for each source",
                         "Epp2" = "Expected proportion for each source",
                         "q" = "Source infectiveness",
                         "MEp" = "Mean expected proportion for each source",
                         "MEpp" = "Mean expected proportion for each source",
                         "MEp2" = "Mean expected proportion for each source",
                         "MEpp2" = "Mean expected proportion for each source",
                         "type" = "Serotype infectiveness"
                         )
  if(! (parameter %in% names(parameter_names) )){
    return(parameter)
  }
  return (unname(unlist(parameter_names[parameter])))
  
}




