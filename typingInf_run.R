
source(paste(getwd(),'/help_functions.R',sep = ''))

#' Title
#' 
#' @param human_dataFile includes current year human cases 
#'
#'
trim_hc <- function(human_dataFile){

  require(readxl)
  require(writexl)
  require(dplyr)
  require(stringr)
  require(stringi)
  source(paste(getwd(),'/help_functions.R',sep = ''))
  
  ty_message("Initializing...\n")
  
  # Unique serotypes 
  unique_serotypes <- read_excel(paste(dirname(getwd()),
                                "/Data/Typing_information/serotype_list.xlsx",
                                sep = ''))
  unique_serotypes <- str_remove_all(unname(unlist(unique_serotypes)) ,"\\s$")
  
  # Cleaning human trait serotypes from current year data
  path <- paste(dirname(getwd()),"/Data/Typing_information/",
                human_dataFile,sep = '')
  
  serotypes <- as.data.frame(read_excel(path))
  
  if(ncol(serotypes) != 1){
    ty_message(paste("Error, file ", human_dataFile,
                     " should have only one column (Serotype)! (Error code 4)"))
    return(NULL)
  }
  
  # Change ryhmä B to "1,4,5,12:i", ignore case match
  serotypes <- str_replace(serotypes[,1], regex("ryhmä B",ignore_case = TRUE),
                           "1,4,5,12:i")
  
  serotypes <- str_remove_all(serotypes ,"\\s$") # Remove blanks
  serotypes <- serotype_changes(serotypes,human_dataFile, ty_message,TRUE)
  if(is.null(serotypes)){
    return(NULL)
  }
  serotype_df <- as.data.frame(table(serotypes))
  names(serotype_df) <- c("Serotype","Count")
  serotype_df <- serotype_df[order(serotype_df$Serotype),]

  return(serotype_df)
}

#' Match the new year's Salmonella cases to the older ones and initialize 
#' source attribution model
#'
#' @param source_and_human_old 
#' @param only_human_old 
#' @param human_d 
#' @param animal_d 
#'
#'
#' @examples
#' Add the year 2020 to the previous typing data and initialize the source
#' Attribution model
#' typingInf_run(human_current = "human_cases_trimmed_2020.xlsx",
#                source_current = "sources_2008_2020.xlsx",
#                source_and_human_old = "human_and_source_2008_2019.xlsx",
#                only_human_old = "only_human_2008_2019.xlsx")

typingInf_run <- function(human_current,source_current,
                          source_and_human_old, only_human_old){

  
  quit_program = FALSE
  
  setwd(getwd())
  require(readxl)
  require(writexl)
  require(dplyr)
  require(stringr)
  source(paste(getwd(),'/help_functions.R',sep = ''))
  ty_message("Initializing...\n")
  
  human_data_path <- paste(dirname(getwd()),"/Data/Typing_information/",
                           human_current,sep = '')
  
  # Cleaned human trait serotypes from current year data
  human_data <- as.data.frame(read_excel(human_data_path))
  
  # Trimmed human cases linked to sources from earlier years
  HC_animals_path <- paste(dirname(getwd()),"/Data/Typing_information/",
                           source_and_human_old, sep = '')
  
  HC_animals <- as.data.frame(read_excel(HC_animals_path))
  HC_animals[,1] <- str_remove_all(HC_animals[,1] ,"\\s$") 
  
  # Change the names of the serotypes to correct ones, if any NA's are
  # produced, quit
  HC_animals[,1] <- serotype_changes(HC_animals[,1], source_and_human_old,
                                     ty_message,FALSE)
  
  # Check if something is wrong
  if(any(is.null(HC_animals[,1]))){ return(NULL) }
  
  # Check that the remaining values of the data matrix are not negative numbers
  if(! is_natural_numbers(unname(unlist(HC_animals[,2:ncol(HC_animals)])))){
    ty_message(paste("Error, datafile ", source_and_human_old,
                  " contains negative values (Error code 3)\n" ))
    return(NULL)
  }
  
  # Trimmed serotypes found only in humans from earlier years
  HC_humans_path <- paste(dirname(getwd()),"/Data/Typing_information/",
                          only_human_old, sep = '')
  
  HC_humans <- as.data.frame(read_excel(HC_humans_path))
  HC_humans[,1] <- str_remove_all(HC_humans[,1] ,"\\s$") 
  
  # Change the names of the serotypes to correct ones, if any
  # NA's are produced, quit
  HC_humans[,1] <- serotype_changes(HC_humans[,1],
                                    only_human_old,ty_message,FALSE)
  
  if(any(is.null(HC_humans[,1]))){ return(NULL) }
  
  # Check that the remaining values of the data matrix are not negative numbers
  if(! is_natural_numbers(unname(unlist(HC_animals[,2:ncol(HC_animals)])))){
    ty_message(paste("Error, datafile ", source_and_human_old,
                  " contains negative values
                  (Error code 3)\n" ))
    return(NULL)
  }
  
  # Cleaning animal trait serotypes
  animal_data_path <- paste(dirname(getwd()),"/Data/Typing_information/",
                            source_current,sep = '')
  animal_data <- as.data.frame(read_excel(animal_data_path))
  animal_data[,4] <- str_remove_all(animal_data[,4] ,"\\s$")
  
  # Change the names of the serotypes to correct ones, if any NA's are
  # produced, quit
  animal_data[,4] <- serotype_changes(animal_data[,4],source_current,ty_message,
                                      TRUE)
  
  if(any(is.null(animal_data[,4]))){ return(FALSE) }
  
  # Check that the remaining values of the data matrix are not negative numbers 
  if(!is_natural_numbers(unname(unlist(animal_data[,5:ncol(animal_data)])))){
    ty_message(paste("Error, datafile ", source_current,
                  " contains negative values
                  (Error code 3)\n" ))
    return(NULL)
  }

  # Unique serotypes
  unique_serotypes <- as.data.frame(read_excel(paste(dirname(getwd()),
                                   "/Data/Typing_information/serotype_list.xlsx",
                                   sep = '')))
  
  unique_serotypes <- unname(unlist(unique_serotypes))
  unique_serotypes <- str_remove_all(unique_serotypes ,"\\s$")
  quit_program <- any(is.na(unique_serotypes))
  
  # Get years 
  years <- get_current_years()
 
  # Serotype + previous years
  columnNames <- c("Serotype",years)

  # Number of subtypes by sources (Needed for the sa-model)
  nt <- tibble(k = animal_data[,2], s = animal_data[,1]) %>%
    count(k = as.factor(k), s = as.factor(s)) %>%
    select(n)
  
  # Unique serotypes found both in humans and sources from earlier years
  # in alphabetical order 
  
  animal_serotypes <- sort(unique(animal_data[,4]))
  
  # Uniques serotypes found in humans in the current year
  human_serotypes_current <- unique(human_data[,1])
  
  pattern <- paste0("^",animal_serotypes,collapse = "$|")
  # Current year serotypes that are found in courses
  human_only_serotypes <- grep(pattern, human_serotypes_current,invert = TRUE,
                               value = TRUE)
  # Current year serotypes that are found only in humans
  human_and_source_serotypes <- human_serotypes_current[! human_serotypes_current 
                                                        %in% human_only_serotypes]
  
  # Check if there is a serotype in current years cases that is found on sources 
  # that was before found only humans
  
 if(any(HC_humans[,1] %in% animal_data$Serotype)){
   print("These serotypes are found on sources first time!")
   print(HC_humans[,1][HC_humans[,1] %in% animal_data[,4]])
   moved_serotypes <- HC_humans[HC_humans[,1] %in% animal_data[,4],]
   HC_humans <- HC_humans[! HC_humans[,1] %in% animal_data[,4],]
   HC_animals <- rbind(HC_animals,moved_serotypes)
 }
  
  # Combine this year results to earlier years results
  HC_animals <- merge(HC_animals,human_data[human_data[,1]
                                            %in% human_and_source_serotypes,],
                      all.x = TRUE, all.y = TRUE)
  
  HC_animals[is.na(HC_animals)] <- 0
  colnames(HC_animals) <- columnNames
  
  HC_humans <- merge(HC_humans,human_data[human_data[,1] %in%
                                            human_only_serotypes,],
                     all.x = TRUE, all.y = TRUE)
  
  HC_humans[is.na(HC_humans)] <- 0
  colnames(HC_humans) <- columnNames
  
  # Indexing ----------------------------------------------------------------------------------------------------------
  
  ind = 1
  t <- rep(0, 8 * length(animal_serotypes))
  
  # Index variable (I) determines which salmonella subtype in a full list 
  # corresponds to the same subtype
  
  ind_array <- array(t, dim = c(8,length(animal_serotypes)))
  
  # Number of  subtypes by sources
  nt <- c()
  
  for(type in unique(animal_data[,2])){
    for(source in unique(animal_data[,1])){
      # Spesific data (source and domestic/import)
      data <- animal_data[animal_data[,1] == source & animal_data[,2] == type,]
      inds <- rep(length(data[,4])+1,length(animal_serotypes))
      inds[match(data[,4], animal_serotypes)] <- rep(1:length(data[,4]))
      ind_array[ind,] <- inds
      nt[ind] <- nrow(data)
      ind = ind + 1
    }
  }
  
  df_I <- data.frame(ind_array)
  colnames(df_I) <- HC_animals[,1]
  
  # Proportion of human cases with common subtype (with sources)
  ot <-colSums(HC_animals[,-1]) / (colSums(HC_humans[,-1])
                                   + colSums(HC_animals[,-1]))
  
  # Total number of different subtypes in all sources
  ntot <- length(HC_animals[,1])
  
  # Saving results -----------------------------------------------------------------------------------------------
  
  write_xlsx(data.frame(ot, years),paste(dirname(getwd()),
                                         "/Data/SA/Input/ot.xlsx", sep = ''))
  
  write_xlsx(animal_data,paste(dirname(getwd()),
                               "/Data/SA/Input/sc.xlsx",sep = '' ))
  
  write_xlsx(data.frame(ntot),paste(dirname(getwd()),
                                    "/Data/SA/Input/ntot.xlsx",sep = '' ))
  
  write_xlsx(data.frame(nt),paste(dirname(getwd()),
                                  "/Data/SA/Input/nt.xlsx",sep = '' ))

  write_xlsx(HC_animals, paste(dirname(getwd()),
                               "/Data/SA/Input/hc.xlsx",sep = ''))
  
  write_xlsx(df_I,paste(dirname(getwd()),
                        "/Data/SA/Input/I.xlsx", sep = ''))
  
  # Save updated Typing info tables
  human_only_title_file <- paste("Only_human_", years[1],
                                 "_",years[length(years)],".xlsx" ,sep = '')
  
  human_and_source_title_file <- paste("Human_and_source_", years[1], "_",
                                       years[length(years)],".xlsx", sep = '')
  
  write_xlsx(HC_humans, paste(dirname(getwd()),"/Data/Typing_information/",
                              human_only_title_file, sep = ''))
  
  write_xlsx(HC_animals, paste(dirname(getwd()),"/Data/Typing_information/",
                               human_and_source_title_file, sep = ''))
  
  ty_message("Model is initialized!\n")
}


