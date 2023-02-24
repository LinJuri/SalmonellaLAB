
require(rlist)
require(jsonlite)
require(stringr)
require(readxl)
require(writexl)


sourceFinder_info_table <- function(){
  setwd(getwd())
  id_inf_path <- paste(dirname(getwd()),"/Data/Uljas/id_information.xlsx",
                       sep = '')
  data_table <- as.data.frame(read_excel(id_inf_path))
  return(data_table)
  
}


sourceFinder_initialize <- function(id_list){
  
  setwd(getwd())
  ul_message("Initializing Uljas search\n")
  
  id_path <- paste(dirname(getwd()),"/Data/Uljas/",id_list,sep = '')
  id_inf_path <- paste(dirname(getwd()),"/Data/Uljas/id_information.xlsx",
                       sep = '')
  
  # Ids that are searched

  ids <- scan(id_path, character(), quote = "")
  ids <- str_split(paste(ids, collapse = ''),',')[[1]]


  id_information <- as.data.frame(read_excel(id_inf_path))
  # Check if there are new IDs that are not in the info list
  unmatched_ids <- ids[! as.character(ids) %in% id_information[,1]]
  if(length(unmatched_ids) > 0){
    ul_message("These ids were not found in the id info list:\n")
    ul_message(paste(unmatched_ids, sep = "\n", collapse = ''))
  }else{
    ul_message("All Ids were found in the ID list\n")
  }
}






sourceFinder <- function(year, id_list){

  setwd(getwd())
  source(paste(getwd(),'/help_functions.R',sep = ''))
  
  ul_message("Start search information from Uljas database\n")
  variables = list(
    'lang' = 'fi',
    'konv' = 'json-stat',
    'atype' = 'data',
    'ifile' = '/ifile/01%20ULKOMAANKAUPPATILASTOT/01%20CN/ULJAS_CN',
    'tavaraluokitus' = 'Tavaraluokitus%20CN',
    'maa' = '==',
    'suunta' = '1',
    'indikaattorit' = 'V3', 
    'odds' = list(
      'Nauta' = 0.78,
      'Sika' = 0.87,
      'Broileri'= 0.64,
      'Kalkkuna'= 0.87
    )
  )
  
  id_path <- paste(dirname(getwd()),"/Data/Uljas/",id_list,sep = '')
  
  id_inf_path <- paste(dirname(getwd()),"/Data/Uljas/id_information.xlsx",
                       sep = '')
  
  # Ids that are searched
  ids <- scan(id_path, character(), quote = "")
  ids <- str_split(paste(ids, collapse = ''),',')[[1]]
  ids_request <- paste(ids, collapse = ",")
  ids_request <- paste("[",unlist(ids_request),"]", sep = '')
  
  webAdd <- paste("https://uljas.tulli.fi/uljas/graph/api.aspx?lang=",
                  variables$lang,"&atype=",variables$atype,"&konv=",
                  variables$konv,"&ifile=",variables$ifile,"&",
                  variables$tavaraluokitus,"=",ids_request,"&Maa=",
                  variables$maa,"&Vuosi=",year,"&Suunta=",variables$suunta,
                  "&Indikaattorit=",variables$indikaattorit, sep = '')
  
  json_data <- jsonlite::fromJSON(webAdd, options(timeout = 44))
  json_data <- json_data$dataset
  
  
  data <- json_data$value
  data <- matrix(data, ncol = length(ids))
  data[is.na(data)] <- 0
  
  countries <- json_data$dimension$Maa$category$label
  countries <- unlist(unname(countries))
  countries <- lapply(countries, function(x) unlist(str_split(x, ' '))[-(0:2)])
  countries <- unlist(lapply(countries, function(x) paste(unlist(x), collapse = ' ')))
  

  id_information <-  as.data.frame(read_excel(id_inf_path))
  if(any(is.na(id_information))){
    ul_message("Error, id_information.xlsl has empty columns\n")
    return(NULL)
  }
  colnames(id_information) <- c("ID","Tuore","Luullinen","Kategoria")
  id_list <- as.character(id_information[,1])
  
  source_classification <- list(
    'Broiler_F' = 0,
    'Broiler_P' = 0,      
    'Turkey_F' = 0,
    'Turkey_P' = 0,
    'Beef_F' = 0,
    'Beef_P' = 0,
    'Pork_F' = 0,
    'Pork_P' = 0
  )
  
  ind <- 1
  for(id in ids){
    ind <- (match(id, id_list))
    id_inf <- unname(unlist(id_information[ind,]))
    fresh <- tobool(id_inf[2], ifna = "return-na")
    bony <- tobool(id_inf[3], ifna = "return-na")
    if(is.na(fresh | is.na(bony))){
      ul_message(paste("Error, id", id, " has a value other than TRUE/FALSE",
                       "in bony or fresh"))
    }
    source <- id_inf[4]
    import_volume <- data[,ind]
    null_indexes <- unlist(lapply(import_volume, function(x) is.null(x)))
    import_volume <- unlist(replace(import_volume, null_indexes,0))
    ind <- ind + 1
    # Source is beef
    if(grepl(source, "Broileri",ignore.case = TRUE)){
      # If bony need to diminish weight
      if(bony){
        import_volume <- import_volume * variables$odds$Broileri
      }
      if(fresh){
        source_classification$Broiler_F <- source_classification$Broiler_F + import_volume
      }
      else{
        source_classification$Broiler_P <- source_classification$Broiler_P + import_volume
      }
    }
    else if(grepl(source, "Kalkkuna",ignore.case = TRUE)){
      # If bony need to diminish weight
      if(bony){
        import_volume <- import_volume * variables$odds$Kalkkuna
      }
      if(fresh){
        source_classification$Turkey_F <- source_classification$Turkey_F + import_volume
      }
      else{
        source_classification$Turkey_P <- source_classification$Turkey_P + import_volume
      }
    }
    else if(grepl(source, "Nauta",ignore.case = TRUE)){
      # If bony need to diminish weight
      if(bony){
        import_volume <- import_volume * variables$odds$Nauta
      }
      if(fresh){
        source_classification$Beef_F <- source_classification$Beef_F + import_volume
      }
      else{
        source_classification$Beef_P <- source_classification$Beef_P + import_volume
      }
    }
    else if(grepl(source, "Sika",ignore.case = TRUE)){
      # If bony need to diminish weight
      if(bony){
        import_volume <- import_volume * variables$odds$Sika
      }
      if(fresh){
        source_classification$Pork_F <- source_classification$Pork_F + import_volume
      }
      else{
        source_classification$Pork_P <- source_classification$Pork_P + import_volume
      }
    }
    else{
      print("Mystique error happened!!")
    }
    
  }
  source_classification <- data.frame(source_classification)
  # Delete rows with only zeros
  countries <- countries[rowSums(source_classification) > 0]
  source_classification <- source_classification[rowSums(source_classification) > 0,]
  year <- rep(year, length(countries))
  source_classification <- cbind(countries, year,source_classification)
  # Delete summary row
  source_classification <- source_classification[-1,]
  ul_message("Done!")
  return(source_classification)
}






