require(writexl)
require(readxl)
require(R2OpenBUGS)
require(ggplot2)


model_diagnostic <- function(selected_years, selected_sources, parameter, extra_parameter = NULL){
  source(paste(getwd(),'/help_functions.R',sep = ''))
  years <- get_current_years()
  if(parameter == "Cont_DC_total"){
    file_name = "diagnostic.xlsx"
    sources <- c("Chicken","Turkey","Beef","Pork")
    # Difference between import and pikma model
    years <- if(extra_parameter == 1) years[length(years)] else years
    if(extra_parameter == 1){
      selected_years <- get_current_years()
      selected_years <- selected_years[length(selected_years)]
      diag_path <- paste(dirname(getwd()),"/Data/Diagnostic/Import/", file_name, sep = '')
      
    }else{
      diag_path <- paste(dirname(getwd()),"/Data/Diagnostic/Domestic/", file_name, sep = '')
    }
    parameter_names <- paste(rep(sources,each = length(years)), years)
  }
  else if(parameter %in% c("Ep","Epp","Ep2", "Epp2")){
    sources <- c("Chicken(d)","Turkey(d)","Beef(d)","Pork(d)", "Chicken(i)","Turkey(i)","Beef(i)","Pork(i)" )
    diag_path <- paste(dirname(getwd()),"/Data/Diagnostic/SA/",parameter,'.xlsx',sep = '')
    parameter_names <- paste(rep(sources,each = length(years)), years)
  }
  else if(parameter %in% c("MEp","MEpp","MEp2","Mepp2")){
    sources <- c("Chicken(d)","Turkey(d)","Beef(d)","Pork(d)", "Chicken(i)","Turkey(i)","Beef(i)","Pork(i)" )
    selected_years <- get_current_years()
    selected_years <- selected_years[length(selected_years)]
    years <- years[length(years)] 
    parameter_names <- sources
    
  }
  else{
    return(NULL)
  }
    
    data <- as.data.frame(read_xlsx(diag_path))
    
    number_of_iterations = nrow(data)
    number_of_years = length(years)
    number_of_parametes = ncol(data)

    index_vector <- c()
    for(e in sources %in% selected_sources){
      if(e){
        index_vector <- c(index_vector,years %in% selected_years )
      }
      else{
        index_vector <- c(index_vector, rep(FALSE, number_of_years))
      }
    }
    data <- data[,index_vector]
    
    source_rep <- rep(c(selected_sources), each =number_of_iterations * length(number_of_years) )
    param_names <- parameter_names[index_vector]
    param_names <- rep(param_names, each = number_of_iterations)
    iterations <- rep(1:number_of_iterations,(length(selected_sources) * length(selected_years)))
    values <- unlist(unname(data))
    df <- data.frame(values,param_names,iterations)
    
    # Choose the columns that user has spesivied
    names(df) <- c("parameter","type", "iteration")
    ggplot(df, aes(iteration, parameter)) + 
      geom_line()+
      facet_wrap(vars(type)) + 
      theme(axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.y=element_blank(),
            axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.ticks.x=element_blank(),
            strip.text.x = element_text(size = 15)
      )

    
  }

model_diagnostic2 <- function(parameter){
  
  sa_parameter_path <- paste(dirname(getwd()),"/Data/SA/Diagnostic/", sep = '')
  dom_parameter_path <- paste(dirname(getwd()),"/Data/Domestic/Diagnostic/",
                              sep = '')
  import_parameter_path <- paste(dirname(getwd()),"/Data/Import/Diagnostic/",
                                 sep = '')
  
  
  if(parameter %in% c("Ep", "Ep2","Epp","Epp2")){
    parameter_path <- paste(sa_parameter_path,parameter,'.xlsx',sep = '')
    sources <- c("Chicken(d)","Turkey(d)","Beef(d)","Pork(d)", "Chicken(i)","Turkey(i)","Beef(i)","Pork(i)" )
    
  }else if(parameter == c("cont.DC.total", "p.cc","p.cont.DC","slaughpreva")){
    parameter_path <- paste(dom_parameter_pathv,parameter, sep = '')
  }else if(parameter %in% c("IC.res")){
    include_import = FALSE
    parameter_path <-  paste(import_parameter_path,parameter,'.xlsx',sep = '') 
  }else{
    return(NULL)
  }
  
  main_title <- convert_parameter_name(parameter)
  main_title <- if(is.null(main_title)) parameter else main_title
  
  
  if(! file.exists(parameter_path)){
    return(NULL)
  }
  
  data <- as.data.frame(read_excel(parameter_path))
  
  parameter <- "Ep"
  parameter_path <- paste(dirname(getwd()),"/Data/Diagnostic/SA/",parameter,'.xlsx',sep = '')
  data <- as.data.frame(read_excel(parameter_path))
  
  years <- get_current_years() 
  selected_sources <- c("Turkey(d)", "Chicken(i)")
  
  selected_years <- c(2008, 2012, 2020)
  sources <- c("Chicken(d)","Turkey(d)","Beef(d)","Pork(d)", "Chicken(i)","Turkey(i)","Beef(i)","Pork(i)" )
  
  number_of_iterations = nrow(data)
  number_of_years = length(years)
  number_of_parametes = ncol(data)
  parameter_names <- paste(rep(sources,each = length(years)), years)
  
  index_vector <- c()
  for(e in sources %in% selected_sources){
    if(e){
      index_vector <- c(index_vector,years %in% selected_years )
    }
    else{
      index_vector <- c(index_vector, rep(FALSE, number_of_years))
    }
  }
  data <- data[,index_vector]
  source_rep <- rep(c(selected_sources), each =number_of_iterations * length(number_of_years) )
  param_names <- parameter_names[index_vector]
  param_names <- rep(param_names, each = number_of_iterations)
  iterations <- rep(1:number_of_iterations,(length(selected_sources) * length(selected_years)))
  values <- unlist(unname(data))
  df <- data.frame(values,param_names,iterations)
  
  names(df) <- c("parameter","type", "iteration")
  ggplot(df, aes(iteration, parameter)) + 
    geom_line()+
    facet_wrap(vars(type)) + 
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x=element_blank(),
          strip.text.x = element_text(size = 15)
    )
}



