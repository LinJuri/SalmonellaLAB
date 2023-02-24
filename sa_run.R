##################
##### SA RUN #####
##################




#' Initialize source attribution model. The data is in the folder 
#' SalmonellaLAB/Data/SA/Input/. Before initializing the model, 
#' the user must run the pikma model and the import model and save the results.
#' The creation of typing data must also be done for the most recent year.
#'
#'
#' @examples
sa_initialize <- function(){
  require(readxl)
  require(writexl)
  require(R2OpenBUGS)
  require(stringr)
  setwd(getwd())
  input_path <- paste(dirname(getwd()),"/Data/SA/Input/", sep = '')
  hc_path <- paste(input_path,"hc.xlsx", sep = '')
  nt_path <- paste(input_path,"nt.xlsx", sep = '')
  ntot_path <- paste(input_path,"ntot.xlsx", sep = '')
  ot_path <- paste(input_path,"ot.xlsx", sep = '')
  sc_path <- paste(input_path,"sc.xlsx",sep = '')
  I_path <- paste(input_path,"I.xlsx", sep = '')
  Em_path <- paste(input_path,"Em.xlsx", sep = '')
  Esd_path <- paste(input_path,"Esd.xlsx", sep = '')
  
  if(! file.exists(hc_path)){
    message(paste("File hc.xlsx does not exists in the folder ", input_path))
    return(NULL)
  }
  hc <- t(unname(as.matrix(read_excel(hc_path)[,-1])))
  class(hc) <- "numeric"
  
  if(! file.exists(nt_path)){
    message(paste("File nt.xlsx does not exists in the folder ", input_path))
    return(NULL)
  }
  nt <- array(read_excel(nt_path))[[1]]
  
  if(! file.exists(ntot_path)){
    message(paste("File ntot.xlsx does not exists in the folder ", input_path))
    return(NULL)
  }
  ntot <- array(read_excel(ntot_path))[[1]]
  
  if(! file.exists(ot_path)){
    message(paste("File ot.xlsx does not exists in the folder ", input_path))
    return(NULL)
  }
  ot <- array(read_excel(ot_path))[[1]]

  if(! file.exists(sc_path)){
    message(paste("File sc.xlsx does not exists in the folder ", input_path))
    return(NULL)
  }
  source_typing_results <- read_excel(sc_path,col_types = c())
  
  source_types <- unique(source_typing_results[,1])[[1]]
  kotvi <- unique(source_typing_results[,2])[[1]]
  
  x1 <- as.matrix(source_typing_results[source_typing_results[,1] == source_types[1] & source_typing_results[,2] == kotvi[1],])
  x1 <- t(unname(x1[,5:ncol(x1)]))
  class(x1) <- "numeric"
  
  x2 <- as.matrix(source_typing_results[source_typing_results[,1] == source_types[2] & source_typing_results[,2] == kotvi[1],])
  x2 <- t(unname(x2[,5:ncol(x2)]))
  class(x2) <- "numeric"
  
  x3 <- as.matrix(source_typing_results[source_typing_results[,1] == source_types[3] & source_typing_results[,2] == kotvi[1],])
  x3 <- t(unname(x3[,5:ncol(x3)]))
  class(x3) <- "numeric"
  
  x4 <- as.matrix(source_typing_results[source_typing_results[,1] == source_types[4] & source_typing_results[,2] == kotvi[1],])
  x4 <- t(unname(x4[,5:ncol(x4)]))
  class(x4) <- "numeric"
  
  x5 <- as.matrix(source_typing_results[source_typing_results[,1] == source_types[1] & source_typing_results[,2] == kotvi[2],])
  x5 <- t(unname(x5[,5:ncol(x5)]))
  class(x5) <- "numeric"
  
  x6 <- as.matrix(source_typing_results[source_typing_results[,1] == source_types[2] & source_typing_results[,2] == kotvi[2],])
  x6 <- t(unname(x6[,5:ncol(x6)]))
  class(x6) <- "numeric"
  
  x7 <- as.matrix(source_typing_results[source_typing_results[,1] == source_types[3] & source_typing_results[,2] == kotvi[2],])
  x7 <- t(unname(x7[,5:ncol(x7)]))
  class(x7) <- "numeric"
  
  x8 <- as.matrix(source_typing_results[source_typing_results[,1] == source_types[4] & source_typing_results[,2] == kotvi[2],])
  x8 <- t(unname(x8[,5:ncol(x8)]))
  class(x8) <- "numeric"
  
  if(! file.exists(I_path)){
    message(paste("File I.xlsx does not exists in the folder ", input_path))
    return(NULL)
  }
  I <- unname(as.matrix(read_excel(I_path)))
  
  if(! file.exists(Em_path)){
    message(paste("File Em.xlsx does not exists in the folder ", input_path))
    return(NULL)
  }
  Em <- unname(as.matrix(read_excel(Em_path)))
  
  if(! file.exists(Esd_path)){
    message(paste("File Esd.xlsx does not exists in the folder ", input_path))
    return(NULL)
  }
  Esd <- unname(as.matrix(read_excel(Esd_path)))
  
  ny <- ncol(Em)
  ns <- nrow(Em)
  sa = list("hc" = hc,"ns" =  ns,"ny" =  ny,"nt" =  nt, "ntot" =  ntot,"ot" =  ot, "x1" = x1, "x2" =  x2,
            "x3" =  x3, "x4" =  x4, "x5" = x5, "x6" = x6, "x7" =  x7, "x8" =  x8, "Em"=Em, "Esd"= Esd, "I"=I)
  # Save results
  bugs.data(data = sa,data.file = "sa_data.txt")
  message("Model is initialized\n")
}


#' Title
#'
#' @return
#' @export
#'
#' @examples
sa_run <- function(n_iter,n_burnin){
  require(stringr)
  
  diagnostic_path <- paste(dirname(getwd()),"/Data/SA/Diagnostic/", sep = '')
  result_path <- paste(dirname(getwd()),"/Data/SA/Results/", sep = '')
  parameters <- c("Ec", "Ec2", "Ep", "Ep2", "Epp", "Epp2", "MEp", "MEp2", "MEpp","q","type")
  
  if(! file.exists("sa_data.txt")){
    message("File sa_data.txt file not found")
    return(NULL)
  }
  
  inits <- function() {
    list(q = c(1.554E-4,0.001375,0.001804,2.115E-4,0.008855,
               0.04007,8.205E-4,1.289E-4))
  }
  
  message("Running the SA-model... \n")
  
  start_time <- Sys.time()
  bugs_sim <- bugs("sa_data.txt" , inits, parameters,n.thin = 1, "sa_model.txt",
                   n.chains=1,n.burnin = n_burnin, n.iter=n_iter,
                   save.history = FALSE)
  
  end_time <- Sys.time()
  compile_time <- get_time_difference(end_time, start_time)
  
  message("The model has been driven.\n")
  message(paste("Model update took", compile_time,"\n"))

  pattern <- "\\[[^)]*\\]" # Pattern to tidy app the data
  
  ##############################################################################
  # Save simulated values for model diagnostic 
  
  message("Saving simulated values...\n")
  parameter_simulation <- bugs_sim$sims.matrix

  for(parameter in parameters){
    temp <- parameter_simulation[,str_remove_all(colnames(parameter_simulation),pattern) %in% parameter]
    temp <- cbind(row.names(temp),temp)
    write_xlsx(data.frame(temp),paste(diagnostic_path,parameter,".xlsx", sep = ''))
  }
  message(paste("Simulated values are saved in a folder. ",diagnostic_path, "\n"))
  
  
  ##############################################################################
  # Save parameter summaries
  summary_data <- bugs_sim$summary
  
  for(parameter in parameters){
    temp <- summary_data[str_remove_all(row.names(summary_data),pattern) %in% parameter,]
    temp <- cbind(row.names(temp),temp)
    colnames(temp)[1] <- "parameter"
    write_xlsx(data.frame(temp),paste(result_path,parameter,".xlsx", sep = ''))
  }
  
  message(paste("Parameter values are saved in a folder ",result_path, "\n"))
  
}



