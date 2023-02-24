#' Initialize and run openBUGS pikma-model. Save parameters (cont.DC.total,
#' pfin_s, p.cc,"slaughpreva", "p.cont.DC") simulation stats and summary 
#' results if the model run was succeed.
#'
#' @param updates How many iterations model runs
#' @param burn_ins How many iterations at the beginning are deleted
#' @param ... Excel files that contains information from the different sources
#' (Broiler, Turkey, Beef and Pork in that order)
#' 
#' @return Simulated stats for the contamined meat
#'
#' 
#' 
pikma_run <- function(updates, burn_ins,...){
  require(R2OpenBUGS)
  require(writexl)
  require(readxl)
  require(stringr)
  setwd(getwd())
  source(paste(getwd(),'/help_functions.R',sep = ''))
  ns = 4

  diagnostic_path <- paste(dirname(getwd()),
                           "/Data/Domestic/Diagnostic/",
                           sep = '')
  result_path <- paste(dirname(getwd()),"/Data/Domestic/Results/", sep = '')

  # List of datafile names 
  sources <- unlist(list(...))
  
  message("Source files were downloaded in the order:\n")
  message(paste(sources, sep = ' '))
  message("\n")
  
  sDim <- if (length(sources) > 1) length(sources) else NULL
  
  # Model parameters
  lymphpos <- c()
  lymphN <- c()
  senstest <- c()
  nrfin <- c()
  drfin <- c()
  DC.prod <- c()
  export <- c()
  FCAT <- c()
  source_dims <- c()
  bm = c()
  
  keep_on = TRUE
  stat = NULL
  
  for(source in sources){
    source_data <- read_excel(paste(dirname(getwd()),"/Data/Domestic/Input/",
                                    source, sep = ''))
    # Remove first column that includes parameter names
    source_data <- as.matrix(source_data[,-1])
    source_dims <- c(source_dims, dim(source_data))
    ny = ncol(source_data) - 1
    # Check that the file is in the correct form
    if(nrow(source_data) != 18){
      message(paste("Error, datafile ", source, " has ", nrow(source_data),
                    " rows, should have 18 (Error code 1)\n" ))
      keep_on = FALSE
      break
    }
    if(!is.numeric(source_data)){
      message(paste("Error, datafile ", source, " contains values other than ",
      "numbers (Error code 2)\n" ))
      keep_on = FALSE
      break
    }
    class(source_data) <- "numeric"
    if(any(source_data < 0)){
      message(paste("Error, datafile ", source, " contains negative values ",
      "(Error code 3)\n" ))
      keep_on = FALSE
      break
    }
    if(length(unique(source_dims)) != 2){
      message(paste("Error, files have different number of columns (years)! ",
              "(Error code 4)"))
      keep_on = FALSE
      break
    }
    
    source_data <- unname(source_data)
    
    # One info column, rest are different years
    ny <- ncol(source_data)
    lymphpos <- c(lymphpos,source_data[1,] )
    lymphN <- c(lymphN,source_data[2,])
    senstest <- c(senstest, source_data[17,ncol(source_data)] )
    nrfin <- c(nrfin,source_data[c(5,7,9,11),])
    drfin <- c(drfin,source_data[c(6,8,10,12),])
    DC.prod <- c(DC.prod,source_data[3,])
    export <- c(export, source_data[4,])
    FCAT <- c(FCAT, source_data[13:16,ncol(source_data)])
    bm <- c(bm, source_data[18,ncol(source_data)])
  }
  
  if(keep_on){
    lymphpos <- matrix(lymphpos, nc = ny, byrow = TRUE)
    
    lymphN <- matrix(lymphN, nc = ny, byrow = TRUE)

    
    nrfin <- array(nrfin, dim = c(sDim,ny,4))
    nrfin <- re_arrang(nrfin)
    
    drfin <- array(drfin, dim = c(sDim,ny,4))
    drfin <- re_arrang(drfin)
    
    DC.prod <- array(DC.prod, dim = c(sDim,ny))
    DC.prod <- re_arrang(DC.prod)
    
    export <- array(export, dim = c(sDim,ny))
    export <- re_arrang(export)
    
    FCAT <- array(FCAT, dim = c(sDim,4))
    FCAT <- re_arrang(FCAT)
    
    pikma_data = list("ns" = ns,"ny" = ny,"senstest" = senstest,
                      "lymphN" = lymphN, "lymphpos" = lymphpos,"nrfin" = nrfin,
                      "drfin" = drfin, "DC.prod" = DC.prod, "export" = export,
                      "FCAT" = FCAT, "bm" = bm)
    
    message("Model is initialized!\n")
    
    # Save data to the current folder
    bugs.data(data = pikma_data, data.file = "pikma_data.txt", digits = 10)
    
    inits <- function(){
      list (slaughpreva = array(rep(0.001,ns*ny),dim = c(4,ny)),ps = 0.99)
    }
    
    parameters <- c("cont.DC.total", "pfin_s", "p.cc","slaughpreva",
                    "p.cont.DC")
    
    message("Running simulation...\n")
    
    start_time <- Sys.time()
    pikma.sim <- bugs ("pikma_data.txt", inits = inits, parameters,
                       model = "pikma_model.txt", n.chains=1,
                       n.burnin = burn_ins, n.iter=updates)
    end_time <- Sys.time()
    compile_time <- get_time_difference(end_time, start_time)
    
    message("The model has been driven.\n")
    message(paste("Model update took", compile_time,"\n"))
    message("Saving simulated values...\n")
    
    pattern <- "\\[[^)]*\\]" # Pattern to tidy app the data
    
    ############################################################################
    # Save simulated values for model diagnostic 
    parameter_simulation <- as.data.frame(pikma.sim$sims.matrix
                                          [,-ncol(pikma.sim$sims.matrix)])

    for(parameter in parameters){
      temp <- parameter_simulation[,str_remove_all
                                   (colnames(parameter_simulation),pattern) 
                                   %in% parameter]
      temp <- cbind(row.names(temp),temp)
      write_xlsx(data.frame(temp),paste(diagnostic_path,parameter,".xlsx",
                                        sep = ''))
    }
    message(paste("Simulated values are saved in a folder ",
                  paste(dirname(getwd()),"/Data/Domestic/Diagnostic", "\n")))
    
    ############################################################################
    # Save parameter summaries
    
    message("Saving parameter values...\n")
    summary_data <- pikma.sim$summary
    # Save every parameter to the own excel file
    for(parameter in parameters){
      temp <- summary_data[str_remove_all(row.names(summary_data),pattern)
                           %in% parameter,]
      temp <- cbind(row.names(temp),temp)
      colnames(temp)[1] <- "parameter"
      write_xlsx(data.frame(temp),paste(dirname(getwd()),
                "/Data/Domestic/Results/",parameter,".xlsx", sep = ''))
    }
    
    message(paste("Parameter values are saved in a folder ",
            paste(dirname(getwd()),"/Data/Domestic/Results", "\n")))
    
    ############################################################################
    
    # Collects the values of the contaminated meat for mean and standard
    # deviation
    
    Em_stats <- summary_data[str_remove_all(row.names(summary_data),pattern) 
                             %in% "cont.DC.total",]
    Em_stats <-  unname(unlist(Em_stats[,1]))
    Em_stats <- matrix(Em_stats, nr = 4, byrow = TRUE)
    
    Esd_stats <- summary_data[str_remove_all(row.names(summary_data),pattern) 
                              %in% "cont.DC.total",]
    Esd_stats <- unname(unlist(Esd_stats[,2])) 
    Esd_stats <- matrix(Esd_stats, nr = 4, byrow = TRUE)
    stat <- list("Em" = Em_stats, "Esd" = Esd_stats)
    
    class(stat) <- "Exposure stats"
    
    message("Exposure assessments for different sources:\n")
    message(paste("Chicken: ", round(Em_stats[1,ncol(Em_stats)],1),"\n"))
    message(paste("Turkey: ",round(Em_stats[2,ncol(Em_stats)],1),"\n"))
    message(paste("Beef: ", round(Em_stats[3,ncol(Em_stats)],1), "\n"))
    message(paste("Pork: ", round(Em_stats[4,ncol(Em_stats)],1), "\n"))
  }
  return(stat)
}


#' Updates Em and Esd excel files that are needed for the SA-model. This 
#' function should be called after the pikma_run function is called and the user
#' wants to update the Em and Esd tables.
#'
#' @param Em_stats 
#' @param Esd_stats 
#'

pikma_update <- function(Em_stats, Esd_stats, add_new_year){

  if(is.null(Em_stats) | is.null(Esd_stats)){
    return()
  }
  # Mean and  
  Em_path <- paste(dirname(getwd()),"/Data/SA/Input/Em.xlsx", sep = '')
  Esd_path <- paste(dirname(getwd()),"/Data/SA/Input/Esd.xlsx", sep = '')
  
  if(excelCheck(Em_path) & excelCheck(Esd_path)){
    Em <- read_excel(Em_path)
    Esd <- read_excel(Esd_path)
    
    # Check that files are ok
    years <- as.integer(colnames(Em))
    
    # Add new year
    if(add_new_year){
      # A year can be added if the files have the same number of years and the 
      # last years of the table to be added are NA values (Import results are 
      # added first) or the file to be added has one year more 
      
      if( (ncol(Em) == ncol(Em_stats) & (ncol(Esd) == ncol(Esd_stats) & 
          any(is.na(Em[1:4,ncol(Em)]))) | ((ncol(Em) == ncol(Em_stats) - 1 ) &
          (ncol(Esd) == ncol(Esd_stats) - 1) ))){
        
        # Add Nan values
        if(ncol(Em) == ncol(Em_stats) - 1 ){
          Em <- cbind(Em,NaN)
          Esd <- cbind(Esd, NaN)
          years <- c(years[1], years + 1)
        }
        
        message(paste("Add new year", years[length(years)],"\n"))
        
        Em[1:4,] <- Em_stats
        Esd[1:4,] <- Esd_stats
        colnames(Em) <- years
        colnames(Esd) <- years
        
        message("Results are saved!\n")
      }else{
        message(paste("Error! The files Em and Esd contains(",ncol(Em), "and",
                      ncol(Esd), ") years of data  and \n"))
        message(paste("the added tables contains (", ncol(Em_stats),"and",
                      ncol(Esd_stats) ,") years of data (Error code 9) \n"))              
      }
    }else{
      if(ncol(Em) == ncol(Em_stats) & ncol(Esd) == ncol(Esd_stats) &
        !any(is.na(Em[1:4,]))){
        
        message("Upgrade results\n")
        Em[1:4,] <- Em_stats
        Esd[1:4,] <- Esd_stats
        message("Results are saved!\n")
      }else{
        message(paste("Error! The files Em and Esd contains(",ncol(Em),
                      "and",ncol(Esd), ") years of data  and \n"))
        message(paste("the added tables contains (", ncol(Em_stats),"and",
                      ncol(Esd_stats) ,") years of data (Error code 10) \n")) 
      }
    }
  
    write_xlsx(Em,Em_path)
    write_xlsx(Esd,Esd_path)
  }
  else{
    message("You have either Em or Esd file open, please close it before saving
            results!")
  }
  
}


