require(readxl)
require(writexl)
require(R2OpenBUGS)


#' Initialize and run openBUGS import model. Save parameters (IC.res,piCUR,
#' qCUR,IC.prop) simulation stats and summary results if the model run 
#' was succeed.
#'
#' @param updates How many iterations model runs 
#' @param burn_ins How many iterations at the beginning are deleted
#' @param import_volume Import volumes in different meat categories
#' @param AG_data Which countries have specified AG requirements
#' @param ... Excel files that contains information from the different sources
#'
#' @return Simulated stats for the contained meat



import_run <- function(updates, burn_ins,import_volume, AG_data,...){

  require(readxl)
  require(writexl)
  require(R2OpenBUGS)
  require(stringr)
  
  keepOn = TRUE
  setwd(getwd())
  source(paste(getwd(),'/help_functions.R',sep = ''))
  
  # Path for diagnostic results
  diagnostic_path <- paste(dirname(getwd()),
                           "/Data/Import/Diagnostic/", sep = '')
  parameter_simulation <- c()
  parameter_summary <- c()
  # File names that contains information about sources
  prevalence_results <- unlist(list(...))
  
  message("Source files were downloaded in the order:\n")
  message(paste(prevalence_results))
  message("\nRunning simulation...\n")

  
  # Dataset containing import volumes for the sources. 
  uljas_data <- as.matrix(read_excel(paste(dirname(getwd()),
                                           "/Data/Import/Input/",import_volume,
                                           sep = '')), byrow = TRUE)
  uljas_data <- uljas_data[order(uljas_data[,1]),]
  uljas_all_sources <- uljas_data[,3:10]
  uljas_all_countries <- uljas_data[,1]
  suppressWarnings(
    class(uljas_all_sources) <- "numeric")
  
  if(!is.numeric(uljas_all_sources)){
    message(paste("Error, datafile ", import_volume, " contains values other",
                  "than numbers (Error code 2)\n" ))
    return(NULL)
  }

  
  if(any(uljas_all_sources < 0)){
    message(paste("Error, datafile ", import_volume, " contains negative",
                  "numbers (Error code 3)\n" ))
    return(NULL)
  }

  AG <- as.matrix(read_excel(paste(dirname(getwd()),"/Data/Import/Input/",
                                   AG_data, sep = '')))
  
  ag_countries <-  as.vector(t(AG))[! is.na(as.vector(t(AG)))]
  if(any(! ag_countries %in% uljas_all_countries)){
    
    message(paste("Error, these countries in the  datafile ", AG_data,
                  "are not found in the ",import_volume, " (Error code 5)\n" ))
    message(unique(
      ag_countries[! as.vector(t(AG)) %in% uljas_all_countries]))
    return(NULL)
  }
  
  ##############################################################################
  # Create variables from the data matrices 
  
  Em_stats <- c()
  Esd_stats <- c()

  ind = 1
  start_time <- Sys.time()
  if(keepOn){
    
    for(source_file in prevalence_results){
      uljas_source <- uljas_all_sources[
                     uljas_all_sources[,ind] != 0 | 
                     uljas_all_sources[,ind +1] != 0,][,ind:(ind + 1)]
      
      uljas_countries <- uljas_all_countries[uljas_all_sources[,ind] != 0 | 
                                               uljas_all_sources[,ind +1] != 0]
      
      preva_countries <- suppressMessages(suppressWarnings(
                                as.data.frame(read_excel(paste(
                                dirname(getwd()),"/Data/Import/Input/",
                                source_file, sep = '')))))
      
      headline_check <- suppressWarnings(as.integer(
                          names(preva_countries)))
      
      # All headers should be characters
      if(any(! is.na(headline_check))){
        message(paste("Error, datafile ", source_file,
                      " header contains characters other than letters.
                      (Error code 5)\n" ))
        return(NULL)
      }
      
      if(! is_natural_numbers(
           unname(unlist(preva_countries[,2:ncol(preva_countries)])))){
        message(paste("Error, datafile ", source_file, 
                      " contains values other than numbers (Error code 3)\n" ))
        return(NULL)
      }
      
      preva_countries <- preva_countries[order(preva_countries[,1]),]
      rownames(preva_countries) <- rep(1:nrow(preva_countries))
      
      # Replace NAN to 0
      preva_countries[is.na(preva_countries)] <- 0
      prevalence_countries_raw <- preva_countries[,1][preva_countries[,2] != 0]
      prevalence_countires_pros <- preva_countries[,1][preva_countries[,4] != 0]
      
      typing_inf <- preva_countries[,2:5]
      
      # Collect countries with no additional guarantees needed
      fresh_ag <- AG[,ind]
      fresh_ag <- fresh_ag[!is.na(fresh_ag)] 
      proses_ag <- AG[,ind + 1]
      proses_ag <- proses_ag[!is.na(proses_ag)]
      ind = ind + 2
      
      # If countries are not found in uljas table 
      if(any(!(preva_countries[,1] %in% uljas_all_countries))){
        message(paste("Error,  datafile ", source_file,
                      " contains countries that are not found in the  ",AG_data,
                      " file (Error code 6) \n"))
        message("Those countries are:\n")
        message(preva_countries[,1][!preva_countries[,1] %in%
                                      uljas_all_countries])
        message("\n")
        return(NULL)
      }
      
      # Number of import countries (with prevalence results in fresh meat + 2)
      n <- sum(typing_inf[,1] != 0) + 2
      
      # Number of samples (fresh meat (IC1)) in country i
      np <- typing_inf[,1][typing_inf[,1] != 0]
      
      # Number of positive samples (fresh meat (IC1)) in country i
      dp <- typing_inf[,2][typing_inf[,1] != 0]
      
      # Number of import countries (with prevalence results in meat
      # preparations/products + 2)
      n2 <- sum(typing_inf[,3] != 0) + 2
      
      # Number of samples (meat preparations (IC4)) in country i
      nq <- typing_inf[,3][typing_inf[,3] != 0]
      
      # Number of positive samples (meat preparations (IC4)) in country i
      dq <- typing_inf[,4][typing_inf[,3] != 0]
      

      # Ensure that sample quantities are also available for countries that
      # have imports
      
      if((length(uljas_source[,1] != 0) != length(typing_inf[,1] != 0)) 
         | length(uljas_source[,2] != 0) != length(typing_inf[,3] != 0)){
        
        message(paste("Error, File ", source_file, 
                      " has a different number of countries with prevalence 
                      data compared to file ",import_volume  ,
                      " with the import amounts of importing countries 
                      (Error code 7) \n"))
        return(NULL)
      }
      
      # Amount of imported raw meat from country i with samples
      volRAW = uljas_source[,1][uljas_source[,1] != 0 & typing_inf[,1] != 0]
      
      # Total amount of imported raw meat from countries without samples and
      # no AGs
      volRAW[n - 1] <- sum(uljas_source[,1][typing_inf[,1] == 0 & 
                                              ! (uljas_countries %in% fresh_ag)])
      
      # Total amount of imported raw meat from countries with AGs without
      # samples 
      volRAW[n] <- sum(uljas_source[,1][typing_inf[,1] == 0 & 
                                          (uljas_countries %in% fresh_ag)])
      
      # Amount of imported meat in category 4 from country i
      volPROS <- uljas_source[,2][uljas_source[,2] != 0  & typing_inf[,3] != 0]
      
      # Total amount of imported meat in category 4 from countries without 
      # samples and no AGs
      volPROS[n2 - 1] <- sum(uljas_source[,2][typing_inf[,3] == 0 &
                                                !(uljas_countries %in%
                                                    proses_ag)])
      
      volPROS[n2] <- sum(uljas_source[,2][typing_inf[,3] == 0 &
                                            (uljas_countries %in% proses_ag)])
      
      # Share of different meat categories in raw meat
      share=c(0.58,0.02,0.4)
      
      if(any(!fresh_ag %in% uljas_all_countries)){
        message("These countries were not found in the AG list (Error code 6)\n")
        message(fresh_ag[!fresh_ag %in% uljas_all_countries])
        return(NULL)
      }
      
      if(any(!proses_ag %in% uljas_all_countries)){
        message("These countries were not found in the AG list (Error code 6)\n")
        message(proses_ag[!proses_ag %in% uljas_all_countries])
        return(NULL)
      }
      
      ## NORWAY + Countries w/ no prevalalence data
      indRAW <- rep(c(ifelse(prevalence_countries_raw %in% fresh_ag,1,0),0,1),3)
      indRAW <- matrix(data = indRAW, nrow = 3, byrow = TRUE)
      indPROS <- c(ifelse(prevalence_countires_pros %in% proses_ag,1,0),0,1)
      
      # save results 
      sa = list("n" = n,"n2" = n2,"np" = np,"dp" = dp,"nq" = nq,"dq" = dq,
                "share" = share,"volRAW" = volRAW,"volPROS" = volPROS,
                "indRAW" = indRAW,"indPROS" = indPROS)
      bugs.data(data = sa, data.file = "import_data.txt")
      
      # Initialise some parameters
      inits <- function() {
        list(ap=1,bp=1,aq=1,bq=1,ps=0.97,servingsize=137)
        
      }
      
      # Total amount of imported contamined meat
      parameters <- c("IC.res","piCUR","qCUR","IC.prop")
      # Start simulation
      bugs_sim <- bugs("import_data.txt", inits, parameters, "import_model.txt",
                       n.chains=1,n.burnin = burn_ins, n.iter=updates,
                       save.history = FALSE)
      
      # IC.res[3]  AGs applied to spesific countries (current practise)
      Em_stats <-  c(Em_stats,bugs_sim$summary[3,1])
      Esd_stats <- c(Esd_stats,bugs_sim$summary[3,2])
      
      # Save simulated values for model diagnostic
      parameter_summary <- rbind(parameter_summary, bugs_sim$summary)
    }
  }
  
  end_time <- Sys.time()
  compile_time <- get_time_difference(end_time, start_time)
  
  message("The model has been driven.\n")
  message(paste("Model update took", compile_time,"\n"))

  
  pattern <- "\\[[^)]*\\]" # Pattern to tidy app the data
  
  ############################################################################
  # Save simulated values for model diagnostic 
  message("Saving simulated values...\n")
  
  parameter_simulation <- as.data.frame(bugs_sim$sims.matrix
                                        [,-ncol(bugs_sim$sims.matrix)])
  
  for(parameter in parameters){
    temp <- parameter_simulation[,str_remove_all
                                 (colnames(parameter_simulation),pattern) 
                                 %in% parameter]
    temp <- cbind(row.names(temp),temp)
    write_xlsx(data.frame(temp),paste(diagnostic_path,parameter,".xlsx",
                                      sep = ''))
  }
  message(paste("Simulated values are saved in a folder ",
                diagnostic_path, "\n"))
  
  
  
  ##############################################################################
  # Save parameter summaries
  
  message("Saving parameter values...\n")
  # Save every parameter to the own excel file
  for(parameter in parameters){
    temp <- parameter_summary[str_remove_all(
            row.names(parameter_summary),pattern) %in% parameter,]
    
    temp <- cbind(row.names(temp),temp)
    colnames(temp)[1] <- "parameter"
    write_xlsx(data.frame(temp),paste(dirname(getwd()),"/Data/Import/Results/",
                                      parameter,".xlsx", sep = ''))
  }
  
  message(paste("Parameter values are saved in a folder ",
                paste(dirname(getwd()),"/Data/Domestic/Results", "\n")))
  
  ##############################################################################
  
  message("Exposure assesments for different sources:\n")
  message(paste("Chicken: ", round(Em_stats[1],1),"\n"))
  message(paste("Turkey: ",round(Em_stats[2],1),"\n"))
  message(paste("Beef: ", round(Em_stats[3],1), "\n"))
  message(paste("Pork: ", round(Em_stats[4],1), "\n"))
  
  stat <- list("Em" = Em_stats, "Esd" = Esd_stats)
  class(stat) <- "Exposure stats"
  return(stat)
}




#' Updates Em and Esd excel files that are needed for the SA-model
#'
#' @param Em_stats 
#' @param Esd_stats 
#'
#'
import_update <- function(Em_stats, Esd_stats, add_year){
  Em_path <- paste(dirname(getwd()),"/Data/SA/Input/Em.xlsx", sep = '')
  Esd_path <- paste(dirname(getwd()),"/Data/SA/Input/Esd.xlsx", sep = '')

  
  if(excelCheck(Em_path) & excelCheck(Esd_path)){
    Em <- read_excel(Em_path)
    Esd <- read_excel(Esd_path)
    years <- as.integer(colnames(Em))
    
    # Try add new column (year) to Em and Esd files
    if(add_year){
      if(!any(is.na(Em[5:8,]))){
        Em <- cbind(Em,NA)
        Esd <- cbind(Esd,NA)
        years <- c(years[1], years + 1)
      }

      message(paste("Add new year", years[length(years)],"\n"))
      Em[5:8,ncol(Em)] <- Em_stats
      colnames(Em) <- years
      Esd[5:8,ncol(Esd)] <- Esd_stats
      colnames(Esd) <- years
      write_xlsx(Em,Em_path)
      write_xlsx(Esd,Esd_path)
      message("Results are saved!\n")
    }else{
      if(any(is.na(Em[5:8,]))){
        message("Cant uppgrade\n")
      }
      else{
        Em[5:8,ncol(Em)] <- Em_stats
        Esd[5:8,ncol(sd)] <- Esd_stats
        message("Uppgrade results\n")
        write_xlsx(Em,Em_path)
        write_xlsx(Esd,Esd_path)
        message("Results are saved!\n")
      }
    }
  }
  else{
    message("Error, either Em or Esd file is open, (Error code 6)\n")
  }

}



