# Function to plot expected salmonella proportion for each source
# w/ detected subtypes or all cases
# This function is made for SA-model

#' Title
#'
#' @param parameter 
#'
#' @return
#' @export
#'
#' @examples

require(dplyr)
require(ggplot2)
require(readxl)
require(writexl)
require(gridExtra)
require(smooth)
require(forecast)


source(paste(getwd(),'/help_functions.R',sep = ''))

# Disable scientific notation 
options(scipen=10000)

axis_text_size = 12
axis_title_size = 16
plot_title_size = 25
legend_text_size = 14
legend_title_size = 16
strip_text_size  = 16
middle = 0.5
line_width = 1.5

# Main colours
blueberry = "#004F71"
straw = "#CEB888"
rasberry = "#D0006F"

# Backround
coal = "#343841"
fluff = "#F4F3F2"
white = "#FFFFFF"
gem = "#EEEEEE"

# Side colours
brick = "#912D20"
pig_tail = "#F4C8C2"
soil = "#5C442C"
spruce = "#0D5F2C"
pollen = "#F7CE3C"
water = "#ADD2EE"
salmon = "#FF6F49"




sa_parameter_path <- paste(dirname(getwd()),"/Data/SA/Results/", sep = '')
dom_parameter_path <- paste(dirname(getwd()),"/Data/Domestic/Results/",
                            sep = '')
em_parameter_path <- paste(dirname(getwd()),"/Data/SA/Input/", sep = '')




#' Line plot with source and year information 
#' 
#' @param parameter either Ep, Ep2, Epp, Epp2 from the source attribution model
#' @param moving_average extra parameter for moving average, default
#'  is 1 (none)
#' @param order for moving average 
#'
#' @return graph of the parameter (ggplot)
#' @export
#'
#' @examples
#' Ep_plot_with_moving_average_three <- proportion_plot("Ep", 3)
#' 
proportion_plot <- function(parameter,moving_average = NA, order = 1){

  if(is.null(parameter) | is.null(moving_average) | is.null(order)){
    return(NULL)
  }

  include_import = TRUE
  if(parameter %in% c("Ep", "Ep2","Epp","Epp2")){
    parameter_path <- paste(sa_parameter_path,parameter,'.xlsx',sep = '')
  }else if(parameter == "cont.DC.total"){
    parameter_path <- paste(em_parameter_path,"Em.xlsx", sep = '')
  }else if(parameter %in% c("slaughpreva","p.cc", "p.cont.DC")){
    include_import = FALSE
    parameter_path <-  paste(dom_parameter_path,parameter,'.xlsx',sep = '') 
  }else{
    return(NULL)
  }
  
  main_title <- convert_parameter_name(parameter)
  main_title <- if(is.null(main_title)) parameter else main_title

  
  if(! file.exists(parameter_path)){
    return(NULL)
  }
  
  data <- as.data.frame(read_excel(parameter_path))
  if(parameter %in% c("cont.DC.total")){
    data_mean <- unname(unlist(as.list(t(data))))
  }else{
    data_mean <- as.numeric(unname(unlist(data[,2])))
  }
  
  years <- get_current_years()

  # Every column has results from each course
  data_mat <- if(include_import) matrix(data_mean, nc = 8) else
                matrix(data_mean, nc = 4)
  other <- NULL
  source_cat <- c("Broiler","Turkey","Beef","Pork") 
  ns = 4 # Number of sources
  line_colours <- c(blueberry,rasberry,water,pollen)
  
  if(parameter %in% c("Epp","Epp2")){
    other <- 100 - rowSums(data_mat)
    source_cat <- c(source_cat,"Other")
    ns = 5
    line_colours <- c(line_colours,salmon)
  }
  
  if(include_import){
    dom <- c(as.vector(data_mean[1:(length(data_mean) / 2)]),other)
    imp <- c(as.vector(data_mean[((length(data_mean) / 2) + 1) :
                                   length(data_mean)]),other)
    
    
    dom_data <- data.frame(y = dom,source = rep(source_cat,
                                                each = length(dom) / ns),
                           x = years)
    imp_data <- data.frame(y = imp, source = rep(source_cat,
                                                 each = length(imp) / ns),
                           x = years)
    
    data_df <- rbind(cbind(dom_data, "Origin" = "Domestic"),
                     cbind(imp_data, "Origin" = "Import"))
  }else{
    data_df <- data.frame(y = data_mean,source = rep(source_cat,
                                                     each = length(years)),
                          x = years, "Origin" = "Domestic")
  }
  

  if(! is.na(moving_average)){

    if(moving_average == "cma" & order >= 1){
      data_df <- data_df %>%
        group_by(source,Origin) %>%
        mutate(y = as.vector(cma(y,order)$fitted)) %>%
        na.omit()
    }
    else if(moving_average == "ma" & order >= 1){
      data_df <- data_df %>%
        group_by(source,Origin) %>%
        mutate(y = as.vector(ma(y,order))) %>%
        na.omit()
    }
    else if(moving_average == "es" & 0 <= order & order <= 1){
      data_df <- data_df %>%
        group_by(source,Origin) %>%
        mutate(y = as.vector(ses(y, alpha = order)$fitted)) %>%
        na.omit()
    }
    else{

      return(NULL)
    }
  }

  x_label <- unique(data_df$x)
  x_label_tics <- seq(round(x_label[1],-1), round(x_label[length(x_label)],-1),5)
  x_label[! x_label %in% x_label_tics] <- ""
  x_breaks <- unique(data_df$x)
  
  plot_to_return <- ggplot(data_df, aes(x, y, group = source)) + 
    geom_line(aes(linetype = source, col = source), linetype = "solid",
              size = line_width)+
    scale_color_manual(values=line_colours)   +
    geom_point(aes(color = source), size = 2) +
    xlab("Year") + 
    ylab("Percentage")+
    ggtitle(main_title) + 
    scale_x_continuous(breaks = x_breaks,labels = x_label) + 
    theme(
      panel.background = element_rect(fill = gem,
                                      colour = gem,
                                      size = 0.5, linetype = "blank"),
      axis.text = element_text(size = axis_text_size),
      axis.title = element_text(size = axis_title_size),
      plot.title = element_text(size = plot_title_size,hjust = middle),
      legend.text = element_text(size = legend_text_size),
      legend.title = element_text(size = legend_title_size),
      strip.text  = element_text(size = strip_text_size)
    )
  if(include_import){
    plot_to_return <- plot_to_return + facet_wrap(vars(Origin)) 
  }
  return(plot_to_return)
}


#' Boxplot Source Attribution parameter
#'
#' @param parameter 
#'
#' @return
#' @export
#'
#' @examples
mean_proportion_plot <- function(parameter){
  parameter_path <- paste(sa_parameter_path,parameter,'.xlsx',sep = '')
  ns = 4 # Number of sources
  ny = length(get_current_years()) # Number of years
  if(! file.exists(parameter_path)){
    return(NULL)
  }
  
  box_colours <- rep(c(blueberry,rasberry,water,pollen),each = 2)
  data <- read_excel(parameter_path)
  data_mean <- as.numeric(unname(unlist(data[,2])))
  mainTitle <- "Mean expected proportion for each source"
  
  data_2_5 <- as.double(unlist(data[,4]))
  data_25 <- as.double(unlist(data[,5]))
  data_75 <- as.double(unlist(data[,7]))
  data_97 <- as.double(unlist(data[,8]))
  years <- get_current_years()
  if(parameter == "q"){
    mainTitle <- "Source infectiveness"
  }
  if(parameter %in% c("Ep", "Ep2", "Epp", "Epp2")){
    years <- get_current_years()
    sources <- rep(c("Broileri","Turkey","Beef","Pork"),each = ny, times = 2)
    origin <- rep(c("Domestic","Import"), each = ns * ns)
    data_df <- data.frame(sources,years,data_mean, data_2_5,data_25,
                          data_75,data_97)
    data_df <- data_df[1:52,]
    
    ggplot(data_df,aes(x=years)) + 
      geom_boxplot(aes(lower=data_25,upper=data_75,middle=data_mean,
                       ymin=data_2_5,ymax=data_97,
                       group = interaction(years, sources)),
                   stat="identity") + 
      facet_wrap(~sources)
  }
  else{
    sources <- c("Broiler(d)","Turkey(d)","Beef(d)","Pork(d)","Broiler(i)",
                 "Turkey(i)","Beef(i)","Pork(i)")
    data_df <- data.frame(sources,data_mean, data_2_5,data_25, data_75,data_97)
    plot_to_return <- ggplot(data_df,aes(x=sources)) + 
      geom_boxplot(aes(lower=data_25,upper=data_75,middle=data_mean,
                       ymin=data_2_5,ymax=data_97),
                   stat="identity", fill = box_colours) + 
      xlab("Year") + 
      ylab("Percentage") +
      ggtitle(mainTitle) + 
      theme(
        axis.text = element_text(size = axis_text_size),
        axis.title = element_text(size = axis_title_size),
        plot.title = element_text(size = plot_title_size,hjust = middle)
      )
  }
  return(plot_to_return)
}



#' Boxplot parameter with year and source results
#'
#' @param parameter 
#' @param origin_type
#' @param 
#'
#' @return
#' @export
#'
#' @examples
#' ep_plot <- sa_boxplot("Ep","import","year")
sa_boxplot <- function(parameter, origin_type = "domestic",
                       grouped_by = "source"){
  plot_to_return <- NULL

  if(is.null(parameter) | is.null(origin_type) | is.null(grouped_by)){
    return(NULL)
  }
  
  if(parameter %in% c("Ep", "Ep2", "Epp", "Epp2")){
    parameter_path <- paste(sa_parameter_path,parameter,'.xlsx',sep = '')
  }else if(parameter %in% c("p.cc","slaughpreva", "cont.DC.total","p.cont.DC")
           & origin_type == "domestic"){
    parameter_path <- paste(dom_parameter_path,parameter,'.xlsx',sep = '')
  }
  else{return(NULL)}
  
  main_title <- unlist(unname(parameter_names[parameter]))
  main_title <- if(is.null(main_title)) parameter else main_title

  if((! file.exists(parameter_path))) {
    return(NULL)
  }
  if(! grouped_by %in% c("year","source")){
    return(NULL)
  }
  ns = 4
  year <- get_current_years()
  ny = length(year)

  data <- read_excel(parameter_path)
  data_mean <- as.numeric(unname(unlist(data[,2])))
  mainTitle <- "Mean expected proportion for each source"

  data_2_5 <- as.double(unlist(data[,4]))
  data_25 <- as.double(unlist(data[,5]))
  data_75 <- as.double(unlist(data[,7]))
  data_97 <- as.double(unlist(data[,8]))
  
  if(parameter %in% c("Ep", "Ep2", "Epp", "Epp2", "p.cc","slaughpreva",
                      "cont.DC.total","p.cont.DC")){
    year <- get_current_years()
    source <- rep(c("Broiler","Turkey","Beef","Pork"),each = ny)
    data_df <- data.frame(source,year,data_mean, data_2_5,data_25,
                          data_75,data_97)
    if(origin_type== "domestic"){
      origin <- "Domestic"
      main_title <- paste(main_title, "(Domestic)")
      data_df <- data_df[1:(ny * ns),]
    }else{
      origin <- "Import"
      main_title <- paste(main_title, "(Import)")
      data_df <- data_df[((ny*ns) + 1) : nrow(data_df),]
    }
    if(grouped_by == "source"){
      box_colours <- rep(c(blueberry,rasberry,water,pollen),each = ny)
      plot_to_return <- ggplot(data_df,aes(x=year)) + 
        geom_boxplot(aes(lower=data_25,upper=data_75,middle=data_mean,
                         ymin=data_2_5,ymax=data_97,
                         group = interaction(year, source)),
                     stat="identity", fill = box_colours) + 
        facet_wrap(~source,scales="free") + 
        xlab("Year") 
    }else{
      box_colours <- rep(c(blueberry,rasberry,water,pollen), times = ny)
      plot_to_return <- ggplot(data_df,aes(x=source)) + 
        geom_boxplot(aes(lower=data_25,upper=data_75,middle=data_mean,
                         ymin=data_2_5,ymax=data_97,
                         group = interaction(year, source)),
                     stat="identity", fill = box_colours) + 
        facet_wrap(~year,scales="free")
    }
  }
  years <- get_current_years()
  x_label <- years
  x_label_tics <- seq(round(years[1],-1), round(years[length(years)],-1),5)
  x_label[! x_label %in% x_label_tics] <- ""
  
    plot_to_return <- plot_to_return + 
      ylab("Percentage") +
      ggtitle(main_title) + 
      scale_x_continuous(breaks = years,labels = x_label) +
      theme(
        panel.background = element_rect(fill = gem,
                                        colour = gem,
                                        size = 0.5, linetype = "blank"),
        axis.text = element_text(size = axis_text_size),
        axis.title = element_text(size = axis_title_size),
        plot.title = element_text(size = plot_title_size,hjust = middle),
        legend.text = element_text(size = legend_text_size),
        legend.title = element_text(size = legend_title_size),
        strip.text  = element_text(size = strip_text_size)
      )

    return(plot_to_return)
}
  
#' Plot Serotype infectiveness
#'
#' @param parameter 
#'
#' @return
#' @export
#'
#' @examples
infection_plot <- function(parameter){
  parameter_path <- paste(sa_parameter_path,parameter,'.xlsx',sep = '')
  if(! file.exists(parameter_path)){
    return(NULL)
  }
  
  data <- read_excel(parameter_path)
  data_mean <- as.numeric(unname(unlist(data[,2])))
  # Serotype ordering
  
  num <- 1:length(data_mean)
  data_df <- data.frame(num, data_mean)
  
  plot_to_return <- ggplot(data_df, aes(x = num, y = data_mean)) + 
    geom_bar(stat = "identity",fill = "#d0016f") + 
    scale_x_continuous(breaks=seq(1,length(data_mean),2)) +
    xlab("Serotype") + 
    ggtitle("Ability of serotypes to infect humans") + 
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_text(size = axis_text_size),
          axis.title.x = element_text(size = axis_title_size),
          plot.title = element_text(size = plot_title_size,hjust = middle)
    )
  return(plot_to_return)
}

# result table
#' Return datatable for spesific parameter
#'
#' @param parameter whick stats are returned
#'
#' @return
#' @export
#'
#' @examples
tab_plot <- function(parameter){
  # From import or pikma model
  if(parameter %in%  c("cont.DC.total", "pfin_s")){
    parameter_path <- paste(sa_parameter_path,parameter,'.xlsx',sep = '')
  }
  parameter_path <- paste(sa_parameter_path,parameter,'.xlsx',sep = '')
  serotypes <- NULL
  if(file.exists(parameter_path)){
    if(parameter == 'type'){
      data <- read_excel(parameter_path)
      data_mean <- as.numeric(data$mean)
      serotype_data_path <-  paste(dirname(getwd()),"/Data/SA/Input/hc.xlsx",
                                   sep = '')
      if(! file.exists(serotype_data_path)){
        print("Serotyyppidataa ei löydy...")
      }
      serotype_data <- read_excel(serotype_data_path)
      # Scale infection rates between 0-1
      data_mean <- (data_mean - min(data_mean)) / 
                    (max(data_mean) - min(data_mean))
      data_mean <- round(data_mean,2)
      serotypes <- data.frame(serotype_data$Serotype, data_mean)
      colnames(serotypes) <- c("Serotype", "Scaled infection rate")

    }
  }
  return(serotypes)
}


# 
#' Returns exposure assesment table (Domestic or import)
#'
#' @param type 
#'
#' @return
#' @export
#'
#' @examples
sum_tab <- function(type){
  data_df <- NULL
  if(!is.null(type)){
    return(NULL)
  }
  parameter_path <- paste(em_parameter_path, "Em.xlsx", sep = '')
  if(! file.exists(parameter_path)){
    return(NULL)
  }
  
  if(type == "domestic"){
    data_df <- t(as.data.frame(read_excel(parameter_path))[1:4,])
  }
  else if(type == "import"){
    data_df <- t(as.data.frame(read_excel(parameter_path))[5:8,])

  }
  else{
    return(NULL)
  }
  
  data_df <- data_df[complete.cases(data_df),]
  class(data_df) <- "integer"
  data_df <- cbind(rownames(data_df), data_df)
  colnames(data_df) <- c("year","Broileri","Kalkkuna","Nauta","Sika")
  
  return(data_df)
}

save_plot <- function(parameter, plot_data){
  save_path <- paste(dirname(getwd()),"/Data/Saved_images/", sep = '')
  file_name <- paste(parameter, ".png",sep = '')
  
  ggsave(filename = file_name, plot = plot_data, path = save_path )
  ty_message("Saved")
}



























