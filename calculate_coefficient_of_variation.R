# Using R version 3.5.2

#' Return a dataframe that calculates the coefficient of variation for each
#' functional group over a specified (monthly) interval of time.

#' @param data a matrix with x rows (functional groups/bodymass/species) and 
#' y cols (monthly timesteps)
#' @param interval the number of monthly timesteps over which you want to measure 
#' variation
#' 
# indicators_project <- "N:/Quantitative-Ecology/Indicators-Project/" # File path for entire project directory
# location <- 'Serengeti' # Modelled location you want to process
# scenario <- 'Harvesting_carnivores' # Scenario you want to process
# simulation <- '201_BuildModel/' # Model simulation number aka 'BuildModel' directory you want to process
# burnin <- 1000*12 # number of years burn in * 12 (to convert to monthly)
# simulation_number <- "201" # Number of your buildmodel file
# variable <- "adult_biomass"
# timeframe <- 20
# func <- mean
# pre_exploitation_period <- 50
# colour_scheme <- c("black", "red", "blue","turquoise3", "purple","green3", "pink")
# data <- prepare_coefficient_of_variation_inputs(indicators_project, scenario,
#                                   simulation_number, variable, burnin, interval, func)

calculate_coefficient_of_variation <- function(data, timeframe){
  
  # Calculate the mean biomass of each group (row) over the 
  # specified interval of timesteps (columns)
  
    timeframe <- timeframe*12
    
    monthly_matrix <- t(data)
    
    mean_matrix <- t(aggregate(monthly_matrix,
                               list(rep(1:(nrow(monthly_matrix) %/% 
                               timeframe + 1), each = timeframe, 
                               len = nrow(monthly_matrix))), mean, na.rm = TRUE))
    
    test <- matrix()
    
    for (i in ncol(mean_matrix)){
    
        
      
    }

  # Remove weird first line
  ## TO DO: Fix whatever is generating the first line so we don't need this
    mean_matrix <- mean_matrix[-1,]
    
    mean_matrix[is.nan(mean_matrix)] = NA
    
  # Calculate the standard deviation biomass of each group (row) over the
  # specified interval of timesteps (columns)
    
    sd_matrix <- t(aggregate(monthly_matrix,
                 list(rep(1:(nrow(monthly_matrix) %/% 
                 timeframe + 1), each = timeframe, 
                 len = nrow(monthly_matrix))), sd, na.rm = TRUE))
    
  
  # Remove weird first line
  ## TO DO: Fix whatever is generating the first line so we don't need this
    sd_matrix <- sd_matrix[-1,]
  
  
  # Calculate the coefficient of variation biomass of each group (row) over the
  # specified interval of timesteps (columns)
  ## TO DO: Returns cv value sof around 2 - 16, check this is correct
    
    cv_matrix <- (100*sd_matrix)/mean_matrix
    

  # Calculate the total coefficient of variation
    
    cv_total <- colSums(cv_matrix, na.rm = TRUE)
  
  # Divide by 1
    
    cv_total <- 1/cv_total
    
    cv_output <- as.data.frame(cv_total) %>%
                 dplyr::mutate(interval_no = seq(0 + timeframe/12,round(ncol(data)/12), 
                                              by = timeframe/12)) %>%
                 dplyr::filter(cv_total != Inf) ## Remove this later
    
    names(cv_output) <- c("variability","Interval_no")
    
    return(cv_output)
    
}


#' Return a line plot that plots the coefficient of variation
#' over a specified interval of time.

#' @param data matrix with two variables, time and cv
#' @param x - string specify what colour you want your points and line to be

plot_coefficient_of_variation <- function(data, x, yminimum, ymaximum, impactstart, impactend){
  
plot <- ggplot(data = data, aes(x = data$Interval_no, y = data$variability)) +
        geom_line(col = x) +
        geom_point(col = x, shape = 16, size = 2) +
        labs(x = "Time (years)", 
        y = "Coefficient of variation") +
        labs(colour = "Functional group") +
        theme(panel.grid.major = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey97"),
        axis.line = element_line(colour = "black"),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.key.size = unit(0.4,"cm")) +
        geom_vline(xintercept = impactstart, colour = "red") +
        geom_vline(xintercept = impactend, colour = "blue") +
        ylim(yminimum, ymaximum)

return(plot)

}

plot_coefficient_of_variation_multiple <- function(data, x, yminimum, ymaximum, impactstart, impactend){
  
  plot <- ggplot(data = data, aes(x = data$Interval_no, y = data$inverse,
                  group = scenario)) +
    geom_path(aes(colour = scenario)) +
    geom_point(col = x, shape = 18, size = 2) +
    labs(x = "Time (years)", 
         y = "Coefficient of variation") +
    labs(colour = "Functional group") +
    theme(panel.grid.major = element_blank(),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 18),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "grey97"),
          axis.line = element_line(colour = "black"),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 18),
          legend.key.size = unit(0.4,"cm")) +
    geom_vline(xintercept = impactstart, colour = "red") +
    geom_vline(xintercept = impactend, colour = "blue") +
    ylim(yminimum, ymaximum)
  
  return(plot)
  
}

# test function
# yminimum <- -1
# ymaximum <- 1
# impactstart <- 0.5
# impactend <- 2
# x <- "black"
# data <- calculate_coefficient_of_variation(data, interval)
# 
# test <- plot_total_cv(coefficient_of_variation, -1, 1 , 2, 6, "black")
# test

#' Return a dataframe that calculates the coefficient of variation for each
#' functional group over a specified (monthly) interval of time and then takes it
#' as a proportion of total CV

#' @param data a matrix with x rows (functional groups/bodymass/species) and 
#' y cols (monthly timesteps)
#' @param interval the number of monthly timesteps over which you want to measure 
#' variation

calculate_coefficient_of_variation_proportion <- function(data, interval){
  
  # Calculate the mean biomass of each group (row) over the 
  # specified interval of timesteps (columns)
  
  monthly_matrix <- t(data)
  
  mean_matrix <- t(aggregate(monthly_matrix,
                             list(rep(1:(nrow(monthly_matrix) %/% 
                                           interval + 1), each = interval, 
                                      len = nrow(monthly_matrix))), mean))
  
  # Remove weird first line
  ## TO DO: Fix whatever is generating the first line so we don't need this
  mean_matrix <- mean_matrix[-1,]
  
  # Calculate the standard deviation biomass of each group (row) over the
  # specified interval of timesteps (columns)
  
  sd_matrix <- t(aggregate(monthly_matrix,
                           list(rep(1:(nrow(monthly_matrix) %/% 
                                         interval + 1), each = interval, 
                                    len = nrow(monthly_matrix))), sd))
  
  # Remove weird first line
  ## TO DO: Fix whatever is generating the first line so we don't need this
  sd_matrix <- sd_matrix[-1,]
  
  
  # Calculate the coefficient of variation biomass of each group (row) over the
  # specified interval of timesteps (columns)
  ## TO DO: Returns cv value sof around 2 - 16, check this is correct
  
  cv_matrix <- (100*sd_matrix)/mean_matrix
  
  # Calculate the total coefficient of variation
  
  # cv_total <- colSums(cv_matrix, na.rm = TRUE)
  
  # Calculate the total coefficient of variation with which to compare the groups
  
  ## TO DO: Check the methodology for calculating total CV because it should
  ## be lower than the individual groups but currently it isn't)
  
  monthly_total <- colSums(data, na.rm = TRUE)
  
  mean_total <- t(aggregate(monthly_total,
                            list(rep(1:(length(monthly_total) %/% 
                                          interval + 1), each = interval, 
                                     len = length(monthly_total))), mean))
  
  # Remove weird first line
  ## TO DO: Fix whatever is generating the first line so we don't need this
  mean_total <- mean_total[-1,]
  
  
  ## Calculate total SD
  
  sd_total <- t(aggregate(monthly_total,
                          list(rep(1:(length(monthly_total) %/% 
                                        interval + 1), each = interval, 
                                   len = length(monthly_total))), sd))
  
  # Remove weird first line
  ## TO DO: Fix whatever is generating the first line so we don't need this
  sd_total <- sd_total[-1,]
  
  ## Calculate total cv
  
  cv_total <- (100*sd_total)/mean_total
  
  
  # Calculate the individual group cv as a proportion of total biomass cv
  
  cv_proportion <- cv_total/cv_matrix
  
  # Add a cv_total as a row so you can plot it against proportions
  
  cv_proportion_total <- rbind(cv_total, cv_proportion)
  
  return(cv_proportion_total)
  
}
