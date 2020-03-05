# Using R version 3.5.2

#' Return a dataframe that denotes the Red List Index over time
#' for each functional group 

#' @param data a matrix containing the red list status of each 'species'
#' (functional groups/bodymass/species) over time
#' y cols (monthly timesteps), burn-in period removed
#' @param timeframe single number denoting assessment period to grab RLI scores from
#' @return a dataframe that calculates the value of the Red List Index over time
#' for each functional group 
#' 
#' TODO: Check the timeframe parameter - is it needed?
#' TODO: Does having the same number of species in each functional group mess with
#' the scores?
#' TODO: Split data by correct group/taxa when species matching better sorted
#' 

calculate_red_list_index <- function(data, timeframe){
  
  require(tidyverse)

  # Assign category weights
  
  weighted_data <- data %>%
       dplyr::mutate(RL_weight = ifelse(RL_status == "LC", 0,
                                 ifelse(RL_status == "NT", 1,
                                 ifelse(RL_status == "VU", 2,
                                 ifelse(RL_status == "EN", 3,
                                 ifelse(RL_status == "CR", 4,
                                 ifelse(RL_status == "EX", 5,
                                 "NA"))))))) 
  
 weighted_data$RL_weight <- as.numeric(as.character(weighted_data$RL_weight))
 
    # Filter out rows with NE and DD
    weighted_data <- filter(weighted_data, RL_weight != "NA" )
    
    # Calculate numerical weights for each species based on risk category
    # weight.data <- calcWeights(filter.data, RL_weight)
    # weight.data <- drop_na(weight.data, .data[[RL_weight]])
    
    # Group data so the index is calculated for each taxa for each year
    grouped.data <- weighted_data %>% group_by(functional_group, timestep)
    
    # Sum category weights for each group, calculate number of species per group
    summed.weights <- summarise(grouped.data, 
                                total.weight = sum(RL_weight, na.rm = TRUE), # calc sum of all weights
                                total.count = n()) # calc number of species
   
    # Calculate RLI scores for each group, rounded to 3 decimal places
    index.scores <- mutate(summed.weights, 
                           RLI = 1 - (total.weight/(total.count * 5)), # actual RLI formula
                           Criteria = "risk") 
    
    index.scores <- index.scores[seq(1, nrow(index.scores), t), ]
    
    return(index.scores)
  
}

#' Save a line plot of the RLI scores over time 

#' @param data a data frame (output from calculate_red_list_index) with columns:
#' functional_group, timestep, total.weight, total.count, RLI criteria
#' @param y_minimum numeric minimum limit for y axis
#' @param y_maximum numeric maximum limit for y axis
#' @return a line plot of RLI over time for each functional group/taxa or whatever 

plot_red_list_index <- function(data, y_minimum, y_maximum) {
  
  require(ggplot2)

  plot <- ggplot(data = data, aes(x = data$timestep, y = data$RLI,
                                  colour = data$functional_group)) +
    geom_line(group = data$functional_group) +
    labs(x = "Time (years)", 
         y = "Red List Index Score") +
    labs(colour = "Functional group") +
    facet_wrap( ~ functional_group) +
    ylim( y_minimum, y_maximum) +
    theme(panel.grid.major = element_blank(),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 18),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "grey97"),
          axis.line = element_line(colour = "black")) +
    geom_vline(xintercept = 100, colour = "red") +
    geom_vline(xintercept = 200, colour = "blue")
  
  return(plot)
  
}
