# Using R version 3.5.2


############################################################################
### Calculate the proportion of biomass indicator
############################################################################

# Function that calculates proportion of biomass and takes two arguments:
# First argument can be a dataframe or matrix, where rows are members of 
# whatever group you are calculating for (eg mammal species, all species etc) and
# columns are yearly timesteps (values in cells are biomass)
# Second argument is the number of timesteps you want to calculate your mean
# pre-exploitation level over

# Output is another dataframe with two variables: The relative proportion of 
# total group biomass post exploitation to group biomass pre exploitation; and
# year

#' Return a dataframe with two variables: the proportion of biomass relative
#' to a pre-exploitation reference point, and the annual timestep

#' @param dataframe a converted time matrix with x rows (functional groups/bodymass) 
#' and y cols (annual timesteps) of whatever group you want to calculate proportion
#' biomass (eg all groups, herbivores, mammals etc)
#' @param years the number of yearly, pre-exploitation timesteps over which you 
#' want to measure variation

## TO DO: Add a warning message if there is no biomass in the input data
## TO DO: Work out how to calculate uncertainty/variability/confidence intervals


calculate_proportion_biomass <- function(data, years) {
  
  # Calculate the sum of biomass in the specified group
  
  if (class(data) == "data.frame") {
    
    #dataframe <- dataframe[sapply(dataframe, function(x) is.numeric(x))]
    
    matrix <- data[,sapply(data,is.numeric)]
    
  } else {
    
    matrix <- data
  }
  
  total_biomass <- colSums(matrix, na.rm = TRUE)
  
  # Calculate pre-exploitation levels of biomass (take the mean of the first 
  # 50 years post-burnin, pre-impact)
  
  pre_exploitation_biomass <- mean(total_biomass[1:years])
  
  #Subset total biomass to not include pre-exploitation years
  
  post_exploitation_biomass <- total_biomass[(years + 1):length(total_biomass)]
  
  # Calculate the proportion of biomass in timestep t relative to pre_exploitation
  # biomass
  
  proportion_biomass <- as.data.frame(post_exploitation_biomass/pre_exploitation_biomass)
  
  proportion_biomass <- proportion_biomass %>%
    mutate(year = seq(years+1, ncol(data), 1)) %>%
    setNames(c("relative_proportion_of_biomass", "year"))
  
  return(proportion_biomass)
  
}


plot_total_proportion_biomass <- function(data, yminimum, ymaximum, 
                                          pre_exploitation_period, 
                                          startimpact, endimpact){
  
 xmaximum <- as.numeric(max(data$year, na.rm = TRUE))
  
 plot <- ggplot(data = data, aes(x = data$year, y = data$relative_proportion_of_biomass)) +
                                geom_path() +
                                labs(x = "Time (years)", 
                                     y = "Relative proportion of total biomass") +
                                ylim(yminimum,ymaximum) +
                                xlim(pre_exploitation_period,xmaximum) +
                                theme(panel.grid.major = element_blank(),
                                axis.title = element_text(size = 18),
                                axis.text = element_text(size = 18),
                                panel.grid.minor = element_blank(),
                                panel.background = element_rect(fill = "grey97"),
                                axis.line = element_line(colour = "black")) +
                                geom_hline(yintercept = 1, colour = "black") +
                                geom_vline(xintercept = startimpact, colour = "red") +
                                geom_vline(xintercept = endimpact, colour = "blue")
  
 return(plot)
  
}

# Test function
#plot_total_proportion_biomass(proportion_total_biomass)



plot_group_proportion_biomass <- function(data, yminimum, ymaximum, startimpact, endimpact, colour_scheme){
  
  plot <- ggplot(data = data, aes(x = data$year, y = data$relative_proportion_of_biomass,
                 group = group)) +
                 geom_path(aes(colour = group)) +
          labs(x = "Time (years)", 
               y = "Relative proportion of biomass by group") +
          ylim(yminimum,ymaximum) +
          labs(colour = "Group") +
    theme(panel.grid.major = element_blank(),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 18),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "grey97"),
          axis.line = element_line(colour = "black"),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 18),
          legend.key.size = unit(0.4,"cm"),) +
    geom_hline(yintercept = 1, colour = "black") +
    geom_vline(xintercept = startimpact, colour = "red") +
    geom_vline(xintercept = endimpact, colour = "blue") +
    facet_wrap( ~ group, nrow = 2) +
    theme(legend.position = "none") +
    theme(strip.text.x = element_text(size = 16), 
          strip.background = element_rect(fill ="#B3B3B3"),
          panel.spacing = unit(1, "lines")) +
    scale_color_manual(values = colour_scheme)
  
  return(plot)
  
}

# Test function
# colour_scheme <- c("black", "red", "blue","turquoise3", "purple","green3", "pink")
# plot_group_proportion_biomass(group_proportion_biomass, 2, 5, colour_scheme)
