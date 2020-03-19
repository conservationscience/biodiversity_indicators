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

#' @param data a converted time matrix with x rows (timesteps) 
#' and y cols (timestep, relative proportion of biomass) of whatever group you
#' want to calculate proportion of biomass (eg all groups, herbivores, mammals etc)
#' @param years the number of yearly, pre-exploitation timesteps over which you 
#' want to measure variation

## TO DO: Add a warning message if there is no biomass in the input data


calculate_proportion_biomass <- function(inputs, output_folder, simulation_number, 
                                         replicate_number,years) {
  
  
  if( !dir.exists( file.path(output_folder) ) ) {
    dir.create( file.path(output_folder), recursive = TRUE )
    
  }
  
  scenario <- basename(output_folder)
  
  # Calculate the sum of biomass in the specified group
  
  if (class(inputs) == "data.frame") {
    
    #dataframe <- dataframe[sapply(dataframe, function(x) is.numeric(x))]
    
    matrix <- inputs[,sapply(inputs,is.numeric)]
    
  } else {
    
    matrix <- inputs
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
    mutate(year = seq(years+1, ncol(inputs), 1)) %>%
    setNames(c("relative_proportion_of_biomass", "year"))
  
  saveRDS( proportion_biomass, file = file.path(output_folder,
           paste(scenario, simulation_number, replicate_number, 
                 "proportion_total_biomass_outputs", sep = "_" ))) 
  
  return(proportion_biomass)
  
}


# data <- test_input # Data that plotting function originally worked on
# 
# data <- scenario_proportion_of_biomass_final[[1]]
# 
# pre_exploitation_period <- 2
# startimpact <- 3
# endimpact <- 5

#' TODO: Add an argument to specify line colour?

plot_total_proportion_biomass <- function(data, pre_exploitation_period, 
                                          startimpact, endimpact){
  
 xmaximum <- as.numeric(max(data$year, na.rm = TRUE))

 yminimum <- min(data$lower_bound) - 
             min(data$lower_bound) * 0.05
 
 ymaximum <- max(data$upper_bound) + 
             max(data$upper_bound) * 0.05
 
 if ("lower_bound" %in% names(data)) {
   
 plot <- ggplot(data = data, aes(x = data$year, y = data$mean_relative_proportion_of_biomass)) +
                                geom_path() +
                                labs(x = "Time (years)", 
                                     y = "Relative proportion of total biomass") +
                                geom_ribbon(aes(ymin=data$lower_bound, ymax=data$upper_bound), 
                                            alpha=0.2) +
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
 
 } else {
   
plot <- ggplot(data = data, aes(x = data$year, y = data$mean_relative_proportion_of_biomass)) +
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
   
   
 }
  
return(plot)
  
}

# Test function

# plot_total_proportion_biomass(data, pre_exploitation_period, 
#                               startimpact, endimpact)



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
