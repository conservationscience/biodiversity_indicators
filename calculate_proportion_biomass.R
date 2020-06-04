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

#' @param inputs a converted time matrix with x rows (timesteps) 
#' and y cols (timestep, relative proportion of biomass) of whatever group you
#' want to calculate proportion of biomass (eg all groups, herbivores, mammals etc)
#' 
#' @param output_folder string, the file path of the directory where you want
#' to store the calculated indicator value time series.  If the folder
#' doesn't exist yet the function will create it.
#' 
#' @param simulation_number the simulation number the inputs came from (for
#' labelling purposes)
#' 
#' @param years the number of yearly, pre-exploitation timesteps over which you 
#' want to take the mean against which change in biomass will be measured

## TO DO: Add a warning message if there is no biomass in the input data

# inputs <- readRDS("N:\\Quantitative-Ecology\\Indicators-Project\\Serengeti\\Outputs_from_indicator_code\\Indicator_inputs\\proportion_total_biomass\\Test_runs\\Test_runs_ae_0_proportion_total_biomass_inputs")
# output_folder <- "N:\\Quantitative-Ecology\\Indicators-Project\\Serengeti\\Outputs_from_indicator_code\\Indicator_outputs\\proportion_total_biomass\\Test_runs"
# simulation_number <- "ae"
# replicate_number <- "0"
# years <- 1

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
    mutate(year = seq(years + 1, ncol(inputs), 1)) %>%
    setNames(c("relative_proportion_of_biomass", "year")) %>%
    arrange(year) %>%
    slice(-n())
  
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

plot_total_proportion_biomass <- function(inputs, output_folder, scenario, 
                                          pre_exploitation_period, 
                                          startimpact, endimpact) {

  if( !dir.exists( file.path(output_folder) ) ) {
 
     dir.create( file.path(output_folder), recursive = TRUE )
  
}

 scenario_title <- sub("_", " ", scenario)
  
 xmaximum <- as.numeric(max(inputs$year, na.rm = TRUE))

 yminimum <- min(inputs$lower_bound) - 
             min(inputs$lower_bound) * 0.05
 
 ymaximum <- max(inputs$upper_bound) + 
             max(inputs$upper_bound) * 0.05
 
 if ("lower_bound" %in% names(inputs)) {
   
plotName <- paste(scenario_title, " relative proportion of total biomass",".tiff",sep="")
tiff(file = (paste(output_folder,plotName, sep = "/")), units ="in", width=10, height=5, res=200)
   
 plot <- ggplot(data = inputs, aes(x = inputs$year, y = inputs$mean_relative_proportion_of_biomass)) +
                                geom_path() +
                                labs(x = "Time (years)", 
                                     y = "Relative proportion of total biomass") +
                                geom_ribbon(aes(ymin=inputs$lower_bound, ymax=inputs$upper_bound), 
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
                                geom_vline(xintercept = endimpact, colour = "blue") +
          ggtitle(paste(scenario_title, "Relative Proportion of Total Biomass", sep = " "))
 
 } else {

plotName <- paste(scenario_title, "relative_proportion_of_total_biomass",".tiff",sep="")
tiff(file = (paste(output_folder,plotName, sep = "/")), units ="in", width=10, height=5, res=200)
   
plot <- ggplot(data = inputs, aes(x = inputs$year, y = inputs$mean_relative_proportion_of_biomass)) +
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
               geom_vline(xintercept = endimpact, colour = "blue") +
  ggtitle(paste(scenario_title, "Relative Proportion of Total Biomass", sep = " "))


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
