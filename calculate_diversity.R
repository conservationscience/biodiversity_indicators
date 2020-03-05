# Using R version 3.5.2

#' Return a list of dataframes (one for each replicate) that gives the diversity 
#' (number of pseudo species) present at each time step

#' @param data a matrix containing the abundance of each group over time

#' @return a list of dataframes


# age <- "adult"
# burnin <- 1 *12
# interval <- 12
# func <- mean

#' TODO: IMPORTANT - this currently combines data prep with indicator calculation,
#' need to split them when it is complete
#' 
#' TODO: Probably need some pre define groups and code to create them (elsewhere)

# groups_all <- readRDS('N:/Quantitative-Ecology/Indicators-Project/Serengeti/Outputs_from_adaptor_code/map_of_life/Test_runs/ae_BuildModel/groups.rds')
# 
# groups_df <- groups_all %>%
#           select(functional_group_name, group_id)
# 
# groups <- split(groups_df, groups_df$functional_group_name)
# 

calculate_diversity <- function(data, age, burnin, interval, func, groups){
  
  require(tidyverse)
  
  # Find the abundance files
  
  model_results <- "N:/Quantitative-Ecology/Indicators-Project/Serengeti/Outputs_from_adaptor_code/map_of_life/Test_runs/ae_BuildModel"
  files <- list.files(model_results)
  all_abundance_dataframes <- files[str_detect(files, "abundance")]
  
  # Subset according to whether adult or juvenile has been specified
  
  if (age == "adult") {
  age_abundance_dataframes <- all_abundance_dataframes[str_detect(all_abundance_dataframes, 
                                                              "MassBinsOutputs")]
  
  } else if (age == "juvenile"){
  
  age_abundance_dataframes <- all_abundance_dataframes[str_detect(all_abundance_dataframes, 
                                                                    "juveniles")]
  }
  
  # Get final file names
  
  abundance_dataframe_filenames <- age_abundance_dataframes[str_detect(age_abundance_dataframes, ".rds")]
  
  
  # Function to convert monthly timesteps to yearly by taking the mean of a specified interval (12 to convert monthly to yearly)
  
  convert_timesteps <- function(dataframe, interval, func){
    
    monthly_matrix <- t(dataframe)
    
    n <- interval
    
    time_converted_matrix <- t(aggregate(monthly_matrix,list(rep(1:(nrow(monthly_matrix) %/% 
                             n + 1), each = n, len = nrow(monthly_matrix))), 
                             func, na.rm = TRUE))
    
    time_converted_matrix <- time_converted_matrix[-1,]
    
    time_converted_matrix[is.nan(time_converted_matrix)] = NA
    
    return(time_converted_matrix)
    
  }
  
  # Loop through and turn each replicate into a dataframe, converting no value 
  # numbers (-9999) to NA, and remove burnin period
  
  abundance_dataframes <- list()
  
  for (i in seq_along(abundance_dataframe_filenames)) {
    
  abundance_dataframes[[i]] <- readRDS(file.path(model_results, 
                                               abundance_dataframe_filenames[i]))
  abundance_dataframes[[i]] <- dplyr::na_if(abundance_dataframes[[i]], -9999)
  
  abundance_dataframes[[i]] <- abundance_dataframes[[i]][,(burnin + 1) :
                                      ncol(abundance_dataframes[[i]])]
  
  abundance_dataframes[[i]] <- convert_timesteps(abundance_dataframes[[i]], 
                                                 interval, func)
    
  }
  
  # Calculate total diversity
  
  indicator_inputs <- abundance_dataframes
  
  total_diversity <- list()
  
  for (i in seq_along(indicator_inputs)) {
    
  total_diversity[[i]] <- colSums(indicator_inputs[[i]], na.rm = TRUE)
    
  }

  # Split each replicate dataframe by group (produce a list of replicates,
  # with a list of groups nested within each replicate)
  
  split_by_group <- list()
 
  for ( i in seq_along(abundance_dataframes)) {
  
  split_by_group[[i]] <- as.data.frame(abundance_dataframes[[i]])
  
  split_by_group[[i]] <- split_by_group[[i]] %>% dplyr::mutate(group_id = rownames(.)) %>% 
                merge(groups_df, by = "group_id")
  
  rownames(split_by_group[[i]]) <- split_by_group[[i]]$group_id
  
  split_by_group[[i]] <- split(split_by_group[[i]], split_by_group[[i]]$functional_group_name)
  
  }
  

  ## UP TO HERE - REST DOESN'T WORK YET ...
  
  # Remove the descriptive variables now we have split the groups, still want
  # a list of replicates with groups inside

  numeric_split_by_group <- list()
  
  for (i in seq_along (split_by_group)) {
    
    x_list <- split_by_group[[i]]
  
  for (j in seq_along(x_list)) {
    
    numeric_split_by_group[[j]] <- x_list[[j]] %>% 
                           dplyr::select(- functional_group_name) %>%
                           dplyr::select(- group_id)
  }
    
    group_indicator_inputs[[i]] <- numeric_split_by_group
    
  }

  # Calculate diversity per group.  Should make a list of replicates, with a list
  # of groups nested within each (each group should just be a single vector of diversity
  # values)
  
  group_diversity <- list()
  
  for (i in seq_along(group_indicator_inputs)) {
    
    group_diversity[[i]] <- colSums(group_indicator_inputs[[i]], na.rm = TRUE)
    
  }
  
  # Rbind the groups in each replicate to produce a matrix/dataframe
  # where columns are time steps and rows are groups diversity
  
    
    # index.scores <- index.scores[seq(1, nrow(index.scores), t), ]
    # 
    # return(index.scores)
  
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
