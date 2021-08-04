################################################################################

#### Function to import data and join for each scenario
# then create a summary to use for further analyses

################################################################################

#### packages

library(tidyverse)
library(INLA)
library(nimble)

#### scripts

source('./Functions/Extract_best_result.R')

################################################################################

summarise_data <- function(pattern_string,
                           interaction_type = c("c", "m", "p")){
  # interactions stored in results in order above
  # save a marker of what numbers this is
  focal_interaction_marker <- ((100*which(c("c", "m", "p")==interaction_type))-99):
    (100*which(c("c", "m", "p")==interaction_type))
  
### FIRST: read in all of the data
    
  ## read in inla data
  inla_results <- readRDS(paste0("./Results/", pattern_string, "_inla_results_2021.rds"))[focal_interaction_marker]
  
  # clean - remove all entries in list that had an error
  
  # locations where error = NULL
  errors <- lapply(inla_results, '[[', "error")
  clean_rows <- unlist(map(.x = errors, ~{is.null(.x[1])}))
  
  # extract just the results
  inla_results_clean <- lapply(inla_results, '[[', 
                               "result")
  
  # then remove any entries that had an error
  inla_results_clean <- inla_results_clean[which(clean_rows==TRUE)]
  
  ## Now import nimble results
  
  # get file names
  filenames_nimble <- list.files("./Results/", pattern = paste0(pattern_string, 
                                                        "_naive_nimble_results_2021"))
  
  #filenames_gridded <- list.files("./", pattern = paste0(pattern_string, 
  #                                                      "_gridded_nimble_results"))
  
  # import all the files into a single list
  nimble_results <- unlist(map(.x = filenames_nimble, ~{
                           readRDS(paste0("./Results/", .x))}), recursive = FALSE)[focal_interaction_marker]
  
  #nimble_gridded_results <- unlist(map(.x = filenames_gridded, 
  #                         readRDS), recursive = FALSE)[focal_interaction_marker[1]:
  #                                                        (focal_interaction_marker[100]*25)]
  
### SECOND: extract the summaries that are needed
  
  # want the mean and CI boundaries for all parameters, in that order
  
  ## INLA
  
  # extract the mean and CI boundaries for the all.chains summary for each entry
  
  final_results_inla <- map(.x = inla_results_clean, ~{
    # extract fixed and rename
    fixed <- summary(.x)$fixed[,c(1,3,5)]
    rownames(fixed) <- c("r_i", "r_j")
    
    # extract hyper params and rename
    hyper <- summary(.x)$hyper[,c(1,3,5)]
    rownames(hyper) <- c("tau_i", "tau_j", "Rho",
                         "c_i", "c_j", "alpha_ionj", "alpha_joni")
    
    all <- rbind(fixed, hyper)
    return(all)  
  })
  
  ## Nimble
  
  # extract the mean and CI boundaries for the all.chains summary for each entry
  
  final_results_nimble <- map(.x = nimble_results, ~{
    output <- .x$summary$all.chains[,c(1,4,5)]
    return(output)
  })
  
  ## Gridded
  
  # find the result with the highest log probability across each run
  # so extract the 25 runs that go together and then select the highest logProb
  
  # need to break into blocks of 25
  #split_marker <- as.list(seq(1, 100*25, 25))
  
  #final_results_nimble_gridded <- map(.x = split_marker, ~{
  #  extract_best_result(nimble_gridded_results[as.numeric(.x):
  #                                               (as.numeric(.x)+24)])
  #})
  
### THIRD: add a column of the true values for each parameter
  
  # import all true parameter values
  
  true_all <- filter(read.csv("true_all.csv", header = TRUE, 
                              stringsAsFactors = TRUE), 
                     interaction == interaction_type, scenario == pattern_string)
  
  # should all be in order, so need to match each row of 'true' to a list 
  # element in the results
  
  ## INLA
  
  # split true into list and remove rows that failed
  inla_true <- split(true_all[which(clean_rows==TRUE),], 
                     seq(nrow(true_all[which(clean_rows==TRUE),])))
  
  # add truth to inla results, add as a column - match row names
  final_results_inla_T <- map2(.x = final_results_inla, .y = inla_true, 
                               ~{colnames(.x) <- c("Mean", "95%CI_low", "95%CI_upp")
                                 .x$true <- rep(NA, length(.x[,1]))
                                 .x["r_i", "true"] <- .y$r_i 
                                 .x["r_j", "true"] <- .y$r_j 
                                 .x["c_i", "true"] <- .y$c_i 
                                 .x["c_j", "true"] <- .y$c_j 
                                 .x["alpha_ionj", "true"] <- .y$alpha_ionj 
                                 .x["alpha_joni", "true"] <- .y$alpha_joni 
                                 .x["tau_i", "true"] <- .y$tau_i
                                 .x["tau_j", "true"] <- .y$tau_j 
                                 .x["Rho", "true"] <- .y$Rho
                                 .x$interaction_type <- interaction_type
                                 .x$model <- "inla"
                                 return(.x)
                               })
  
  ## NIMBLE
  
  # split true into list
  
  nimble_true <- split(true_all, seq(nrow(true_all)))
  
  # add truth to nimble results, add as a column - match row names
  final_results_nimble_T <- map2(.x = final_results_nimble, .y = nimble_true, 
                               ~{.x <- as.data.frame(.x)
                                  colnames(.x) <- c("Mean", "95%CI_low", "95%CI_upp")
                                 .x$true <- rep(NA, length(.x[,1]))
                               .x["r_i", "true"] <- .y$r_i 
                               .x["r_j", "true"] <- .y$r_j 
                               .x["c_i", "true"] <- .y$c_i 
                               .x["c_j", "true"] <- .y$c_j 
                               .x["alpha_ij", "true"] <- .y$alpha_ionj 
                               .x["alpha_ji", "true"] <- .y$alpha_joni 
                               .x["prec[1, 1]", "true"] <- .y$tau_i
                               .x["prec[2, 2]", "true"] <- .y$tau_j 
                               .x["prec[1, 2]", "true"] <- .y$Rho
                               .x["prec[2, 1]", "true"] <- .y$Rho
                               .x$interaction_type <- interaction_type
                               .x$model <- "nimble"
                               rownames(.x)[which(rownames(.x) == "alpha_ij")] <- "alpha_ionj"
                               rownames(.x)[which(rownames(.x) == "alpha_ji")] <- "alpha_joni"
                               rownames(.x)[which(rownames(.x) == "prec[1, 1]")] <- "tau_i"
                               rownames(.x)[which(rownames(.x) == "prec[2, 2]")] <- "tau_j"
                               return(.x)
                               })
  
  # add truth to nimble results, add as a column - match row names
  #final_results_nimble_gridded_T <- map2(.x = final_results_nimble_gridded, 
  #                                     .y = nimble_true, 
  #                             ~{.x <- as.data.frame(.x)
  #                             colnames(.x) <- c("Mean", "95%CI_low", "95%CI_upp")
  #                             .x$true <- rep(NA, length(.x[,1]))
  #                             .x["r_i", "true"] <- .y$r_i 
  #                             .x["r_j", "true"] <- .y$r_j 
  #                             .x["c_i", "true"] <- .y$c_i 
  #                             .x["c_j", "true"] <- .y$c_j 
  #                             .x["alpha_ij", "true"] <- .y$alpha_ionj 
  #                             .x["alpha_ji", "true"] <- .y$alpha_joni 
  #                             .x["prec[1, 1]", "true"] <- .y$tau_i
  #                             .x["prec[2, 2]", "true"] <- .y$tau_j 
  #                             .x["prec[1, 2]", "true"] <- .y$Rho
  #                             .x["prec[2, 1]", "true"] <- .y$Rho
  #                             .x$interaction_type <- interaction_type
  #                             .x$model <- "gridded"
  #                             rownames(.x)[which(rownames(.x) == "alpha_ij")] <- "alpha_ionj"
  #                             rownames(.x)[which(rownames(.x) == "alpha_ji")] <- "alpha_joni"
  #                             rownames(.x)[which(rownames(.x) == "prec[1, 1]")] <- "tau_i"
  #                             rownames(.x)[which(rownames(.x) == "prec[2, 2]")] <- "tau_j"
  #                             return(.x)
  #                             })
  
  
  #### RETURN ALL SUMMARY RESULTS
  
  return(list(final_results_inla_T,
              final_results_nimble_T))
              #final_results_nimble_gridded_T))
  
}