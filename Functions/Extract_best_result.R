################################################################################

#### Function to find the highest log prob result from gridded nimble

################################################################################

#### scripts

source('./Functions/StoreLogProb.R')

################################################################################

extract_best_result <- function(nimble_gridded_results){

LogProb <- t(map(nimble_gridded_results, ~{
  .x['sumLogProb', 1]
}))

if(length(LogProb) > 25){print(nimble_gridded_results)}

# find the maximum LogProb and extract full results

final_nimble_result_gridded <- nimble_gridded_results[[which(unlist(LogProb) == 
                                                      max(unlist(LogProb)))]][,c(1,4,5)]

alphas <- as.matrix(expand_grid(alphaij = seq(-1,1,length.out = 5),
                                alphaji = seq(-1,1,length.out = 5)))

best_alpha <- alphas[which(unlist(LogProb) == 
               max(unlist(LogProb))),]

temp <- data.frame(Mean = best_alpha,
                   CI_low = NA,
                   CI_upp = NA)

colnames(temp) <- colnames(final_nimble_result_gridded)

final_nimble_result_gridded_alpha <- rbind(final_nimble_result_gridded, 
                                     temp) 

rownames(final_nimble_result_gridded_alpha) <- c(rownames(final_nimble_result_gridded),
                                                 "alpha_ij", "alpha_ji")

return(final_nimble_result_gridded_alpha)

}
