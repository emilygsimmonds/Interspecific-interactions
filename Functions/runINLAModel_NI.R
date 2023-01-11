# This script runs an INLA model to quantify inter-specific interaction 
# strength using the simulated time series

###############################################################################

# If you open in RStudio go to Edit > Folding > Collapse all

###############################################################################

run_inla_model <- function(x){
  
  # set up INLA data
  
  n <- length(x[,1]) # length to time series
  
  # create a matrix where you save data to make state space
  
  Y = matrix(NA, 4*n-2, 4) 
  
  m = n-1
  Y[1:n,     1] = x$Y_i # Species 1 obs
  Y[n + 1:n, 2] = x$Y_j # Species 2 obs
  
  Y[2*n + 1:m, 3] = 0 # Species 1 state discrepancy
  Y[3*n + 1:m-1, 4] = 0 # Species 2 state discrepancy
  
  # next line assigns the value Y to global envir so R can find it
  
  assign("Y", Y, .GlobalEnv)
  
  # and indexes
  
  NAs.Y <- rep(NA, n) # indexes: NA length = n
  NAsm1 <- rep(NA, m) # indexes: NA length = n-1
  
  # connect state and count sp1
  # indexes should be 1:n column 1, NA col 2 and 4, 1:n column 3 but first is NA
  sp1.lamda <- c(1:n, NAs.Y, 2:n, NAsm1) 
  # weights results 1 = observation, -1 = state = mean of 0
  w.sp1.lamda <- c(rep(1,n), NAs.Y, rep(-1, m), NAsm1)
  
  # same for sp2 but shift columns by 1
  sp2.lamda <- c(NAs.Y, 1:n, NAsm1, 2:n) # connect state and count sp2
  w.sp2.lamda <- c(NAs.Y, rep(1,n),  NAsm1, rep(-1, m))
  
  # need a copy for each coefficient
  # create the effect of species on itself
  # do this for the STATE part, NA in first bit then 1:n-1
  sp2.copy1 <- c(NAs.Y, NAs.Y, NAsm1, 1:m) # time lag influence on own species
  
  # indexed to the state/ discrepancy not to the observations
  
  # time lag influence on other species (j on i)
  #sp2.copy2 <- c(NAs.Y, NAs.Y, 1:m, NAsm1) 
  
  sp1.copy1 <- c(NAs.Y, NAs.Y, 1:m, NAsm1) # time lag influence on own species
  # time lag influence on other species (i on j)
  #sp1.copy2 <- c(NAs.Y, NAs.Y, NAsm1, 1:m) 
  
  # Finally, create the shared noise component
  # this focuses on the state, first entry is NA
  combo.noise <- c(NAs.Y, NAs.Y, 2:n, 2:n) # shared noise term (correlated)
  
  # create vectors for the intercepts
  
  muI <- c(NAs.Y, NAs.Y, rep(1, m), NAsm1)
  muJ <- c(NAs.Y, NAs.Y,  NAsm1, rep(1, m))
  
  # save as a data frame
  
  data = data.frame(Y=Y, 
                    sp1.lamda=sp1.lamda, # species i counts
                    w.sp1.lamda = w.sp1.lamda, # species i obs process
                    sp2.lamda=sp2.lamda, # species j counts
                    w.sp2.lamda= w.sp2.lamda, # species j obs process
                    sp2.copy1=sp2.copy1, # species j intra-specific
                    #sp2.copy2=sp2.copy2, # species j inter-specific
                    sp1.copy1=sp1.copy1, # species i intra-specific
                    #sp1.copy2=sp1.copy2, # species i inter-specific
                    combo.noise=combo.noise, # noise
                    muI=muI, # intercepts
                    muJ=muJ)
  
  # set up formula
  
  formula <- Y ~ f(sp1.lamda, w.sp1.lamda, model="iid", initial=-10, fixed=T) + 
    f(sp2.lamda, w.sp2.lamda, model="iid", initial=-10, fixed=T) +
    
    # density dependence effect
    
    f(sp1.copy1, copy = "sp1.lamda", 
      hyper = list(Beta = list(initial = 0.5, 
                               prior = "gaussian", param=c(0.5, 1/(5^2)), fixed=F))) + 
    f(sp2.copy1, copy = "sp2.lamda", 
      hyper = list(Beta = list(initial = 0.5, 
                               prior = "gaussian", param=c(0.5, 1/(5^2)), fixed=F))) +
    
    # inter-specific effects
    
    #f(sp1.copy2, copy = "sp1.lamda", 
     # hyper = list(Beta = list(initial = 0.000000001, 
      #                         prior = "gaussian", param=c(0,1/(0.1^2)), fixed=F))) +
    #f(sp2.copy2, copy = "sp2.lamda", 
     # hyper = list(Beta = list(initial = 0.000000001, 
      #                         prior = "gaussian", param=c(0,1/(0.1^2)), fixed=F))) +
    
    f(combo.noise, model="iid2d", n = 2*(n-1)) + 
    
    muI + muJ - 1
  
  # run model
  INLAModel <- inla(formula = formula, data=data, 
                    family = rep(c("poisson", "gaussian"), each=2), 
                    control.family = list(list(), list(), 
                                          list(initial=10, fixed=T), 
                                          list(initial=10, fixed=T)), 
                    control.fixed = list(mean = list(muI=6, muJ=6), 
                                         prec = 1))
  
  return(INLAModel)
}