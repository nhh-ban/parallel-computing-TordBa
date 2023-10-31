# Assignment 1:  
library(tweedie) 
library(ggplot2)
library(tictoc)

simTweedieTest <-  
  function(N){ 
    t.test( 
      rtweedie(N, mu=10000, phi=100, power=1.9), 
      mu=10000 
    )$p.value 
  } 


# Assignment 2:  
MTweedieTests <-  
  function(N,M,sig){ 
    maxcores <- 8
    Cores <- min(detectCores(), maxcores)
    
    c1 <- makeCluster(Cores)
    
    registerDoParallel(c1)
    
    sum(replicate(M,simTweedieTest(N)) < sig)/M 
    
    stopCluster(c1)
  } 


# Assignment 3:  
df <-  
  expand.grid( 
    N = c(10,100,1000,5000, 10000), 
    M = 1000, 
    share_reject = NA) 


for(i in 1:nrow(df)){ 
  df$share_reject[i] <-  
    MTweedieTests( 
      N=df$N[i], 
      M=df$M[i], 
      sig=.05) 
} 


