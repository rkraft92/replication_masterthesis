# Initialization of covariance-variance matrix

v_matrix <- function(n_reg, b, c){

V <- diag(n_reg)

if (n_reg == 3){
  V[1,2] <- b
  V[1,3] <- c
  V[2,1] <- b
  V[2,3] <- b
  V[3,1] <- c
  V[3,2] <- b
}
else {
  

# first row
V[1, 2] <- b
V[1, 3] <- c

#second row
V[2, 1] <- b
V[2, 3] <- b
V[2, 4] <- c

#last row
V[n_reg, n_reg-1] <- b
V[n_reg, n_reg-2] <- c

#second-last row
V[n_reg-1, n_reg] <- b
V[n_reg-1, n_reg-2] <- b
V[n_reg-1, n_reg-3] <- c



for (idx in seq(n_reg)){
  
  if (idx >= 3 & idx <= n_reg -2){
    
    V[idx, idx +1] <- b
    V[idx, idx +2] <- c
    V[idx, idx -1] <- b
    V[idx, idx -2] <- c
    
  }
  
  
}
}
return(V)
}