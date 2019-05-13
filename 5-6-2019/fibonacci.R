N <- 12

F <- rep(0,N)

fibonacci <- function(n){
  if ( n == 1 || n == 2 ){
    Fa[n] <- 1
    return(Fa)
  } else {
    Fa[n] <- fibonacci(n-2) + fibonacci(n-1) 
  }
}
