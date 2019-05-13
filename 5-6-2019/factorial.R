n_factorial <- function(n){
  if ( n == 1 ) return(n)
  else return( n * n_factorial(n-1) )
}
