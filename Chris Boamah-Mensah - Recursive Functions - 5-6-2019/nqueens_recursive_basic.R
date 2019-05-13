N <- 4
Qs <- rep(0,N)

r <- 1

place_queens <- function(Qs,r){
  if (r == length(Qs) + 1){
    print(Qs)
  } else {
    for ( j in 1:length(Qs) ){
      legal <- TRUE
      for ( i in seq_len(r-1) ){
        if ( (Qs[i] == j) || (Qs[i] == j+r-i) || (Qs[i] == j-r+i) ){
          legal <- FALSE
        }
      }
      if ( legal == TRUE ){
        Qs[r] <- j
        place_queens(Qs,r+1)
      }
    }
  }
}
