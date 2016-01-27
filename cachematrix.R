makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  get <- function() x
  inverse <- function() inv 
  set_inverse <- function(m) inv <<- m
  list(get=get, inverse=inverse, set_inverse=set_inverse)
}

cacheSolve <- function(x, ...){
  inv <- x$inverse()
  if(!is.null(inv)) return(inv)
  dat <- x$get()
  inv <- solve(dat, ...)
  x$set_inverse(inv)
  inv
}
