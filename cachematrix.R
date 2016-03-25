## Put comments here that give an overall description of what your
## functions do

## This function creates a list representing a matrix that can have its inverse cached to avoid a reevalation

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  getInv <- function() inv
  setInv <- function(z) inv <<- z
  list(get = get, set=set, getInv=getInv, setInv=setInv)
}


## Giving a matrix constructed by the above function this function solves for the inverse, 
## either by calculating it or returning the cached inverse calculated previously

cacheSolve <- function(x, ...) {
  # If no cached inverse, solve for it
  if (is.null(x$getInv())){
    inv <- solve(x$get())
    x$setInv(inv)
    
  }
  # return the now cached inverse
  x$getInv()
}
