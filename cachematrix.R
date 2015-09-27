## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Set matrix
makeCacheMatrix <- function(m = matrix()) {
  inv <- matrix()
  set <- function(y) {
    m <<- y
    inv <<- matrix()
  }
  get <- function() m
  setInv <- function(i) inv <<- i
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## Function to cached data
cacheSolve <- function(m, ...) {
## Return inverse matrix of 'm' by checking, if not NA then return cached data
  inv <- m$getInv()
  if(!is.na(inv[1,1])) {
    message("cached data")
    return(inv)
  }
  else{
# If the cached data is NA then calculate the inversion matrix of 'm'
  x <- m$get()
  inv <- solve(x)
  m$setInv(inv)
  inv 
  }
}
