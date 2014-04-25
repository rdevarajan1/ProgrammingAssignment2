## Put comments here that give an overall description of what your
## functions do

## Caches the matrix and its inverse to the environment so it can be accessed at a later time

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
     x <<- y
     inv <<- NULL
  }
  get <- function() x
  setinv <- function (inv_m) inv <<-inv_m
  getinv <- function () inv
  list(set = set, get = get,
       setinv = setinv, getinv = getinv)

}


## Accesses the inverse of the cached matrix and if there is no 
## cached inverse of the inverse computes the inverse and caches it

cacheSolve <- function(x, ...) {
  # look for the inverse in cache
  inv_m <- x$getinv()
  
  ## Return a matrix that is the inverse of 'x'
  if(!is.null(inv_m)){
    message("getting cached data")
    return(inv_m)  
    }
  ## If there is no inverse in cache retreive the matrix and compute the inverse
   mat <- x$get()
   inv_m <-solve(mat)
   x$setinv(inv_m)
   inv_m
}
