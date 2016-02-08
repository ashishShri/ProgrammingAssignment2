## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly.
## Following functions creates a special "Matrix" object that can cache its inverse.


## makeCacheMatrix function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  
  inv =  NULL
  set <- function(y) {
    x <<- y
    inv<-NULL
  }
  get <- function() x
  setInv <- function(matrixInv) inv <<- matrixInv
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}



## cacheSolve function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve should retrieve 
## the inverse from the cache.
## solve() function is used to generate the inverse of a matrix
## Note: No checks are performed for invertibility of the matrix except 
## errors/warnings from the solve functions.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matrixInv <- x$getInv()
  if(!is.null(matrixInv)) {
      message("getting cached data")
      return(matrixInv)
  }
  dataMatrix <- x$get()
  matrixInv <- solve(dataMatrix)
  x$setInv(matrixInv)
  matrixInv
}
