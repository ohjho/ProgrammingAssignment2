##------------------- Functions in this file:----------------------------------
## makeCacheMatrix: This function creates a special "matrix" object
##                  that can cache its inverse.
## cacheSolve: This function computes the inverse of the special
##             "matrix" returned by `makeCacheMatrix` above. If the inverse has
##              already been calculated (and the matrix has not changed), then
##             `cacheSolve` should retrieve the inverse from the cache.
## ----------------------------------------------------------------------------

###############################################################################
## makeCacheMatrix: This function creates a special "matrix" object
##                  that can cache its inverse.
## Input: x (a matrix)
## Output: An object with the methods getinverse, setinverse, set, and get
##         set(y): reset the function's matrix from x to y and clear
##          the already set inverse (storedInverse)
##         get(): returns the matrix x
##         getinverse(): returns the stored inverse matrix of x (storedInverse)
##         setinverse(in_inverse): set the stored inverse matrix of x to 
##          inverseM
###############################################################################
makeCacheMatrix <- function(x = matrix()) {
  storedInverse <- NULL
  
  set <- function(y){
    x <<- y
    storedInverse <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(in_inverse) storedInverse <<- in_inverse
  
  getinverse <- function() storedInverse
  
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
}


###############################################################################
## cacheSolve: This function computes the inverse of the special
##             "matrix" returned by `makeCacheMatrix` above. If the inverse has
##              already been calculated (and the matrix has not changed), then
##              `cacheSolve` should retrieve the inverse from the cache.
## Input: x (a matrix object returned by the function makeCacheMatrix)
## Output: An inverse of matrix x 
###############################################################################
cacheSolve <- function(x, ...) {
  outInverse <- x$getinverse()
  if (is.null(outInverse)){
    iMatrix <- x$get()
    outInverse <- solve(iMatrix)
    x$setinverse(outInverse)
  } 
  outInverse
}
