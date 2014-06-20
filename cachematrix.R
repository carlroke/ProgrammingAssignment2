## Below are two functions that are used to create a special object 
## that stores a matrix and cache's its inverted matirx.
## 
## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinvert <- function(invert) inv <<- invert
  getinvert <- function() inv
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}

## The following function calculates the inverse of the 
## special "matrix" created with the above function. 
## However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, 
## it calculates the inverse of the data and sets the value 
## of the inverse in the cache via the setinvert function.
##
##  Here is a test case:
##  - create the matrix object and populate with a 3x3 matrix
##  - call the getter the first time, it should not be cached,
##    and should cache
##  - call the getter the second time, it should fetch the 
##    cached version and display a message
##
##  				> mat <- matrix(c(1,1,1,3,4,3,3,3,4), nrow = 3, ncol = 3)
##					> yy<-makeCacheMatrix(mat)                               
##					> cacheSolve(yy)                                         
##					     [,1] [,2] [,3]                                      
##					[1,]    7   -3   -3                                      
##					[2,]   -1    1    0                                      
##					[3,]   -1    0    1                                      
##					
##					> cacheSolve(yy)                                         
##					getting cached data                                      
##					     [,1] [,2] [,3]                                      
##					[1,]    7   -3   -3                                      
##					[2,]   -1    1    0                                      
##					[3,]   -1    0    1                                      
##					

cacheSolve <- function(x, ...) {
    inv <- x$getinvert()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinvert(inv)
    inv
}
