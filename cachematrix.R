## My functions ultimately create an inverse of a matrix, or recall the inverse 
## if it already exists.  To run this I advise to first create a matrix
## with equal rows as columns such as "m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)"
##then assign this matrix "myMatrix_object <- makeCacheMatrix(m1)"
##last run the Cachesolve cacheSolve(myMatrix_object).

## This function creates a special "matrix" object that can cache its inverse.
## It does this by creating an empty matrix environment that can be filled
##by the "get" function.
makeCacheMatrix <- function(x = matrix()) {
  
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- matrix()
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
 
}



## cacheSolve will compute the inverse of the matrix 
##created by makeCacheMatrix, unless it has already been created.
## If it has been created it will just return the value already available.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

   m <- x$getinverse()
   if(!is.null(m)) {
     message("getting cached data")
     return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setinverse(m)
   m
}
  
  

