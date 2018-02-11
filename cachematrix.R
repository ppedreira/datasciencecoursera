
# makeCahceMatrix() will be a function used to create a special matrix object
# that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

## We can inicialize using NULL  
  
  inverse_matrix <- NULL
  
## set function "sets" the matrix
  set <- function (y) {
       x <<- y
       inverse_matrix <<- NULL
    }
 ## get function provides the matrix (not the inverse!!)
    get <- function() x
    
## We set the inverse    
    setinverse <- function(inverse) inverse_matrix <<- inverse
## We get the inverse
    getinverse <- function () inverse_matrix

## List of functions
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}



## cacheSolve() returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse_matrix <- x$getinverse()
  if(!is.null(inverse_matrix)) {
          message("Getting Cached Data")
          return(inverse_matrix)
  }
  data <- x$get()

  ## solve function computes the inverse of square matrix
  
  inverse_matrix <- solve(data, ...)
  
  ##
  x$setinverse(inverse_matrix)
  ## It provides the matrix inverse from the cache
  inverse_matrix
  
}

