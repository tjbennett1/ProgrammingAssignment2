## These functions will compute and chache the inverse of a matrix - allowing recall of the inverse to save consuming computations
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## cacheSolve retrieves the cached inverse if one exists.  If one does not exist it will compute the inverse of of the special "matrix" returned by makeCacheMatrix.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## here x is a matrix
    m <- NULL
    set <- function(y) {
      x<<-y
      m<<-NULL
    }
    
    get  <- function()x
    setinverse <- function(solve) m<<- solve
    getinverse <- function() m
    
## prints a list of values
   list(set=set, get=get, setinverse=setinverse,getinverse = getinverse)
}


## cacheSolve retrieves the cached inverse if one exists otherwise it will compute the inverse of the matrix returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
  ##here x is variable for makeCacheMatrix - not  a matrix. m gets cached value of x
  
  m <- x$getinverse()
  ## need to see if x has a cached inverse by comparing the value of the matrix to what was input in this function
  
     ## if the m is not null then retrieve the cached value
    if(!is.null(m)){
      message("getting cached inverse")
      return(m)
    }
    ## if no cached value exists then calculate the inverse of the matrix
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
    
    
        ## Return a matrix that is the inverse of 'x'
}
