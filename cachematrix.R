## Code for Week 3 peer assessment

## Caches the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    matinv <- NULL                              
    set <- function(y) {                   
      x <<- y                             
      matinv <<- NULL                       
    }
    get <- function() x                     
    setinv <- function(inverse) matinv <<- inverse  
    getinv <- function() matinv                     
    list(set = set, get = get, setinv = setinv, getinv = getinv)  
}


## Gets matrix inverse if it exists. Else evaluates the inverse of the set matrix x

cacheSolve <- function(x, ...) {
  matinv <- x$getinv()
    if(!is.null(matinv)) {
      message("getting cached data")
      return(matinv)
    }
    data <- x$get()
    matinv <- solve(data, ...)
    x$setinv(matinv)
    matinv
}
