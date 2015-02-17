## These two R scripts are from Programming Assignment #2 which ##covers how R handles scoping. R uses lexical scoping as ##opposed to dynamic scoping which is used in many other common ##languages.
##The first script, makeCacheMatrix.R, returns a special matrix ##object (actually a list of functions) that can cache its ##inverse.
##The second script, cacheSolve.R can will calculate the inverse ##of the special matrix that was returned by the makeCacheMatric ##script.  If it has previously been calculated it will simply ##return the inverse matrix that has been cached instead of ##recalculating it.

## This function returns a special matrix object (a list of functions) that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL          ##initialize to NULL to detect change
set <- function(y) {  ## Create "set" function
    x <<- y
    inv <<- NULL
  }
get <- function() x         ##Create "get" function

setinverse<- function(inverse) inv <<- inverse  ##Create "setinverse" function

getinverse<- function() inv  ##Create "getinverse" function

list(set = set,              ##return list of functions
     get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}


## This function computes, if necessary, the inverse of the special "matrix" returned by makeCacheMatrix above. If it has been previously calculated, it will retrieve the inverse matrix from cache.

cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
inv <- x$getinverse()
  if(!is.null(inv)) {        ##if not NULL, return from cache
    message("getting cached data")
    return(inv)
  }
  matrx <- x$get()   ##Otherwise recalculate inverse
  inv <- solve(matrx)
  x$setinverse(inv)
  inv                 ##Return inverse
}
