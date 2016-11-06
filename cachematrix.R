## These two functions (makeCacheMatrix and cacheSolve) will create a special
## Matrix object, that can be filled with values, calculates the mean, and caches it. 
## By using the second function, the mean will be inversed.

## Creates a special Matrix object, and calculates & caches it's mean value

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setmeaninverse <- function(inverse) inv <<- inverse
  getmeaninverse <- function() inv
  list(set = set,
       get = get,
       setmeaninverse = setmeaninverse,
       getmeaninverse = getmeaninverse)
}

## This function inverses the mean received from the makeCacheMatrix above.
## If nothing changed in the Matrix, the cached version will be fetched.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getmeaninverse()
        if(!is.null(inv)){
          message("using cached data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setmeaninverse(inv)
        inv
}
