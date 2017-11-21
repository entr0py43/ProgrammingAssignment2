## Function to cache the inverse of a matrix to save resources
##first part creates an set of functions for setting/getting, sets global inv value to null when 'reset'

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function (y){
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv<-function(solve) inv<<-solve ## this is super confusing
      getinv<-function() inv
      list(set = set, get = get,
            setinv = setinv,
            getinv = getinv)
}


## function to create an inverse matrix if its not already present
## if it has already created and cached this, it just returns the cached value inv

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }      
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
      
}

## wow, how does this in any way relate to the lectures and swirl practice for this week? 