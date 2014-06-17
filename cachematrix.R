## This sccript performs caching the inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) 
      {
            slv <- NULL
            set <- function(y) {
                  x <<- y
                  slv <<- NULL
            }
            get <- function() x
            setsolve <- function(solve) slv <<- solve
            getsolve <- function() slv
            
            list(set = set, get = get,
                 setsolve = setsolve,
                 getsolve = getsolve)
      }


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix function

cacheSolve <- function(x, ...) 
      {
            m <- x$getsolve()
            if(!is.null(m)) {
                  message("getting cached data")
                  return(m)
            }
            data <- x$get()
            m <- mean(data, ...)
            x$setsolve(m)
            m
      }
