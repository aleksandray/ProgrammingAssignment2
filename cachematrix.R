## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  INV <- NULL  # holding place for the inverse matrix
  set <- function(y){ # new input 
      x <<- y
    INV <<- NULL
  }
  get <- function() x # function that returns input matrix
  setsolve <- function(solve) INV <<- solve # function that calculates inverse of input matrix
  getsolve <- function() INV # result of the solve function
    list(set=set,get=get, # list of the results that can be used in a new function
        setsolve=setsolve,
        getsolve=getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
            INV <- x$getsolve() # calculates inverse of input matrix
            if(!is.null(INV)) { # checks weateher inverse existis in memory
                    message("getting cached data")
                    return(INV)
            }
    # rest of the code calculates inverse in case it does not exist
            data <- x$get() # gets matrix generated with makeCacheMatrix
            INV <- solve(data, ...) #calculates inverse of that matrix
            x$setsolve(INV) # store inverse in the object 
            INV # print inverse
    }
