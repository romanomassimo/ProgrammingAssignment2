## Two functions have been created to calculate and store in a function environment 
# the result of a function, here the calculated inverse matrix.

# The "makeCacheMatrix" function creates an R object which stores input and 
# output values (x and m, respectively), as well as four functions used
# by the "cacheSolve" function: "set" is to set the original matrix (the input),
# "get" is to call that matrix, "setsolve" is to set the solved matrix and 
# the "getsolve" is to call the solved matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve  
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

# The "cacheSolve" calculates the solved matrix or calls the result of solved matrix
# if the solution has been calculated and cached already and informs the user that 
# the result comes from the function environment. 

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  return(m)
}

