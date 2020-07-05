

x<- matrix(11:14,2,2) ## Any matrix
solve(x) ## the inverse of the matrix 

## The first function, makeCacheMatrix creates a special "Matrix", which is really a list containing a function to

# 1.set the value of the Matrix
# 2.get the value of the Matrix
# 3.set the value of the solve
# 4.get the value of the solve

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

#The following function calculates the inverse of the special "Matrix" created with the above function

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}


## solution: Return a matrix that is the inverse of 'x'

cacheSolve(makeCacheMatrix(x)) 

# Please, take the following comand to compare the solution

solve(x) 

