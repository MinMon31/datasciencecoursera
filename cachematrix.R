#this function containing a list of four values to be used for the process of the function:
#set, get, and getInversa setInversa. It use "<< -" as an operator to operate 
#internally without affecting external environment of the function.  
makeCacheMatrix <- function(x = matrix()) {  
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL 
  } 
  get <- function() x 
  setInversa <- function(inv) Inv <<- inv
  getInversa <- function() Inv 
  list(set = set, get = get,
       setInversa = setInversa,
       getInversa = getInversa)
}

#In this part, the function return the inverse matrix from the objetc X, it will be null if uncalculated, 
#the function return the message.
cacheSolve <- function(x, ...) {
  m <- x$getInversa()
  if(!is.null(m)) { 
    message("getting cached data")
    return(m) 
  }
  data <- x$get() 
  m <- solve(data) 
  x$setInversa(m) 
  m 
}

# Test 1
# generate a random square, non-singular matrix
Matriz1 <- matrix(runif(25,1,500),5,5)
# generate the makeCacheMatrix object with this matrix
Matriz1Cached <- makeCacheMatrix(Matriz1)
# from now on calculate or retrieve calculated inversion using the cacheSolve function
Matriz1Inv <- cacheSolve(Matriz1Cached)
