## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function creates a special "matrix" object that can cache its inverse so it does not
#have to be calculated multiple times
makeCacheMatrix <- function(x = matrix()){
  #set the inverse as null as default
  inv <- NULL
  ##
  set = function(y) {
    #This function allows the cachesolve function to check if the inverse of a matrix exist 
    #and if it has not changed then retrieves the value. 
    x <<- y
    inv <<- NULL
  }
  #we now define the funtion that will allow us to solve the inverse
  get = function() x
  setinv = function(solve) inv <<- solve 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
       }

#This function returns the inverse of a matrix created with
#the makeCacheMatrix function, defined previously.
#The function checks if the cached inverse is available, if this is the case
#it retrieves it, otherwise it computes the inverse, caches it, and returns it.
cachesolve <- function(x, ...) { 
  inv<- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv<- solve(data, ...)
  x$setinv(inv)
  return(inv)
}

test <- matrix(runif(12,1,100),3,3)
# generate the makeCacheMatrix object with this matrix
testCached <- makeCacheMatrix(test)
# from now on calculate or retrieve calculated inversion using the cacheSolve function
testInv <- cachesolve(testCached)
