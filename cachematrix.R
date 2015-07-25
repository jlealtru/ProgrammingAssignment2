#Jesus Leal Trujillo program assignment 2

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function creates a special "matrix" object that can cache its inverse so it does not
#have to be calculated multiple times
makeCacheMatrix <- function(x = matrix()){
  #set the inverse as null as default
  inv <- NULL
  #
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
#the makeCacheMatrix function defined previously.
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
#we now test the code with a 3x3 matrix based on a uniform distribution that takes
#values between 1 and 100
proof <- matrix(runif(12,1,100),3,3)
#generate the makeCacheMatrix object with this matrix
test <- makeCacheMatrix(proof)
#we calculate or retrieve the calculated inversion using the cachesolve function
testInv <- cachesolve(test)
