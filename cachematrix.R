## Put comments here that give an overall description of what your
## functions do

## this function takes a matrix and converts it into 
## a matrix capable of caching its inverse.
## this matrix can save time on not recalculating the already calculated inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL ##this is making a variable to store the calculated inverse
                  ##it is set to NULL so it wont mess up the calculations.
  
  set <- function(input){
    x <<- input
    inverse <<- NULL
    ## this function takes an input matrix, and replaces the parent function's 
    ## variable 'x', so it replaces the 'x' assigned to 'makeCacheMatrix' function.
    
  }
  
  ## 'get' function returns the current value of 'x' inside 'makeCacheMatrix' Environment
  get <- function (){ 
    return (x)
  }
  
  ## this function replaces the value of 'inverse' whithin
  ## the 'makeCacheMatrix' Environment with a new one.
  setinv <- function(inv){
    inverse <<- inv
  }
  
  ## this function returns the current value of 'inverse'
  ## inside the 'makeCacheMatrix' Environment.
  getinv <- function(){
    return (inverse)
  }
  
  ## makes and returns a list of our newly made functions
  ## so it is easy to access from another function.
  ## sort of like a Menu of functions within 'makeCacheMatrix'
  list (set = set,
        get = get,
        setinv = setinv,
        getinv = getinv)

}


## this function first checks if there's an already calculated inverse
## within our 'Special' made matrix and returns it if available.
## if not available, it calculates the inverse of our matrix
## and stores it inside the matrix for future use.

cacheSolve <- function(x, ...) {
  
  inverse <- x$getinv()   ##gets the value of inverse from our 'Special Matrix' and puts it inside 'inverse'
  
  ## now we check if there is a value attached to 'inverse'
  ## that means there's a previously calculated inverse inside it
  ## so we dont need to recalculate 'inverse' and we just have to return it.
  if (!is.null(inverse)) {
    message("getting cached data")
    return (inverse)
  }
  
  ##if we're here, that means 'inverse' was NULL, so we have to calculate it
  ## so now we get the stored matrix and put it inside 'data' so we can do stuff with it.
  data <- x$get()
  
  ## so we simply calculate the inverse with a built-in functino of R
  ## and store it inside 'inverse'
  inverse <- solve(data)
  
  ## here we store the newly calculated 'inverse' inside our "Special Matrix"
  ## so we have access to it in the future and dont recalculate it.
  x$setinv (inverse)
  
  ## and finally, we return the calculated inverse
  return (inverse)
}
