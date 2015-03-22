##NOTE: additional comments were removed in this iteration.
## i only left comments for the main functions 'makeCacheMatrix' and 'cacheSolve'
## solely because those are the only ones specified to be commented inside the Assignment Page.
## you should realize that the PREVIOUS iteration is FULLY commented. you can access it through
## my github repository if needed.
## but then again, didnt we write the SAME function, only with matrices as inputs and a different calculation?!
## ALSO: i changed the name of the variables to a more meaningful one. 



## this function can take a matrix and put it inside a 'special entity' capable of caching its inverse.
## the new entity has the ability to store the matrix and its inverse, and is able to return both if needed.
## this new entity can save time on not recalculating the already calculated inverse by essentially caching it.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL 

  set <- function(input){
    x <<- input
    inverse <<- NULL
    
  }
  
  get <- function (){ 
    return (x)
  }
  
  setinv <- function(inv){
    inverse <<- inv
  }
  
  getinv <- function(){
    return (inverse)
  }
  
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
  
  inverse <- x$getinv() 
  
  if (!is.null(inverse)) {
    message("getting cached data")
    return (inverse)
  }
  
  data <- x$get()
  
  inverse <- solve(data)
  
  x$setinv (inverse)
  
  return (inverse)
}
