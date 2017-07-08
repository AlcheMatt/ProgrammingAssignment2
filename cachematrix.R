## The pair of functions below will cache and return the reverse
## of a given matrix

## makeCacheMatrix returns a list of functions to set or get the original
## or inversed matrix

makeCacheMatrix <- function(x = matrix()) {
      inv = NULL #inv is the reverse of x
      set <- function(y) {
            x <<- y
            inv <<- NULL
      } # set new matrix
      get <- function() x # get the matrix
      setInv <- function(invers) inv <<- invers # set inv to invers
      getInv <- function() inv # retrieve inv
      list (set = set, get = get, 
            setInv = setInv, getInv = getInv)
}


## cacheSolve return the inversed matrix by retrieving it from cache if it 
## is solved already, or solve the inverse and store it with setInv.

cacheSolve <- function(x=matrix(), ...) {
      inv <- x$getInv()
      if (!is.null(inv)) {
            print ("Retrieved from cache")
            return (inv)
      } #if inv is stored, return directly
      tmpMtrx <- x$get() #use tmpMtrx to store x temparorily
      inv <- solve(tmpMtrx)
      x$setInv(inv)
      print ("Solved and cahced")
      inv
}
