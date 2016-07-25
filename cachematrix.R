## In this file, makeCacheMatrix is a function that is written to create a "special" matrix
## Next, another function cacheSolve will solve the special matrix for inverse if the inverse is not already calculated
## If the inverse is already available, the cacheSolve simply returns the value of the stored inverse without recomputation and thus 
##potentially reducing repetitive call to the solve function on a matrix that is not necessarily changing

## This makeCacheMatrix function creates a special matrix that has the properties described above in the generic comments

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
      
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
      
  }


## This cacheSolve function returns the a precalculated inverse for a known matrix(from makeCacheMatrix) or solves for a new matrix inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
           m<-x$getinverse()
           
           if(!is.null(m)) {
             
             message("getting cached data")
             return(m)
           }
          
          data<-x$get()
          m<-solve(data,...)
          x$setinverse(m)
          m
  
  
}
