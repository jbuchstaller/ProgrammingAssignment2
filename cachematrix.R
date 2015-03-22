## Put comments here that give an overall description of what your
## functions do

#These functions allow to compute the inverse of a matrix and store the
#result in the cache. The cache stores recently used information so that
#it can be quickly accessed at later time.

## Write a short comment describing this function:

#This function takes a user defined matrix as an argument and contains 4 
#functions. The first function (set) sets the data matrix (x) to any user input.
#The function get returns the current matrix. The function setinverse sets the 
#stored inverse matrix to the replacement inverse matrix that is computed by the
#CacheSolvefunction. The getinverse function returns the stored inverse matrix. 
#Thus the makeCacheMatrix function creates an object that has functions. This object 
#will then be used by the cacheSolve function to compute the inverse.


makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

#cacheSolve takes the object created by makeCacheMatrix and sets the local inverse 
#matrix(i) to the inverse matrix created by makeCacheMatrix. If the local inverse matrix
#is different from zero (has been computed before, it returns it from the cache.
#If it has not been computed before (on the first iteration or because the user 
#entered another matrix via the set function in makeCacheMatrix, which also sets i 
#to NULL), it will get the local matrix from makeCacheMatrix (data) and use it to 
#compute the inverse matrix.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i    
        

}

#test code with:
#m <- matrix(c(-1, -2, 1, 1), 2,2)
#mymatrix<-makeCacheMatrix(m)
#cacheSolve(mymatrix)
#cacheSolve(mymatrix)
