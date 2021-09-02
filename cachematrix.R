## create a special "matrix" object that can cache its inverse
## The first function, makeCacheMatrix creates a special “matrix”, 
## which is really a list containing a function to:

## Step1. To set the value of the matrix
## Step2. To get the value of the matrix
## Step 3: To set the value of the inverse 
## Step 4. To get the value of the invers matrix



makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


## Testing
## Below we call the function with a matrix, compute the inverse, retrieve the inverse from the cache list, change the call matrix to the inverse, compute the inverse on that and return the original function.


Test1 <- matrix(c(1,2,3,4),2,2)
#solve(Test1) #We pretend that this cant't happen xD


Test1_inverse <- makeCacheMatrix(Test1)
cacheSolve(Test1_inverse) #inverse returned after computation


cacheSolve(Test1) #inverse returned from cache
