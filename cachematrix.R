## These functions cut down on time creating inversed matrices

## makeCacheMatrix sets the value of a special matrix and gets the value of that matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
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


## This function computes the inverse of the matrix returned by makeCacheMatrix.
## It first checks to see if the mean has already been calculated. If true, then it
## gets mean from cache. If not, it produces the inverse sets it via setinverse. 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
              message("Getting cached data")
              return(m)
        }
        dat <- x$get()
        m <- solve(dat,...)
        x$setinverse(m)
        m
}

#The following is a test on a 2x2 matrix of numbers 1 through 4
#mat <- matrix(c(1:4),2,2)
#mat_f <- makeCacheMatrix(mat)
#cacheSolve(mat_f)


