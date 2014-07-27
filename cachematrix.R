## The two functions included in this program are designed to improve efficiency for matrix inversion.
## The user must provide an invertible square matrix (ie., the matrix's number of rows and columns are equal,
## and the matrix's determinant is nonzero.)
## The functions will allow the user to invert the matrix, store that result in a cache, and then return the
## inverted result from the cache for all subsequent inversion requests of the same matrix. 
## The computationally-intensive inversion process only occurs once per R session, as long as the original
## matrix is unchanged.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse  <- function(inverse) m <<- inverse 
    getinverse <- function() m
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'


cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {p <- 
                     message("getting cached data")
                     return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
        