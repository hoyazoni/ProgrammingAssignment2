## The two functions in this program improve efficiency for matrix inversion.
## The user must provide an invertible square matrix 
## (ie., the matrix's number of rows and columns are equal,
## and the matrix's determinant is nonzero.)
## The inversion process only occurs once per R session, as long as 
## the original matrix is unchanged.

## makeCacheMatrix takes an invertible square matrix as an input.
## It returns a list of four functions that can be called by
## cacheSolve to avoid unnecessarily repeated inversions.
## Example: > mymatrix <- matrix(c(13, 7, 1, 22), 2, 2)  
##          > myfunctionlist <- makeCacheMatrix(mymatrix)

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

## cacheSolve uses the list of functions produced by makeCacheMatrix as input.
## It inverts and returns the inverse of the matrix the first time it is run
## in a session, or if makeCacheMatrix is rerun in a session.
## For subsequent runs on the same input matrix, it returns the result
## from cache memory and notifies the user it has done so.
## Example: > myinvertedmatrix <- cacheSolve(myfunctionlist)

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
        