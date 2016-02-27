# makeCacheMatrix: This function creates a special "matrix" object 
# that can cache its inverse.

# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then cacheSolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
# Creation of matrix
 inversematrix <- NULL
 setmatrix <- function(y) {
    x <<- y
    inversematrix <<- NULL
}
# getting created matrix
 getmatrix <- function() x
# Creation of inverse matrix
 setInverse <- function(inverse) inversematrix <<- inverse
# getting created inverse matrix
 getInverse <- function() inversematrix
 list(setmatrix = setmatrix,
       getmatrix = getmatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}

# If inverse matrix isn't calculate in previus function, then this function will calcalate it
# and put inverse mstrix to cache.
# Next time if you call inverse matrix, this function will get it from cache.

cacheSolve <- function(x, ...) {
# Return a matrix that is the inverse of 'x'
 
# getting inverse matrix from the cache if it is created before.
 inversematrix <- x$getInverse()
  if (!is.null(inversematrix)) {
    message("Getting cached matrix")
    return(inversematrix)
  }
 matrix <- x$getmatrix()
 inversematrix <- solve(matrix, ...)
 x$setInverse(inversematrix)
 inversematrix
}
