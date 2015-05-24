## Put comments here that give an overall description of what your
## functions do
#   These two functions enable calculation and caching of the inverse of an 
#   invertable square matrix. The inverse of the input matrix is cached the 
#   first time that the inverse of the matrix is calculated. Subsequent calls for
#   the inverse of the matrix retrieve the cached value, so that repeated 
#   calculation of the inverse is not required.
#   To use these functions:
#   1) The input matrix is initiated and stored by the command:
#        x <- makeCacheMatrix(<input_matrix_specification>)
#   2) The inverse of the input matrix is obtained by:
#       x_inv <- cacheSolve(x)

## Write a short comment describing this function
#   This function creates a special matrix with an associated a list of functions that:
#   1) set the value of the matrix, 2) get the value of the matrix,
#   3) set the inverse of the matrix, 4) get the inverse of the matrix
#   This special matrix is initialized by:
#       x <- makeCacheMatrix()
#   Special matrix functions are called  using the $ operator, 
#       e.g. x$set(<matrix_label>)

makeCacheMatrix <- function(x) {
    minv <- NULL
    set <- function(y) {
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) minv <<- solve
    getinv <- function() minv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
#   This function inputs a matrix and checks to see if the inverse of the matrix
#   has previously be calculated and stored. It uses functions from makeCacheMatrix().
#   If the inverse has been calculated and stored, it uses getinv() to retrive
#   the inverse and prints the message "getting cached data".
#   If the inverse has not been calculated, the inverse is calculated,
#   saved using setinv(), and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    minv <- x$getinv()
    if(!is.null(minv)) {
        message("getting cached data")
        return(minv)
    }
    data <- x$get()
    minv <- solve(data, ...)
    x$setinv(minv)
    minv    
}
