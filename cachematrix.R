#' The following two functions cache the inverse of a matrix to speed up calulation times.

#' \code{makeCacheMatrix} creates a matrix object that can cache its inverse.
#'
#' @param x matrix
#'
#' @return a special "matrix", which is really a list containing functions to cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    # cachedMatrix will store the cached inverse matrix
    cachedMatrix <- NULL

    # Matrix setter
    setMatrix <- function(y) {
        x            <<- y
        cachedMatrix <<- NULL
    }
    # Matrix getter
    getMatrix <- function() x

    # Inverse matrix setter
    setMatrixInverse <- function(inverse) cachedMatrix <<- inverse

    # Inverse matrix getter
    getMatrixInverse <- function() cachedMatrix

    # Return the matrix of functions
    list(setMatrix        = setMatrix,
         getMatrix        = getMatrix,
         setMatrixInverse = setMatrixInverse,
         getMatrixInverse = getMatrixInverse
    )
}

#' \code{cacheSolve} computes the inverse of the matrix returned by \code{makeCacheMatrix}.
#'
#' If the inverse has already been calculated and the matrix is unchanged,
#' then \code{cacheSolve} should retrieve the inverse from the cache.
#'
#' ASSUMPTION: the matrix supplied is always invertible.
#'
#' @param x  matrix
#'
#' @return cachedMatrix  the cached inverse of a matrix

cacheSolve <- function(x, ...) {
    cachedMatrix <- x$getMatrixInverse()

    # IF the inverse is already calculated, return it
    if (!is.null(cachedMatrix)) {
        message("Hold on - don't get your jimmies rustled, I'm getting your cached data...")
        return(cachedMatrix)
    }

    # ELSE calculate the inverse of the matrix
    data <- x$getMatrix()
    cachedMatrix <- solve(data, ...)

    # Cache the inversed matrix
    x$setMatrixInverse(cachedMatrix)

    # Return the cashed inverse of the matrix
    cachedMatrix
}