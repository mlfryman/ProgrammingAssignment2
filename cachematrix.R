#' The following two functions cache the inverse of a matrix to speed up calulation time.

#' \code{makeCacheMatrix} creates a matrix object that can cache its inverse.
#'
#' @param x matrix
#'
#' @return a special "matrix", which is really a list containing functions to cache the inverse of a matrix
#'
#' @examples
#' x <- matrix(rnorm(16), nrow = 4)      # init matrix 'x'
#' cm <- makeCacheMatrix(x)              # init cached matrix 'cm'
#' cm$getMatrix()                        # return matrix 'cm'
#' cacheSolve(cm)                        # return the inverse of matrix 'cm'
#' cacheSolve(cm)                        # inverse of 'cm' is already cached, so return it

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
#' then \code{cacheSolve} will retrieve the inversed matrix from the cache.
#'
#' ASSUMPTION: the matrix supplied is always invertible.
#'
#' @param x  matrix
#'
#' @return cachedMatrix  the cached inverse of a matrix
#'
#' @examples
#' cacheSolve(cm)           # calculates, caches, & returns the inverse of matrix 'cm'
#' cacheSolve(cm)           # inverse of 'cm' is already cached, so return it

cacheSolve <- function(x, ...) {
    cachedMatrix <- x$getMatrixInverse()

    # IF the inverse is already calculated, return it from the cache
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

#' Worked Example
#'
#' > x = rbind(c(2, -1/8), c(-1/8, 2))
#'
#' > cm <- makeCacheMatrix(x)
#'
#' > cm$getMatrix()
#' [,1]   [,2]
#' [1,]  2.000 -0.125
#' [2,] -0.125  2.000
#'
#' The first time you call cacheSolve(), the inverse of the matrix
#' is calculated, cached, & returned.
#' > cacheSolve(cm)
#' [,1]       [,2]
#' [1,] 0.50196078 0.03137255
#' [2,] 0.03137255 0.50196078
#'
#' The second time you call cacheSolve(), the cached matrix is returned.
#' > cacheSolve(cm)
#' Hold on - don't get your jimmies rustled, I'm getting your cached data...
#' [,1]       [,2]
#' [1,] 0.50196078 0.03137255
#' [2,] 0.03137255 0.50196078
