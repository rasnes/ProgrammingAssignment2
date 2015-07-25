## cachematrix.R contains two functions that, combined, enables the inverse
## of a given invertible matrix to be cached. By doing so, the
## inverse of a matrix does not need to be calculated each time; if 
## it is previously calculated it can be obtained from memory/cache.

## The makeCacheMatrix function takes a matrix as the only parameter
## and returns a list with four functions. In the "subfunctions" set and
## setinverse the '<<-' assignment operators are used to make the assignments
## made in the subfunctions available to the parent (makeCacheMatric) 
## environment. Three of the four subfunctions are called by the cachSolve
## function, set is only used if is wanted to "re-set" the matrix already
## given as a parameter to the makeCacheMatrix function.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)

}


## The cacheSolve function returns the inverse of the matrix given as
## parameter to the makeCacheMatrix function. cacheSolve take
## a makeCacheMatrix item as the only required parameter.
## In short, the function checks whether the inverse
## of the matrix in the makeCacheMatrix item already has been calculated
## and cached, and if so it returns this cached value. If not found, the
## function calculates the inverse of the matrix using solve() and 
## returns that value.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message('Returning cached data')
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv
}
