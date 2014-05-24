##  These functions cache the inverse of a matrix.  Function "makeCacheMatrix"
##  creates a special"matrix" object that can cache its inverse.  Function 
##  "cacheSolve" computes the inverse of the special "matrix" returned by 
##  makeCacheMatrix above.  If the inverse has already been calculated(and the 
##  matrix has not changed), then the cacheSolve should retrieve the inverse 
##  from the cache.


## "makeCacheMatrix" does the following works:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

    i <- NULL

    set <- function(y) {
        x <<- y
        i <<- NULL
    }
   
    get <- function() x
    setsolve <- function(solve) i <<- solve
    getsolve <- function() i

    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## "cacheSolve" computes the inverse of a matrix. If the inverse already
## exists, it returns the cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    i <- x$getsolve()

    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }

    data <- x$get()
    i <- solve(data, ...)
    x$setsolve(i)

    i
}
