## Wenyin San
## R Programming Assigment 2
## The functions are used to cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	##set the value of the matrix
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    ##get the value of matrix
    get <- function() x
    ##set the value of the inverse of the matrix
    setinverse <- function(inverse) inv <<- inverse
    ##get the value of the inverse of the matrix
    getinverse <- function() inv

    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix()

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of the 'x'
    ## First checks to see if the inverse has already been calculated.
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    ##solve(X) returns the inverse
    inv <- solve(data)
    x$setinv(inv)
    inv
}
