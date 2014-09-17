# Creates new "class" of data which is a matrix

makeCacheMatrix <- function(A = matrix()) {
    Ainv <- NULL
    
    # Function to instantiate the matrix values
    set <- function(X) {
        A <<- X
        Ainv <<- NULL
    }
    
    # "Methods" for getting and setting attributes of "makeCacheMatrix"
    get <- function() A
    setInv <- function(solve) Ainv <<- solve
    getInv <- function() Ainv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

# Used for Setting the inv if it hasn't been calculated already.
cacheSolve <- function(A, ...) {
    Ainv <- A$getInv()
    if(!is.null(Ainv)) {
        message("getting cached data")
        return(Ainv)
    }
    data <- A$get()
    Ainv <- solve(data, ...)
    A$setInv(Ainv)
    A
}