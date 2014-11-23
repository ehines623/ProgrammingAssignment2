## Put comments here that give an overall description of what your
## functions do

## Takes a matrix as input, returns a list with the inverse set to NULL
makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL #inv will be the matrix inverse and is reset every time that
                # makeCacheMatrix is called, can be set later


    # define all the other useful functions to return the orignial matrix,
    # return the inverse
    # set the inverse

    get <- function() {x} #this function returns the original matrix

    set <- function(newMatrix){
        x <<- newMatrix
        inv <<- NULL
    }

    setInverse <- function(inverse) { inv <<- inverse}

    getInverse <- function() {inv}

    list(get = get,
         set = set,
         setInverse = setInverse,
         getInverse = getInverse
         )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    inv<-x$getInverse()
    if(!is.null(inv)) {
        message('Inverse already calculated and stored, getting cached data')
        return(inv)
    }

    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
