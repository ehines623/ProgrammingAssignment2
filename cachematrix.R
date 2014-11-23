## Two functions that are used to cache the inverse of a matrix
## WARNING: Functions assume matrix is invertible


## Takes a matrix as input, returns a list with the inverse initially to NULL
## Functions get, set, getInverse, setInverse defined
makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL #inv will be the matrix inverse and is reset every time that
                # makeCacheMatrix is called, can be set later


    # define all the other useful functions to return the orignial matrix,
    # return the inverse
    # set the inverse

    get <- function() {x} #this function returns the original matrix

    set <- function(newMatrix){  #the function sets the matrix to a new value
        x <<- newMatrix          # and resets the cached inverse to NULL
        inv <<- NULL
    }

    setInverse <- function(inverse) { inv <<- inverse}  #set the inverse,
                                                        #probably should not be
                                                        #calling this outside
                                                        # the cacheSolve func.

    getInverse <- function() {inv}  #returns the inverse

    #return the list
    list(get = get,
         set = set,
         setInverse = setInverse,
         getInverse = getInverse
         )
}


## takes the list produced by makeCacheMatrix and returns the inverse
## if the inverse has already been cached, it simply returns the value
## if it is NULL, it calculates the inverse, caches it for future use
## and returns the inverse
cacheSolve <- function(x, ...) {

    inv<-x$getInverse() #get the inverse saved in makeCacheMatrix list

    # if inv is already saved in the cache, just return it
    if(!is.null(inv)) {
        message('Inverse already calculated and stored, getting cached data')
        return(inv)
    }

    # Else if we get here inverse is null and we must calculate it
    message('Calculating inverse and caching')
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv #return the inverse
}
