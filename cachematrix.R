## <makeCacheMatrix> takes an invertible matrix and creates an object containing a list of 
## two setter and getter functions, the matrix passed as an argument, <x>, and the variable <inverse>
## which is used to cache the inverse of the matrix.
## <cacheSolve> takes an object of <makeCacheMatrix>, <x>, and either calculates and returns the inverse of the matrix
##  in <x> if it has not been calculated or if the matrix has been changed, or <cacheSolve> returns the stored
## inverse of the matrix if it has been calculated previously.


## <makeCacheMatrix> takes a matrix, <x>, and returns a named list, <x> and <inverse>.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL                           ## <inverse> is initialized to <NULL>
    
    set <- function (new_data) {              ## <set> updates the matrix passed to <makeCacheMatrix>
        x <<- new_data                        ## and resets the cached inverse to <NULL>
        inverse <<- NULL
    }
    get <- function() x                       ## <get> returns the value of <x>
    
    setinverse <- function(solved_matrix) {   ## <setinverse> passes the inverse of the matrix
        inverse <<- solved_matrix             ## stored in <x> from its call in <cacheSolve>
    }                                         ## to <inverse>, which is in the environment of
                                              ## <makeCacheMatrix>
    
    getinverse <- function() inverse          ## returns the value of <inverse>   
    
    list(set = set, get = get,                ## returns a named list of the 4 above functions
         setinverse = setinverse,
         getinverse = getinverse)
}

## <cacheSolve> takes an object of <makeCacheMatrix>, determines if the inverse of the matrix
##has already been calculated, and either retrieves the cached inverse and returns it 
## or calculates, caches, and returns it.

cacheSolve <- function(x, ...) {

    
    inverse <- x$getinverse()                 ## <inverse> takes the value returned by the 
                                              ## <getinverse> function in the x object.
    
     if (!is.null(inverse)) {                 ## The cached inverse of the matrix is returned 
        message("Getting cached data")        ## if it has previously been calculated. 
        return(inverse)                       ## The call to return <inverse> exits <cacheSolve>
     }
                                              ## If the inverse has not previously been calculated:
    matrix_data <- x$get()                    ## <matrix_data> is passed the matrix held in the x object.
    inverse <- solve(matrix_data, ...)        ## <inverse> is assigned the inverse of the matrix through <solve>.
    x$setinverse(inverse)                     ## <inverse> in the x object is passed the value of <inverse> in
                                              ##<cache_solve>, caching the value.
    inverse                                   ##<inverse> is returned.
}
