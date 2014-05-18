## The following functions allow to cache the inverse of
## an invertable matrix which could be a potentially time
## consuming operation.
##
## use makeCacheMatrix function to create a matrix
## use cacheSolve function to compute the matrix inverse


## function makeCacheMatrix 
##
## Creates a special matrix object with functions to 
## set matirx, get matrix, set matrix inverse and get
## matrix inverse 
##

makeCacheMatrix <- function(m = matrix()) {  ## default is an empty matrix
        inv <- NULL;            ## clear the  inverse when a matrix object is created
        set <- function(y) {    ## set - funcion to modify the orighinal matrix
                m <<- y;        ## save matrix
                inv <<- NULL;   ## clear the inverse cache, since matrix has been
                                ## modifed
        }
        get <- function() m;    ## get() - function to get original matrix
        ## setinverse() - function to set the stored matrix's inverse
        setinverse <- function(inverse) inv <<- inverse; 
        ## getinverse() - function to get the stored matrix inverse
        getinverse <- function() inv;
        ## create and return a list of functions associated with the
        ## matrix object
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## function cacheSove
## 
## computes, caches and returns the inverse of a matrix created 
## by makeCacheMatrix
##
## if the inverse has already been computed, cacheSolve skips the 
## inverse computation process and returns the cached inverse.
##

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'm'
        inv <- m$getinverse(); ## get the inverse associated with matrix object m
        if(!is.null(inv)) {    ## if it already set, return the cached inverse
                message("getting cached data");
                return(inv);
        }
        ## At this point there is no cached value for inverse
        ## compute it and save it
        data <- m$get();        ## get original matrix
        inv <- solve(data, ...);## compute inverse
        m$setinverse(inv);      ## save computed inverse for caching
        inv;                    ## return inverse
}
