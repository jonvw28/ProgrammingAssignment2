## Below are two functions that enable the inverse of a matrix to be cached once
## calculated, avoiding recalculation.
##
## The first function, makeCacheMatrix, initially takes a matrix and returns a 
## list of 4 functions in the global environment. Two can be used to get and set
## the matrix in question. The other 2 can be used to get and set its inverse.
##
## The second function, cacheSolve, first tries to get a cached version of the 
## matrix inverse. If so it prints a message to say it is doing so and prints
## the cached value. If there isn't a value then it calculates this value by
## retrieving the matrix from makeCacheMatrix and prints this inverse.


## On first call this function stores the matrix given and sets a null value
## for its inverse. It then gives 4 functions that can be used to:
## retrieve the matrix
## set a new matrix (clearing any cached inverse)
## set and store an inverse matrix
## retrieve any cached inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function uses an object created by the above function to output an
## inverse for the stored input matrix. First it looks to see if an inverse is 
## cached in the above, in which case it prints a message and outputs this 
## inverse. If there isn't one cached, it then retrieves the input matrix cached 
## above, computes its inverse and outputs it. This can then be cached using the
## setinverse function above, avoiding the need to compute it again.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        
        matrix.data <- x$get()
        i <- solve(matrix.data,...)
        i
}
