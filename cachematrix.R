## This uses two main functions to create and cache the inverse of a matrix. It retrieves the cache, if the inverse has been created.
## Each part of the code is described in detail below, function-by-function, line-by-line


## using this function like SOMEVARIABLE <- makeCacheMatrix(SOMEMATRIX) will store the matrix in a form than can be used by cacheSolve()
## this also creates the four functions set, get, setinverse, and  getinverse; these will be used by cacheSolve to calculate and retrieve matrix inverse values

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                                              #sets inv as null, so that it can be used as a variable later, and so that the !is.null check in cacheSolve works properly
        set <- function(y) {
                x <<- y
                inv <<- NULL                                     #when set(y) is called, x is set equal to y, and inv is reset to its null value
        }
        get <- function() x                                      #when get() is called, x is returned
        setinverse <- function(inverse) inv <<- inverse          #when setinverse(inverse) is called, inv is set equal to inverse
        getinverse <- function() inv                             #when getinverse is called, inv is returned
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)                            #creates a list containing all previously specicifed functions

}


## This checks to see if an inverse has been created (the if statement), and, if so, returns it.  
## If the inverse has not been calculated yet, it calculates it, and stores it so that it's existence can be cahcked for later.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()                                    #uses getinverse() to retrive value of x
        
        if(!is.null(inv)) {
                return(inv)                                      #if inv exists (is not null), then the cached value of inv is returned
        }

        data <- x$get()                                          #uses get() to retrieve x, and store as the local variable, 'data'
        inv <- solve(data, ...)                                  #uses solve() to calculate the inverse of 'data'
        x$setinverse(inv)                                        #sets the inverse of x, so that it's existence can be checked later
        inv                                                      #returns inv, which is the inverse of x. inv is only returned here when calcualting it for the first time. Otherwise inv is returned from the cached data, in the code a few lines above this.

}
