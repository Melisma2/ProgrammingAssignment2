## These functions compute and cache inverses of matrices.


## Creates a cache matrix object shell
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL                           #create variable for inverse
        set <- function(y) {                #set function
                x <<- y
                i <<- NULL
        }
        get <- function () x                #get function
        setinverse <- function(inverse){    #set inverse
                i <<- inverse}
        getinverse <- function() i          #get inverse
        list(set = set, get = get,          #return object list
             setinverse = setinverse,
             getinverse = getinverse)
}


## Finds inverse of matrix saved in makeCacheMatrix object.
## If the inverse has previously been calculated, it will be retrieved.
## If it has not been calculated, the inverse will be calculated and stored.


## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        i<- x$getinverse()                     #check if inverse is already cached
        if(!is.null(i)) {                      #if cached, simply return inverse
                message("getting cached data")
                return(i)
        }
        data <- x$get()                        #if not cached, get matrix and solve
        i <- solve(data)
        x$setinverse(i)                        #cache inverse 
        i                                      #return inverse        
}
