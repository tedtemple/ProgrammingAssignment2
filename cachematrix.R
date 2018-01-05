## This function creates a special object of type makeCacheMatrix
##and uses that object to compute and store the inverse of the matrix

##makeCacheMatrix: This function creates a special
##"matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        ##while currently NULL, i will hold the computed inverse
        i <- NULL 
        
        ##Define a new function, set, which takes one variable, y
        ##set will assign the value of y to x in the parent frame
        ##set will assign NULL to i in the parent frame,
        ##to clear any value of i that had been cached by a prior execution
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ##Define a new function, get
        ##get will return x from the parent frame
        get <- function() x
        
        ##Define a new function, setinverse
        ##setinverse takes one variable, inverse,
        ##which it will pass to i in the parent frame
        setinverse <- function(inverse) i <<- inverse
        
        ##Define a new function, getinverse
        ##getinverse will return i
        getinverse <- function() i
        
        ##Return a list of the output from each function,
        ##where each variable has a default value
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

##cacheSolve: This function computes the inverse of the special
##"matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated
##(and the matrix has not changed), then cacheSolve should
##retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ##cachemean takes at least one variable, x, and whatever other arguments are necessary
        
        i <- x$getinverse()
        ##the variable i gets the value of the list x's getinverse
        
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        message("computing new inverse")
        i
}

##define two matrices
my.matrix = matrix(c(2, 4, 3, 1), 
           nrow=2, 
           ncol=2)
my.matrix.2 = matrix(c(2, 4, 3, 1, 6, 4, 5, 3, 1), 
                   nrow=3, 
                   ncol=3)
##use makeCacheMatrix to create the required input for cacheSolve
my.matrix.object <- makeCacheMatrix(my.matrix)
my.matrix.object.2 <- makeCacheMatrix(my.matrix.2)

##run cacheSolve
cacheSolve(my.matrix.object)
cacheSolve(my.matrix.object.2)

