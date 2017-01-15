## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    #creatings variable for this environment
    i <- NULL
    
    #creating function if change de original variable value is needed
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    #retrieve original matrix value
    get <- function() x
    
    #save the inverted matrix value
    setInvertMatrix <- function(invMatrix) i <<- invMatrix
    
    #Retrieve the inverted matrix value
    getInvertMatrix <- function() i
    
    #return a list so that all environment can be accessed outside this function and after it has been used
    list(set = set, get = get,
         setInvertMatrix = setInvertMatrix,
         getInvertMatrix = getInvertMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInvertMatrix()
    
    # Checks if already has an inverted matrix set
    if(!is.null(i)) {
        message("getting cached data")
        
        #Since it is set. Function returns this value and ends
        return(i)
    }
    
    #If no inverse set, then get the value, invert the matrix and set to the cache function
    data <- x$get()
    i <- solve(data)
    x$setInvertMatrix(i)
    i
}