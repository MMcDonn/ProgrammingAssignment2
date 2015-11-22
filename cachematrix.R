
## This function initializes a specific matrix via user input
makeCacheMatrix <- function(x = matrix()) {
        #Resets the inverse object 'm' to zero.
        inv <- NULL
        #Will replace previous matrix with matrix in args for set() when called
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        #Assigns the value of the (later) calculated inverse
        setinv <- function(inverse) 
                inv <<- inverse
        #Returns the inverse
        getinv <- function() inv
        #Creates a list with the previously defined functions for easier access
        #via subsetting
        list(set = setinv, get = get, setinv = setinv, getinv = getinv)
        

}


## This function will test the value of an inverse for a matrix. If not yet
#calculated, it will calculate it. If already calculated, will return cached inverse.

cacheSolve <- function(x, ...) {
        ## Testing for exisitng inverse of object
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
                
        }
        #Calling the get function from makeCacheMatrix function for the input matrix
        data <- x$get()
        #Calculating inverse of matrix
        inv <- solve(data, ...)
        #Assigning the value of inverse to the inv variable
        x$setinv(inv)
        inv
        
}
