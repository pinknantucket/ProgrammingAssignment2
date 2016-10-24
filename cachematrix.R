# The following two functions cache the inverse of a matrix.
# The first, makeCacheMatrix() creates a matrix object that can cache its inverse.
# It contains four functions: set(), get(), setinverse() and getinverse(), plus two data objects x and m. 

# Step 1: initialise makeCacheMatrix() and the x data object argument (a matrix)
makeCacheMatrix <- function(x = matrix()) { 
        
        # Step 2: initialise a vector (m) within the makeCacheMatrix() environment, to be used later.
        m <- NULL  
        
        # Step 3: initialise the "set" function within the makeCacheMatrix() environment. 
        # This sets the matrix itself but not the inverse. 
        # Set() changes the vector stores in the main function.
        # A future value (y) is assigned to the x object in the parent environment.
        # An initial null value is assigned to the m object in the parent environment.
        # So, if a valid figure is already cached in m, whenever x is reset the cached 
        # value of m is cleared, forcing a recalculation.
        
        set <- function(y) {
                x <<- y  
                m <<- NULL      
        }
        
        # Step 4: initialise "get()" function. 
        # This returns the vector x stored in the main function (the matrix, but not its inverse).
        # Because x is not defined within get(), its value is retrieved from the parent environment.
        
        get <- function() x 
        
        # Step 5: initialise "setinverse()" function. 
        # setinverse() stores the value of the input in the variable m (ie. the inverse). 
        # Because m is defined in the parent environment and we need to access it after
        # setinverse() completes, <<- is used to assign input argument to value of m in parent environment.
        
        setinverse <- function(inverse) m <<- inverse 
        
        # Step 6: initialise "getinverse()" function. This gets the inverse.
        # Again, lexical scoping is used to find the correct value of m.
        
        getinverse <- function() m 
        
        #Step 7: create a list with each function as an element of the list.
        # Return the list to the parent environment. 
        # Names of previously defined functions are assigned. 
        # This allows us to access them in the future by $name.
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# The cacheSolve() function computes the inverse of the matrix returned by makeCacheMatrix().

# Step 8: initialise cacheSolve() function using an argument that is returned by makeCacheMatrix().
# Return a matrix that is the inverse of x.

cacheSolve <- function(x, ...) { 
        m <- x$getinverse()
        
        # If the inverse has already been calculated:
        # 1. retrieve m from cache and skip calculation.
        # 2. display message that indicates pre-computed result is being returned.
        
        if(!is.null(m)) {
                message("please wait...retrieving cached data")
                return(m)
        }
        
        # If the inverse has not already been calculated:
        # 1. get the matrix (x)
        # 2. calculate the inverse with solve() and assign to m.
        # 3. cache the inverse in the object using setinverse().
        # 4. return new result (m).
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}