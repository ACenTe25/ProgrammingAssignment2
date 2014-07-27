#This function creates a special "Matrix" object containing functions for caching its inverse

makeCacheMatrix <- function(x = numeric()){                #FIRST FUNCTION
        inv <- NULL                                        #Set the inverse to NULL
        set <- function(x){                                #Define the set function
                x <<- x                                    #Assign matrix to 'x'
                inv <<- NULL                               #Set global 'inv' to NULL
        }
        get <- function() x                                #Get the matrix stored in 'x'
        setinverse <- function(inverse) inv <<- inverse    #Set 'inv' to the inverse value
        getinverse <- function() inv                       #Get the inverse from 'inv'
        list(set = set, get = get,                         #Build the 'Matrix object' list
             setinverse = setinverse,
             getinverse = getinverse)
        
}

#This function takes a special "Matrix" object and returns its inverse

cacheSolve <- function(x, ...){                           #SECOND FUNCTION
        inv <- x$getinverse()                             #Apply 'get' from 'x'
        if(!is.null(inv)){                                #If the inverse exists...
                message("Getting cached inverse...")        #Display a message
                return(inv)                                 #Return the cached inverse
        }                                                 #Otherwise...
        data <- x$get()                                   #Apply 'get' from 'x'
        inv <- solve(data,...)                            #Compute the inverse
        x$setinverse(inv)                                 #Apply 'set' from 'x' to cache inverse
        inv                                               #Return the inverse
}