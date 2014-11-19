## The function makeCacheMatrix computes the inverse of the input matrix.
## The computed result is cached so that its not computed again.
# Please refer to the below section for example usage
## Example usage:
# > x <- matrix(rnorm(36), nrow = 6)          // Create a matrix x
# > cx <- makeCacheMatrix(x)                  // Create our special matrix
# > cx$get()                                  // Return the matrix
# > cacheSolve(cx)                            // Return the inverse
# > cacheSolve(cx)                            // Call the 2nd time, so return
#                                             // the cached inverse


# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	
	# matrix inverse 
	mat_inv <- NULL
		
	# getter and Setter functions
		
	set <- function(y){
		
		x <<-y
		mat_inv <<- NULL
			
	}

	get <- function()x
		
    # getter and setter function for inverse
	
	setmat_inv <- function (inverse) mat_inv <<- inverse
	getmat_inv <- function() mat_inv
		
	# list with functions
		list(set = set, get = get, setmat_inv = setmat_inv,getmat_inv = getmat_inv)		
		
		
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	mat_inv <- x$getmat_inv()
		
    # check if the inverse is computed and cached	
	if(!is.null(mat_inv)){
		message("Cached data !")
	    return(mat_inv)
	}
	
    # compute if not cached earlier
	data <-x$get()
	mat_inv <- solve(data,...)
		
    # cache inverse
    x$setmat_inv(mat_inv)
		
	mat_inv	
	
}
