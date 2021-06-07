
## The below two functions serve the below purposes:
## create a matrix, calculate it's inverse and cache its result

## makeCacheMatrix function takes a matrix as parameter, returns this matrix when called for
## and also caches the inverse of the last computed matrix

makeCacheMatrix <- function(x = matrix()) {
  
  #Check if the inverse of a matrix has been calculated and is identical to the new matrix being
  #passed to it
  if(exists("new_matrix")){
    if(identical(new_matrix,x)){
      cached_inverse <- new_inverse
    } else {
      cached_inverse <- NULL
    }
  }
  
  #Another check to see if the inverse cache has to be cleared
  if(!exists("new_matrix")){
    cached_inverse <- NULL
  }
  
  #Function to retrieve the newly defined matrix
  get_matrix <- function(){
    x
  }
  
  #Function to cache the calculated inverse
  cache_inverse <- function(result) {
    cached_inverse <<- result
  }
  
  #Function to retrieve the inverse
  get_inverse <- function(){
    cached_inverse
  }
  
  #List of functions
  list(get_matrix = get_matrix,
       cache_inverse = cache_inverse,
       get_inverse = get_inverse)

}


## cacheSolve function takes the makeCacheMatrix object as the parameter and calculates the
## inverse. It retrieves the cached value, if available. If not, calculates the inverse

cacheSolve <- function(x, ...) {
 
  #Retrieve the cached inverse, if any.
  old_inverse <- x$get_inverse()
  
  #If a cached inverse has been retrieved, this will be returned as result. Execution stops here
  #if the cached inverse has been retrieved
  if (!is.null(old_inverse)){
    print ("Returning cached result")
    return(old_inverse)
  }
  
  #Retrieve the matrix. This section of code is executed only if there is no cached
  #inverse retrieved
  new_matrix <<- x$get_matrix()
  
  #Calculated the inverse of the matrix
  new_inverse <<- solve(new_matrix)
  
  #Cache the newly calculated inverse
  x$cache_inverse(new_inverse)
  
  #Print out the calculated inverse
  new_inverse
  
}
