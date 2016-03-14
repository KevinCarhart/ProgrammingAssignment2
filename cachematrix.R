## makeCacheMatrix: instantiates a special vector which holds four functions
## I used the following convention for the four functions: 
## set_matrix_original
## get_matrix_original
## set_matrix_inverse
## get_matrix_inverse
## I used a short name for the 'private' values in the cache: mo, mi
## In a worse/more complex situation, this could help you tell apart what the user sees and how makeCacheMatrix manages the cache

makeCacheMatrix <- function(x = matrix()) {
  # naming conventions:
  # the four functions are like [get|set]_matrix_[original|inverse]
  # the convention for 'private' values in the cache are given an abbreviated name: mo, mi
  
  mi <- NULL
  set_matrix_original <- function(y) {
    mo <<- y
    mi <<- NULL
  }
  get_matrix_original <- function() mo
  set_matrix_inverse <- function(newly_solved_inverse) mi <<- newly_solved_inverse
  get_matrix_inverse <- function() mi
  list(set_matrix_original = set_matrix_original, get_matrix_original = get_matrix_original,
       set_matrix_inverse = set_matrix_inverse, get_matrix_inverse = get_matrix_inverse)
  
}

## cacheSolve: when you pass in the object returned by makeCacheMatrix, cacheSolve will return the inverse of the matrix,
## either from the cache if possible, or by doing an original solve() if not possible.  If there is not already a cached
## value. This becomes the cached copy on future calls.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x', either cached or original
  matrix_inverse <- x$get_matrix_inverse()
  if(!is.null(matrix_inverse)) {
    message("getting cached data")
    return(matrix_inverse)
  }
  matrix_original <- x$get_matrix_original()
  matrix_inverse = solve(matrix_original) # this is the expensive part - we already checked the cache
  x$set_matrix_inverse(matrix_inverse)
  matrix_inverse
}
