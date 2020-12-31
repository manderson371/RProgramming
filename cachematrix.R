## This script contains 2 functions, makeCacheMatrix and cacheSolve
##
## makeCacheMatrix takes a matrix, caches it, and creates and returns
##  a list comprised of 4 functions that can then be used to cache and
##  retrieve the inverse of the matrix.
##
## cacheSolve uses the list created by makeCacheMatrix to return the
##  inverse of the matrix passed to makeCacheMatrix. If the inverse
##  has not been calculated, the function will calculate it and cache
##  it. If the inverse has been calculated, the cached inverse is
##  returned.
##
## NOTE that if the matrix is changed, either by calling makeCacheMatrix
##  with a new matrix, or by using the set function in makeCacheMatrix,
##  the cached inverse matrix is cleared so that it will be recalculated
##  for the new matrix values

## -------------------------------------------------------------------
## Function to create a list of functions used to cache and return
## the inverse of a matrix
## -------------------------------------------------------------------
makeCacheMatrix <- function(m = matrix()) {
  ## Parameters:
  ##  m       :numeric matrix that the inverse will be calculated for
  ##
  ## Returns a list of 4 functions:
  ##  set     :caches the passed matrix
  ##  setInv  :caches the passed inverse of the matrix
  ##  get     :returns the cached matrix
  ##  getInv  :returns the cached inverse of the matrix
  ##
  
  # initialize the inverse matrix to NULL
  m.inv <- NULL
  
  # Function to save the passed matrix; clears the cached matrix
  # inverse because it has not yet been calculated on the new matrix
  set <- function(mat) {
    # check if the passed matrix is different from the cached one
    if(!identical(mat,m)) {
      m <<- mat         #cache the matrix
      m.inv <<- NULL    #clear the cached inverse
    }
  }
  
  # Function to save the passed inverse of the matrix
  setInvrse <- function(invrse) m.inv <<- invrse
  
  # Function to return the cached matrix
  get <- function() m
  
  # Function to return the cached inverse
  getInvrse <- function() m.inv
  
  ## return a list containing the 4 functions
  list( set = set, setInv = setInvrse,
        get = get, getInv = getInvrse )
  
}

## -------------------------------------------------------------------
## Function to return the inverse of the cached matrix (the matrix 
## previously passed to makeCacheMatrix). This will be either 
## a cached value if the inverse has already been calculated once,
## or a calculated value for a new matrix, which is also cached for
## later use.
## -------------------------------------------------------------------
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  ## Parameters:
  ##  x       :list of functions returned from the 
  ##            makeCacheMatrix function
  ##
  ## Returns the inverse the matrix:
  ##   If the inverse previously calculated, return that
  ##   If not, calculate the inverse, cache for later use, and
  ##      return the calculated matrix
  ##
  
  # get the cached matrix inverse. If not null, use it
  m.inv <- x$getInv()
  if(!is.null(m.inv)) {
    message("getting cached inverse")
  }
  else {
    ## calculate the inverse for the matrix and cache it
    m.sv <- x$get()             # get the cached matrix
    m.inv <- solve(m.sv, ...)   # calculate the inverse
    x$setInv(m.inv)             # cache the inverse
  }
  
  # return the inverse matrix
  m.inv
}
