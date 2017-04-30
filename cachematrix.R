## The functions below create an object matrix which can store (cache) its inverse and creates/recover the inverse matrix from cache

## This function creates an object matrix

makeCacheMatrix <- function(a = matrix()) { 
  invM <- NULL                              
  set <- function(b) {											
    a <<- b
	  invM <<- NULL
	}
	get <- function() x

	setInv <- function(inverse) invM <<- inverse
	getInv <- function() invM
	list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This one is the function which calculates the inverse of the object matrix returned by function above. 
## If it was already calculated, the function retrives it from cache

cacheSolve <- function(a, ...) {
        ## Return a matrix that is the inverse of 'a'
	invM <- a$getInv()
	if(!is.null(invM)) {
  	message("Retrieving stored data")
		return (invM)
	}

	dataM <- a$get()
	invM <- solve(dataM, ...)
	a$setInv(invM)
	invM
}
