## This function will idenify and return the inverse of any given matrix 
## assuming the matrix is invertible... so please... only use invertible matrices. 

## This function gets the matrix already to go in order for you to invert it. WOO. 

makeCacheMatrix <- function(x = matrix()) {
                    inv_M <- NULL 
                    org_store <- function(z) { ##setting the cached original matrix
                      x <<- z 
                      inv_M<<- NULL
                    }
                    get_org <- function() { ## pulling the original matrix 
                      x
                    }
                    org_inv_store <- function(new_MATT) { ## setting the inverse matrix
                      inv_M <<- new_MATT
                    }
                    get_inv <- function() { ## pulling the inverse matrix. 
                      inv_M
                    }
                    list(org_store = org_store, get_org = get_org,
                         org_inv_store = org_inv_store,
                         get_inv = get_inv)

}


## cacheSolve is used to return the inverse of a matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cache_money <- x$get_inv() ## setting cache_money which is the inversed matrix to the INV_M from above. 
  if(!is.null(cache_money)) { ##if the cache_money is not null then we will return the INV_M from the previous function
    message("Cached Data is Full") 
    return(cache_money)
  }
  data <- x$get_org() ##Other we are calculating the inverse of the original x matrix above. 
  cache_money <- solve(data, ...) ## inversion. 
  x$org_inv_store (cache_money) ## we are setting the cache_money to the INV_M or inverse matrix
  cache_money ## return your inverted cache_money matrix... BALL-A. 
}
