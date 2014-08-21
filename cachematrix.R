
# object makeCacheMatrix() with methods $set, $get, $setsolve, $getsolve

makeCacheMatrix <- function(x = matrix()) {  # x - input matrix
        m <- NULL			     # m - if m exsist will be set as NULL during every use of makeCacheMatrix()
        set <- function(y) {                 # set function - in fact not necessary, but you can use it 
        	x <<- y			     #   to change  matrix content like: x$set(newMatrix)   
                m <<- NULL
        }
		get <- function() x          # get function - gives you value of x (input matrix)
        setsolve <- function(solve)          # setsolve function - takes invert matrix computed by cacheSolve solve() function   
	   m <<- solve                       #   and assigns it as m and store in cache
        getsolve <- function() m             # gives you value of m stored in cache
        list(set = set, get = get,           # list of methods in makeCacheMatrix object
             setsolve = setsolve,
             getsolve = getsolve)
}


# function cacheSolve using methods from object makeCacheMatrix

cacheSolve <- function(x, ...) {             #x - matrix  created by makeCacheMatrix
        m <- x$getsolve()                    #m - invert matrix values stroed in makeCacheMatrix (not NULL only if invert matrix was created by cacheSolve before)
        if(!is.null(m)) {                    #if statment - if m exsist(if invert matrix alredy exist)
                message("getting cached data")# then send message
                return(m)                    # and show existing matrix m
        }                                    # if invert matrix do not exist (m is NULL)compute invert matrix
        data <- x$get()                      # data - take input matrix (x) values as data
        m <- solve(data, ...)                # m - create invert matrix (by solve()) and assign as m
        x$setsolve(m)                        # use setsolve from makeCacheMatrix to set m in cache, 
        m                                    #   next use of cacheSolve() will not compute but just take values m from memory
}

#to evaluate this functions try:

a<-matrix(c(2,2,2,3),ncol=2)  # a - invertable squared matrix
matrix<-makeCacheMatrix(a)
cacheSolve(matrix)

