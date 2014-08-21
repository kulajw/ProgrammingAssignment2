
# object makeCacheMatrix() with methods $set, $get, $setsolve, $getsolve

makeCacheMatrix <- function(x = matrix()) {  # x - input matrix
        m <- NULL			     # m - if cacheSolve was ran earlier mlue v alredy exist -  will be celear (set as NULL)during every use of makeCacheMatrix()
        set <- function(y) {                 # set function - in fact not necesary, but you can use it to change  matrix content like: x$set(newMatrix) 
                x <<- y
                m <<- NULL
        }
		get <- function() x          # get function - gives you value of x (input matrix)
        setsolve <- function(solve)          # setsolve function - takes computed by cacheSolve solve() function  invert matrix  and assigns as m not only in current function but in obect enviroment stored in cache
	   m <<- solve                        
        getsolve <- function() m             # gives you value of m
        list(set = set, get = get,           # list of methods in makeCacheMatrix object
             setsolve = setsolve,
             getsolve = getsolve)
}


# function cacheSolve using methods from object makeCacheMatrix

cacheSolve <- function(x, ...) {             #x - matrix earlier created by makeCacheMatrix
        m <- x$getsolve()                    #m - invert matrix values stroed in makeCacheMatrix (only if invert matrix was created by cacheSolve before)
        if(!is.null(m)) {                    #if statment - if m exsist(if invert matrix alredy exist)
                message("getting cached data")# then send message
                return(m)                    # and show existing matrix m
        }                                    # if invert matrix do not exist (m is NULL)compute invert matrix using solve()
        data <- x$get()                      # data - take input matrix (x) values as data
        m <- solve(data, ...)                # m - create invert matrix (by solve()) and assign as m
        x$setsolve(m)                        # use setsolve from makeCacheMatrix to set m in cache, next use of cacheSolve() will not compute but just take values from memory
        m
}

#to evaluate this functions try:

a<-matrix(c(2,2,2,3),ncol=2)  # a - invertable squared matrix
matrix<-makeCacheMatrix(a)
cacheSolve(matrix)

