#1. Consider A=matrix(c(2,0,1,3), ncol=2) and B=matrix(c(5,2,4,-1), ncol=2).
A=matrix(c(2,0,1,3), ncol=2)
B=matrix(c(5,2,4,-1), ncol=2)

#a) Find A + B
A + B
#b) Find A - B
A - B

#2. Using the diag() function to build a matrix of size 4 with the following values in the diagonal 4,1,2,3.
diag(c(4,1,2,3))

#3. Generate the following matrix:
  
  ## [,1] [,2] [,3] [,4] [,5]
  ## [1,] 3 1 1 1 1
  ## [2,] 2 3 0 0 0
  ## [3,] 2 0 3 0 0
  ## [4,] 2 0 0 3 0
  ## [5,] 2 0 0 0 3
  #Hint: You can use the command diag() to build it.

matrix(c(3,2,2,2,2,1,3,0,0,0,1,0,3,0,0,1,0,0,3,0,1,0,0,0,3), ncol = 5)


#expiriment
v <- c(5,2,4,-1)
B=matrix(c(v), ncol=2)
B

#or

x = c(3,3,3,3,3)

d <- c(0,2,2,2,2)
e <- c(1,0,0,0,0)
f <- c(1,0,0,0,0)
g <- c(1,0,0,0,0)
h <- c(1,0,0,0,0)

j <- c(d,e,f,g,h)

y<-diag(x, nrow = 5, ncol = 5) + matrix(c(j), ncol =5)
y


