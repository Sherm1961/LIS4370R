#Your Assignment:
#Find the value of inverse of a matrix, determinant of a matrix by using the following values:
#A=matrix(1:100, nrow=10)
#B=matrix(1:1000, nrow=10)
#Post your result and procedure you took on your blog.
#A good start will be:

#starting point
A <- matrix(1:100, nrow=10)  
B <- matrix(1:1000, nrow=10)

#Finding Determinant of A
det(A)

#R code for inverse Matrix: solve(A)
solve(A)

#But we get an error as the matrix is exactly singular which means that since our determinant 
#is 0, the inverse of Matrix A doesn't exist

#Finding Determinant of B
det(B)

#R code for inverse Matrix: solve(A)
solve(B)


