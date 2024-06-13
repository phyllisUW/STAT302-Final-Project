x = c(1, 2, 3)
y = c(5, 5, 2)

nodes3 = matrix(c(x,y), nrow = 3)

distBNodes = as.matrix(dist(nodes3))

# If R = 3.5 then 
findTranMat(distBNodes, 3.5)
#returns
tranR3.5 = matrix(c(1/2, 1/2, 0, 1/3, 1/3, 1/3, 0, 1/2, 1/2), 
                  byrow = TRUE, nrow = 3)


# and 
getEigen2(tranR3.5)
#returns 0.5 

# If R = 2 then
findTranMat(distBNodes, 2)
#returns
tranR2 = matrix(c(1/2, 1/2, 0, 1/2, 1/2, 0, 0, 0, 1), 
                byrow = TRUE, nrow = 3)

#And 
getEigen2(tranR2)
#returns 1

#Additionally, 
findRange(distBNodes)
#returns approximately
# 3.162278 3.162278

#AND
findRc(nodes3, tol = 0.001)
# Returns 
# 3.162

################
#Another example
nodes5 = matrix(c(1,3,2,1,3,3,3,2,0,0), nrow =5)
distBNodes5 = as.matrix(dist(nodes5))

findTranMat(distBNodes5, 2)
tranR2 = matrix(c(1/3, 1/3, 1/3, 0, 0, 
                  1/3, 1/3, 1/3, 0, 0,
                  1/3, 1/3, 1/3, 0, 0,
                  0, 0, 0, 1/2, 1/2,
                  0, 0, 0, 1/2, 1/2), 
                byrow = TRUE, nrow = 5)

getEigen2(tranR2)
# returns 1

#Additionally, 
findRange(distBNodes5)
#returns approximately
# 2   2.236068

#Also
tranR2.23 = findTranMat(distBNodes5, 2.23)
getEigen2(tranR2.23)
# returns 1

#AND
findRc(nodes5, tol = 0.0001)
# Returns approximately
#2.23608

# Note that with the tolerances we want to only check
# values that are 0.0001 apart in our Rc range
