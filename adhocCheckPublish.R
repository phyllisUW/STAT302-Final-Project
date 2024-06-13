#Set Up
  nodes2state = matrix( c(
    46.25722095370, 45.18229861278,
    22.49341397546,  7.66819571145,
    67.68764129374, 23.56366263703,
    37.55860633682, 84.12622071337,
    12.53100726753, 22.94614179991,
    4.06135825906, 81.71979754698,
    12.47756232042, 17.52703324892,
    66.06890531257, 33.23447662406,
    23.36478442885, 23.19627322722,
    58.50740030874, 35.34655489493), 
    byrow = TRUE, nrow = 10)
  dist2state = as.matrix(dist(nodes2state))

  nodesSimple = matrix(c(1,1,1,3,2,1,3,2), 
                     ncol = 2, byrow = TRUE)
  distSimple = as.matrix(dist(nodesSimple))

  intervalTol = c(39.90307, 39.90375)
  rangeRc = c( 33.58357487, 55.81648195)
  pt2 = matrix(c(1,1,1,2), ncol = 2, byrow = TRUE)
  dist1 = as.matrix(dist(pt2))

  
  tranMatLower2 = matrix( 
    c(0.2, 0, 0.2, 0, 0, 0, 0, 0.2, 0.2, 0.2,
      0, 0.2, 0, 0, 0.2, 0, 0.2, 0, 0.2, 0.2,
      0.25, 0, 0.25, 0, 0, 0, 0, 0.25, 0, 0.25,
      0, 0, 0, 0.5, 0, 0.5, 0, 0, 0, 0,
      0, 0.25, 0, 0, 0.25, 0, 0.25, 0, 0.25, 0,
      0, 0, 0, 0.5, 0, 0.5, 0, 0, 0, 0,
      0, 0.25, 0, 0, 0.25, 0, 0.25, 0, 0.25, 0,
      0.25, 0, 0.25, 0, 0, 0, 0, 0.25, 0, 0.25,
      1/6, 1/6, 0, 0, 1/6, 0, 1/6, 0, 1/6, 1/6,
      0.2, 0, 0.2, 0, 0, 0, 0, 0.2, 0.2, 0.2),
    nrow = 10, byrow = TRUE)
  
  tranMatUpper2 = matrix( 
    c(0.2, 0, 0.2, 0, 0, 0, 0, 0.2, 0.2, 0.2,
      0, 0.2, 0, 0, 0.2, 0, 0.2, 0, 0.2, 0.2,
      0.25, 0, 0.25, 0, 0, 0, 0, 0.25, 0, 0.25,
      1/3, 0, 0, 1/3, 0, 1/3, 0, 0, 0, 0,
      0, 0.25, 0, 0, 0.25, 0, 0.25, 0, 0.25, 0,
      0, 0, 0, 0.5, 0, 0.5, 0, 0, 0, 0,
      0, 0.25, 0, 0, 0.25, 0, 0.25, 0, 0.25, 0,
      0.25, 0, 0.25, 0, 0, 0, 0, 0.25, 0, 0.25,
      1/6, 1/6, 0, 0, 1/6, 0, 1/6, 0, 1/6, 1/6,
      0.2, 0, 0.2, 0, 0, 0, 0, 0.2, 0.2, 0.2),
    nrow = 10, byrow = TRUE)
  
  tran2by2 = matrix(c(0.95, 0.05, 0.03, 0.97),
                    nrow = 2, byrow = TRUE)
  
  nodes1 = matrix(c(1,1,2,1,2,2,1,2),
                  ncol = 2, byrow = TRUE)
  nodes10 = matrix(c(1,1,2,1,2,2,1,2,12,1),
                   ncol = 2, byrow = TRUE)
  
  ##### TESTS
  
# Tests findTranMat
  row4a = findTranMat(dist2state, intervalTol[1])
  row4b = findTranMat(dist2state, intervalTol[2])
  
  # test a
  all(row4a[4,] == c(0,0,0,0.5,0,0.5,0,0,0,0))
  # test b
  all(row4b[4,] == c(1/3,0,0,1/3,0,1/3,0,0,0,0))
  # test c
  all(apply(row4a, 1, sum) == 1) | all(apply(row4a, 2, sum) == 1)
 
   
  ## Tests for genNodes
  # genNodes generate for n=1
  pts = replicate(10, genNodes(1))
  length(pts) == 20
  # test a
  avgPts = mean(apply(genNodes(500), 1, sum))
  avgPts >= 75 & avgPts <=98
  # test b
  pt200a = try(genNodes(200))
  pt200b = try(genNodes(200))
  pt200c = try(genNodes(200))
  upperPts = min(median(pt200a[pt200a[,1] > 75, 2]),
                 median(pt200b[pt200b[,1] > 75, 2]),
                 median(pt200c[pt200c[,1] > 75, 2]))
  upperPts <= 44

  
## Tests for findRange
  # test a
  sum(abs(rangeRc - findRange(dist2state))) < 0.001
  # test b
  sum(abs(c(2, 2.236068) - findRange(distSimple))) < 0.001
  #test c
  all(findRange(dist1) == c(1,1))

#Test for getEigen2
  # test q
  all.equal(Mod(getEigen2(tranMatLower2)), 1)
  # test b
  all.equal(Mod(getEigen2(tranMatUpper2)), 0.833333333) 
  # test c
  all.equal(Mod(getEigen2(tran2by2)), 0.91999999999)

## Test findRc
  # test a
  abs(findRc(nodesSimple, 0.01) - 2) < 0.01
  # test b
  abs(findRc(nodes2state, 0.001) - 39.903) < 0.01
  # test c
  abs(findRc(nodes1, 0.0001) - 1) < 0.01
  # test d
  abs(findRc(nodes10, 0.0001) - 10) < 0.01

  