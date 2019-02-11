x  = c(7,6,-6,0,3,-4)
punto = 3
i = 1
result = 0
repeat{
  result = result*punto + x[i]
  if(i==length(x)) break
  i = i+1
}
cat(result," opereaciones: ",i-1)
