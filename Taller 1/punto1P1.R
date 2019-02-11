x  = c(2,0,-3,3,-4)
punto = -2
i = 1
result = 0
repeat{
  result = result*punto + x[i]
  if(i==length(x)) break
  i = i+1
}
cat(result," opereaciones: ",i-1)
