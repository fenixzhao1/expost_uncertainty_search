##### Simulation preparation #####
# add packages
library(ggplot2)

## set up utility functions and optimization functions
# utility function
crra = function(x, theta){
  if (theta == 1){return(log(x))}
  else{return((x^(1-theta)-1)/(1-theta))}
}

cara = function(x, theta){
  return(1-exp(-theta*x))
}  

# solve for the optimal solution for uncertainty treatment
solution_u = function(theta, alpha, beta, vh, vl, c){
  
  # calculate utility
  u_vh = crra(vh, theta)
  u_vhc = crra(vh-c, theta)
  u_vl = crra(vl, theta)
  u_vlc = crra(vl-c, theta)
  
  # calculate coefficients for the formula
  a = (u_vhc - u_vlc)/2
  b = (beta-alpha)*(u_vl - u_vh) + alpha*(u_vlc-u_vhc)
  c = u_vhc*(beta^2/2) + u_vlc*(beta-beta^2/2) - alpha*u_vlc - (beta-alpha)*u_vl
  
  # set up conditions for valid solutions
  result0 = c
  result1 = a + b + c
  exist = b^2 - 4*a*c
  axis = -b/(2*a)
  
  if (exist < 0){
    return(c(result0, result1, exist, axis, 1, 1, a, b, c))
  }
  else{
    sol1 = (-b - sqrt(exist))/(2*a)
    sol2 = (-b + sqrt(exist))/(2*a)
    return(c(result0, result1, exist, axis, sol1, sol2, a, b, c))
  }
}

# solve for the optimal solution for certainty treatment
solution_c = function(theta, vh, vl, c){
  
  # skip theta=1 and theta=2
  if (theta==1 | theta==2){
    return(c(NA,NA,NA,NA))
  }
  else{
    
    # calculate LHS of the equation and find the value closest to 0
    z = seq(from=0, to=1, by=0.001)
    phi1 = (vh*z+vl*(1-z)-c)^(1-theta)*z + (vh-c)^(2-theta)/((2-theta)*(vh-vl))
    phi2 = -(vh*z+vl*(1-z)-c)^(2-theta)/((2-theta)*(vh-vl)) - (vh*z+vl*(1-z))^(1-theta)
    phi = phi1 + phi2
    dist = abs(phi)
    min_dist = min(dist)
    min_z = z[which.min(dist)]
  
    # return the solution conditional on the original value and the distance
    if (max(phi) <= 0){return(c(-10000,0,max(phi),min(phi)))}
    else if(min(phi) >= 0){return(c(10000,1,max(phi),min(phi)))}
    else{
      return(c(min_dist,min_z,max(phi),min(phi)))
    }
  }
}


##### Reservation prob vs risk preference #####
# set up parameters
vh = 500
vl = 100
c = 5
alpha = 0
beta = 1

df = data.frame(
  theta = seq(from=-3, to=3, by=0.01)
)

for (i in 1:length(df$theta)){
  solution = solution_u(df$theta[i], alpha, beta, vh, vl, c)
  df$size0[i] = solution[1]
  df$size1[i] = solution[2]
  df$exist[i] = solution[3]
  df$axis[i] = solution[4]
  df$prob[i] = solution[5]
  df$prob2[i] = solution[6]
  df$a[i] = solution[7]
  df$b[i] = solution[8]
  df$c[i] = solution[9]
  
  solution2 = solution_c(df$theta[i], vh, vl, c)
  df$dist_c[i] = solution2[1]
  df$prob_c[i] = solution2[2]
  df$max_phi[i] = solution2[3]
  df$min_phi[i] = solution2[4]
}

# graph the relation between risk aversion parameter and reservation price
title = paste('compare', 'v_H', as.character(vh), 'v_L', as.character(vl),
              'c', as.character(c), sep = ' ')
file = paste("D:/Dropbox/Working Papers/Expost Uncertainty in Searching and Ranking/data/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 600, height = 400)

pic = ggplot(data = df) +
  geom_line(aes(x=theta, y=prob, colour = 'blue')) +
  geom_line(aes(x=theta, y=prob_c, colour = 'red')) +
  scale_x_discrete(name='CRRA parameter', waiver(), limits=c(-3,-1,0,1,3)) +
  scale_y_continuous(name='reservation prob', limits=c(0.5,1)) +
  ggtitle(title) + 
  theme_bw() + 
  scale_colour_manual(values=c('blue', 'red'), labels=c('uncertainty', 'certainty')) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

print(pic)
dev.off()


##### Reservation prob vs decreasing interval #####
# set up parameters
vh = 200
vl = 100
beta = 1
alpha = 0.5

# create df
theta = 0.5
df = data.frame(
  lower = alpha - seq(from=0, to=0.5, by=0.01),
  upper = beta - seq(from=0, to=0.5, by=0.01)
)

for (i in 1:length(df$lower)){
  solution = solution_u(theta, df$lower[i], df$upper[i], vh, vl, c)
  df$size0[i] = solution[1]
  df$size1[i] = solution[2]
  df$exist[i] = solution[3]
  df$axis[i] = solution[4]
  df$prob[i] = solution[5]
  df$prob2[i] = solution[6]
  df$a[i] = solution[7]
  df$b[i] = solution[8]
  df$c[i] = solution[9]
}

# create df2
theta = 0
df2 = data.frame(
  lower = alpha - seq(from=0, to=0.5, by=0.01),
  upper = beta - seq(from=0, to=0.5, by=0.01)
)

for (i in 1:length(df2$lower)){
  solution = solution_u(theta, df2$lower[i], df2$upper[i], vh, vl, c)
  df2$size0[i] = solution[1]
  df2$size1[i] = solution[2]
  df2$exist[i] = solution[3]
  df2$axis[i] = solution[4]
  df2$prob[i] = solution[5]
  df2$prob2[i] = solution[6]
  df2$a[i] = solution[7]
  df2$b[i] = solution[8]
  df2$c[i] = solution[9]
}

# create df2
theta = 1.5
df3 = data.frame(
  lower = alpha - seq(from=0, to=0.5, by=0.01),
  upper = beta - seq(from=0, to=0.5, by=0.01)
)

for (i in 1:length(df3$lower)){
  solution = solution_u(theta, df3$lower[i], df3$upper[i], vh, vl, c)
  df3$size0[i] = solution[1]
  df3$size1[i] = solution[2]
  df3$exist[i] = solution[3]
  df3$axis[i] = solution[4]
  df3$prob[i] = solution[5]
  df3$prob2[i] = solution[6]
  df3$a[i] = solution[7]
  df3$b[i] = solution[8]
  df3$c[i] = solution[9]
}

# graph the relation between risk aversion parameter and reservation price
title = paste('decreasing interval', 'v_H', as.character(vh), 'v_L', as.character(vl),
              'c', as.character(c), sep = ' ')
file = paste("D:/Dropbox/Working Papers/Expost Uncertainty in Searching and Ranking/data/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 600, height = 400)

pic = ggplot() +
  geom_line(data = df, aes(x=upper, y=prob, colour = 'blue')) +
  geom_line(data = df2, aes(x=upper, y=prob, colour = 'red')) +
  geom_line(data = df3, aes(x=upper, y=prob, colour = 'green')) +
  scale_x_discrete(name='upper bound (lower bound + 0.5)', waiver(), limits=c(0.5,1)) +
  scale_y_continuous(name='reservation prob', limits=c(0,1)) +
  ggtitle(title) + 
  theme_bw() + 
  scale_colour_manual(values=c('blue','red','green'), labels=c('theta=0.5','theta=1.5','theta=0')) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

print(pic)
dev.off()

