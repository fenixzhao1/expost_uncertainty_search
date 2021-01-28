# add packages
library(ggplot2)

# utility function
utility = function(x, theta){
  return(x^(1-theta)/(1-theta))
}  

# solve for the optimal solution for expost uncertainty
solution_post = function(theta, vh, vl, c){
  u_vh = utility(vh, theta)
  u_vhc = utility(vh-c, theta)
  u_vl = utility(vl, theta)
  u_vlc = utility(vl-c, theta)
  a = u_vhc + u_vlc
  b = 2*(u_vl - u_vh - 2*u_vlc)
  c = u_vhc + 3*u_vlc - 2*u_vl
  exist = b^2 - 4*a*c
  if (exist < 0){
    return(1)
  }
  else{
    sol1 = max(0, (-b - sqrt(exist))/(2*a))
    sol2 = min(1, (-b + sqrt(exist))/(2*a))
    return(c(sol1, sol2))
  }
}

# solve for the optimal solution for only exante uncertainty
solution_ante = function(theta, vh, vl, c){
  
}

# set up parameters
vh = 100
vl = 10
c = 10

df = data.frame(
  aversion_parameter = seq(from=-3, to=0.99, by=0.01),
  reservation_prob1 = seq(from=-3, to=0.99, by=0.01),
  reservation_prob2 = seq(from=-3, to=0.99, by=0.01)
)

for (i in 1:length(df$aversion_parameter)){
  df$reservation_prob1[i] = solution_post(df$aversion_parameter[i], vh, vl, c)[1]
  df$reservation_prob2[i] = solution_post(df$aversion_parameter[i], vh, vl, c)[2]
}

# graph the relation between risk aversion parameter and reservation price
title = paste('risk aversion', 'v_H', as.character(vh), 'v_L', as.character(vl),
              'c', as.character(c), sep = ' ')
file = paste("D:/Dropbox/Working Papers/Expost Uncertainty in Searching and Ranking/data/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 600, height = 400)

pic = ggplot(data = df) +
  geom_line(aes(x=aversion_parameter, y=reservation_prob1)) +
  scale_x_discrete(name='CRRA parameter', waiver(), limits=c(-3,-2,-1,0,1)) +
  scale_y_continuous(name='reservation prob', limits=c(0,1)) +
  ggtitle(title) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

print(pic)
dev.off()
