# STA2201 Final Project
# Generating Hybrid Arc + Gaussian Dataset
# Authors: Evan Shamov and Ian Zhang
# Code: Evan Shamov

set.seed(20937635)

# Generate Arc
theta_vec = seq(from = 3*pi/2, to = 11*pi/4, length.out = 150)
x_arc = 2*theta_vec*cos(theta_vec) + rnorm(150, mean = 0, sd = 0.1)
y_arc = 2*theta_vec*sin(theta_vec) + rnorm(150, mean = 0, sd = 0.1)
arc = cbind(x_arc, y_arc, 1)

# Generate Gaussian cluster  
x_gauss = rnorm(150, mean = -10, sd = 0.5)
y_gauss = rnorm(150, mean = -10, sd = 1)
gauss = cbind(x_gauss, y_gauss, 2)

HAG = rbind(arc, gauss)

HAG_df = data.frame(x1 = HAG[,1],
                    x2 = HAG[,2],
                    class = HAG[,3])

# Export File
write.csv(HAG_df, 'HAG1.csv', row.names = FALSE)