library(ggplot2)
library(lattice)
library(ggpubr)
library(readxl)
library(gridExtra)

theme_set(theme_bw())
lattice.options(default.theme = standard.theme(color = FALSE))

distributions <- read_excel("Documents/CampoElectricoField/cargas.xlsx")

getEmptyDataset <- function (rows,cols, by){
  x <- list()
  y <- list()
  
  for (i in seq(-rows, rows, by=by)){
    for (j in seq(-cols,cols, by=by)){
      x <- append(x,i)
      y <- append(y,j)
    }
  }
  return(do.call(rbind, Map(data.frame, x=x, y=y)))
}
getElectricVector <- function (Q, Q_x, Q_y, x,y){
  k = 9*10^9
  r_mod=sqrt((x-Q_x)^2+(y-Q_y)^2)
  r_hat = c(x-Q_x,y-Q_y)/r_mod
  
  return((k*Q/r_mod^2)*r_hat)
}
getPotentialScalar <- function (Q,Q_x,Q_y,x,y){
  k = 9*10^9
  r_mod=sqrt((x-Q_x)^2+(y-Q_y)^2)
  
  return(k*Q/r_mod)
}
getPotentialField <- function(dataset, distribution){
  for (row in 1:nrow(dataset)){
    potential_net <- 0
    for (i in 1:3){
      Q <- distribution$q[i]
      Q_x <- distribution$x[i]
      Q_y <- distribution$y[i]
      
      x <- dataset[row, "x"]
      y <- dataset[row, "y"]
      
      potential <- getPotentialScalar(Q,Q_x,Q_y,x,y)
      potential_net <- potential_net + potential
    }
    dataset[row, "v"] <- potential_net
  }
  return (dataset)
}
getElectricField <- function(dataset, distribution){
  for (row in 1:nrow(dataset)){
    e_x <- 0
    e_y <- 0
    for (i in 1:3){
      Q <- distribution$q[i]
      Q_x <- distribution$x[i]
      Q_y <- distribution$y[i]
      
      x <- dataset[row, "x"]
      y <- dataset[row, "y"]
      electric_field_vector = getElectricVector(Q, Q_x, Q_y, x, y)
      
      e_x <- e_x + electric_field_vector[[1]]
      e_y <- e_y + electric_field_vector[[2]]
    }
    dataset[row, "e"] = sqrt(e_x^2+e_y^2)
    dataset[row,"e_x"] <- e_x
    dataset[row,"e_y"] <- e_y
  }
  return (dataset)
}

getElectricField_pos <- function(distribution, x, y){
  e_x <- 0
  e_y <- 0
  for (i in 1:3){
    Q <- distribution$q[i]
    Q_x <- distribution$x[i]
    Q_y <- distribution$y[i]

    electric_field_vector = getElectricVector(Q, Q_x, Q_y, x, y)
    
    e_x <- e_x + electric_field_vector[[1]]
    e_y <- e_y + electric_field_vector[[2]]
  }
  return(c(e_x,e_y))
}


positions_x <- c()
positions_y <- c()

n_dist <- 4
pos_0 <- c(2,2)
v_0 <- c(0,0)
q_test <- 1*10^(-6)
m <- 1*10^(-10)

for (i in 1:900){
  t <- 0.00005
  
  curr_e <- getElectricField_pos(distributions[distributions$dist == n_dist, ], pos_0[1], pos_0[2])
  curr_a <- curr_e*q_test/m
  next_pos <- pos_0 + v_0*t + (curr_a/2)* t^2
  
  v_0 <- curr_a*t
  
  positions_x <- c(positions_x, pos_0[[1]])
  positions_y <- c(positions_y, pos_0[[2]])
  pos_0 <- next_pos
}
positions <- data.frame(x=positions_x, y=positions_y)

datasetElectricField <- getEmptyDataset(3,3,0.2)
datasetElectricField <- getElectricField(datasetElectricField,distributions[distributions$dist == n_dist, ])
datasetElectricField <- datasetElectricField[!is.infinite(rowSums(datasetElectricField)),]

# Plot electric field
ef <- ggplot(datasetElectricField, aes(x = x, y = y)) + ggtitle("Campo Eléctrico en función de una distribución de cargas") +
  geom_segment(aes(xend = x+e_x/(e*10), yend = y+e_y/(e*10), alpha = e),
               arrow = arrow(length = unit(0.1,"cm")), size = 0.6) + 
            geom_point(data=positions, size=1, alpha=0.7, color='red') +
              xlim(-3,3) + ylim(-3,3) + theme_bw()

# EXP 2
datasetPotentialField <- getEmptyDataset(3,3,0.05)
datasetPotentialField <- getPotentialField(datasetPotentialField, distributions[distributions$dist == n_dist, ] )
datasetPotentialField <- datasetPotentialField[!is.infinite(rowSums(datasetPotentialField)),]
datasetPotentialField$v[datasetPotentialField$v < -1000000] <- -1000000

# Plot potential field
my.settings <- list(par.main.text = list(font = 1,just = "left", x = grid::unit(5, "mm")))

pf <- levelplot(v ~ x*y, datasetPotentialField[datasetPotentialField$v >= -1000000, ],par.settings=my.settings, main="Campo potencial eléctrico de una distribución de cargas.")

ggarrange(ef,pf, ncol = 2, nrow = 1)

