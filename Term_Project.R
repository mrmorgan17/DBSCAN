# Reading in necessary packages
library(dbscan)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)

######################
## Simulation Study ##
######################

###########
## Setup ##
###########

truth <- c(rep(1, 100), rep(2, 100), rep(3, 100), rep(4, 100))

# Collinear mean vectors
mean_vecs1 <- list(
  c(5:8),
  c(-5:-8),
  c(seq(from = 15, to = 24, by = 3)),
  c(rep(0, 4))
)

# Overlapping mean vectors 
mean_vecs2 <- list(
  c(5:8),
  c(3:6),
  c(7:10),
  c(rep(0, 4))
)

mean_vecs3 <- list(
  c(5:12),
  c(-5:-12),
  c(seq(from = 15, to = 36, by = 3)),
  c(rep(0, 8))
)

mean_vecs4 <- list(
  c(5:16),
  c(-5:-16),
  c(seq(from = 15, to = 48, by = 3)),
  c(rep(0, 12))
)

# Covaraince matrices for each setting of mean vectors
cov_mat1 <- matrix(.5, nrow = 4, ncol = 4)
cov_mat1[row(cov_mat1) == col(cov_mat1)] = 5

cov_mat2 <- matrix(.5, nrow = 4, ncol = 4)
cov_mat2[row(cov_mat2) == col(cov_mat2)] = 5

cov_mat3 <- matrix(.5, nrow = 8, ncol = 8)
cov_mat3[row(cov_mat3) == col(cov_mat3)] = 5

cov_mat4 <- matrix(.5, nrow = 12, ncol = 12)
cov_mat4[row(cov_mat4) == col(cov_mat4)] = 5

n_vec <- rep(100, 4)

# par(mfrow = c(1, 4), oma = c(0, 0, 2, 0))

set.seed(16)

#############
# Setting 1 #
#############

# Generate data 
X_g1 <- mvtnorm::rmvnorm(n_vec[1], mean_vecs1[[1]], cov_mat1)
X_g2 <- mvtnorm::rmvnorm(n_vec[2], mean_vecs1[[2]], cov_mat1)
X_g3 <- mvtnorm::rmvnorm(n_vec[3], mean_vecs1[[3]], cov_mat1)
X_g4 <- mvtnorm::rmvnorm(n_vec[4], mean_vecs1[[4]], cov_mat1)

# Standardize data
X <- rbind(X_g1, X_g2, X_g3, X_g4)
X <- scale(X)

# Pick eps for DBSCAN
kNNdistplot(X, minPts = 8)
lines(x = 1:400, y = rep(.3, 400), lty = 'dashed', col = "red")
text(x = 50, y = .6, expression(paste(epsilon[1], " = 0.3")))

# Use dbscan to cluster data
db_res1 <- dbscan(X, eps = .3, minPts = 8)

pairs1 <- GGally::ggpairs(
  data.frame(X), 
  columns = 1:4,
  mapping = aes(color = as.factor(db_res1$cluster + 1L)),
  upper = list(continuous = "points")
) + 
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  labs(
    title = "Pairs Plot for Non-Overlapping Mean Vectors",
    subtitle = "DBSCAN Clustering Method",
    caption = expression(paste(s, " = 1"))
  )

#############
# Setting 2 #
#############

# Generate data 
X_g1 <- mvtnorm::rmvnorm(n_vec[1], mean_vecs2[[1]], cov_mat2)
X_g2 <- mvtnorm::rmvnorm(n_vec[2], mean_vecs2[[2]], cov_mat2)
X_g3 <- mvtnorm::rmvnorm(n_vec[3], mean_vecs2[[3]], cov_mat2)
X_g4 <- mvtnorm::rmvnorm(n_vec[4], mean_vecs2[[4]], cov_mat2)

# Standardize data
X <- rbind(X_g1, X_g2, X_g3, X_g4)
X <- scale(X)

# Pick eps for DBSCAN
kNNdistplot(X, minPts = 8)
lines(x = 1:400, y = rep(.7, 400), lty = 'dashed', col = "red")
text(x = 50, y = .9, expression(paste(epsilon[2], " = 0.7")))

# Use dbscan to cluster data
db_res2 <- dbscan(X, eps = .7, minPts = 8)

pairs2 <- GGally::ggpairs(
  data.frame(X), 
  columns = 1:4,
  mapping = aes(color = as.factor(db_res2$cluster + 1L)),
  upper = list(continuous = "points")
) + 
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  labs(
    title = "Pairs Plot for Overlapping Mean Vectors",
    subtitle = "DBSCAN Clustering Method",
    caption = expression(paste(s, " = 2"))
  )

g1 <- grid.grabExpr(print(pairs1))
g2 <- grid.grabExpr(print(pairs2))
grid.arrange(g1, g2, widths = c(0.5, 0.5))

#############
# Setting 3 #
#############

# Generate data 
X_g1 <- mvtnorm::rmvnorm(n_vec[1], mean_vecs3[[1]], cov_mat3)
X_g2 <- mvtnorm::rmvnorm(n_vec[2], mean_vecs3[[2]], cov_mat3)
X_g3 <- mvtnorm::rmvnorm(n_vec[3], mean_vecs3[[3]], cov_mat3)
X_g4 <- mvtnorm::rmvnorm(n_vec[4], mean_vecs3[[4]], cov_mat3)

# Standardize data
X <- rbind(X_g1, X_g2, X_g3, X_g4)
X <- scale(X)

# Pick eps for DBSCAN
kNNdistplot(X, minPts = 16)
lines(x = 1:400, y = rep(.55, 400), lty = 'dashed', col = "red")
text(x = 50, y = .8, expression(paste(epsilon[3], " = 0.55")))

#############
# Setting 4 #
#############

# Generate data 
X_g1 <- mvtnorm::rmvnorm(n_vec[1], mean_vecs4[[1]], cov_mat4)
X_g2 <- mvtnorm::rmvnorm(n_vec[2], mean_vecs4[[2]], cov_mat4)
X_g3 <- mvtnorm::rmvnorm(n_vec[3], mean_vecs4[[3]], cov_mat4)
X_g4 <- mvtnorm::rmvnorm(n_vec[4], mean_vecs4[[4]], cov_mat4)

# Standardize data
X <- rbind(X_g1, X_g2, X_g3, X_g4)
X <- scale(X)

# Pick eps for DBSCAN
kNNdistplot(X, minPts = 24)
lines(x = 1:400, y = rep(.67, 400), lty = 'dashed', col = "red")
text(x = 50, y = .95, expression(paste(epsilon[4], " = 0.67")))

# Add a main title to the layout
mtext(
  "Figure 1: kNN Dist Plots with Varying minPts", 
  outer = TRUE, 
  cex = 1.5, 
  line = 0,
  font = 2
)

# Add a subtitle to the layout
mtext(
  expression(paste("Red dashed line represents ", epsilon)), 
  outer = TRUE, 
  cex = 1, 
  line = -2
)

#make into a function so we can change by mean vectors
#do simulations three times, once for mean vectors of length 4,5, and 6 respectively
set.seed(16)
do_sim <- function(mean_vecs, n_vec, cov_mat, eps) {
  
  sims <- 10000
  n_groups <- 4
  found_groups <- logical(sims)
  db_acc <- numeric(sims)
  kmean_acc <- numeric(sims)
  db_rand <- numeric(sims)
  kmean_rand <- numeric(sims)
  perc_noise <- numeric(sims)
  
  for (i in 1:sims) { 
    
    # Generate data
    X_g1 <- mvtnorm::rmvnorm(n_vec[1], mean_vecs[[1]], cov_mat)
    X_g2 <- mvtnorm::rmvnorm(n_vec[2], mean_vecs[[2]], cov_mat)
    X_g3 <- mvtnorm::rmvnorm(n_vec[3], mean_vecs[[3]], cov_mat)
    X_g4 <- mvtnorm::rmvnorm(n_vec[4], mean_vecs[[4]], cov_mat)
    X <- rbind(X_g1, X_g2, X_g3, X_g4)
    
    # Standardize data
    X <- scale(X)
    
    # Use dbscan to cluster data
    db_res <- dbscan::dbscan(X, eps = eps, minPts = 2*length(mean_vecs[[1]]))$cluster
    
    # Check if dbscan found 4 groups
    if (max(db_res) != n_groups) {
      found_groups[i] <- FALSE
      next
    }
    
    # How many point did dbscan classify as noise?
    perc_noise[i] <- sum(db_res == 0) / length(db_res)
    found_groups[i] <- TRUE
    
    # If 4 group were found by dbscan, compare it to k-means
    k_res1 <- kmeans(X[db_res != 0,], n_groups)$cluster
    k_res2 <- k_res1
    u_k <- unique(k_res1)
    k_res2[k_res1 == u_k[1]] <- 1
    k_res2[k_res1 == u_k[2]] <- 2
    k_res2[k_res1 == u_k[3]] <- 3
    k_res2[k_res1 == u_k[4]] <- 4
    
    # Check if groups were identified correctly bewteen the two methods
    db_red <- db_res[db_res != 0]
    db_acc[i] <- sum(db_red == truth[db_res != 0]) / length(db_red)
    kmean_acc[i] <- sum(k_res2 == truth[db_res != 0]) / length(db_red)
    # db_rand[i] <- fossil::adj.rand.index(db_red, truth[db_res != 0])
    # kmean_rand[i] <- fossil::adj.rand.index(k_res2, truth[db_res != 0])
    
  }
  
  # Return metrics
  return(
    list(
      perc_noise = perc_noise, 
      found_groups = found_groups, 
      db_acc = db_acc, 
      db_rand = db_rand, 
      kmean_acc = kmean_acc, 
      kmean_rand = kmean_rand)
    )
  
}

sim1 <- do_sim(mean_vecs1, n_vec, cov_mat1, .3)

sim2 <- do_sim(mean_vecs2, n_vec, cov_mat2, .7)

sim3 <- do_sim(mean_vecs3, n_vec, cov_mat3, .55)

sim4 <- do_sim(mean_vecs4, n_vec, cov_mat4, .67)

data.frame(
  setting = c(1:4),
  founds_groups = c(mean(sim1[['found_groups']]), mean(sim2[['found_groups']]), mean(sim3[['found_groups']]), mean(sim4[['found_groups']])),
  perc_noise = c(mean(sim1[['perc_noise']]), mean(sim2[['perc_noise']]), mean(sim3[['perc_noise']]), mean(sim4[['perc_noise']])),
  dbscan_acc  = c(mean(sim1[['db_acc']]), mean(sim2[['db_acc']]), mean(sim3[['db_acc']]), mean(sim4[['db_acc']])),
  kmeans_acc = c(mean(sim1[['kmean_acc']]), mean(sim2[['kmean_acc']]), mean(sim3[['kmean_acc']]), mean(sim4[['kmean_acc']]))
) %>% 
  knitr::kable(
    format = "latex", 
    digits = 4,
    col.names = c("$s$", "Found Groups", "Percent Noise", "DBSCAN Accuracy", "k-means Accuracy"),
    booktabs = TRUE
  )

#################
## Application ##
#################

golf_dat <- read.csv('2014GolfMod1.csv')
X <- golf_dat[, c(11:14, 24:27)]

# Standardize data
X <- scale(X)

# Pick eps for DBSCAN
kNNdistplot(X, minPts = 16)

db_res <- dbscan(X, eps = 1.4, minPts = 16)
db_res

pairs(X, col = db_res$cluster + 1L)

golf_red <- golf_dat[db_res$cluster == 1,]

golf_dat$cluster <- db_res$cluster
golf_dat  %>%
  group_by(cluster) %>%
  summarize(n_players = length(unique(Player.Name)), n_courses = length(unique(Course.Number))) %>%
  print(n = 29)