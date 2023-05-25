# Load required libraries
library(mvtnorm)
library(ggplot2)

# Set random seed for reproducibility
set.seed(123)

# Generate correlated bivariate normal data
mu <- c(0, 0)        # Mean vector
sigma <- matrix(c(1, 0.9, 0.9, 1), nrow = 2, ncol = 2)   # Covariance matrix
data <- rmvnorm(n = 500, mean = mu, sigma = sigma)

# Calculate mean and covariance matrix
mean_data <- colMeans(data)
cov_data <- cov(data)

# Calculate eigenvalues and eigenvectors of the covariance matrix
eig <- eigen(cov_data)
eigenvalues <- eig$values
eigenvectors <- eig$vectors

# Compute confidence levels (in percentage)
confidence_levels <- c(0.5, 0.9)   # Modify as desired

# Calculate chi-square quantiles for confidence levels
quantiles <- qchisq(confidence_levels, df = 2)

# Calculate corresponding radii based on eigenvalues and quantiles
radii <- sqrt(eigenvalues)

# Generate ellipse points
theta <- seq(0, 2 * pi, length.out = 100)
ellipse1 <- t(mean_data + t(radii * t(eigenvectors)) %*% rbind(cos(theta), sin(theta)))
ell_dt1 <- as.data.table(ellipse1)
ell_dt1[, idx:=1]
ellipse2 <- t(mean_data + t(radii*2 * t(eigenvectors)) %*% rbind(cos(theta), sin(theta)))
ell_dt2 <- as.data.table(ellipse2)
ell_dt2[, idx:=2]
ellipse3 <- t(mean_data + t(radii*3 * t(eigenvectors)) %*% rbind(cos(theta), sin(theta)))
ell_dt3 <- as.data.table(ellipse3)
ell_dt3[, idx:=3]

ell_dt <- rbindlist(list(ell_dt1, ell_dt2, ell_dt3))

setnames(ell_dt, c("x", "y", "idx"))

# Create a data frame for plotting
df <- data.frame(x = data[, 1], y = data[, 2])

# Plot the data points and ellipses
xmin <- min(data[, 1]) - 1
xmax <- max(data[, 1]) + 1
ymin <- min(data[, 2]) - 1
ymax <- max(data[, 2]) + 1

ggplot(df, aes(x, y)) +
  geom_point() +
  coord_equal() +
  theme_bw() +
  xlim(xmin, xmax) +
  ylim(ymin, ymax) +
  geom_path(data = ell_dt, aes(x, y, group=idx), col="red") +
  geom_vline(xintercept=2, col="blue") +
  theme(text=element_text(size=20))

ggsave("nondegenerate_bivariate_norm.png", units="cm", width=15, height=15)

a <- 2
m <- mean_data
S <- cov_data
m2 <- m[2] + S[2,1,drop=FALSE] %*% solve(S[2,2,drop=FALSE]) %*% (a - m[1])
S2 <- S[2,2,drop=FALSE] - S[2,1,drop=FALSE] %*% solve(S[2,2,drop=FALSE]) %*% S[1,2,drop=FALSE]

xmesh <- seq(as.vector(xmin), as.vector(xmax), length=100)
yvals <- dnorm(xmesh, as.vector(m2), sqrt(as.vector(S2)))
df2 <- data.table(x = xmesh, y = yvals)

ggp <- ggplot() + theme_bw() + geom_path(aes(x=y, y=x), data=df2)
ggp <- ggp + xlab('probability density') + ylab('y')
ggp <- ggp + theme(text=element_text(size=20))
ggp <- ggp + ylim(ymin, ymax)
ggp

ggsave("nondegenerate_bivariate_norm_conditional.png", units="cm", width=10, height=15)