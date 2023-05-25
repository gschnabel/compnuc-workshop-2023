library(mvtnorm)

covfun <- function(x, a, l, nugget=1e-6) {
  res <- a^2 * exp(-0.5/l^2 * outer(x, x, `-`)^2)
  res <- res + diag(rep(nugget, length(x)))
  res
}

n_steps <- 30
n_samples <- 10

x <- seq(0, n_steps, length=100)
A0 <- covfun(x, a=1, l=5)
A1 <- covfun(x, a=3, l=1)

samples <- rmvnorm(n_samples, rep(0, length(x)), A1)
flat_samples <- samples
dim(flat_samples) <- NULL

dt <- data.table(
  smp = as.factor(rep(seq_len(n_samples), length(x))),
  x = rep(x, each=n_samples),
  y = flat_samples
)

ggp <- ggplot() + theme_bw()
ggp <- ggp + geom_line(aes(x=x, y=y, col=smp), data=dt)
ggp <- ggp + guides(col="none")
ggp <- ggp + ylim(-10, 10)
ggp <- ggp + theme(text=element_text(size=20))
ggp <- ggp + ggtitle('delta=3, lambda=1')
ggp

ggsave("gp_hyperpar_impact_02.png", units="cm", width=10, height=8)