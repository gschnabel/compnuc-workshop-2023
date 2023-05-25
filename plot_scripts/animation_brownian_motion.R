library(data.table)
library(ggplot2)
library(gganimate)
library(mvtnorm)

# Brownian motion

n_steps <- 30
n_samples <- 5
samples <- matrix(0, nrow=n_samples, ncol=n_steps)

for (i in seq_len(n_samples)) {
  curpos <- 0
  samples[i, 1] <- curpos
  for (j in seq(2, n_steps)) {
    curpos <- curpos + sample(c(-1, 1), size=1)
    samples[i, j] <- curpos
  }
}

step <- seq_len(n_steps)
flat_samples <- samples
dim(flat_samples) <- NULL

dt <- data.table(
  smp = as.factor(rep(seq_len(n_samples), n_steps)),
  x = rep(step, each=n_samples),
  y = flat_samples
)

dt_list <- list()
cumdt <- data.table()
for (curx in sort(unique(dt$x))) {
  # select time step and merge it into all subsequent time steps
  curdt <- copy(dt[x == curx])
  newdt <- data.table(
    smp=curdt$smp,
    x=curdt$x,
    y=curdt$y,
    cumtime=rep(seq(as.integer(curx), n_steps), each=n_samples)
  )
  cumdt <- rbindlist(list(cumdt, newdt))
  dt_list[[length(dt_list)+1]] <- cumdt
}
cumdt <- rbindlist(dt_list)


ggp <- ggplot() + theme_bw()
ggp <- ggp + geom_line(aes(x=x, y=y, col=smp), data=dt)
ggp <- ggp + guides(col="none")
ggp

p <- ggplot(cumdt, aes(x, y, col = smp)) + geom_line() + ylim(-10, 10)
p <- p + theme_bw()
p <- p + xlab('time') + ylab('position')
p <- p + transition_manual(cumtime)
p <- p + guides(col="none")
p <- p + theme(text=element_text(size=20))
animate(p)


densdt <- cumdt[, list(numpts=copy(.N)), by=c("cumtime", "x", "y")]
p <- ggplot(densdt, aes(x, y)) + geom_point(aes(size=numpts)) + ylim(-30, 30)
p <- p + theme_bw() + guides(col="none")
p <- p + xlab('time') + ylab('position')
p <- p + scale_size_area(breaks=seq(0, 4000, by=100))
p <- p + transition_manual(cumtime)
p <- p + guides(size="none")
p <- p + theme(text=element_text(size=20))
animate(p)

# Save the animation as a GIF file
animation_file <- "animation_multiple_brownian_motion.gif"
anim_save(animation_file, animate(p, width=600, height=500))


anim <- ggplot(mtcars, aes(factor(gear), mpg)) +
  geom_boxplot() +
  transition_manual(gear) + transition_components("cumulative")
animate(anim, fps=1000)

n_steps <- 30
n_samples <- 10
mvec <- rep(0, n_steps+1)
covmat <- matrix(0, nrow=n_steps+1, ncol=n_steps+1)
for (i in seq(0, n_steps)) {
  covmat[(i+1),(i+1):(n_steps+1)] <- i
  covmat[(i+1):(n_steps+1),(i+1)] <- i
}
covmat[1, 1] <- 1e-6

samples <- rmvnorm(n_samples, mvec, covmat)
flat_samples <- samples
dim(flat_samples) <- NULL

dt <- data.table(
  smp = as.factor(rep(seq_len(n_samples), n_steps+1)),
  x = rep(c(0,step), each=n_samples),
  y = flat_samples
)

ggp <- ggplot() + theme_bw()
ggp <- ggp + geom_line(aes(x=x, y=y, col=smp), data=dt)
ggp <- ggp + guides(col="none")
ggp <- ggp + xlab('time') + ylab('position')
ggp <- ggp + theme(text=element_text(size=20))
ggp <- ggp + ylim(-10, 10)
ggp

ggsave("brownian_process_gauss_approx.png", units="cm", width=20, height=15)


cidx <- c(10)
a <- c(5)
S <- covmat
m <- rep(0, nrow(covmat))
m2 <- m + S[,cidx,drop=FALSE] %*% solve(S[cidx,cidx,drop=FALSE]) %*% (a - m[cidx])
S2 <- S[,,drop=FALSE] - S[,cidx,drop=FALSE] %*% solve(S[cidx,cidx,drop=FALSE]) %*% S[cidx,,drop=FALSE]

red_samples <- rmvnorm(n_samples, m2, S2)
flat_red_samples <- red_samples
dim(flat_red_samples) <- NULL

red_dt <- data.table(
  smp = as.factor(rep(seq_len(n_samples), n_steps+1)),
  x = rep(c(0,step), each=n_samples),
  y = flat_red_samples
)

ggp <- ggplot() + theme_bw()
ggp <- ggp + geom_line(aes(x=x, y=y, col=smp), data=red_dt)
ggp <- ggp + guides(col="none")
ggp <- ggp + xlab('time') + ylab('position')
ggp <- ggp + theme(text=element_text(size=20))
ggp <- ggp + ylim(-10, 10)
ggp

ggsave("constrained_brownian_process_gauss_approx.png", units="cm", width=20, height=12)