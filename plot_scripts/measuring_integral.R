library(ggplot2)
library(data.table)

# Generate data for the oscillating function
x <- seq(0, 10, length.out = 100)
y <- sin(0.5*x) + 0.1 * cos(2 * x) + 1

# Specify the range to be highlighted
highlight_start <- 3
highlight_end <- 7

# Create a data frame
df <- data.table(data.frame(x = x, y = y))
df_ext <- rbindlist(list(df[x >= highlight_start & x <= highlight_end], data.table(
  x = c(7, 3), y = c(0, 0)
)))

# Create the plot using ggplot
ggplot(df, aes(x, y)) +
  geom_line() +  # Add a line plot
  geom_rect(aes(xmin = highlight_start, xmax = highlight_end,
                ymin = 0, ymax = 2.25),
            color = "red", fill=NA, alpha = 0.3) +  # Add the red rectangle
  geom_polygon(data=df_ext, aes(x=x, y=y), fill="blue", alpha=0.3) +
  xlim(0, 10) +  # Set the x-axis limits
  ylim(0, 2.5) +  # Set the y-axis limits
  labs(x = "X", y = "Y") +  # Set the axis labels
  theme_bw() +  # Use a black-and-white theme
  theme(text=element_text(size=20)) +
  xlab('energy') + ylab('cross section')

ggsave("measuring_integral.png", units="cm", width=15, height=15)

# unknown energy calibration
exp_idx <- sample(seq_len(length(x)), size=20, replace = FALSE)
x_exp <- x[exp_idx] + 1
y_exp <- y[exp_idx] + rnorm(length(x_exp), sd=0.1)
dfexp <- data.table(x=x_exp, y=y_exp)

ggplot(df, aes(x, y)) +
  geom_errorbar(aes(x=x, ymin=y-0.1, ymax=y+0.1), data=dfexp) +
  geom_point(aes(x=x, y=y), data=dfexp) +
  geom_line() +  # Add a line plot
  xlim(0, 10) +  # Set the x-axis limits
  ylim(0, 2.5) +  # Set the y-axis limits
  labs(x = "X", y = "Y") +  # Set the axis labels
  theme_bw() +  # Use a black-and-white theme
  theme(text=element_text(size=20)) +
  xlab('energy') + ylab('cross section')

ggsave("measuring_wrong_energy_calibration.png", units="cm", width=15, height=15)
