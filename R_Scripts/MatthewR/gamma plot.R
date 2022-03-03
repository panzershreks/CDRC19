



# We generate Gamma distribution with shape 3, and rate 0.5

x <- rgamma(1000, shape = 3, rate = 0.5)

den <- density(x)
plot_df <- data.frame(x = den$x, y = den$y)

gamma_plot <- ggplot(plot_df, aes(x, y)) + 
  geom_point(size = 1, color = "aquamarine3") + 
  labs(title = ("Theoretical Gamma Distribution"), subtitle = "Shape = 3, rate 0.5, n = 1000")
ggsave(gamma_plot, filename = "R_Scripts//MatthewR//Report Graphs//gamma_plot.png", height = 8, width = 14, units = "in")