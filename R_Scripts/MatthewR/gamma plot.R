

x <- seq(0, 10, by=0.01)   
y <- dgamma(x, shape = 3) 

df <- data.frame(x,y)

#create density plot

gamma_plot <- ggplot(df, aes(x, y)) + 
  geom_point(size = 1, color = "aquamarine3") + 
  labs(title = ("Theoretical Gamma Distribution"), subtitle = "Shape = 3")

gamma_plot

ggsave(gamma_plot, filename = "R_Scripts//MatthewR//Report Graphs//gamma_plot.png", height = 8, width = 14, units = "in")
