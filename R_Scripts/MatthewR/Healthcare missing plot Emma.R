library(readr)
library(janitor)
library(ggplot2)
library(hexbin)
library(RColorBrewer)
library(grid)
library(gridExtra)


miss_table_healthcare_entity <- read_csv("R_Scripts/EmmaR/Report plots/miss_table_healthcare_entity.csv")
miss_table_healthcare_entity <- subset(miss_table_healthcare_entity, select = -c(1,3,4))


hm_1 <- ggplot(data = miss_table_healthcare_entity[0:41,], aes(x = entity , y = pct_miss)) + 
  geom_point()  + xlab(NULL) +
  ylab(NULL) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +  scale_x_discrete(label=function(x) abbreviate(x, minlength=7))

hm_2 <- ggplot(data = miss_table_healthcare_entity[42:84,], aes(x = entity , y = pct_miss)) + 
  geom_point() + xlab(NULL) +
  ylab(NULL)  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +  scale_x_discrete(label=function(x) abbreviate(x, minlength=7))

hm_3 <- ggplot(data = miss_table_healthcare_entity[85:126,], aes(x = entity , y = pct_miss)) + 
  geom_point() + xlab(NULL) +
  ylab(NULL) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +  scale_x_discrete(label=function(x) abbreviate(x, minlength=7))

hm_4 <- ggplot(data = miss_table_healthcare_entity[127:167,], aes(x = entity , y = pct_miss)) + 
  geom_point() + xlab(NULL) +
  ylab(NULL)  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +  scale_x_discrete(label=function(x) abbreviate(x, minlength=7))


healthcare_missing_plot <- grid.arrange(hm_1, hm_2, hm_3, hm_4, left = "Percentage of Missing Data", 
                                   top = textGrob("Healthcare Missing Data by Country", gp=gpar(col="black", fontface = "bold", fontsize = 15)))

ggsave(healthcare_missing_plot, filename = "R_Scripts//MatthewR//Report Graphs//healthcare_missing_plot.png", height = 8, width = 14, units = "in")

