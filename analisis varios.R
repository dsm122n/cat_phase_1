library(dplyr)
library(ggplot2)
library(tibble)
library(gridExtra)


theme_graphpad <- function(){
    base_size <- 8
    title_size <- 8
    line_size <- 1

    theme_foundation(base_size = base_size, base_family = "sans") +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(), # that will remove border when using ggsave!
    
        axis.line = element_line(colour="black", size = line_size),
        axis.ticks = element_line(colour="black", size = 1),
        axis.ticks.length = unit(4, "pt"),
        ggh4x.axis.ticks.length.minor = rel(0.5),
        text = element_text(colour = "black"),
        plot.title = element_text(
          face = "bold",
          size = title_size,
          hjust = 0.5
        ),
        axis.title = element_text(face = "bold", size = title_size),
        axis.title.y = element_text(angle = 90, vjust = 2),
        axis.title.x = element_text(vjust = -0.2),
        axis.text = element_text(face = "bold", size = title_size
        ),
        axis.text.x = element_text(
          angle = 0,
          hjust = 0.5,
          vjust = 0
        ),
        legend.key = element_rect(fill = "white", colour = "white"), 
            legend.title = element_blank(),
            legend.text = element_text(size = title_size, family = "verdana"),
        
        legend.position = "bottom",
    )+
    theme(
        text = element_text(family = "verdana")
    )
}

# import data
all_data_long <- tibble(read.csv("output/all_data_long.csv", header = TRUE, sep = ","))

#plot all data
p0 <- ggplot(all_data_long) + 
    geom_point(aes(x = asc, y = frap), col = "#000000") +
    theme_bw()
p1 <- ggplot(all_data_long) + 
    geom_point(aes(x = asc, y = f2iso), col = "#ff7474") +
    theme_bw()
p2 <- ggplot(all_data_long) + 
    geom_point(aes(x = asc, y = au), col = "#ffc547") +
    theme_bw()
p3 <- ggplot(all_data_long) + 
    geom_point(aes(x = nac, y = frap), col = "#000000") +
    theme_bw()
p4 <- ggplot(all_data_long) + 
    geom_point(aes(x = nac, y = f2iso), col = "#ff7474") +
    theme_bw()
p5 <- ggplot(all_data_long) + 
    geom_point(aes(x = nac, y = au), col = "#ffc547") +
    theme_bw()
p6 <- ggplot(all_data_long) + 
    geom_point(aes(x = dfo, y = frap), col = "#000000") +
    theme_bw()
p7 <- ggplot(all_data_long) +
    geom_point(aes(x = dfo, y = f2iso), col = "#ff7474") +
    theme_bw()
p8 <- ggplot(all_data_long) +
    geom_point(aes(x = dfo, y = au), col = "#ffc547") +
    theme_bw()

grid.arrange(p0, p1, p2, p3, p4, p5, p6, p7, p8, ncol = 3, nrow = 3)

p0 <- ggplot(all_data_long) + 
    geom_point(aes(x = asc, y = frap, col = sex)) +
    theme_bw()+
    # delete legend
    theme(legend.position = "none")

p1 <- ggplot(all_data_long) + 
    geom_point(aes(x = asc, y = f2iso, col = sex)) +
    theme_bw()+
    theme(legend.position = "none")
p2 <- ggplot(all_data_long) + 
    geom_point(aes(x = asc, y = au, col = sex)) +
    theme_bw()+
    theme(legend.position = "none")
p3 <- ggplot(all_data_long) + 
    geom_point(aes(x = nac, y = frap, col = sex)) +
    theme_bw()+
    theme(legend.position = "none")
p4 <- ggplot(all_data_long) + 
    geom_point(aes(x = nac, y = f2iso, col = sex)) +
    theme_bw()+
    theme(legend.position = "none")
p5 <- ggplot(all_data_long) + 
    geom_point(aes(x = nac, y = au, col = sex)) +
    theme_bw()+
    theme(legend.position = "none")
p6 <- ggplot(all_data_long) + 
    geom_point(aes(x = dfo, y = frap, col = sex)) +
    theme_bw()+
    theme(legend.position = "none")
p7 <- ggplot(all_data_long) +
    geom_point(aes(x = dfo, y = f2iso, col = sex)) +
    theme_bw()+
    theme(legend.position = "none")
p8 <- ggplot(all_data_long) +
    geom_point(aes(x = dfo, y = au, col = sex)) +
    theme_bw()+
    theme(legend.position = "none")

grid.arrange(p0, p1, p2, p3, p4, p5, p6, p7, p8, ncol = 3, nrow = 3)

plot(density(all_data_long$asc, na.rm = TRUE))
hist(all_data_long$asc, na.rm = TRUE)
plot(density(all_data_long$nac, na.rm = TRUE))
plot(density(all_data_long$dfo, na.rm = TRUE))

shapiro.test(all_data_long$asc)
shapiro.test(all_data_long$nac)
shapiro.test(all_data_long$dfo)

# boxcox transformation
library(bestNormalize)
boxcox(all_data_long$asc)
# plot boxcox
lambda <- boxcox(all_data_long$asc)$lambda
lambda
transformed_asc <- ((all_data_long$asc^lambda)-1) / lambda

# linear mixed effects model
install.packages("lme4")
library(lme4)

install.packages("devtools")
devtools::install_github("dustinfife/flexplot")
library(flexplot)
# model 1
lme <- lmer(frap ~ 1  + asc + (1 + asc |cat), data = all_data_long)
summary(lme)
estimates(lme)
visualize(lme, plot = "model")
