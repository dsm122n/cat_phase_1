library(ggplot2)
library(dplyr)
library(tibble)
library(ggthemes)
library(ggh4x)
# Import data from covariables_asc.csv covariables_nac.csv and covariables_dfo.csv
asc <- tibble(read.csv("clean data/cv_mean_asc.csv", header = TRUE, sep = ","))
nac <- tibble(read.csv("clean data/cv_mean_nac.csv", header = TRUE, sep = ","))
dfo <- tibble(read.csv("clean data/cv_mean_dfo.csv", header = TRUE, sep = ","))


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
        plot.background = element_blank() # that will remove border when using ggsave!
    ) +
    theme(
        axis.line = element_line(colour="black", size = line_size),
        axis.ticks = element_line(colour="black", size = 1),
        axis.ticks.length = unit(4, "pt"),
        ggh4x.axis.ticks.length.minor = rel(0.5)
    )+
    theme(
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
        )
    ) +
    theme(legend.key = element_rect(fill = "white", colour = "white"), 
            legend.title = element_blank(),
            legend.text = element_text(size = title_size, family = "verdana"),
        ) +
    theme(legend.position = "bottom")+
    theme(text = element_text(family = "verdana"))
}



# Plot
# Plot mean and standard deviation for ASC mean_conc over time grouped by cat1, cat2 and p
asc_graph <- ggplot(data = asc) +
    # add mean and standard deviation
    stat_summary(aes(x = time, y = mean_conc, col = cat), fun.y = mean, geom = "line", size = 1) +
    stat_summary(aes(x = time, y = mean_conc, col = cat, shape = cat), fun.y = mean, geom = "point", size = 3) +
    stat_summary(aes(x = time, y = mean_conc, col = cat), fun.data = mean_sdl, fun.args=list(mult=1), geom = "errorbar", width = 6) +
    scale_x_continuous(breaks = seq(0, 180, 30), 
                        minor_breaks = seq(0, 180, 15), 
                        guide = "axis_minor",
                        expand = expansion(mult = c(0.01, 0.05))
                        ) +
    scale_y_continuous(limits = c(0, 1200), 
                        breaks = seq(0, 1200, 200), 
                        minor_breaks = seq(0, 1200, 100),
                        guide = "axis_minor",
                        expand = expansion(mult = c(0.02, 0.006))
                       ) +
    scale_color_manual(values =  c("#471061", "#208f8d", "#cae11e"), 
                        labels = c("CAT 1",     "CAT 2",      "Placebo")) +
    scale_shape_manual(values = c(16, 15, 17),
                        labels = c("CAT 1", "CAT 2", "Placebo")) +
    labs(x = "Time [min]", y = "Concentration [μM]", title = "Vitamin C") +
    theme_graphpad()
asc_graph
ggsave("C:/Users/sanma/Desktop/concentraciones_asc_new.png", asc_graph, width = 8.4, height = 6, dpi = 1000, units = "cm")
#same graph but with boxplot instead of mean and standard deviation grouping by cat and time

asc_graph_boxplot <- ggplot() +
    geom_boxplot(data = asc, aes(x = time, y = mean_conc, group = interaction(time, cat), col = cat), size = 1) +
    # set colors
    scale_colour_manual(values = c("#5c5c5c",  "#6c0000", "#04176b"), 
                        labels = c("Placebo", "CAT 1", "CAT 2")) +
    labs(x = "Time [min]", y = "Concentration [μM]", title = "Vitamin C") +
    # change font size
    scale_x_continuous(breaks = seq(0, 180, 30)) +
    theme_bw()+
    theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=8),
        axis.text = element_text(size = 8),
        plot.title = element_text(size = 12))
asc_graph_boxplot

# Plot mean and standard deviation for NAC mean_conc over time grouped by cat1, cat2 and p
nac_graph <- ggplot() +
    # geom_point(data = nac, aes(x = time, y = mean_conc, col = cat)) +
    # add mean and standard deviation
    stat_summary(data = nac, aes(x = time, y = mean_conc, col = cat), fun.y = mean, geom = "line", size = 1) +
    stat_summary(data = nac, aes(x = time, y = mean_conc, col = cat, shape = cat), fun.y = mean, geom = "point", size = 3) +
    stat_summary(data = nac, aes(x = time, y = mean_conc, col = cat), fun.data = mean_sdl, fun.args=list(mult=1), geom = "errorbar", width = 6) +
    scale_x_continuous(breaks = seq(0, 180, 30), 
                        minor_breaks = seq(0, 180, 15), 
                        guide = "axis_minor",
                        expand = expansion(mult = c(0.01, 0.05))
                        ) +
    scale_y_continuous(limits = c(0, 1000), 
                        breaks = seq(0, 1000, 200), 
                        minor_breaks = seq(0, 1200, 100),
                        guide = "axis_minor",
                        expand = expansion(mult = c(0.02, 0.006))
                       ) +
    scale_color_manual(values =  c("#471061", "#208f8d", "#cae11e"), 
                        labels = c("CAT 1",     "CAT 2",      "Placebo")) +
    scale_shape_manual(values = c(16, 15, 17),
                        labels = c("CAT 1", "CAT 2", "Placebo")) +
    labs(x = "Time [min]", y = "Concentration [μM]", title = "N-Acetylcysteine") +
    theme_graphpad()
nac_graph
ggsave("output/concentraciones_nac_new.png", nac_graph, width = 10, height = 7.5, dpi = 1000, units = "cm")

#box plot
nac_graph_boxplot <- ggplot() +
    geom_boxplot(data = nac, aes(x = time, y = mean_conc, group = interaction(time, cat), col = cat), size = 1) +
    # set colors
    scale_colour_manual(values = c("#6c0000", "#04176b"), 
                        labels = c("CAT 1", "CAT 2")) +
    labs(x = "Time [min]", y = "Concentration [μM]", title = "N-Acetylcysteine") +
    # change font size
    scale_x_continuous(breaks = seq(0, 180, 30)) +

    theme_bw()+
    theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=8),
        axis.text = element_text(size = 8),
        plot.title = element_text(size = 12))
nac_graph_boxplot

# Plot mean and standard deviation for DFO mean_conc over time grouped by cat1, cat2 and p
dfo_graph <- ggplot() +
    # geom_point(data = dfo, aes(x = time, y = mean_conc, col = cat)) +
    # add mean and standard deviation
    stat_summary(data = dfo, aes(x = time, y = mean_conc, col = cat), fun.y = mean, geom = "line", size = 1) +
    stat_summary(data = dfo, aes(x = time, y = mean_conc, col = cat, shape = cat), fun.y = mean, geom = "point", size = 3) +
    stat_summary(data = dfo, aes(x = time, y = mean_conc, col = cat), fun.data = mean_sdl, fun.args = list(mult=1), geom = "errorbar", width = 6) +
    scale_x_continuous(breaks = seq(0, 180, 30), 
                        minor_breaks = seq(0, 180, 15), 
                        guide = "axis_minor",
                        expand = expansion(mult = c(0.01, 0.05))
                        ) +
    scale_y_continuous(limits = c(0, 12), 
                        breaks = seq(0, 12, 2), 
                        minor_breaks = seq(0, 12, 1),
                        guide = "axis_minor",
                        expand = expansion(mult = c(0.02, 0.006))
                       ) +
    scale_color_manual(values =  c("#471061", "#208f8d", "#cae11e"), 
                        labels = c("CAT 1",     "CAT 2",      "Placebo")) +
    scale_shape_manual(values = c(16, 15, 17),
                        labels = c("CAT 1", "CAT 2", "Placebo")) +
    labs(x = "Time [min]", y = "Concentration [μM]", title = "Deferoxamine") +
    theme_graphpad()
dfo_graph

ggsave("C:/Users/sanma/Desktop/concentraciones_dfo_new.png", dfo_graph, width = 8.4, height = 6, dpi = 1000, units = "cm")
#box plot
dfo_graph_boxplot <- ggplot() +
    geom_boxplot(data = dfo, aes(x = time, y = mean_conc, group = interaction(time, cat), col = cat), size = 1) +
    # set colors
    scale_colour_manual(values = c("#6c0000", "#04176b"), 
                        labels = c("CAT 1", "CAT 2")) +
    labs(x = "Time [min]", y = "Concentration [μM]", title = "Deferoxamine") +
    # change font size
    scale_x_continuous(breaks = seq(0, 180, 30)) +

    theme_bw()+
    theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=8),
        axis.text = element_text(size = 8),
        plot.title = element_text(size = 12))
dfo_graph_boxplot

# facet with the three graphs
# install.packages("gridExtra")
library(gridExtra)
all_plots <- grid.arrange(asc_graph, nac_graph, dfo_graph, ncol = 3)
all_plots
# library to export pdf with verdana
install.packages("extrafont")
library(extrafont)
# export pdf
ggsave("C:/Users/sanma/Desktop/concentraciones_todas_new.png", all_plots, width = 174, height = 70, units = "mm", dpi = 1000)
ggsave("C:/Users/sanma/Desktop/concentraciones_todas.pdf", all_plots, width = 174, height = 70, units = "mm")
#change letters size
#all_plots <- all_plots + theme(text = element_text(size=12))
#all_plots
# save the grid.arrange plots
ggsave("output/concentraciones.png", all_plots, width = 12.5, height = 4, dpi = 1000)
all_boxplots <- grid.arrange(asc_graph_boxplot, nac_graph_boxplot, dfo_graph_boxplot, ncol = 3)
all_boxplots_vert <- grid.arrange(asc_graph_boxplot, nac_graph_boxplot, dfo_graph_boxplot, nrow = 3)
ggplot(all_boxplots)

# save the grid.arrange plots
ggsave("output/concentraciones_boxplot.png", all_boxplots, width = 12.5, height = 4, dpi = 1000)
ggsave("output/concentraciones_boxplot_vert.png", all_boxplots_vert, width = 7.5, height = 10, dpi = 1000)
