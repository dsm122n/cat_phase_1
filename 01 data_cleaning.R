library(tidyr)
library(dplyr)
library(ggplot2)
install.packages("ggthemes")
install.packages("ggh4x")
library(ggthemes)
library(ggh4x)
# oxidative stress

oe <- tibble(read.csv("raw data/oxidative_stress_long.csv", header = TRUE, sep = ","))
oe <- oe[, -3]
# order by ascendig time
oe <- oe[order(oe$time), ]
View(oe)
# import data and create column with patient id and sample number. Then delete "patient_sample" column
asc <- read.csv("raw data/asc_raw.csv", header = TRUE, sep = ",") %>%
        pivot_longer(cols = -c(time), values_to = "concentration", names_to = "patient_sample") %>%
            mutate(id = as.integer(substring(patient_sample, 2, 3))) %>%
                mutate(sample = as.integer(substring(patient_sample, 5, 5)))
asc <- asc[, -2]
View(asc)
nac <- read.csv("raw data/nac_raw.csv", header = TRUE, sep = ",") %>%
        pivot_longer(cols = -c(time), values_to = "concentration", names_to = "patient_sample") %>%
            mutate(id = as.integer(substring(patient_sample, 2, 3))) %>%
                mutate(sample = as.integer(substring(patient_sample, 5, 5)))
nac <- nac[, -2]
View(nac)
dfo <- read.csv("raw data/dfo_raw.csv", header = TRUE, sep = ",") %>%
        pivot_longer(cols = -c(time), values_to = "concentration", names_to = "patient_sample") %>%
            mutate(id = as.integer(substring(patient_sample, 2, 3))) %>%
                mutate(sample = as.integer(substring(patient_sample, 5, 5)))
dfo <- dfo[, -2]
View(dfo)

# ad drug column
asc_drug <- asc %>%
    mutate(drug = "asc")
nac_drug <- nac %>%  
    mutate(drug = "nac")
dfo_drug <- dfo %>%  
    mutate(drug = "dfo")
# bind all data
all_data <- bind_rows(asc_drug, nac_drug, dfo_drug, .id = NULL)
# graph individual data

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


ggplot() +
    stat_summary(data = all_data, aes(x = time, y = concentration), fun = "mean", geom = "line", size = 1) +
    # stat_summary(data = all_data, aes(x = time, y = concentration), fun = "sd", geom = "errorbar", size = 0.25) +
    geom_point(data = all_data, aes(x = time, y = concentration, colour = as.character(sample)), alpha = 0.3, size = 2) +
    facet_grid(cols = vars(id), rows = vars(drug), scales = "free") +
    theme_bw()
ggsave("raw data/drugs_concentration_visualization.pdf", width = 70, height = 20, units = "cm")
 
# Visual inspection and deleting inadequate data
delete_inadequate <- read.csv("raw data/deleting data.csv", header = TRUE, sep = ",")
delete_inadequate_nac <- delete_inadequate[delete_inadequate$drug == "nac", ]
delete_inadequate_dfo <- delete_inadequate[delete_inadequate$drug == "dfo", ]
nac_clean <- tibble(nac_drug)
nac_clean <- anti_join(nac_clean, delete_inadequate_nac, by = c("id", "sample", "time"))
nac_clean <- filter(nac_clean, concentration >= 0)
View(nac_clean)
dfo_clean <- tibble(dfo_drug)
dfo_clean <- anti_join(dfo_clean, delete_inadequate_dfo, by = c("id", "sample", "time"))
dfo_clean <- filter(dfo_clean, concentration >= 0)
View(dfo_clean)
all_data_clean <- bind_rows(asc_drug, nac_clean, dfo_clean, .id = NULL) 
ggplot() +
    stat_summary(data = all_data_clean, aes(x = time, y = concentration), fun = "mean", geom = "line", size = 1) +
    # stat_summary(data = all_data, aes(x = time, y = concentration), fun = "sd", geom = "errorbar", size = 0.25) +
    geom_point(data = all_data_clean, aes(x = time, y = concentration, colour = as.character(sample)), alpha = 0.3, size = 2) +
    facet_grid(cols = vars(id), rows = vars(drug), scales = "free") +
    theme_bw()+
    theme(legend.title = element_blank())
ggsave("raw data/drugs_concentration_visualization_clean.pdf", width = 70, height = 20, units = "cm")



# add covariates columns
covariates <- read.csv("raw data/covariates.csv", header = TRUE, sep = ",")

asc_covariates <- asc_drug %>%
    left_join(covariates, by = "id")
View(asc_covariates)
nac_covariates <- nac_clean %>%
    left_join(covariates, by = "id")
View(nac_covariates)
dfo_covariates <- dfo_clean %>%
    left_join(covariates, by = "id")
View(dfo_covariates)
write.csv(asc_covariates, "output/covariates_asc.csv", row.names = FALSE)
write.csv(nac_covariates, "output/covariates_nac.csv", row.names = FALSE)
write.csv(dfo_covariates, "output/covariates_dfo.csv", row.names = FALSE)

# summary of data by patient and graphs

mean_asc <- asc_covariates %>%
    group_by(time, id, cat, sex, age, body_weight, height, bmi, tbq, oh, oh_units_week) %>%
        summarise(mean_conc = mean(concentration, na.rm = TRUE), .groups = "keep")
# reorder columns, first time, then id, then mean_conc
mean_asc <- mean_asc[, c(1, 2, 12, 3:11)]
View(mean_asc)
ggplot()+
    geom_line(data = mean_asc, aes(x = time, y = mean_conc, colour = as.character(id))) +
    # add geom point with different colors for cat column
    geom_point(data = mean_asc, aes(x = time, y = mean_conc, shape = cat), alpha = 1, size = 2) +
    scale_x_continuous(breaks = seq(0, 180, 30),
                        minor_breaks = seq(0, 180, 15)) +
    theme_bw()
    # theme_graphpad()

mean_nac <- nac_covariates %>%
    group_by(time, id, cat, sex, age, body_weight, height, bmi, tbq, oh, oh_units_week) %>%
        summarise(mean_conc = mean(concentration, na.rm = TRUE), .groups = "drop")
mean_nac <- mean_nac[, c(1, 2, 12, 3:11)]

ggplot()+
    geom_line(data = mean_nac, aes(x = time, y = mean_conc, colour = as.character(id))) +
    geom_point(data = mean_nac, aes(x = time, y = mean_conc, shape = cat), alpha = 1, size = 2) +
    scale_x_continuous(breaks = seq(0, 180, 60),
                        minor_breaks = seq(0, 180, 15)) +
    theme_bw()

mean_dfo <- dfo_covariates %>%
    group_by(time, id, cat, sex, age, body_weight, height, bmi, tbq, oh, oh_units_week) %>%
        summarise(mean_conc = mean(concentration, na.rm = TRUE), .groups = "drop")  
mean_dfo <- mean_dfo[, c(1, 2, 12, 3:11)]
View(mean_dfo)
ggplot()+   
    geom_line(data = mean_dfo, aes(x = time, y = mean_conc, colour = as.character(id))) +
    geom_point(data = mean_dfo, aes(x = time, y = mean_conc, shape = cat), alpha = 1, size = 2) +
    scale_x_continuous(breaks = seq(0, 120, 60),
                        minor_breaks = seq(0, 120, 15)) +
    theme_bw()
write.csv(mean_asc, "clean data/cv_mean_asc.csv", row.names = FALSE)
write.csv(mean_nac, "clean data/cv_mean_nac.csv", row.names = FALSE)
write.csv(mean_dfo, "clean data/cv_mean_dfo.csv", row.names = FALSE)

# corrected asc by baseline concentrations at time 0

asc_corrected <- read.csv("clean data/cv_mean_asc.csv", header = TRUE, sep = ",")
for(i in unique(asc_corrected$id)){
    asc_corrected[asc_corrected$id == i, "mean_conc"] <- asc_corrected[asc_corrected$id == i, "mean_conc"] - asc_corrected[asc_corrected$id == i & asc_corrected$time == 0, "mean_conc"]
}
tibble(asc_corrected)
View(asc_corrected)
write.csv(asc_corrected, "clean data/cv_mean_asc_corrected.csv", row.names = FALSE)

# FULL DATABASE
# joining all data os, asc, nac, dfo
asc_join <- mean_asc %>%
    rename(asc = "mean_conc")
nac_join <- mean_nac[, c(1:3)] %>%
    rename(nac = "mean_conc")
dfo_join <- mean_dfo[, c(1:3)] %>%
    rename(dfo = "mean_conc")
all_join <- left_join(asc_join, nac_join, by = c("time", "id")) %>%
    left_join(dfo_join, by = c("time", "id")) %>%
        left_join(oe, by = c("time", "id"))

all_join <- all_join[, c(1:3, 13, 14, 15, 16, 17, 4:12)]
View(all_join)

write.csv(all_join, "clean data/all_data_long.csv", row.names = TRUE)


# wide for graphpad

# delete columns from 4 to 13
# asc
asc_wide <- mean_asc[, -c(4:12)]


# pivot wider to have one column per patient
asc_wide <- asc_wide %>%
    pivot_wider(names_from = id, values_from = mean_conc)

# nac
nac_wide <- mean_nac[, -c(4:12)]
nac_wide <- nac_wide %>%
    pivot_wider(names_from = id, values_from = mean_conc)
#dfo
dfo_wide <- mean_dfo[, -c(4:12)]
dfo_wide <- dfo_wide %>%
    pivot_wider(names_from = id, values_from = mean_conc)

write.csv(asc_wide, "output/asc_wide.csv", row.names = FALSE)
write.csv(nac_wide, "output/nac_wide.csv", row.names = FALSE)
write.csv(dfo_wide, "output/dfo_wide.csv", row.names = FALSE)

