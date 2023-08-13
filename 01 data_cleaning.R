library(tidyr)
library(dplyr)
library(ggplot2)

# import data and create column with patient id and sample number. Then delete "patient_sample" column
asc <- read.csv("raw data/asc.csv", header = TRUE, sep = ",") %>%
        pivot_longer(cols = -c(time), values_to = "concentration", names_to = "patient_sample") %>%
            mutate(px = strtrim(patient_sample, 3)) %>%
                mutate(sample = substring(patient_sample, 5, 5))
asc <- asc[, -2]
View(asc)
nac <- read.csv("raw data/nac.csv", header = TRUE, sep = ",") %>%
        pivot_longer(cols = -c(time), values_to = "concentration", names_to = "patient_sample") %>%
            mutate(px = strtrim(patient_sample, 3)) %>%
                mutate(sample = substring(patient_sample, 5, 5))
nac <- nac[, -2]
View(nac)
dfo <- read.csv("raw data/dfo.csv", header = TRUE, sep = ",") %>%
        pivot_longer(cols = -c(time), values_to = "concentration", names_to = "patient_sample") %>%
            mutate(px = strtrim(patient_sample, 3)) %>%
                mutate(sample = substring(patient_sample, 5, 5))
dfo <- dfo[, -2]
View(dfo)

# # delete rows with "Invalid Number" values from concentration column
# asc <- asc %>%
#         filter(concentration != "Invalid Number")
# View(asc)
# nac <- nac %>%
#         filter(concentration != "Invalid Number")
# View(nac)
# if invalid number is in concentration column, change it to NA
asc <- asc %>%
        mutate(concentration = ifelse(concentration == "Invalid Number", NA, concentration))
View(asc)
nac <- nac %>%
        mutate(concentration = ifelse(concentration == "Invalid Number", NA, concentration))
View(nac)
dfo <- dfo %>%
        mutate(concentration = ifelse(concentration == "Invalid Number", NA, concentration))
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
    geom_point(data = all_data, aes(x = time, y = concentration, colour = sample), alpha = 0.3, size = 2) +
    facet_grid(cols = vars(px), rows = vars(drug), scales = "free") +
    theme_bw()
#    theme_graphpad()
ggsave("todas_muestras_asc_nac_dfo.pdf", width = 70, height = 20, units = "cm")
 
# add covariates columns
covariates <- read.csv("raw data/covariates.csv", header = TRUE, sep = ",")

asc_covariates <- asc %>%
    left_join(covariates, by = "px")
View(asc_covariates)
nac_covariates <- nac %>%
    left_join(covariates, by = "px")
View(nac_covariates)
dfo_covariates <- dfo %>%
    left_join(covariates, by = "px")
View(dfo_covariates)
write.csv(asc_covariates, "output/covariates_asc.csv", row.names = FALSE)
write.csv(nac_covariates, "output/covariates_nac.csv", row.names = FALSE)
write.csv(dfo_covariates, "output/covariates_dfo.csv", row.names = FALSE)

# summary of data by patient and graphs

mean_asc <- asc_covariates %>%
    group_by(time, px, cat, sex, age, body_weight, height, bmi, tbq, oh, oh_units_week) %>%
        summarise(mean_conc = mean(concentration, na.rm = TRUE), .groups = "keep")
# reorder columns, first time, then px, then mean_conc
mean_asc <- mean_asc[, c(1, 2, 12, 3:11)]
View(mean_asc)
ggplot()+
    geom_line(data = mean_asc, aes(x = time, y = mean_conc, colour = px)) +
    # add geom point with different colors for cat column
    geom_point(data = mean_asc, aes(x = time, y = mean_conc, shape = cat), alpha = 1, size = 2) +
    scale_x_continuous(breaks = seq(0, 180, 60),
                        minor_breaks = seq(0, 180, 15)) +
    theme_bw()+
    theme_graphpad()

mean_nac <- nac_covariates %>%
    group_by(time, px, cat, sex, age, body_weight, height, bmi, tbq, oh, oh_units_week) %>%
        summarise(mean_conc = mean(concentration, na.rm = TRUE), .groups = "drop")
mean_nac <- mean_nac[, c(1, 2, 12, 3:11)]

ggplot()+
    geom_line(data = mean_nac, aes(x = time, y = mean_conc, colour = px)) +
    geom_point(data = mean_nac, aes(x = time, y = mean_conc, shape = cat), alpha = 1, size = 2) +
    scale_x_continuous(breaks = seq(0, 180, 60),
                        minor_breaks = seq(0, 180, 15)) +
    theme_bw()

mean_dfo <- dfo_covariates %>%
    group_by(time, px, cat, sex, age, body_weight, height, bmi, tbq, oh, oh_units_week) %>%
        summarise(mean_conc = mean(concentration, na.rm = TRUE), .groups = "drop")  
mean_dfo <- mean_dfo[, c(1, 2, 12, 3:11)]
ggplot()+   
    geom_line(data = mean_dfo, aes(x = time, y = mean_conc, colour = px)) +
    geom_point(data = mean_dfo, aes(x = time, y = mean_conc, shape = cat), alpha = 1, size = 2) +
    scale_x_continuous(breaks = seq(0, 180, 60),
                        minor_breaks = seq(0, 180, 15)) +
    theme_bw()
write.csv(mean_asc, "output/cv_mean_asc.csv", row.names = FALSE)
write.csv(mean_nac, "output/cv_mean_nac.csv", row.names = FALSE)
write.csv(mean_dfo, "output/cv_mean_dfo.csv", row.names = FALSE)

# corrected asc by baseline concentrations at time 0

asc_corrected <- read.csv("output/cv_mean_asc.csv", header = TRUE, sep = ",")
for(i in unique(asc_corrected$px)){
    asc_corrected[asc_corrected$px == i, "mean_conc"] <- asc_corrected[asc_corrected$px == i, "mean_conc"] - asc_corrected[asc_corrected$px == i & asc_corrected$time == 0, "mean_conc"]
}
tibble(asc_corrected)
View(asc_corrected)
write.csv(asc_corrected, "output/cv_mean_asc_corrected.csv", row.names = FALSE)

# wide for graphpad

# delete columns from 4 to 13
# asc
asc_wide <- mean_asc[, -c(4:12)]


# pivot wider to have one column per patient
asc_wide <- asc_wide %>%
    pivot_wider(names_from = px, values_from = mean_conc)

# nac
nac_wide <- mean_nac[, -c(4:12)]
nac_wide <- nac_wide %>%
    pivot_wider(names_from = px, values_from = mean_conc)
#dfo
dfo_wide <- mean_dfo[, -c(4:12)]
dfo_wide <- dfo_wide %>%
    pivot_wider(names_from = px, values_from = mean_conc)

write.csv(asc_wide, "output/asc_wide.csv", row.names = FALSE)
write.csv(nac_wide, "output/nac_wide.csv", row.names = FALSE)
write.csv(dfo_wide, "output/dfo_wide.csv", row.names = FALSE)

