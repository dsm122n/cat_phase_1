library(dplyr)
library(tibble)
frap <- read.csv("raw data/os_frap.csv", header = TRUE, sep = ",") %>%
        pivot_longer(cols = -c(time), values_to = "frap", names_to = "px")
f2iso <- read.csv("raw data/os_f2iso.csv", header = TRUE, sep = ",") %>%
        pivot_longer(cols = -c(time), values_to = "f2iso", names_to = "px")
au <- read.csv("raw data/os_au.csv", header = TRUE, sep = ",") %>%
        pivot_longer(cols = -c(time), values_to = "au", names_to = "px")

write.csv(frap, "output/oxidative_stress/os_frap_long.csv")
write.csv(f2iso, "output/oxidative_stress/os_f2iso_long.csv")
write.csv(au, "output/oxidative_stress/os_au_long.csv")


# dataset with all data binding columns
asc <- tibble(read.csv("output/cv_mean_asc.csv", header = TRUE, sep = ",")) %>%
    dplyr::select(time, px, mean_conc, cat) %>%
        rename(asc = mean_conc)
View(asc)
nac <- tibble(read.csv("output/cv_mean_nac.csv", header = TRUE, sep = ","))  %>%
    dplyr::select(time, px, mean_conc)  %>%
        rename(nac = mean_conc)
dfo <- tibble(read.csv("output/cv_mean_dfo.csv", header = TRUE, sep = ",")) %>%
    dplyr::select(time, px, mean_conc, sex, age, body_weight, height, bmi, tbq, oh, oh_units_week) %>%
        rename(dfo = mean_conc)

all_data <- right_join(asc, nac, by = c("time", "px")) %>%
            right_join(dfo, by = c("time", "px"))
# full join
all_data <- full_join(all_data, frap, by = c("time", "px")) %>%
            full_join(f2iso, by = c("time", "px")) %>%
            full_join(au, by = c("time", "px"))


all_data <- right_join(all_data, frap, by = c("time", "px")) %>%
            right_join(f2iso, by = c("time", "px")) %>%
            right_join(au, by = c("time", "px"))
# reorder all_data columns bringing frap, f2iso and au to the begining
all_data <- all_data[, c(1, 2, 4, 3, 5, 6, 15, 16, 17, 7:14)]

View(all_data)

write.csv(all_data, "output/all_data_long.csv", row.names = FALSE)
