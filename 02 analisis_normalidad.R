library(tibble)

#import data
asc <- tibble(read.csv("output/cv_mean_asc.csv"))
dfo <- tibble(read.csv("output/cv_mean_dfo.csv"))
nac <- tibble(read.csv("output/cv_mean_nac.csv"))

#shapiro wilk test for normality for each time cat1 and cat2 together
shapiro_pvalue_asc <- tibble(time = c(15, 30, 60, 90, 120, 180), pvalue_asc = c(0))
shapiro_pvalue_dfo <- tibble(time = c(15, 30, 60, 90, 120, 180), pvalue_dfo = c(0))
shapiro_pvalue_nac <- tibble(time = c(15, 30, 60, 90, 120, 180), pvalue_nac = c(0))
for (i in c(15, 30, 60, 90, 120, 180)) {
    shapiro_pvalue_asc$pvalue_asc[shapiro_pvalue_asc$time == i] <- shapiro.test(na.omit(asc$mean_conc[asc$time == i]))$p.value
    shapiro_pvalue_dfo$pvalue_dfo[shapiro_pvalue_dfo$time == i] <- shapiro.test(na.omit(dfo$mean_conc[dfo$time == i]))$p.value
    shapiro_pvalue_nac$pvalue_nac[shapiro_pvalue_nac$time == i] <- shapiro.test(na.omit(nac$mean_conc[nac$time == i]))$p.value

}
asc[1,2]
#shapiro_pvalue_asc
todos <- cbind(shapiro_pvalue_asc, shapiro_pvalue_dfo, shapiro_pvalue_nac)
todos <- todos[, -c(3,5)]
#convert to tibble 
todos <- as.tibble(todos)
todos
#delete columns of time
todos
# mark the significant values
todos$asc <- ifelse(todos$pvalue_asc < 0.05, "sig", "no_sig")
todos$dfo <- ifelse(todos$pvalue_dfo < 0.05, "sig", "no_sig")
todos$nac <- ifelse(todos$pvalue_nac < 0.05, "sig", "no_sig")
todos
#reorder the columns
todos <- todos[, c(1, 2, 5, 3, 6, 4, 7)]
todos

#shapiro wilk test for normality for each time, filtered by "cat" column
# cat1
(asc_cat1 <- asc[asc$cat == "cat1" & is.na(asc$mean_conc) == FALSE, ])
(dfo_cat1 <- dfo[dfo$cat == "cat1" & is.na(dfo$mean_conc) == FALSE, ])
(nac_cat1 <- nac[nac$cat == "cat1" & is.na(nac$mean_conc) == FALSE, ])
shapiro_pvalue_asc_cat1 <- tibble(time = c(15, 30, 60, 90, 120), pvalue_asc = c(0))
shapiro_pvalue_dfo_cat1 <- tibble(time = c(15, 30, 60, 90, 120), pvalue_dfo = c(0))
shapiro_pvalue_nac_cat1 <- tibble(time = c(15, 30, 60, 90, 120), pvalue_nac = c(0))


for (i in c(15, 30, 60, 90, 120)) {
  if (sum(!is.na(asc_cat1$mean_conc[asc_cat1$time == i])) >= 3) {
    shapiro_pvalue_asc_cat1$pvalue_asc[shapiro_pvalue_asc_cat1$time == i] <- shapiro.test(na.omit(asc_cat1$mean_conc[asc_cat1$time == i]))$p.value
  }
  
  if (sum(!is.na(dfo_cat1$mean_conc[dfo_cat1$time == i])) >= 3) {
    shapiro_pvalue_dfo_cat1$pvalue_dfo[shapiro_pvalue_dfo_cat1$time == i] <- shapiro.test(na.omit(dfo_cat1$mean_conc[dfo_cat1$time == i]))$p.value
  }
  
  if (sum(!is.na(nac_cat1$mean_conc[nac_cat1$time == i])) >= 3) {
    shapiro_pvalue_nac_cat1$pvalue_nac[shapiro_pvalue_nac_cat1$time == i] <- shapiro.test(na.omit(nac_cat1$mean_conc[nac_cat1$time == i]))$p.value
  }
}

todos_cat1 <- cbind(shapiro_pvalue_asc_cat1, shapiro_pvalue_dfo_cat1, shapiro_pvalue_nac_cat1)
todos_cat1 <- todos_cat1[, -c(3,5)]
#convert to tibble 
todos_cat1 <- as.tibble(todos_cat1)
todos_cat1
# mark the significant values
todos_cat1$asc <- ifelse(todos_cat1$pvalue_asc < 0.05, "sig", "no_sig")
todos_cat1$dfo <- ifelse(todos_cat1$pvalue_dfo < 0.05, "sig", "no_sig")
todos_cat1$nac <- ifelse(todos_cat1$pvalue_nac < 0.05, "sig", "no_sig")
todos_cat1
#reorder the columns
todos_cat1 <- todos_cat1[, c(1, 2, 5, 3, 6, 4, 7)]
todos_cat1
# transfer data to .csv file in output/normalidad folder
write.csv(todos_cat1, file = "output/normalidad/todos_cat1.csv", row.names = FALSE)

#shapiro wilk test for cat2
(asc_cat2 <- asc[asc$cat == "cat2" & is.na(asc$mean_conc) == FALSE, ])
(dfo_cat2 <- dfo[dfo$cat == "cat2" & is.na(dfo$mean_conc) == FALSE, ])
(nac_cat2 <- nac[nac$cat == "cat2" & is.na(nac$mean_conc) == FALSE, ])
shapiro_pvalue_asc_cat2 <- tibble(time = c(15, 30, 60, 90, 120), pvalue_asc = c(0))
shapiro_pvalue_dfo_cat2 <- tibble(time = c(15, 30, 60, 90, 120), pvalue_dfo = c(0))
shapiro_pvalue_nac_cat2 <- tibble(time = c(15, 30, 60, 90, 120), pvalue_nac = c(0))

for (i in c(15, 30, 60, 90, 120)) {
  if (sum(!is.na(asc_cat2$mean_conc[asc_cat2$time == i])) >= 3) {
    shapiro_pvalue_asc_cat2$pvalue_asc[shapiro_pvalue_asc_cat2$time == i] <- shapiro.test(na.omit(asc_cat2$mean_conc[asc_cat2$time == i]))$p.value
  }
  
  if (sum(!is.na(dfo_cat2$mean_conc[dfo_cat2$time == i])) >= 3) {
    shapiro_pvalue_dfo_cat2$pvalue_dfo[shapiro_pvalue_dfo_cat2$time == i] <- shapiro.test(na.omit(dfo_cat2$mean_conc[dfo_cat2$time == i]))$p.value
  }
  
  if (sum(!is.na(nac_cat2$mean_conc[nac_cat2$time == i])) >= 3) {
    shapiro_pvalue_nac_cat2$pvalue_nac[shapiro_pvalue_nac_cat2$time == i] <- shapiro.test(na.omit(nac_cat2$mean_conc[nac_cat2$time == i]))$p.value
  }
}

todos_cat2 <- cbind(shapiro_pvalue_asc_cat2, shapiro_pvalue_dfo_cat2, shapiro_pvalue_nac_cat2)
todos_cat2 <- todos_cat2[, -c(3,5)]
#convert to tibble
todos_cat2 <- as.tibble(todos_cat2)
todos_cat2
# mark the significant values
todos_cat2$asc <- ifelse(todos_cat2$pvalue_asc < 0.05, "sig", "no_sig")
todos_cat2$dfo <- ifelse(todos_cat2$pvalue_dfo < 0.05, "sig", "no_sig")
todos_cat2$nac <- ifelse(todos_cat2$pvalue_nac < 0.05, "sig", "no_sig")
todos_cat2
#reorder the columns
todos_cat2 <- todos_cat2[, c(1, 2, 5, 3, 6, 4, 7)]
todos_cat2
# transfer data to .csv file in output/normalidad folder
write.csv(todos_cat2, file = "output/normalidad/todos_cat2.csv", row.names = FALSE)

#qqplot for cat1 at each time point
# save all the plots in one pdf file
pdf("output/normalidad/qqplot_cat1_cat2.pdf")
# qqplots for cat1 at each time point
#asc
asc_cat1_qqplot <- asc[asc$cat == "cat1" & is.na(asc$mean_conc) == FALSE, ]
asc_cat1_qqplot <- as.data.frame(asc_cat1_qqplot[order(asc_cat1_qqplot$time), ])
for (i in c(15, 30, 60, 90, 120, 180)) {
    asc_cat1_qqplot_time <- asc_cat1_qqplot[asc_cat1_qqplot$time == i, "mean_conc"]
    qqnorm(asc_cat1_qqplot_time, main = paste("asc_cat1_qqplot", i))
    qqline(asc_cat1_qqplot_time)
  
}

#dfo
dfo_cat1_qqplot <- dfo[dfo$cat == "cat1" & is.na(dfo$mean_conc) == FALSE, ]
dfo_cat1_qqplot <- as.data.frame(dfo_cat1_qqplot[order(dfo_cat1_qqplot$time), ])
for (i in c(15, 30, 60, 90, 120, 180)) {
    dfo_cat1_qqplot_time <- dfo_cat1_qqplot[dfo_cat1_qqplot$time == i, "mean_conc"]
    qqnorm(dfo_cat1_qqplot_time, main = paste("dfo_cat1_qqplot", i))
    qqline(dfo_cat1_qqplot_time)
  
}

#nac
nac_cat1_qqplot <- nac[nac$cat == "cat1" & is.na(nac$mean_conc) == FALSE, ]
nac_cat1_qqplot <- as.data.frame(nac_cat1_qqplot[order(nac_cat1_qqplot$time), ])
for (i in c(15, 30, 60, 90, 120, 180)) {
    nac_cat1_qqplot_time <- nac_cat1_qqplot[nac_cat1_qqplot$time == i, "mean_conc"]
    qqnorm(nac_cat1_qqplot_time, main = paste("nac_cat1_qqplot", i))
    qqline(nac_cat1_qqplot_time)
  
}

#qqplot for cat2 at each time point
#asc
asc_cat2_qqplot <- asc[asc$cat == "cat2" & is.na(asc$mean_conc) == FALSE, ]
asc_cat2_qqplot <- as.data.frame(asc_cat2_qqplot[order(asc_cat2_qqplot$time), ])
for (i in c(15, 30, 60, 90, 120, 180)) {
    asc_cat2_qqplot_time <- asc_cat2_qqplot[asc_cat2_qqplot$time == i, "mean_conc"]
    qqnorm(asc_cat2_qqplot_time, main = paste("asc_cat2_qqplot", i))
    qqline(asc_cat2_qqplot_time)
  
}

#dfo
dfo_cat2_qqplot <- dfo[dfo$cat == "cat2" & is.na(dfo$mean_conc) == FALSE, ]
dfo_cat2_qqplot <- as.data.frame(dfo_cat2_qqplot[order(dfo_cat2_qqplot$time), ])
for (i in c(15, 30, 60, 90, 120, 180)) {
    dfo_cat2_qqplot_time <- dfo_cat2_qqplot[dfo_cat2_qqplot$time == i, "mean_conc"]
    qqnorm(dfo_cat2_qqplot_time, main = paste("dfo_cat2_qqplot", i))
    qqline(dfo_cat2_qqplot_time)
  
}

#nac
nac_cat2_qqplot <- nac[nac$cat == "cat2" & is.na(nac$mean_conc) == FALSE, ]
nac_cat2_qqplot <- as.data.frame(nac_cat2_qqplot[order(nac_cat2_qqplot$time), ])
for (i in c(15, 30, 60, 90, 120, 180)) {
    nac_cat2_qqplot_time <- nac_cat2_qqplot[nac_cat2_qqplot$time == i, "mean_conc"]
    qqnorm(nac_cat2_qqplot_time, main = paste("nac_cat2_qqplot", i))
    qqline(nac_cat2_qqplot_time)
  
}

#all times pooled

#asc
asc_cat1_qqplot <- asc[asc$cat == "cat1" & is.na(asc$mean_conc) == FALSE, ]
asc_cat1_qqplot <- as.data.frame(asc_cat1_qqplot[order(asc_cat1_qqplot$time), ])
asc_cat1_qqplot_time <- asc_cat1_qqplot[, "mean_conc"]
qqnorm(asc_cat1_qqplot_time, main = "asc_cat1_qqplot")
qqline(asc_cat1_qqplot_time)
#dfo
dfo_cat1_qqplot <- dfo[dfo$cat == "cat1" & is.na(dfo$mean_conc) == FALSE, ]
dfo_cat1_qqplot <- as.data.frame(dfo_cat1_qqplot[order(dfo_cat1_qqplot$time), ])
dfo_cat1_qqplot_time <- dfo_cat1_qqplot[, "mean_conc"]
qqnorm(dfo_cat1_qqplot_time, main = "dfo_cat1_qqplot")
qqline(dfo_cat1_qqplot_time)
#nac
nac_cat1_qqplot <- nac[nac$cat == "cat1" & is.na(nac$mean_conc) == FALSE, ]
nac_cat1_qqplot <- as.data.frame(nac_cat1_qqplot[order(nac_cat1_qqplot$time), ])
nac_cat1_qqplot_time <- nac_cat1_qqplot[, "mean_conc"]
qqnorm(nac_cat1_qqplot_time, main = "nac_cat1_qqplot")
qqline(nac_cat1_qqplot_time)

#cat2
#asc
asc_cat2_qqplot <- asc[asc$cat == "cat2" & is.na(asc$mean_conc) == FALSE, ]
asc_cat2_qqplot <- as.data.frame(asc_cat2_qqplot[order(asc_cat2_qqplot$time), ])
asc_cat2_qqplot_time <- asc_cat2_qqplot[, "mean_conc"]
qqnorm(asc_cat2_qqplot_time, main = "asc_cat2_qqplot")
qqline(asc_cat2_qqplot_time)
#dfo
dfo_cat2_qqplot <- dfo[dfo$cat == "cat2" & is.na(dfo$mean_conc) == FALSE, ]
dfo_cat2_qqplot <- as.data.frame(dfo_cat2_qqplot[order(dfo_cat2_qqplot$time), ])
dfo_cat2_qqplot_time <- dfo_cat2_qqplot[, "mean_conc"]
qqnorm(dfo_cat2_qqplot_time, main = "dfo_cat2_qqplot")
qqline(dfo_cat2_qqplot_time)
#nac
nac_cat2_qqplot <- nac[nac$cat == "cat2" & is.na(nac$mean_conc) == FALSE, ]
nac_cat2_qqplot <- as.data.frame(nac_cat2_qqplot[order(nac_cat2_qqplot$time), ])
nac_cat2_qqplot_time <- nac_cat2_qqplot[, "mean_conc"]
qqnorm(nac_cat2_qqplot_time, main = "nac_cat2_qqplot")
qqline(nac_cat2_qqplot_time)


dev.off()



####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
##########shapiro test with standarized mean_conc over time z-distribution##########
##########shapiro test with standarized mean_conc over time z-distribution##########
##########shapiro test with standarized mean_conc over time z-distribution##########
##########shapiro test with standarized mean_conc over time z-distribution##########
##########shapiro test with standarized mean_conc over time z-distribution##########
##########shapiro test with standarized mean_conc over time z-distribution##########
##########shapiro test with standarized mean_conc over time z-distribution##########
##########shapiro test with standarized mean_conc over time z-distribution##########
##########shapiro test with standarized mean_conc over time z-distribution##########
##########shapiro test with standarized mean_conc over time z-distribution##########
##########shapiro test with standarized mean_conc over time z-distribution##########
##########shapiro test with standarized mean_conc over time z-distribution##########
####################################################################################
####################################################################################
####################################################################################
####################################################################################
#shapiro test with standarized mean_conc over time
#cat1
#asc
asc_cat1_shapiro <- asc[asc$cat == "cat1" & is.na(asc$mean_conc) == FALSE, ]
asc_cat1_shapiro <- as.data.frame(asc_cat1_shapiro[order(asc_cat1_shapiro$time), ])
#Vector containing all standarized mean_conc values
asc_cat1_shapiro_time <- c()
for (i in c(
  0, 15, 30, 60, 90, 120, 180
  )) {
    asc_vector <- asc_cat1_shapiro$mean_conc[asc_cat1_shapiro$time == i]
    asc_cat1_shapiro_time <- c(asc_cat1_shapiro_time, scale(asc_vector))
  
}
asc_cat1_shapiro_time
hist(asc_cat1_shapiro_time, main = "asc_cat1_shapiro")
#density plot
plot(density(asc_cat1_shapiro_time), main = "asc_cat1_shapiro")
#print shapiro test in previous plot as text
p_value <- shapiro.test(asc_cat1_shapiro_time)$p.value
text(0.5, 10, paste("p-value =", round(p_value, 4)), cex = 1.2)
#histogram
shapiro.test(asc_cat1_shapiro_time)

#dfo
dfo_cat1_shapiro <- dfo[dfo$cat == "cat1" & is.na(dfo$mean_conc) == FALSE, ]
dfo_cat1_shapiro <- as.data.frame(dfo_cat1_shapiro[order(dfo_cat1_shapiro$time), ])
#Vector containing all standarized mean_conc values
dfo_cat1_shapiro_time <- c()
for (i in c(
  15, 30, 60, 90, 120
  )) {
    dfo_vector <- dfo_cat1_shapiro$mean_conc[dfo_cat1_shapiro$time == i]
    dfo_cat1_shapiro_time <- c(dfo_cat1_shapiro_time, scale(dfo_vector))
  
}
#density plot
plot(density(dfo_cat1_shapiro_time), main = "dfo_cat1_shapiro")
#histogram
hist(dfo_cat1_shapiro_time, main = "dfo_cat1_shapiro")
shapiro.test(dfo_cat1_shapiro_time)

#nac
nac_cat1_shapiro <- nac[nac$cat == "cat1" & is.na(nac$mean_conc) == FALSE, ]
nac_cat1_shapiro <- as.data.frame(nac_cat1_shapiro[order(nac_cat1_shapiro$time), ])
#Vector containing all standarized mean_conc values
nac_cat1_shapiro_time <- c()
for (i in c(
  15, 30, 60, 90, 120
  )) {
    nac_vector <- nac_cat1_shapiro$mean_conc[nac_cat1_shapiro$time == i]
    nac_cat1_shapiro_time <- c(nac_cat1_shapiro_time, scale(nac_vector))
  
}
#density plot
plot(density(nac_cat1_shapiro_time), main = "nac_cat1_shapiro")
#histogram
hist(nac_cat1_shapiro_time, main = "nac_cat1_shapiro")
#shapiro test
shapiro.test(nac_cat1_shapiro_time)


#cat2
#asc
asc_cat2_shapiro <- asc[asc$cat == "cat2" & is.na(asc$mean_conc) == FALSE, ]
asc_cat2_shapiro <- as.data.frame(asc_cat2_shapiro[order(asc_cat2_shapiro$time), ])
#Vector containing all standarized mean_conc values
asc_cat2_shapiro_time <- c()
for (i in c(
  0, 15, 30, 60, 90, 120, 180
  )) {
    asc_vector <- asc_cat2_shapiro$mean_conc[asc_cat2_shapiro$time == i]
    asc_cat2_shapiro_time <- c(asc_cat2_shapiro_time, scale(asc_vector))
  
}
#density plot
plot(density(asc_cat2_shapiro_time), main = "asc_cat2_shapiro")
#histogram
hist(asc_cat2_shapiro_time, main = "asc_cat2_shapiro")
shapiro.test(asc_cat2_shapiro_time)

#dfo
dfo_cat2_shapiro <- dfo[dfo$cat == "cat2" & is.na(dfo$mean_conc) == FALSE, ]
dfo_cat2_shapiro <- as.data.frame(dfo_cat2_shapiro[order(dfo_cat2_shapiro$time), ])
#Vector containing all standarized mean_conc values
dfo_cat2_shapiro_time <- c()
for (i in c(
  15, 30, 60, 90, 120
  )) {
    dfo_vector <- dfo_cat2_shapiro$mean_conc[dfo_cat2_shapiro$time == i]
    dfo_cat2_shapiro_time <- c(dfo_cat2_shapiro_time, scale(dfo_vector))
  
}
#density plot
plot(density(dfo_cat2_shapiro_time), main = "dfo_cat2_shapiro")
#histogram
hist(dfo_cat2_shapiro_time, main = "dfo_cat2_shapiro")
shapiro.test(dfo_cat2_shapiro_time)

#nac
nac_cat2_shapiro <- nac[nac$cat == "cat2" & is.na(nac$mean_conc) == FALSE, ]
nac_cat2_shapiro <- as.data.frame(nac_cat2_shapiro[order(nac_cat2_shapiro$time), ])
#Vector containing all standarized mean_conc values
nac_cat2_shapiro_time <- c()
for (i in c(
  15, 30, 60, 90, 120
  )) {
    nac_vector <- nac_cat2_shapiro$mean_conc[nac_cat2_shapiro$time == i]
    nac_cat2_shapiro_time <- c(nac_cat2_shapiro_time, scale(nac_vector))
  
}
#density plot
plot(density(nac_cat2_shapiro_time), main = "nac_cat2_shapiro")
#histogram
hist(nac_cat2_shapiro_time, main = "nac_cat2_shapiro")
#shapiro test
shapiro.test(nac_cat2_shapiro_time)


# normalized z-distribution values for each cat are named "drug_cat#_shapiro_time"
##############################################
########box-cox transformation################
########box-cox transformation################
########box-cox transformation################
########box-cox transformation################
########box-cox transformation################
##############################################
##############################################
##############################################
pdf("output/normalidad/boxcox_cat1_cat2.pdf")
install.packages("MASS")
library(MASS)
#cat1
#asc
asc_boxcox <- asc_cat1_shapiro_time + 2
hist(asc_boxcox, main = "asc_boxcox")
#dfo
dfo_boxcox <- dfo_cat1_shapiro_time + 2
#nac
nac_boxcox <- nac_cat1_shapiro_time + 2
# perform box-cox transformation
#asc
MASS::boxcox(lm(asc_boxcox ~ 1))
MASS::boxcox(lm(nac_boxcox ~ 1))
MASS::boxcox(lm(dfo_boxcox ~ 1))
MASS::boxcox(lm(asc_boxcox ~ 1))$x[which.max(MASS::boxcox(lm(asc_boxcox ~ 1))$y)]
boxcox(asc_boxcox)
# obtain lambda
boxcox(asc_boxcox)$x[which.max(boxcox(asc_boxcox)$y)]

lambda_asc_boxcox <- boxcox(lm(asc_boxcox ~ 1))$x[which.max(boxcox(lm(asc_boxcox ~ 1))$y)]
install.packages("bestNormalize")
lambda <- 1
new_asc  <- (asc_boxcox^lambda - 1)/lambda

lambda <- 0.5
new_nac <- (nac_boxcox^lambda - 1)/lambda
shapiro.test(new_nac)
shapiro.test(nac_boxcox)
#shapiro_wilk test
shapiro.test(new_asc)
# density plot
plot(density(new_asc), main = "asc_boxcox")

# cat1
asc_boxcox <- asc_cat1_shapiro_time + 2
nac_boxcox <- nac_cat1_shapiro_time + 2
dfo_boxcox <- dfo_cat1_shapiro_time + 2
# perform box-cox transformation
boxcox(lm(asc_boxcox ~ 1))
boxcox(lm(nac_boxcox ~ 1))
boxcox(lm(dfo_boxcox ~ 1))

#asc
lambda <- boxcox(lm(asc_boxcox ~ 1))$x[which.max(boxcox(lm(asc_boxcox ~ 1))$y)]
new_asc  <- (asc_boxcox^lambda - 1)/lambda
shapiro.test(new_asc)
plot(density(new_asc), main = "asc_boxcox")
plot(density(asc_boxcox), main = "asc_boxcox")

#nac
lambda <- boxcox(lm(nac_boxcox ~ 1))$x[which.max(boxcox(lm(nac_boxcox ~ 1))$y)]
new_nac  <- (nac_boxcox^lambda - 1)/lambda
shapiro.test(new_nac)
plot(density(new_nac), main = "nac_boxcox")
plot(density(nac_boxcox), main = "nac_boxcox")

#dfo
lambda <- boxcox(lm(dfo_boxcox ~ 1))$x[which.max(boxcox(lm(dfo_boxcox ~ 1))$y)]
new_dfo  <- (dfo_boxcox^lambda - 1)/lambda
shapiro.test(new_dfo)
plot(density(new_dfo), main = "dfo_boxcox")
plot(density(dfo_boxcox), main = "dfo_boxcox")



# CAT2
asc_boxcox <- asc_cat2_shapiro_time + 2
nac_boxcox <- nac_cat2_shapiro_time + 2
dfo_boxcox <- dfo_cat2_shapiro_time + 2
# perform box-cox transformation
boxcox(lm(asc_boxcox ~ 1)
boxcox(lm(nac_boxcox ~ 1))
boxcox(lm(dfo_boxcox ~ 1))

#asc
lambda <- boxcox(lm(asc_boxcox ~ 1))$x[which.max(boxcox(lm(asc_boxcox ~ 1))$y)]
new_asc  <- (asc_boxcox^lambda - 1)/lambda
shapiro.test(new_asc)
plot(density(new_asc), main = "asc_boxcox")
plot(density(asc_boxcox), main = "asc_boxcox")

#nac
lambda <- boxcox(lm(nac_boxcox ~ 1))$x[which.max(boxcox(lm(nac_boxcox ~ 1))$y)]
new_nac  <- (nac_boxcox^lambda - 1)/lambda
shapiro.test(new_nac)
plot(density(new_nac), main = "nac_boxcox")
plot(density(nac_boxcox), main = "nac_boxcox")

#dfo
lambda <- boxcox(lm(dfo_boxcox ~ 1))$x[which.max(boxcox(lm(dfo_boxcox ~ 1))$y)]
new_dfo  <- (dfo_boxcox^lambda - 1)/lambda
shapiro.test(new_dfo)
plot(density(new_dfo), main = "dfo_boxcox")
plot(density(dfo_boxcox), main = "dfo_boxcox")

# save shapiro-wilk p-value of for CAT1 and CAT2 in one table

dev.off()

plot(density(asc_boxcox), main = "asc_boxcox")
plot(density(asc_cat2_shapiro_time), main = "asc_cat2_shapiro")
#qqplot
qqnorm(new_asc-2)
qqline(new_asc-2)
# add 45° reference line
abline(a = 0, b = 1, col = "#000c7b", lty = 2, lwd = 4)
qqnorm(asc_boxcox)
qqline(asc_boxcox)


# yeo johnson transformation
install.packages("car")
library(car)
install.packages("bestNormalize")
library(bestNormalize)

#save following graphs in one pdf file
pdf("output/normalized/yeojohnson_cat1_cat2.pdf")

#cat1
#asc
asc_yeojohnson <- asc_cat1_shapiro_time
#dfo
dfo_yeojohnson <- dfo_cat1_shapiro_time
#nac
nac_yeojohnson <- nac_cat1_shapiro_time

#asc
yeojohnson(asc_yeojohnson)
plot(density(yeojohnson(asc_yeojohnson)$x.t), main = "asc cat1 distribucion z normalizada por yeo johnson")
plot(density(asc_yeojohnson), main = "asc cat1 distribucion z sin normalizar")
hist(yeojohnson(asc_yeojohnson)$x.t)
hist(asc_yeojohnson)
shapiro.test(yeojohnson(asc_yeojohnson)$x.t)
shapiro.test(asc_yeojohnson)
lambda_asc_cat1 <- yeojohnson(asc_yeojohnson)$lambda
shapiro_pvalue_asc_cat1 <- shapiro.test(yeojohnson(asc_yeojohnson)$x.t)$p.value


#dfo
yeojohnson(dfo_yeojohnson)
plot(density(yeojohnson(dfo_yeojohnson)$x.t), main = "dfo cat1 distribucion z normalizada por yeo johnson")
plot(density(dfo_yeojohnson), main = "dfo cat1 distribucion z sin normalizar")
hist(yeojohnson(dfo_yeojohnson)$x.t)
hist(dfo_yeojohnson)
shapiro.test(yeojohnson(dfo_yeojohnson)$x.t)
shapiro.test(dfo_yeojohnson)
lambda_dfo_cat1 <- yeojohnson(dfo_yeojohnson)$lambda
shapiro_pvalue_dfo_cat1 <- shapiro.test(yeojohnson(dfo_yeojohnson)$x.t)$p.value
#nac
yeojohnson(nac_yeojohnson)
plot(density(yeojohnson(nac_yeojohnson)$x.t), main = "nac cat1 distribucion z normalizada por yeo johnson")
plot(density(nac_yeojohnson), main = "nac cat1 distribucion z sin normalizar")
hist(yeojohnson(nac_yeojohnson)$x.t)
hist(nac_yeojohnson)
shapiro.test(yeojohnson(nac_yeojohnson)$x.t)
shapiro.test(nac_yeojohnson)
lambda_nac_cat1 <- yeojohnson(nac_yeojohnson)$lambda
shapiro_pvalue_nac_cat1 <- shapiro.test(yeojohnson(nac_yeojohnson)$x.t)$p.value

#save lambda and p-value in one table
yeo_cat1 <- tibble(
  drug = c("asc", "dfo", "nac"),
  lambda = c(lambda_asc_cat1, lambda_dfo_cat1, lambda_nac_cat1),
  p_value = c(shapiro_pvalue_asc_cat1, shapiro_pvalue_dfo_cat1, shapiro_pvalue_nac_cat1),
  cat = c("cat1", "cat1", "cat1")
)

#cat2
#asc
asc_yeojohnson <- asc_cat2_shapiro_time
#dfo
dfo_yeojohnson <- dfo_cat2_shapiro_time
#nac
nac_yeojohnson <- nac_cat2_shapiro_time

#asc
yeojohnson(asc_yeojohnson)
plot(density(yeojohnson(asc_yeojohnson)$x.t), main = "asc cat2 distribucion z normalizada por yeo johnson")
plot(density(asc_yeojohnson), main = "asc cat2 distribucion z sin normalizar")
hist(yeojohnson(asc_yeojohnson)$x.t)
hist(asc_yeojohnson)
shapiro.test(yeojohnson(asc_yeojohnson)$x.t)
shapiro.test(asc_yeojohnson)
lambda_asc_cat2 <- yeojohnson(asc_yeojohnson)$lambda
shapiro_pvalue_asc_cat2 <- shapiro.test(yeojohnson(asc_yeojohnson)$x.t)$p.value


#dfo
yeojohnson(dfo_yeojohnson)
plot(density(yeojohnson(dfo_yeojohnson)$x.t), main = "dfo cat2 distribucion z normalizada por yeo johnson")
plot(density(dfo_yeojohnson), main = "dfo cat2 distribucion z sin normalizar")
hist(yeojohnson(dfo_yeojohnson)$x.t)
hist(dfo_yeojohnson)
shapiro.test(yeojohnson(dfo_yeojohnson)$x.t)
shapiro.test(dfo_yeojohnson)
lambda_dfo_cat2 <- yeojohnson(dfo_yeojohnson)$lambda
shapiro_pvalue_dfo_cat2 <- shapiro.test(yeojohnson(dfo_yeojohnson)$x.t)$p.value
#nac
yeojohnson(nac_yeojohnson)
plot(density(yeojohnson(nac_yeojohnson)$x.t), main = "nac cat2 distribucion z normalizada por yeo johnson")
plot(density(nac_yeojohnson), main = "nac cat2 distribucion z sin normalizar")
hist(yeojohnson(nac_yeojohnson)$x.t)
hist(nac_yeojohnson)
shapiro.test(yeojohnson(nac_yeojohnson)$x.t)
shapiro.test(nac_yeojohnson)
lambda_nac_cat2 <- yeojohnson(nac_yeojohnson)$lambda
shapiro_pvalue_nac_cat2 <- shapiro.test(yeojohnson(nac_yeojohnson)$x.t)$p.value

#save lambda and p-value in one table
yeo_cat2 <- tibble(
  drug = c("asc", "dfo", "nac"),
  lambda = c(lambda_asc_cat2, lambda_dfo_cat2, lambda_nac_cat2),
  p_value = c(shapiro_pvalue_asc_cat2, shapiro_pvalue_dfo_cat2, shapiro_pvalue_nac_cat2),
    cat = c("cat2", "cat2", "cat2")
)

# yeo cat1 y cat2 in one table
yeo_cat1_cat2 <- rbind(yeo_cat1, yeo_cat2)

#save pdf
dev.off()



######################################
######################################
######################################
######################################
######################################

# save the following plots in unique pdf
pdf("output/normalidad/density_shapiro_qqplots standarized data.pdf")

# shapiro test for both cat1 and cat2 pooled with standarized mean_conc values
#asc
asc_shapiro <- asc[asc$cat == "cat1" & is.na(asc$mean_conc) == FALSE, ]
asc_shapiro <- as.data.frame(asc_shapiro[order(asc_shapiro$time), ])
#Vector containing all standarized mean_conc values
asc_shapiro_time <- c()
for (i in c(
  0, 15, 30, 60, 90, 120, 180
  )) {
    asc_vector <- asc_shapiro$mean_conc[asc_shapiro$time == i]
    asc_shapiro_time <- c(asc_shapiro_time, scale(asc_vector))
  
}
# add cat2
asc_shapiro2 <- asc[asc$cat == "cat2" & is.na(asc$mean_conc) == FALSE, ]
asc_shapiro2 <- as.data.frame(asc_shapiro2[order(asc_shapiro2$time), ])
#Vector containing all standarized mean_conc values
asc_shapiro_time2 <- c()

for (i in c(
  0, 15, 30, 60, 90, 120, 180
  )) {
    asc_vector2 <- asc_shapiro2$mean_conc[asc_shapiro2$time == i]
    asc_shapiro_time2 <- c(asc_shapiro_time2, scale(asc_vector2))
  
}
# join cat1 and cat2
asc_shapiro_time <- c(asc_shapiro_time, asc_shapiro_time2)
(p_value <- shapiro.test(asc_shapiro_time)$p.value)
(p_value_asc_cat12 <- shapiro.test(asc_shapiro_time)$p.value)
#density plot
plot(density(asc_shapiro_time), main = "asc density plot standarized_data (cat1 and cat2)")
# add shapiro_wilk p-value to plot
text(-1.2, 0.05, paste("Shapiro-Wilk p-value =", round(p_value, 4)), pos = 4, col = "#000e7b")
#histogram
hist(asc_shapiro_time, main = "asc histogram standarized_data (cat1 and cat2)")
#shapiro test
shapiro.test(asc_shapiro_time)
# qqplot with normal distribution, 45 degree line 
qqnorm(asc_shapiro_time, main = "asc qqplot standarized_data (cat1 and cat2)", col = "#535001")
qqline(asc_shapiro_time, col = "#535001", lwd = 2)
abline(a = 0, b = 1, col = "#000c7b", lty = 2, lwd = 4)
text(-1, -1.25, "normal distribution", pos = 4, col = "#000c7b")

# add 45° line named "normal distribution"

#dfo
dfo_shapiro <- dfo[dfo$cat == "cat1" & is.na(dfo$mean_conc) == FALSE, ]
dfo_shapiro <- as.data.frame(dfo_shapiro[order(dfo_shapiro$time), ])
#Vector containing all standarized mean_conc values
dfo_shapiro_time <- c()
for (i in c(
  15, 30, 60, 90, 120
  )) {
    dfo_vector <- dfo_shapiro$mean_conc[dfo_shapiro$time == i]
    dfo_shapiro_time <- c(dfo_shapiro_time, scale(dfo_vector))
  
}
# add cat2
dfo_shapiro2 <- dfo[dfo$cat == "cat2" & is.na(dfo$mean_conc) == FALSE, ]
dfo_shapiro2 <- as.data.frame(dfo_shapiro2[order(dfo_shapiro2$time), ])
#Vector containing all standarized mean_conc values
dfo_shapiro_time2 <- c()

for (i in c(
  15, 30, 60, 90, 120
  )) {
    dfo_vector2 <- dfo_shapiro2$mean_conc[dfo_shapiro2$time == i]
    dfo_shapiro_time2 <- c(dfo_shapiro_time2, scale(dfo_vector2))
  
}
# join cat1 and cat2
dfo_shapiro_time <- c(dfo_shapiro_time, dfo_shapiro_time2)
(p_value <- shapiro.test(dfo_shapiro_time)$p.value)
(p_value_dfo_cat12 <- shapiro.test(dfo_shapiro_time)$p.value)
#density plot
plot(density(dfo_shapiro_time), main = "dfo density plot standarized_data (cat1 and cat2)")
# add shapiro_wilk p-value to plot
text(-1.2, 0.05, paste("Shapiro-Wilk p-value =", round(shapiro.test(dfo_shapiro_time)$p.value, 4)), pos = 4, col = "#000e7b")

#histogram
hist(dfo_shapiro_time, main = "dfo histogram standarized_data (cat1 and cat2)")
#shapiro test
shapiro.test(dfo_shapiro_time)
# qqplot with normal distribution, 45 degree line 
qqnorm(dfo_shapiro_time, main = "dfo qqplot standarized_data (cat1 and cat2)", col = "#535001")
qqline(dfo_shapiro_time, col = "#535001", lwd = 2)
abline(a = 0, b = 1, col = "#000c7b", lty = 2, lwd = 4)
text(-1, -1.25, "normal distribution", pos = 4, col = "#000c7b")

#nac
nac_shapiro <- nac[nac$cat == "cat1" & is.na(nac$mean_conc) == FALSE, ]
nac_shapiro <- as.data.frame(nac_shapiro[order(nac_shapiro$time), ])
#Vector containing all standarized mean_conc values
nac_shapiro_time <- c()
for (i in c(
  15, 30, 60, 90, 120
  )) {
    nac_vector <- nac_shapiro$mean_conc[nac_shapiro$time == i]
    nac_shapiro_time <- c(nac_shapiro_time, scale(nac_vector))
  
}
# add cat2
nac_shapiro2 <- nac[nac$cat == "cat2" & is.na(nac$mean_conc) == FALSE, ]
nac_shapiro2 <- as.data.frame(nac_shapiro2[order(nac_shapiro2$time), ])
#Vector containing all standarized mean_conc values
nac_shapiro_time2 <- c()

for (i in c(
  15, 30, 60, 90, 120
  )) {
    nac_vector2 <- nac_shapiro2$mean_conc[nac_shapiro2$time == i]
    nac_shapiro_time2 <- c(nac_shapiro_time2, scale(nac_vector2))
  
}
# join cat1 and cat2
nac_shapiro_time <- c(nac_shapiro_time, nac_shapiro_time2)
p_value <- shapiro.test(nac_shapiro_time)$p.value
p_value_nac_cat12 <- shapiro.test(nac_shapiro_time)$p.value
#density plot
plot(density(nac_shapiro_time), main = "nac density plot standarized_data (cat1 and cat2)")
# add shapiro_wilk p-value to plot
text(-1.2, 0.05, paste("Shapiro-Wilk p-value =", round(shapiro.test(nac_shapiro_time)$p.value, 4)), pos = 4, col = "#000e7b")
#histogram
hist(nac_shapiro_time, main = "nac histogram standarized_data (cat1 and cat2)")
#shapiro test
shapiro.test(nac_shapiro_time)
# qqplot with normal distribution, 45 degree line
qqnorm(nac_shapiro_time, main = "nac qqplot standarized_data (cat1 and cat2)", col = "#535001")
qqline(nac_shapiro_time, col = "#535001", lwd = 2)
abline(a = 0, b = 1, col = "#000c7b", lty = 2, lwd = 4)
text(-1, -1.25, "normal distribution", pos = 4, col = "#000c7b")

# finish pdf and save in working directory
dev.off()

# p-value table
p_value_table_cat12 <- data.frame(
  cat = c("asc", "dfo", "nac"),
  p_value = c(p_value_asc_cat12, p_value_dfo_cat12, p_value_nac_cat12)
)

########################################################################
########################################################################
########################################################################
########################################################################
# repeat process for each cat

pdf("output/normalidad/cat1 vs cat2 density_shapiro_qqplots standarized data.pdf")

# shapiro test for cat1 standarized mean_conc values pooled
#asc
#cat1
asc_shapiro <- asc[asc$cat == "cat1" & is.na(asc$mean_conc) == FALSE, ]
asc_shapiro <- as.data.frame(asc_shapiro[order(asc_shapiro$time), ])
#Vector containing all standarized mean_conc values
asc_shapiro_time <- c()
for (i in c(
  0, 15, 30, 60, 90, 120, 180
  )) {
    asc_vector <- asc_shapiro$mean_conc[asc_shapiro$time == i]
    asc_shapiro_time <- c(asc_shapiro_time, scale(asc_vector))
  
}

(p_value_cat1_asc <- shapiro.test(asc_shapiro_time)$p.value)

# cat2
asc_shapiro2 <- asc[asc$cat == "cat2" & is.na(asc$mean_conc) == FALSE, ]
asc_shapiro2 <- as.data.frame(asc_shapiro2[order(asc_shapiro2$time), ])
#Vector containing all standarized mean_conc values
asc_shapiro_time2 <- c()
for (i in c(
  0, 15, 30, 60, 90, 120, 180
  )) {
    asc_vector2 <- asc_shapiro2$mean_conc[asc_shapiro2$time == i]
    asc_shapiro_time2 <- c(asc_shapiro_time2, scale(asc_vector2))
  
}

(p_value_cat2_asc <- shapiro.test(asc_shapiro_time2)$p.value)

#density plot cat1 vs cat2, set y axis limits
plot(density(asc_shapiro_time), ylim = c(0, 0.6), main = "asc density plot standarized_data (cat1 vs cat2)", , col = "#000e7b")
# add shapiro_wilk p-value to plot
text(-1.2, 0.05, paste("Shapiro-Wilk cat1 p-value =", round(p_value_cat1_asc, 4)), pos = 4, col = "#000e7b")
#density plot cat2
lines(density(asc_shapiro_time2), col = "#620000")
# add shapiro_wilk p-value to plot
text(-1.2, 0.1, paste("Shapiro-Wilk cat2 p-value =", round(p_value_cat2_asc, 4)), pos = 4, col = "#620000")
# increment y axis limits


#histogram cat1 vs cat2
hist(asc_shapiro_time, col = "#000e7b99", ylim = c(0, 16), main = "asc histogram standarized_data (cat1 vs cat2)")
# add cat2 histogram with transparency
hist(asc_shapiro_time2, col = "#62000099", add = TRUE)


# qqplot cat1 with normal distribution, 45 degree line 
qqnorm(asc_shapiro_time, main = "asc qqplot standarized_data (cat1)", col = "#000e7b99")
qqline(asc_shapiro_time, col = "#000e7b", lwd = 2)
abline(a = 0, b = 1, col = "#000000", lty = 2, lwd = 4)
text(-1, -1.25, "normal distribution", pos = 4, col = "#000c7b")
#qqplot cat2 with normal distribution, 45 degree line
qqnorm(asc_shapiro_time2, main = "asc qqplot standarized_data (cat2)", col = "#62000099", add = TRUE)
qqline(asc_shapiro_time2, col = "#620000", lwd = 2)
abline(a = 0, b = 1, col = "#000000", lty = 2, lwd = 4)
text(-1, -1.25, "normal distribution", pos = 4, col = "#000000")


#dfo
#cat1
dfo_shapiro <- dfo[dfo$cat == "cat1" & is.na(dfo$mean_conc) == FALSE, ]
dfo_shapiro <- as.data.frame(dfo_shapiro[order(dfo_shapiro$time), ])
#Vector containing all standarized mean_conc values
dfo_shapiro_time <- c()
for (i in c(
  15, 30, 60, 90, 120
  )) {
    dfo_vector <- dfo_shapiro$mean_conc[dfo_shapiro$time == i]
    dfo_shapiro_time <- c(dfo_shapiro_time, scale(dfo_vector))
  
}
# add cat2
dfo_shapiro2 <- dfo[dfo$cat == "cat2" & is.na(dfo$mean_conc) == FALSE, ]
dfo_shapiro2 <- as.data.frame(dfo_shapiro2[order(dfo_shapiro2$time), ])
#Vector containing all standarized mean_conc values
dfo_shapiro_time2 <- c()

for (i in c(
  15, 30, 60, 90, 120
  )) {
    dfo_vector2 <- dfo_shapiro2$mean_conc[dfo_shapiro2$time == i]
    dfo_shapiro_time2 <- c(dfo_shapiro_time2, scale(dfo_vector2))
  
}
# p-value cat1 and cat2
(p_value_cat1_dfo <- shapiro.test(dfo_shapiro_time)$p.value)
(p_value_cat2_dfo <- shapiro.test(dfo_shapiro_time2)$p.value)
#density plot
plot(density(dfo_shapiro_time), main = "dfo density plot standarized_data (cat1 vs cat2)", col = "#000e7b")
# add shapiro_wilk p-value to plot
text(-1.2, 0.05, paste("Shapiro-Wilk cat1 p-value =", round(shapiro.test(dfo_shapiro_time)$p.value, 4)), pos = 4, col = "#000e7b")
#add density plot cat2
lines(density(dfo_shapiro_time2), col = "#620000")
# add shapiro_wilk p-value to plot
text(-1.2, 0.1, paste("Shapiro-Wilk cat2 p-value =", round(shapiro.test(dfo_shapiro_time2)$p.value, 4)), pos = 4, col = "#620000")


#histogram
hist(dfo_shapiro_time, main = "dfo histogram standarized_data (cat1 and cat2)", col = "#000e7b99")
# add cat2 histogram with transparency
hist(dfo_shapiro_time2, col = "#62000099", add = TRUE)
#shapiro test
shapiro.test(dfo_shapiro_time)
# qqplot with normal distribution, 45 degree line 
qqnorm(dfo_shapiro_time, main = "dfo qqplot standarized_data (cat1)", col = "#000e7b99")
qqline(dfo_shapiro_time, col = "#000e7b", lwd = 2)
abline(a = 0, b = 1, col = "#000000", lty = 2, lwd = 4)
text(-1, -1.25, "normal distribution", pos = 4, col = "#000000")
#qqplot cat2 with normal distribution, 45 degree line
qqnorm(dfo_shapiro_time2, main = "dfo qqplot standarized_data (cat2)", col = "#62000099", add = TRUE)
qqline(dfo_shapiro_time2, col = "#620000", lwd = 2)
abline(a = 0, b = 1, col = "#000000", lty = 2, lwd = 4)
text(-1, -1.25, "normal distribution", pos = 4, col = "#000000")


#nac
#cat1
nac_shapiro <- nac[nac$cat == "cat1" & is.na(nac$mean_conc) == FALSE, ]
nac_shapiro <- as.data.frame(nac_shapiro[order(nac_shapiro$time), ])
#Vector containing all standarized mean_conc values
nac_shapiro_time <- c()
for (i in c(
  15, 30, 60, 90, 120
  )) {
    nac_vector <- nac_shapiro$mean_conc[nac_shapiro$time == i]
    nac_shapiro_time <- c(nac_shapiro_time, scale(nac_vector))
  
}
# add cat2
nac_shapiro2 <- nac[nac$cat == "cat2" & is.na(nac$mean_conc) == FALSE, ]
nac_shapiro2 <- as.data.frame(nac_shapiro2[order(nac_shapiro2$time), ])
#Vector containing all standarized mean_conc values
nac_shapiro_time2 <- c()

for (i in c(
  15, 30, 60, 90, 120
  )) {
    nac_vector2 <- nac_shapiro2$mean_conc[nac_shapiro2$time == i]
    nac_shapiro_time2 <- c(nac_shapiro_time2, scale(nac_vector2))
  
}
# p-value cat1 and cat2
p_value_cat1_nac <- shapiro.test(nac_shapiro_time)$p.value
p_value_cat2_nac <- shapiro.test(nac_shapiro_time2)$p.value
#density plot cat1 and cat2
plot(density(nac_shapiro_time), ylim = c(0, 0.65), main = "nac density plot standarized_data (cat1 and cat2)", col = "#000e7b")
# add shapiro_wilk p-value to plot
text(-1.2, 0.05, paste("Shapiro-Wilk cat1 p-value =", round(shapiro.test(nac_shapiro_time)$p.value, 4)), pos = 4, col = "#000e7b")
#add density plot cat2
lines(density(nac_shapiro_time2), col = "#620000")
# add shapiro_wilk p-value to plot
text(-1.2, 0.1, paste("Shapiro-Wilk cat2 p-value =", round(shapiro.test(nac_shapiro_time2)$p.value, 4)), pos = 4, col = "#620000")

#histogram
hist(nac_shapiro_time, col ="#000e7b99",  main = "nac histogram standarized_data (cat1 and cat2)")
# add cat2 histogram with transparency
hist(nac_shapiro_time2, col = "#62000099", add = TRUE)
#shapiro test
shapiro.test(nac_shapiro_time)
# qqplot with normal distribution, 45 degree line
qqnorm(nac_shapiro_time, main = "nac qqplot standarized_data (cat1)", col = "#000e7b")
qqline(nac_shapiro_time, col = "#000e7b", lwd = 2)
abline(a = 0, b = 1, col = "#000000", lty = 2, lwd = 4)
text(-1, -1.25, "normal distribution", pos = 4, col = "#000000")
#qqplot cat2 with normal distribution, 45 degree line
qqnorm(nac_shapiro_time2, main = "nac qqplot standarized_data (cat2)", col = "#62000099", add = TRUE)
qqline(nac_shapiro_time2, col = "#620000", lwd = 2)
abline(a = 0, b = 1, col = "#000000", lty = 2, lwd = 4)
text(-1, -1.25, "normal distribution", pos = 4, col = "#000000")



# finish pdf and save in working directory
# table with p-values cat1 and cat2 for each treatment for asc, dfo and nac

dev.off()
# install gridExtra package to save table in pdf


(p_value_table <- data.frame(
  row.names = c("asc", "dfo", "nac"),
  cat1 = c(p_value_cat1_asc, p_value_cat1_dfo, p_value_cat1_nac),
  cat2 = c(p_value_cat2_asc, p_value_cat2_dfo, p_value_cat2_nac)
))
p_value_table
p_value_table_cat12