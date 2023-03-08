##
## MRT model analysis
## Cognitive Science Practical 2022
##
library(tidyverse)
library(dplyr)
library(reshape2)

path <- getwd()

nowander_path <- paste(path, "/output-nowander/", sep="")
wander_path <- paste(path, "/output-wander/", sep="")

# Get csv files
nowanderfiles <- list.files(path = nowander_path, pattern=".csv", full.names = TRUE)
wanderfiles <- list.files(path = wander_path, pattern=".csv", full.names = TRUE)

std <- function(x) sd(x) / sqrt(length(x))

# Read csv files to nowanderdat
nowanderdat <- data.frame()
for (i in 1:length(nowanderfiles)) {
  nowanderdat <- rbind(nowanderdat, read.csv(nowanderfiles[i], sep = ",", strip.white = TRUE))
}

# Convert data types to numeric
nowanderdat$rt <- as.numeric(nowanderdat$rt)
nowanderdat$beat.time <- as.numeric(nowanderdat$beat.time)

# Calculate RRT as the difference in response and beat
rrt<-with(nowanderdat, rt - beat.time)
nowanderdat<-cbind(nowanderdat,rrt)
# Calculate RRTv as the natural log transformed variance in RRT
RRTv_by_trial_attend <- nowanderdat %>% group_by(trial)
RRTv_by_trial_attend <- RRTv_by_trial_attend %>% summarise(attend = var(rrt))
RRTv_by_trial_attend$attend <- log(RRTv_by_trial_attend$attend)
rm(rrt)

# Read csv files to wanderdat
wanderdat <- data.frame()
for (i in 1:length(wanderfiles)) {
  wanderdat <- rbind(wanderdat, read.csv(wanderfiles[i], sep = ",", strip.white = TRUE))
}

wanderdat$rt <- as.numeric(wanderdat$rt)
wanderdat$beat.time <- as.numeric(wanderdat$beat.time)

# Same as before
rrt<-with(wanderdat, rt - beat.time)
wanderdat<-cbind(wanderdat,rrt)

RRTv_by_trial_wander <- wanderdat %>% group_by(trial)
RRTv_by_trial_wander <- RRTv_by_trial_wander %>% summarise(wander = var(rrt))
RRTv_by_trial_wander$wander <- log(RRTv_by_trial_wander$wander)
rm(rrt)

# Merge RRTv dataframes for attend and wander into a single dataframe. Calculate standard error of variance.

RRTv_by_trial <- merge(RRTv_by_trial_attend, RRTv_by_trial_wander)
rm(RRTv_by_trial_attend)
rm(RRTv_by_trial_wander)
wanderSE <- std(RRTv_by_trial$wander)
nowanderSE <- std(RRTv_by_trial$attend)

# Plot, code taken from week 3 assignment

condition <- c("Wandering", "No Wandering")
mean <- c(mean(RRTv_by_trial$wander),mean(RRTv_by_trial$attend))
error <- c(wanderSE, nowanderSE)
plotMeans <- data.frame(condition, mean, error)

ggplot(data = plotMeans, aes(y=mean, x=condition, ymin=mean-error, ymax=mean+error)) +
  geom_col(width=0.7) +
  geom_errorbar(width=0.1) +
  labs(y="Transformed RRT Variance", x="Model Type")

