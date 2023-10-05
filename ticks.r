library(stringr)
library(data.table)
library(tidyverse)

data_path <- "output-nowander/"

# Read the trace file into memory
trace <- file(paste0(data_path, "sart-trace.txt"), "r")
lines <- readLines(trace)

participant <- 0L
time <- 1
n <- length(lines)
# Preallocate a data.table where we can store ATTEND activation values:
# without this, the code runs very slowly due to the size of the trace file
NoWanderTicks <- data.table(participant = rep(0L, n), ticks = rep(0, n)) 
idx <- 0L


for(i in 1:n) {
  
  # Read a single line
  line <- lines[i]
  
  # Detect the start of a new model run
  if(str_detect(line, "Run \\d+")) {
    participant <- participant + 1L
    print(participant)
  }
  
  # Check whether the line contains the activation of the ATTEND chunk, and then store the value
  # Hints:
  #   - you can use str_detect() and str_extract(). See http://stringr.tidyverse.org/
  #   - use regular expressions to describe string patterns. A good resource is https://regexr.com/ 
  #   - you will also need to keep track of the time, which is given at the start of many (but not all!) lines in the trace file. 
  #   - you can add a line to the activations data.table using set(activations, idx, j = 1:3, value = list(participant, time, activation)). See ?set for more information.
  if(str_detect(line, "THE THRESHOLD WAS \\d+")) {
    idx <- idx + 1L
    Ticks <- str_extract_all(line, "\\d+")
    currentTick = Ticks[[1]][2]
    set(NoWanderTicks, idx, j = 1:2, value = list(participant, currentTick))
  }
  
  
}

# Do the same thing for wander
data_path <- "output-wander/"

# Read the trace file into memory
trace <- file(paste0(data_path, "sart-trace.txt"), "r")
lines <- readLines(trace)

participant <- 0L
time <- 1
n <- length(lines)
# Preallocate a data.table where we can store ATTEND activation values:
# without this, the code runs very slowly due to the size of the trace file
WanderTicks <- data.table(participant = rep(0L, n), ticks = rep(0, n)) 
idx <- 0L
for(i in 1:n) {
  
  # Read a single line
  line <- lines[i]
  
  # Detect the start of a new model run
  if(str_detect(line, "Run \\d+")) {
    participant <- participant + 1L
    print(participant)
  }
  
  # Check whether the line contains the activation of the ATTEND chunk, and then store the value
  # Hints:
  #   - you can use str_detect() and str_extract(). See http://stringr.tidyverse.org/
  #   - use regular expressions to describe string patterns. A good resource is https://regexr.com/ 
  #   - you will also need to keep track of the time, which is given at the start of many (but not all!) lines in the trace file. 
  #   - you can add a line to the activations data.table using set(activations, idx, j = 1:3, value = list(participant, time, activation)). See ?set for more information.
  if(str_detect(line, "THE THRESHOLD WAS \\d+")) {
    idx <- idx + 1L
    Ticks <- str_extract_all(line, "\\d+")
    currentTick = Ticks[[1]][2]
    set(WanderTicks, idx, j = 1:2, value = list(participant, currentTick))
  }
  
  
}

# Remove the additional 0s
NoWanderTicks <- subset(NoWanderTicks, NoWanderTicks$ticks != 0)
WanderTicks <- subset(WanderTicks, WanderTicks$ticks != 0)

t.test(NoWanderTicks$ticks, WanderTicks$ticks)

se <- function(x) sd(x) / sqrt(length(x))

df<-tribble(
  ~State, ~Type, ~Mean, ~Se,
  "Attending", "Model", mean(NoWanderTicks$ticks), se(NoWanderTicks$ticks),
  "Wandering", "Model", mean(WanderTicks$ticks), se(NoWanderTicks$ticks),
)

ggplot(df, aes(x = factor(State), y = Mean, fill = State)) + 
  geom_bar(stat = "identity", position = "dodge", color="black", alpha = 0.5) +
  geom_errorbar(aes(ymin=Mean-Se, ymax=Mean+Se,), position = position_dodge(0.9), width = 0.25) +
  labs(y="Average Tick Threshold", x="Model") + coord_cartesian(ylim=c(20, 27)) +
  theme(legend.position = "none")
