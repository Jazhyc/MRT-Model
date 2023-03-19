
## The easiest way to do this is to read the trace file line by line wihle keeping track of the state of the model

library(stringr)
library(data.table)

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