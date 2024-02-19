# ---
# title: "Simulation of disease progress in space"
# author: "Mladen Cucak; Felipe Dalla Lana; Mauricio Serrano; Paul Esker, Miranda DePriest"
# output:
#   html_document:
#     code_folding: show
#     toc: true
#     toc_float:
#       toc_depth: 3
#       collapsed: false
#       smooth_scroll: false
# ---
# 
# ```{r setup, include=FALSE}
# knitr::opts_chunk$set(echo = TRUE, warning = FALSE, eval = FALSE)
# ```
# 
# # **Introduction**
# 
# Understanding how epidemics spread from their origin is a major part of plant pathology. The distribution of disease in the field can offer key insights on where the disease came from, the cause of the disease, and factors influencing its success. Additionally, the likelihood of an individual plant becoming infected can depend on its proximity to other infected individuals. In this lesson, we'll simulate the spread of a disease, showing how R can be used to understand this phenomenon. We will also explore the concept of parallel processing, a technique where independent operations are deployed to multiple processors (CPUs) to perform a computationally-extensive task in less time. We'll also learn how to apply a single function to a list of objects in Example 2. 
# 
# First, we'll set up by loading in packages and reconciling conflicts between commands from different packages. 
# 
# ```{r libs}

list.of.packages <-
  c(
    "dplyr",
    "ggplot2",
    "gt", # Used to customize table 
    "conflicted", # Reconcile command conflicts
    "here", # Create reproducible paths
    "reshape2", # Manipulate data 
    "plotly", # Visualization tool 
    "pbapply", # For parallel processing
    "parallel" # Parallel processing
  )

new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]

#Download packages that are not already present in the library
if (length(new.packages))
  install.packages(new.packages)

# Load packages
packages_load <-
  lapply(list.of.packages, require, character.only = TRUE)

#Print warning if there is a problem with installing/loading some of packages
if (any(as.numeric(packages_load) == 0)) {
  warning(paste("Package/s: ", paste(list.of.packages[packages_load != TRUE], sep = ", "), "not loaded!"))
} else {
  print("All packages were successfully loaded.")
}

# Remove package objects
rm(list.of.packages, new.packages, packages_load)

#Resolve conflicts
   conflict_prefer("filter", "dplyr")
  conflict_prefer("select", "dplyr")
 
# 
# ```
# 
# If you experience difficulties installing or loading packages, please see Lesson 1 - Intro to R. 
# 
# # **Epidemic simulation: Dispersal**
# 
# In this section, we'll use a simulation to explore concepts about disease dispersal. Our example data will contain three randomly positioned initial infections in a field divided into 10 rows and 10 columns. Initial infected status will be indicated by 1, indicating that one plant in that cluster is infected. Clusters with only healthy plants are represented by 0. 
# 
# **Example 1: Creating spatial data**
# 
# ```{r}

# Create a matrix with values of 0 in 10 rows and 10 columns
map1 <- matrix(0,10,10)

# Define the number of infected plants
inf <- 3

# Randomly select the position of 3 infected plants:

# Set seed for reproducibility
set.seed(123)

# Get 3 random row numbers in a vector (row1)
row1 <- sample(1:10, size = inf, replace = TRUE)

# And 3 random columns (col1)
col1 <- sample(1:10, size = inf, replace = TRUE)

# Place infected plants at the locations created from row1 and col1 by adding 1 to the already-existing value in the cell (0)
for (j in as.numeric(1:inf)){
  map1[row1[j], col1[j]] <- map1[row1[j], col1[j]] + 1
}

# Look at map1
map1

# 
# ```
# 
# Now we'll transform the data, using `reshape2::melt()` to do so. We'll also visualize the data. Since we'll be doing this for several examples in this lesson, we'll write both tasks into a function that we can easily apply repeatedly. 
# 
# ```{r PlotMat}

# Define the function and its required input
PlotMat <- function(data){
  
  # Elongate the provided data and pass it on
  reshape2::melt(data, varnames = c("rows", "cols")) %>% 
    
    # Plot the provided data with cols and rows as x and y-variables
    ggplot(aes(factor(cols), factor(rows), 
               
               # Color and label the healthy and diseased plants
               color = value,
               label = as.character(value))) +
    
    # Continue to format the plot
    geom_point(size = 11, shape = 15) +
    geom_text(colour = "black") +
    scale_colour_gradient("Proportion of infected plants:",
                          low = "lightgreen", high = "#993300") +
    theme_bw()+
    theme(axis.title = element_blank(),
          legend.position = "top",
          panel.background = element_rect(terrain.colors(5)[3]),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    coord_equal()
}

# ```
# 
# Now we'll apply that to our data to transform it and get a plot.
# 
# ```{r}

# Transform and plot map1
plot <- PlotMat(map1)
plot + ggtitle("Generation 1")
#   
# ```
# 
# **Question**: The three infected plants are distant to each other, rather than existing in proximity. Would the distribution of infected plants tell you anything about the inoculum? 
# 
# 
# # **Proximal dispersal**
# 
# In this example, we will simulate the spread of disease from infected individuals to adjacent plants. The pattern of simulated dispersion we will use is called 'the rook's move'. Just like the rook in chess, the disease can move directly forward, backward, left and right, but it never skips over direct neighbors to infect more distant plants. The spread of disease in this case is roughly *isotropic*, meaning that the magnitude of spread is equal in all directions. If it was perfectly isotropic, we would need to do distance calculations on diagonal neighbors.
# 
# Example: If the infected plant is in cell [2,3], the following cells/plants will become infected: 
# *[4,2] - the plant in front of it
# *[2,2] - the plant behind it
# *[3,1] - the plant to the right
# *[3,3] - the plant to the left
# 
# We'll manipulate the map1 object from the last example to fit this situation. A *for loop* is used to select each individual cell. A series of *if statements* are used to determine which of its neighbors will inherit its condition (healthy or infected). We'll follow the progression of the disease by simulating generations of reproduction and dispersal. 
# 
# **Example 2.1: Simulating the proximal spread of disease in a generation**
# ```{r}

# Create map2 by taking map1 and modifying it
map2 <- map1

# Select a cell by going through every row
for(i in as.numeric(1:10)){
  # and every column
  for(j in as.numeric(1:10)){
        
    # Take the health status of that cell (0 for healthy, 1 for infected)
    status <- map1[i,j]
        
    # and pass it on to the plant above it, if possible
    if(i > 1){
      map2[i-1,j] <- map2[i-1,j] + status}
        
    # and the plant behind it
    if(i < 10){
      map2[i+1,j] <- map2[i+1,j] + status}
        
    # and the plant to the right
    if(j < 10){
      map2[i,j+1] <- map2[i,j+1] + status}
        
    # and the plant to the left
    if(j > 1){
      map2[i,j-1] <- map2[i,j-1] + status}
        
    # Save new status values to map2
    map2[i,j] <- map2[i,j] + status
  }
}

map2
# 
# ```
# 
# If you need a refresher on for loops and if statements, visit Lessons 4 and 5 respectively. 
# 
# Now we'll use `lapply()` to apply function `PlotMat` to a list of objects. The `lapply()` function from base R allows you to apply the same function to a list of objects. We'll use it to show the progress of the disease in map1 and map2. 
# 
# ```{r map1 vs map2}

# First, let's see what lapply() does and what arguments it needs
?lapply()
# Remember - you can do the same thing by placing your cursor on the command and pressing F1

# We create and provide the list (map1 and map2) and lapply applies PlotMap
lapply(list(map1,map2), PlotMat)

# # Instead of doing this:
# PlotMat(map1)
# PlotMat(map2)
# 
# ```
# 
# This is a simple example, but imagine if you had many map objects. It would be much easier to create a list and put it into `lapply()` than it would be to write a line for each object. 
# 
# Now we can inspect the maps. You can see that each of the three originally infected plants have infected their closest neighbors, spreading the disease. The disease in the originally-infected clusters has also increased to two infected plants. We can continue the simulation through multiple generations (*n*) with the same techniques we've previously used.
# 
# **Example 2.2: Simulating the proximal spread of disease through *n* generations**
# 
# ```{r prox_n}

# Number of generations
n <- 6

# Create a list with n elements (1:n)
mapn <- as.list(1:n)

# Initialize the data for each mapn element with 10x10 matrix containing all 0's with a for loop
for(k in as.numeric(1:n)){
  mapn[[k]] <- matrix(0, 10, 10)
}
# Now we have a list of n matrices, each with 100 cells containing 0

# We'll replace the first element, the matrix for generation 1, with map1
mapn[[1]] <- map1

# Make the matrix for each generation 2:n
for(k in as.numeric(2:n)){
  
  # Copy the previous generation's matrix
  mapn[[k]] <- mapn[[k-1]]
  
  # Use the same technique as in Example 2.1 to spread the disease
  for(i in as.numeric(1:10)){
    for(j in as.numeric(1:10)){
      status <- mapn[[k-1]][i,j]
      if(i > 1){
        mapn[[k]][(i-1),j] <- mapn[[k]][(i-1),j] + status}
      if(i < 10){
        mapn[[k]][(i+1),j] <- mapn[[k]][(i+1),j] + status}
      if(j < 10){
        mapn[[k]][i,(j+1)] <- mapn[[k]][i,(j+1)] + status}
      if(j > 1){
        mapn[[k]][i,(j-1)] <- mapn[[k]][i,(j-1)] + status}
      mapn[[k]][i,j] <- mapn[[k]][i,j] + status
    }
  }
}

# Now, we'll create a gg object containing the ggplot2 graph of each generation
plot <- lapply(mapn, PlotMat)

# Print each plot item 1:n with the appropriate title
for (i in as.numeric(1:n)){
  print(plot[[i]] + ggtitle(paste0("Generation ", as.character(i))))
}
# 
# ```
# 
# Now we've seen how this simple pattern of dispersal can develop through space and time in an example field. We will move on to explore other types of dispersal in similar scenarios. 
# 
# 
# # **Exponential and random dispersal**
# 
# Disease spread is does not always occur in a simple, step-wise pattern with clear limits to distance, like in the last example. In this simulation, the probability of infection is distributed exponentially with distance. Said differently, the chance of infection is highest for the infected plant's nearest neighbors, but infecting more distant plants is possible. 
# 
# In this example, we have 1,000 rows, 1,000 columns, and 20 infected plants in generation 1. The rate of spread must also be defined, and it impacts the number of plants infected each generation.
# 
# ```{r example3.1}

# Number of infected clusters in gen 1
nstart <- 20

# Rows and columns of infected plants
row1 <- round(runif(n = nstart, min = 0, max = 1000))
col1 <- round(runif(n = nstart, min = 0, max = 1000))
# 
# ```
# 
# Now we have 20 infected clusters out of 1,000,000. We can see how the values of the next generation will be distributed based on the rate *r*. 
# 
# ```{r exp dis}

hist(rexp(n=1000, rate=2),col="blue",xlim =range(0:13), ylim = range(0:600))
hist(rexp(n=1000, rate=1),col="blue",xlim =range(0:13), ylim = range(0:600))
hist(rexp(n=1000, rate=0.5),col="blue", xlim =range(0:13), ylim = range(0:600))


# ```
# 
# We can see that a higher rate results in more strongly skewed value distribution. 
# 
# As we continue with this example, the processing time will grow significantly. To reduce the processing time, we will use packages `pbapply` and `parallel`
# 
# To continue with our example, we'll use all of the following parameters:
# 
# - `nclust` is the number of individual clusters
# - `nstart` is the number of *initial* infections
# - `nnew` is the number of *new* infections in a generation 
# - `ngen` is the number of generations
# - `exrt` is the rate parameter for the exponential distribution
# 
# We'll define parameters, create the initial data matrix, and then define a function for calculating the next generation. 
# 
# ```{r definitions}

# Define the following parameters
ntot <- nstart
nnew <- 10
ngen <-  3
exrt <-  0.2

# Create the initial data in a long format
coord <- matrix(runif(n = 2*nstart, min = 0, max = 1000), 
                     nrow = nstart, ncol = 2)

# Create a function for running all generations' computations:
# Function `disperse2` requires nstart, nnew, ngen, and exrt
disperse2 <- function(nstart, nnew, ngen, exrt){
  coord <- matrix(runif(n=2*nstart, min=0, max=1000),
                  nrow=nstart, ncol=2)  
  
  # ntot serves as a way to avoid changing nstart, a constant
  ntot <- nstart
  
  # For each generation
  for(i in 1:ngen){
    
    # And each infected plant
    for(j in 1:ntot){
      
      # Save the values of coord[j, 1] and coord[j, 2] as objects
      tempx <- coord[j,1]
      tempy <- coord[j,2]
      
      # Calculate the distribution around the infected plant, using pi for 360 degrees of infection
      newdir <- runif(n=nnew, min=-pi, max=pi)
      newdist <- rexp(n=nnew, rate=exrt)
      
      # Calculate the new boundary of disease spread
      newx <- tempx + newdist * sin(newdir)
      newy <- tempy + newdist * cos(newdir)
      
      # And replace old x and y values (tempx and tempy) with new values
      newcoord <- cbind(newx,newy)
      coord <- rbind(coord,newcoord)
    }
    
    # Define the new total number of infections
    ntot <- length(coord[,1])
  }
  coord
}

# ```
# 
# Now, we're going to run the simulation with a variety of parameters to see how each influences the result. 
# 
# ```{r var combo}

# Create a data frame combining all possible combinations of rate and gen
combos <- expand.grid(rate = c(0.05, 0.1, 0.5),
                      gen = c(1, 2, 3, 4, 5))
combos

# We'll use our function disperse2() to run each combination of rate and gen, and store the results in a list, sim_ls:

# Create an empty list to save results to
sim_ls <- list()

# We will record the start and end times to see how long each loop of this calculation takes
start.time <- Sys.time()

# For each rate/gen combination
for (i in seq(1:nrow(combos))){
  
  # The corresponding element in sim_ls will be the distance of the disease spread
  sim_ls[[i]] <- disperse2(nstart = 20, nnew = 10, 
                           ngen = combos[i,2],
                           exrt = combos[i,1]) %>%
    # Converted into a tibble/type of data frame with columns 'rate = #' and 'gen = #'
    as_tibble() %>% 
    mutate(rates = paste0("rate = ", combos[i,1]),
           ngen = paste0("gen = ", combos[i,2]))
  
  # Print the result and the time
  print(i)
  print(Sys.time())
}


# 
# ```
# 
# The above chunk of code probably took a while to run. Normally, everything you do in your R session is done on one core (processing unit) of your device. You can use packages `pbapply` and `parallel` to direct R to use multiple cores, or processing units. Normally, R uses a single core. The use of multiple cores to perform independent operations simultaneously is called **parallel processing**. Each core must have all of the relevant information to perform its operation. We will enable parallel processing now.
# 
# ```{r parallel}

# Detect the number of cores available
cores <- detectCores()

# Keep one or two cores free for regular operation
cores <- cores-1

# Cluster the cores
cl <- makeCluster(cores)

# Export the relevant objects
clusterExport(cl, c("combo_ls", "sim_ls", "disperse2"))

# Export the relevant package(s) without generating feedback
clusterEvalQ(cl, library("dplyr", quietly = TRUE, verbose = FALSE))













