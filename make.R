
source("R/packages.R")  
#source("R/functions.R")
# Build workflow plan data frame.
source("R/plan.R")

# Now, your functions and workflow plan should be in your environment.
ls()

# Graph of workflow
vis_drake_graph(plan)      # nolint

make(plan) # Or make(plan, jobs = 2), etc.
