
source("R/packages.R")  
#source("R/functions.R")
# Build workflow plan data frame.
source("R/plan.R")

# Now, your functions and workflow plan should be in your environment.
ls()

# Graph of workflow
   # nolint

config <- drake_config(plan, verbose = 2)
make_impl(config = config)

vis_drake_graph(plan, file = 'dependencies.png', mode = 'all', targets_only = TRUE, build_times =  'none')   

