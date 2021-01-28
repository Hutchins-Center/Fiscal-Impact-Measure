

source("R/packages.R")
source("R/functions.R")
# Build workflow plan data frame.
source("R/plan.R")

# Now, your functions and workflow plan should be in your environment.
ls()

# Graph of workflow
# nolint

config <- drake_config(plan, verbose = 2, lock_envir = FALSE)
make_impl(config = config)


# Or scale up to a supercomputer.
# drake_hpc_template_file("slurm_batchtools.tmpl") # https://slurm.schedmd.com/
# library(future.batchtools)
# future::plan(batchtools_slurm, template = "batchtools.slurm.tmpl", workers = 100)
# make(plan, parallelism = "future_lapply")

vis_drake_graph(plan,
                mode = 'all',
                targets_only = TRUE,
                build_times =  'none')

readd(fim) %>%
  select(date, subsidies) %>% View()
