# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
# Set target options:
tar_option_set(
  # packages that your targets need to run
  packages = c("worcs", "dbscan", "umap", "data.table", "ggplot2", "reticulate", "igraph")
  # format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # For distributed computing in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller with 2 workers which will run as local R processes:
  #
  #   controller = crew::crew_controller_local(workers = 2)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package. The following
  # example is a controller for Sun Grid Engine (SGE).
  #
  #   controller = crew.cluster::crew_controller_sge(
  #     workers = 50,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.0".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

# tar_make_clustermq() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
options(clustermq.scheduler = "multiprocess")

# tar_make_future() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  tar_target(
    name = df,
    command = dat_fun()
    #, cue = tar_cue(mode = "always")
  )
  , tar_target(
    name = mat_vect,
    command = do.call(rbind, df[["embeddings"]])
  )
  # , tar_file(
  #   name = udpipe_model_file2,
  #   command = write_udpipe_model("_targets/user/results/udpipe_model.rdata")
  # )
  # , tar_file_read(
  #   name = udpipe_model,
  #   command = "_targets/user/results/udpipe_model.rdata",
  #   read = readRDS(file = !!.x)
  # )
  , tar_target(
    name = res_umap,
    command = do_umap(mat_vect)
  )
  , tar_target(
    name = res_hdbscan,
    command = do_hdbscan(res_umap$layout, min_cluster_size = 100L, min_samples = 5L)
  )
  , tar_target(
    name = res_exemplars,
    command = write_exemplar_metadata(df, res_umap, res_hdbscan)
  )
  , tar_target(
    name = exmplrs,
    command = yaml::read_yaml("exemplars_recoded.yaml"),
    cue = tar_cue(mode = "always")
  )
  , tar_target(
    name = plot_dist_clust_file,
    command = plot_dist_clust(res_hdbscan),
    cue = tar_cue(mode = "always")
  )
  , tar_target(
    name = plot_freq_file,
    command = plot_cluster_freq(exmplrs, res_hdbscan),
    cue = tar_cue(mode = "always")
  )
  , tar_target(
    name = plot_graph_file,
    command = plot_graph(df, exmplrs, res_hdbscan),
    cue = tar_cue(mode = "always")
  )
  , tarchetypes::tar_render(manuscript, "index.rmd", cue = tar_cue(mode = "always"))
)
