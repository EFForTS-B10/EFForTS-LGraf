#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# EFForTS-LGraf Manuscript Analyses
#
# Content:
# 1. Reclassification of Harapan Map
# 2. Household distributions: household size, field size
# 3. NetLogo sobol runs (n = 8000)
#
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source("LGrafAnalysisFinal_v1_functions.R")
library(raster)
library(tidyverse)
library(SDMTools)
library(MASS)
library(gridExtra)
library(nlrx)
library(future)
library(furrr)
library(future.batchtools)
library(debugme)
Sys.setenv(DEBUGME='batchtools')
library(batchtools)
library(sensitivity)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  1. Reclassification of Harapan Map
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Define selected landscape metrics:
select.metrics <- c("cs.0.landscape.shape.index",
                    "cs.0.largest.patch.index",
                    "cs.0.mean.patch.area",
                    "cs.0.n.patches",
                    "cs.0.patch.cohesion.index",
                    "cs.1.landscape.shape.index",
                    "cs.1.largest.patch.index",
                    "cs.1.mean.patch.area",
                    "cs.1.n.patches",
                    "cs.1.patch.cohesion.index")

## Binary: Matrix/Agriculture:
reclassmatrix <- matrix(c(0, NA, # NA out of boundary -> (NA) Out of boundary
                           1, 0, # Secondary Forest -> (0) Matrix
                           2, 1, # Rubber           -> (1) Agriculture
                           3, 0, # Cloud/Shadow     -> (0) Matrix 
                           4, 1, # Oilpalm          -> (1) Agriculture
                           5, 0, # Shrubs           -> (0) Matrix
                           6, 0, # Bare land        -> (0) Matrix
                           7, 0, # Settlement       -> (0) Matrix
                           8, 0, # Water body       -> (0) Matrix
                           9, 0, # Cloud/Shadow    -> (0) Matrix
                           10, 0), # ...           -> (0) Matrix
                         ncol=2, byrow=TRUE)


lu_harapan <- loadHarapan(rasterlocation='D:/ownCloud/CRC/10_Geodata/GIS/_2013_harapan_land_use/lulc_harapan_raster.tif', 
                          rasterout="maps/_2013_harapan_land_use_r50",
                          reclassmatrix=reclassmatrix,
                          plotout="plot_output/lu_map_raster.tiff")

## Sample landscapes from the harapan map:
lu_harapan_sample <- sampleLandscapes(lu_harapan, 2358, 1, 50, 1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  2. Household histograms (input data)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## PLOT HH area, vlg area and field size distribution:
createDistrPlots(hhdatalocation="hhdata/hh_data.csv",
                 plotout="plot_output/histograms.tiff")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. Sobol runs:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# Local for testing:
nl <- nl(nlversion = "6.0.3",
         nlpath = "C:/Program Files/NetLogo 6.0.3/",
         modelpath = "LGraf/EFForTS-LGraf_trial.nlogo",
         jvmmem = 2024)


# Cluster:
nl <- nl(nlversion = "6.0.3",
         nlpath = "/home/uni08/jsaleck/NetLogo_6.0.3/",
         modelpath = "/home/uni08/jsaleck/NetLogo_6.0.3/app/models/LGraf/EFForTS-LGraf_trial.nlogo",
         jvmmem = 2024)

nl@experiment <- experiment(expname="LGraf",
                            outpath="C:/out/",
                            repetition=1,
                            tickmetrics="false",
                            idrunnum="foldername",
                            idsetup=c("setup"),
                            idgo=c("establish_fields", "assign-land-uses"),
                            idfinal=NA_character_,
                            metrics.turtles=c("who", "pxcor", "pycor"),
                            metrics.patches=c("pxcor", "pycor", "p_landuse-type"),
                            variables = list("perlin-octaves" = list(qfun="qunif", min = 2,  max = 12),
                                             "perlin-persistence" = list(qfun="qunif", min = 0.1, max = 0.9),
                                             "cone-angle" = list(qfun="qunif", min = 90, max = 180),
                                             "dist-weight" = list(qfun="qunif", min = 0, max = 1),
                                             "households-per-cell" = list(qfun="qunif", min = 3, max = 5),
                                             "min-distance" = list(qfun="qunif", min=1, max=10),
                                             "total-road-length" = list(qfun="qunif", min = 500, max = 1000),
                                             "proportion-agricultural-area" = list(qfun="qunif", min = 0.05, max = 0.75),
                                             "hh-area-mean-ha" = list(qfun="qunif", min = 0.25, max = 3),
                                             "hh-area-sd-ha" = list(qfun="qunif", min = 0.25, max = 1),
                                             "vlg-area-mean" = list(qfun="qunif", min = 10, max = 20),   ##default 15
                                             "vlg-area-sd" = list(qfun="qunif", min = 2, max = 10),     ## default 6
                                             "field-size-percentage" = list(qfun="qunif", min = 0.1, max = 0.9),
                                             "field-size-sd-ha" = list(qfun="qunif", min = 0.25, max = 2),
                                             "field-strategies-id" = list(qfun="qunif", min = 1, max = 9),
                                             "change-strategy" = list(qfun="qunif", min = 5, max = 20),
                                             "field.shape.factor" = list(qfun="qunif", min = 0.7, max = 2)),
                            constants = list("reproducable?" = "FALSE",   ## random seed is set via nlrx
                                             "write.param.file?" = "TRUE", ## useful for debugging
                                             "setup-model" = "\"agricultural-area\"",
                                             "width" = 100,
                                             "height" = 100,
                                             "cell-length-meter" = 50,
                                             "road.algorithm" = "\"artificial.perlin\"",
                                             "road-map-nr" = 4,
                                             "min-dist-roads" = 5,
                                             "hh-area-distribution" = "\"log-normal\"",
                                             "vlg-area-distribution" = "\"uniform\"",
                                             "field-size-distribution" = "\"normal\"",
                                             "use-field-size-percentage?" = "TRUE",
                                             "occ-probability" = 0,
                                             "inaccessible-area-fraction" = 0,
                                             "set-field-strategies-by-id?" = "TRUE",
                                             "LUT-1-fraction" = 1,
                                             "LUT-2-fraction" = 0,
                                             "LUT-3-fraction" = 0,
                                             "LUT-4-fraction" = 0,
                                             "LUT-5-fraction" = 0,
                                             "LUT-1-specialize" = 0,
                                             "LUT-2-specialize" = 0,
                                             "LUT-3-specialize" = 0,
                                             "LUT-4-specialize" = 0,
                                             "LUT-5-specialize" = 0,
                                             "LUT-fill-up" = "\"LUT-2-fraction\"",
                                             "land-use-assignment" = "\"landscape-level-fraction\"",
                                             "default.maps" = "\"landuse-type\"",
                                             "write-household-ids" = "\"only-first-households\""))

eval_variables_constants(nl)


## Add eFast design:
nl@simdesign <- simdesign_soboljansen(nl=nl,
                                      samples=500,    #500 = 9500 landscapes
                                      sobolnboot = 10,
                                      sobolconf=0.95,
                                      nseeds=1,
                                      precision=3)



# Postprocess siminput for some variables (to integer)
input.values.integer <- c("perlin-octaves", "cone-angle", "households-per-cell", "change-strategy", "field-strategies-id", "total-road-length")
nl@simdesign@siminput <- nl@simdesign@siminput %>% mutate_at(input.values.integer, funs(as.integer))


# Setup HPC future:
setupFuturePlanHPC(template='lsfexclusive.tmpl', 
                   queue='mpi', 
                   walltime='04:00', 
                   coremem=2000, 
                   processes=16)


## Run simulations
LGraf_sim %<-% run_nl_split(nl, parts=20, cleanup = "all")



## Postprocessing:
setsim(nl, "simoutput") <- LGraf_sim
saveRDS(nl, paste0("LGraf_nl_sobol.rds"))
LGraf_sim <- nl@simdesign@simoutput
saveRDS(LGraf_sim, paste0("LGraf_sobol_raw.rds"))
## Convert data to spatial data (raster)
LGraf_sim_sp <- get_nl_spatial(nl, turtles = FALSE, patches=TRUE, format="spatial")

## Calculate landscape metrics for all rasters:
LGraf_sim_sp_lm <- rasterToMetrics(LGraf_sim_sp$metrics.patches, select.metrics)

## Attach metrics as output to the current simoutput table and remove turtle.metrics and patch.metrics
LGraf_sim_lm <- LGraf_sim %>% dplyr::select(-metrics.turtles, -metrics.patches) %>% bind_cols(LGraf_sim_sp_lm)

## Attach to nl to calculate sensitivity indices:
setsim(nl, "simoutput") <- LGraf_sim_lm
sa_index <- sobolIndices(nl, select.metrics)

sa_index$value <- sa_index$original - sa_index$bias
saveRDS(sa_index, paste0("LGraf_sobol_index.rds"))
## Plot:
saIndexPlot(sa_index, "plot_output/sobol_effects.tiff")

## Store data:
saveRDS(LGraf_sim, "data_output/sobol_simulation_results.rds")
saveRDS(sa_index, "data_output/sobol_sensitivity_indices.rds")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. Comparison of sobol landscapes and harapan landscapes:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Convert the metrics from LGraf to long format:
metrics.LG <- LGraf_sim_lm %>% dplyr::select(c('proportion-agricultural-area', select.metrics))
metrics.LG$agri.area <- 10000 * metrics.LG$`proportion-agricultural-area`
metrics.LG$forest.area <- 10000 - metrics.LG$agri.area
metrics.LG <- metrics.LG %>% dplyr::select(-'proportion-agricultural-area') %>% gather(metric, value, -c(agri.area, forest.area))
metrics.LG$source <- "EFForTS-LGraf"

## Filter metrics from the map samples (see step 1):
metrics.map <- lu_harapan_sample %>% dplyr::filter(metric %in% select.metrics) %>% dplyr::select(-id)
metrics.map$source <- "Land-use-map"

## Combine:
metrics.plot <- rbind(metrics.LG, metrics.map)
metrics.plot$lu <- ifelse(substr(metrics.plot$metric, 4, 4) == 0, "matrix", "fields")
metrics.plot$metric <- substr(metrics.plot$metric, 6, nchar(metrics.plot$metric))

## Labels:
metrics.plot$metric <- ifelse(metrics.plot$metric == "landscape.shape.index", "LSI",
                              ifelse(metrics.plot$metric == "largest.patch.index", "LPI",
                                     ifelse(metrics.plot$metric == "patch.cohesion.index", "PCI", metrics.plot$metric)))

## Plot:
validationPlot(metrics.plot, "plot_output/comparison_model_maps.tiff")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. Validation by genetic algorithm:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## Create 3 samples from the land-use map
nsamples <- 3
sampleseed <- 446 #2358
lu_harapan_sample <- sampleLandscapes(lu_harapan, sampleseed, nsamples, 50, 1:nsamples)

## For each sample create a similar map:
plan(multisession)
nl_samples <- furrr::future_map(seq(nsamples), function(h)
{
  library(raster)
  ## Select basemap for  which we want to generate a similar map:
  basemap <- lu_harapan_sample %>% dplyr::filter(metric %in% select.metrics) %>% 
    dplyr::filter(id == h) %>% 
    dplyr::select(-'id') %>% 
    spread(metric, value)
  
  ## Agri area:
  agriproportion <- (basemap$agri.area / 10000)
  
  # Local for testing:
  nl <- nl(nlversion = "6.0.3",
           nlpath = "C:/Program Files/NetLogo 6.0.3/",
           modelpath = "LGraf_Jana/EFForTS-LGraf_new_GUI.nlogo",
           jvmmem = 2024)
  
  nl@experiment <- experiment(expname="LGraf",
                              outpath="C:/out/",
                              repetition=1,
                              tickmetrics="false",
                              idrunnum="foldername",
                              idsetup=c("setup"),
                              idgo=c("establish_fields", "assign-land-uses", "write-status-hpc"),
                              idfinal=NA_character_,
                              metrics.turtles=c("who", "pxcor", "pycor"),
                              metrics.patches=c("pxcor", "pycor", "p_landuse-type"),
                              variables = list("households-per-cell" = list(qfun="qunif", min = 3, max = 5),
                                               "min-distance" = list(qfun="qunif", min=1, max=10),
                                               "dist-weight" = list(qfun="qunif", min = 0, max = 1),
                                               "total-road-length" = list(qfun="qunif", min = 500, max = 1000),
                                               "hh-area-mean-ha" = list(qfun="qunif", min = 0.25, max = 3),
                                               "hh-area-sd-ha" = list(qfun="qunif", min = 0.25, max = 1),
                                               "vlg-area-mean" = list(qfun="qunif", min = 1, max = 10),   ##default 15
                                               "vlg-area-sd" = list(qfun="qunif", min = 1, max = 3),     ## default 6
                                               "field-size-percentage" = list(qfun="qunif", min = 0.1, max = 0.9),
                                               "field-size-sd-ha" = list(qfun="qunif", min = 0.25, max = 2),
                                               "field-strategies-id" = list(qfun="qunif", min = 1, max = 8),
                                               "change-strategy" = list(qfun="qunif", min = 1, max = 10),
                                               "field.shape.factor" = list(qfun="qunif", min = 0.7, max = 1.5)),
                              constants = list("proportion-agricultural-area" = agriproportion,
                                               "reproducable?" = "FALSE",   ## random seed is set via nlrx
                                               "write.param.file?" = "TRUE", ## useful for debugging
                                               "print-messages?" = "FALSE",
                                               "setup-model" = "\"agricultural-area\"",
                                               "width" = 100,
                                               "height" = 100,
                                               "cell-length-meter" = 50,
                                               "road.algorithm" = "\"artificial.perlin\"",
                                               "perlin-octaves" = 2,
                                               "perlin-persistence" = 0.1,
                                               "cone-angle" = 120,
                                               "min-dist-roads" = 5,
                                               "hh-area-distribution" = "\"log-normal\"",
                                               "vlg-area-distribution" = "\"lognormal\"",
                                               "field-size-distribution" = "\"log-normal\"",
                                               "use-field-size-percentage?" = "TRUE",
                                               "occ-probability" = 0,
                                               "inaccessible-area-fraction" = 0,
                                               "set-field-strategies-by-id?" = "TRUE",
                                               "LUT-1-fraction" = 1,
                                               "LUT-2-fraction" = 0,
                                               "LUT-3-fraction" = 0,
                                               "LUT-4-fraction" = 0,
                                               "LUT-5-fraction" = 0,
                                               "LUT-1-specialize" = 0,
                                               "LUT-2-specialize" = 0,
                                               "LUT-3-specialize" = 0,
                                               "LUT-4-specialize" = 0,
                                               "LUT-5-specialize" = 0,
                                               "LUT-fill-up" = "\"LUT-2-fraction\"",
                                               "land-use-types" = "\"landscape-level-fraction\"",
                                               "default.maps" = "\"landuse-type\"",
                                               "write-household-ids" = "\"only-first-households\""))
  
  genalg.popsize <- 50  # 100
  genalg.iters <- 25 
  
  nl@simdesign <- simdesign_GenAlg(nl, popSize = genalg.popsize, iters=genalg.iters, nseeds=1)
  
  ## Run optimization for current map:
  results <- run_nl_dyn_mod(nl, seed=getsim(nl, "simseeds")[1], basemap = basemap)
  
  fitness <- data.frame(generation=seq(1:length(results$mean)), evaluation=results$mean)
  
  fitness.plot <- ggplot(fitness, aes(x=generation, y=evaluation)) +
    geom_line(size=1) +
    theme_minimal() +
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=14))
  
  ggsave(plot=fitness.plot, paste0("mapsample_", h, "_fitness.png"), width = 6.0, height = 6.0, dpi=300)
  
  ## Extract variable values:
  results.summary <- strsplit(summary(results), " ")
  results.summary.varend <- (length(results.summary[[1]]) - 1)
  results.summary.varstart <- (results.summary.varend - length(nl@experiment@variables) + 1)
  
  results.summary <- setNames(as.list(as.numeric(results.summary[[1]][results.summary.varstart:results.summary.varend])), names(nl@experiment@variables))
  
  fitness.best.table <- tableGrob(tibble(var=names(results.summary), value=unlist(results.summary)), rows=rep("",length(results.summary)))
  ggsave(plot=grid.arrange(fitness.best.table), paste0("mapsample_", h, "_bestparam.png"), width = 3.5, height = 5.0, dpi=300)
  
  
  # Store best parameterization together with remaining constants as constants list
  all.constants <- c(nl@experiment@constants, results.summary)
  
  ## Do run with this parameterisation:
  nl <- nl(nlversion = "6.0.3",
           nlpath = "C:/Program Files/NetLogo 6.0.3/",
           modelpath = "LGraf_Jana/EFForTS-LGraf_new_GUI.nlogo",
           jvmmem = 2024)
  
  nl@experiment <- experiment(expname="LGraf",
                              outpath="C:/out/",
                              repetition=1,
                              tickmetrics="false",
                              idrunnum="foldername",
                              idsetup=c("setup"),
                              idgo=c("establish_fields", "assign-land-uses"),
                              idfinal=NA_character_,
                              metrics.turtles=c("who", "pxcor", "pycor"),
                              metrics.patches=c("pxcor", "pycor", "p_landuse-type"),
                              constants = all.constants)
  
  nl@simdesign <- simdesign_simple(nl, nseeds=5)
  
  res_final <- run_nl_all(nl)
  setsim(nl, "simoutput") <- res_final
  
  
  return(nl)
})

## Store nlsamples as rds:
saveRDS(nl_samples, file="LGraf_validation_genAlg.rds")


## Postprocessing:


mapdev.all <- NULL
for (i in 1:length(nl_samples))
{
  nl <- nl_samples[[i]]
  
  basemap <- lu_harapan_sample %>% dplyr::filter(metric %in% select.metrics) %>% 
    dplyr::filter(id == i) %>% 
    dplyr::select(-'id') %>% 
    spread(metric, value)
  
  res_final_sp <- get_nl_spatial(nl, turtles = TRUE, patches = TRUE, format = "spatial")
  
  ## Define metrics
  select.metrics <- c("cs.0.landscape.shape.index",
                      "cs.0.largest.patch.index",
                      "cs.0.mean.patch.area",
                      "cs.0.n.patches",
                      "cs.0.patch.cohesion.index",
                      "cs.1.landscape.shape.index",
                      "cs.1.largest.patch.index",
                      "cs.1.mean.patch.area",
                      "cs.1.n.patches",
                      "cs.1.patch.cohesion.index")
  
  
  ## Calculate landscape metrics for all rasters:
  lgraf_metrics <- rasterToMetrics(res_final_sp$metrics.patches, select.metrics)
  lgraf_metrics <- nl@simdesign@simoutput %>% dplyr::select('proportion-agricultural-area') %>% bind_cols(lgraf_metrics)
  lgraf_metrics <- lgraf_metrics %>% dplyr::select(c('proportion-agricultural-area', select.metrics))
  lgraf_metrics$agri.area <- 10000 * lgraf_metrics$`proportion-agricultural-area`
  lgraf_metrics$forest.area <- 10000 - lgraf_metrics$agri.area
  lgraf_metrics <- lgraf_metrics %>% dplyr::select(-'proportion-agricultural-area') %>% 
    dplyr::select(forest.area, agri.area, everything())
  
  
  mapdevs <- NULL
  
  ## Plot raster map for each generated map and calculate metrics deviance:
  for (k in 1:nrow(res_final_sp))
  {
    ## Calculate deviances:
    mapdev <- (basemap - lgraf_metrics[k,]) / basemap
    mapdev$sum <- sum(abs(mapdev))
    mapdev$sampleid <- i
    mapdev$lgrafid <- k
    mapdevs <- rbind(mapdevs, mapdev)
    
    ## Convert raster to DF and create nice plot of harapan raster:
    res_final_sp.df <- data.frame(rasterToPoints(res_final_sp$metrics.patches[[k]]))
    names(res_final_sp.df) <- c("x", "y", "z")
    
    cols <- c("0"="#8D8D8D", "1"="#fde725ff")
    ggplot(res_final_sp.df, aes(x=x, y=y, fill=factor(z))) +
      geom_raster() +
      coord_equal() +
      scale_fill_manual(values = cols) +
      guides(fill=FALSE) +
      theme_void() + 
      scale_x_continuous(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      labs(x = NULL, y = NULL)
    
    ggsave(paste0("mapsample_", i, "_lgrafsample_", k, ".png"), width = 6.0, height = 6.0, dpi=300)
  }
  
  ## Convert to long format
  mapdevs <- mapdevs %>% gather(metric, value, 1:12)
  ## Add labels
  mapdevs$lu <- ifelse(substr(mapdevs$metric, 4, 4) == 0, "matrix", "fields")
  mapdevs$metric <- ifelse(mapdevs$metric %in% c("forest.area", "agri.area"), mapdevs$metric, substr(mapdevs$metric, 6, nchar(mapdevs$metric)))
  mapdevs$metric <- ifelse(mapdevs$metric == "landscape.shape.index", "LSI",
                           ifelse(mapdevs$metric == "largest.patch.index", "LPI",
                                  ifelse(mapdevs$metric == "patch.cohesion.index", "PCI", mapdevs$metric)))
  
  
  mapdev.all <- rbind(mapdev.all, mapdevs)
  
}

saveRDS(mapdev.all, "LGraf_genalg_deviations.rds")


### Mapdevs plot:
library(ggpubr)
ggplot(mapdevs, aes(x=metric, y=value, fill=lu)) +
  #coord_flip() +
  geom_hline(yintercept = 0) +
  geom_boxplot() +
  theme_classic2()


## Plot deviations:

library(extrafont)
extrafont::loadfonts()
library(ggsci)
library(hrbrthemes)
mapdevs.agg <- mapdevs %>% group_by(lu, metric) %>% 
  summarize(value.mu = mean(value), value.sd = sd(value))
mapdevs.agg[2,1] <- "matrix"
mapdevs.agg[1,2] <- "total.area"
mapdevs.agg[2,2] <- "total.area"
cols <- c("matrix"="#8D8D8D", "fields"="#fdd610ff")

tiff("plot_output/validation_devs.tiff", width=17.5, height=10, units="cm", res=300)
ggplot(mapdevs.agg, aes(x=metric, y=value.mu, color=lu)) +
  geom_pointrange(aes(ymin=value.mu - value.sd, ymax=value.mu + value.sd), position = position_dodge(width=0.5), fatten=3, size=1) +
  guides(color="none") +
  xlab("Landscape metric") +
  ylab("Percentage deviance") +
  theme_ipsum(base_size=12, axis_title_size = 12, axis_title_just = "m") +
  scale_color_manual(values = cols)
dev.off()


## Multiplot with maps:

library(png)
library(ggplot2)
library(tidyverse)
library(ggpubr)

mapplot.dir <- file.path("D:/owncloud/CRC/00_Manuscripts/EFForTS-LGraf/LGrafAnalysisFinal_v1/plot_output/maps_validation_final/")

sample1 <- grid::rasterGrob(readPNG(file.path(mapplot.dir, "mapsample_1.png")))
sample2 <- grid::rasterGrob(readPNG(file.path(mapplot.dir, "mapsample_2.png")))
sample3 <- grid::rasterGrob(readPNG(file.path(mapplot.dir, "mapsample_3.png")))

lgraf11 <- grid::rasterGrob(readPNG(file.path(mapplot.dir, "mapsample_1_lgrafsample_1.png")))
lgraf12 <- grid::rasterGrob(readPNG(file.path(mapplot.dir, "mapsample_1_lgrafsample_2.png")))
lgraf13 <- grid::rasterGrob(readPNG(file.path(mapplot.dir, "mapsample_1_lgrafsample_3.png")))
lgraf14 <- grid::rasterGrob(readPNG(file.path(mapplot.dir, "mapsample_1_lgrafsample_4.png")))
lgraf21 <- grid::rasterGrob(readPNG(file.path(mapplot.dir, "mapsample_2_lgrafsample_1.png")))
lgraf22 <- grid::rasterGrob(readPNG(file.path(mapplot.dir, "mapsample_2_lgrafsample_2.png")))
lgraf23 <- grid::rasterGrob(readPNG(file.path(mapplot.dir, "mapsample_2_lgrafsample_3.png")))
lgraf24 <- grid::rasterGrob(readPNG(file.path(mapplot.dir, "mapsample_2_lgrafsample_4.png")))
lgraf31 <- grid::rasterGrob(readPNG(file.path(mapplot.dir, "mapsample_3_lgrafsample_1.png")))
lgraf32 <- grid::rasterGrob(readPNG(file.path(mapplot.dir, "mapsample_3_lgrafsample_2.png")))
lgraf33 <- grid::rasterGrob(readPNG(file.path(mapplot.dir, "mapsample_3_lgrafsample_3.png")))
lgraf34 <- grid::rasterGrob(readPNG(file.path(mapplot.dir, "mapsample_3_lgrafsample_4.png")))

# Store sample pngs as plots with border and margin:
mg <- 2
s1 <- ggarrange(sample1, ncol=1, nrow=1)+ theme(plot.margin = margin(rep(mg, 4)), plot.background = element_rect(color = "black"))
s2 <- ggarrange(sample2, ncol=1, nrow=1)+ theme(plot.margin = margin(rep(mg, 4)), plot.background = element_rect(color = "black")) 
s3 <- ggarrange(sample3, ncol=1, nrow=1)+ theme(plot.margin = margin(rep(mg, 4)), plot.background = element_rect(color = "black")) 

# Create subplots from lgraf samples with border without margin:
mg <- 2
lg11 <- ggarrange(lgraf11, ncol=1, nrow=1)+ theme(plot.margin = margin(rep(mg, 4)), plot.background = element_rect(color = "black"))
lg12 <- ggarrange(lgraf12, ncol=1, nrow=1)+ theme(plot.margin = margin(rep(mg, 4)), plot.background = element_rect(color = "black"))
lg13 <- ggarrange(lgraf13, ncol=1, nrow=1)+ theme(plot.margin = margin(rep(mg, 4)), plot.background = element_rect(color = "black"))
lg14 <- ggarrange(lgraf14, ncol=1, nrow=1)+ theme(plot.margin = margin(rep(mg, 4)), plot.background = element_rect(color = "black"))
lg21 <- ggarrange(lgraf21, ncol=1, nrow=1)+ theme(plot.margin = margin(rep(mg, 4)), plot.background = element_rect(color = "black"))
lg22 <- ggarrange(lgraf22, ncol=1, nrow=1)+ theme(plot.margin = margin(rep(mg, 4)), plot.background = element_rect(color = "black"))
lg23 <- ggarrange(lgraf23, ncol=1, nrow=1)+ theme(plot.margin = margin(rep(mg, 4)), plot.background = element_rect(color = "black"))
lg24 <- ggarrange(lgraf24, ncol=1, nrow=1)+ theme(plot.margin = margin(rep(mg, 4)), plot.background = element_rect(color = "black"))
lg31 <- ggarrange(lgraf31, ncol=1, nrow=1)+ theme(plot.margin = margin(rep(mg, 4)), plot.background = element_rect(color = "black"))
lg32 <- ggarrange(lgraf32, ncol=1, nrow=1)+ theme(plot.margin = margin(rep(mg, 4)), plot.background = element_rect(color = "black"))
lg33 <- ggarrange(lgraf33, ncol=1, nrow=1)+ theme(plot.margin = margin(rep(mg, 4)), plot.background = element_rect(color = "black"))
lg34 <- ggarrange(lgraf34, ncol=1, nrow=1)+ theme(plot.margin = margin(rep(mg, 4)), plot.background = element_rect(color = "black"))

# Create subpanels for lgraf with margin:
fontstyle <- list(size=10, face="plain", color="black")
mg <- 2
lg1 <- ggarrange(lg11, lg12, lg13, lg14, ncol=2, nrow=2, labels=c("A.1", "A.2", "A.3", "A.4"), font.label=fontstyle)+ theme(plot.margin = margin(rep(mg, 4)), plot.background = element_rect(color = "black"))
lg2 <- ggarrange(lg21, lg22, lg23, lg24, ncol=2, nrow=2, labels=c("B.1", "B.2", "B.3", "B.4"), font.label=fontstyle)+ theme(plot.margin = margin(rep(mg, 4)), plot.background = element_rect(color = "black"))
lg3 <- ggarrange(lg31, lg32, lg33, lg34, ncol=2, nrow=2, labels=c("C.1", "C.2", "C.3", "C.4"), font.label=fontstyle)+ theme(plot.margin = margin(rep(mg, 4)), plot.background = element_rect(color = "black"))

# Final plot:
tiff("plot_output/validation_maps.tiff", width=18, height=12, units="cm", res=300)
ggarrange(s1, s2, s3, lg1, lg2, lg3,
          ncol=3, nrow=2, 
          labels=c("A", "B", "C", "", "", ""),
          label.x = 0,
          align = "hv",
          font.label = fontstyle)


dev.off()





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5. Specialization experiment:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(nlrx)

nl <- nl(nlversion = "6.0.3",
         nlpath = "C:/Program Files/NetLogo 6.0.3/",
         modelpath = "LGraf/EFForTS-LGraf_trial.nlogo",
         jvmmem = 2024)

nl@experiment <- experiment(expname="LGraf",
                            outpath="C:/out/",
                            repetition=1,
                            tickmetrics="false",
                            idrunnum="foldername",
                            idsetup=c("setup"),
                            idgo=c("establish_fields", "assign-land-uses"),
                            idfinal=NA_character_,
                            metrics.patches=c("pxcor", "pycor", "p_landuse-type"),
                            variables = list("hh-area-mean-ha" = list(min=1, max=3, step=0.1, qfun="qunif"),
                                             "LUT-1-specialize" = list(min=0, max=1, step=0.1, qfun="qunif")),
                            constants = list("households-per-cell" = 1,
                                             "setup-model" = "\"agricultural-area\"",
                                             "proportion-agricultural-area" = 0.5,
                                             "reproducable?" = "FALSE",   ## random seed is set via nlrx
                                             "write.param.file?" = "TRUE", ## useful for debugging
                                             "width" = 100,
                                             "height" = 100,
                                             "cell-length-meter" = 50,
                                             "road.algorithm" = "\"real.shapefile\"",
                                             "road-map-nr" = 3,
                                             "occ-probability" = 0,
                                             "min-dist-roads" = 5,
                                             "hh-area-sd-ha" = 0.91,
                                             "hh-area-distribution" = "\"log-normal\"",
                                             "vlg-area-distribution" = "\"uniform\"",
                                             "vlg-area-mean" = 15,
                                             "vlg-area-sd" = 6,
                                             "min-distance" = 10,
                                             "field-size-distribution" = "\"log-normal\"",
                                             "use-field-size-percentage?" = "FALSE",
                                             "field-size-percentage" = 0,
                                             "field-size-mean-ha" = 0.49,
                                             "field-size-sd-ha" = 0.77,
                                             "set-field-strategies-by-id?" = "TRUE",
                                             "field-strategies-id" = 1,
                                             "change-strategy" = 10,
                                             "field.shape.factor" = 1,
                                             "inaccessible-area-fraction" = 0,
                                             "LUT-1-name" = "\"oilpalm\"",
                                             "LUT-2-name" = "\"rubber\"",
                                             "LUT-1-fraction" = 0.5,
                                             "LUT-2-fraction" = 0.5,
                                             "LUT-3-fraction" = 0,
                                             "LUT-4-fraction" = 0,
                                             "LUT-5-fraction" = 0,
                                             "LUT-2-specialize" = 0,
                                             "LUT-3-specialize" = 0,
                                             "LUT-4-specialize" = 0,
                                             "LUT-5-specialize" = 0,
                                             "LUT-fill-up" = "\"LUT-2-fraction\"",
                                             "land-use-assignment" = "\"household-level-specialization\"",
                                             "default.maps" = "\"landuse-type\"",
                                             "write-household-ids" = "\"only-first-households\""))
                                             

eval_variables_constants(nl)

nl@simdesign <- simdesign_lhs(nl=nl,
                              samples=500,
                              nseeds=3,
                              precision=3)


plan(multiprocess)
spec_sim <- run_nl_all(nl)

## Postprocessing:
setsim(nl, "simoutput") <- spec_sim
saveRDS(nl, paste0("LGraf_nl_spec.rds"))
saveRDS(nl@simdesign@simoutput, "data_output/specialization_simulation_results.rds")

## Convert data to spatial data (raster)
spec_sim_sp <- get_nl_spatial_noTurtles(nl)

## Calculate landscape metrics for all rasters:
select.metrics <- c("cs.0.landscape.shape.index",
                    "cs.0.largest.patch.index",
                    "cs.0.mean.patch.area",
                    "cs.0.n.patches",
                    "cs.0.patch.cohesion.index",
                    "cs.1.landscape.shape.index",
                    "cs.1.largest.patch.index",
                    "cs.1.mean.patch.area",
                    "cs.1.n.patches",
                    "cs.1.patch.cohesion.index",
                    "cs.2.landscape.shape.index",
                    "cs.2.largest.patch.index",
                    "cs.2.mean.patch.area",
                    "cs.2.n.patches",
                    "cs.2.patch.cohesion.index")


spec_sim_sp_lm <- rasterToMetrics(spec_sim_sp[[2]], select.metrics)

## Attach metrics as output to the current simoutput table and remove turtle.metrics and patch.metrics
spec_sim_lm <- spec_sim %>% dplyr::select(-metrics.patches) %>% bind_cols(spec_sim_sp_lm)

## Remove all parameters that we dont need:
spec_sim_lm <- spec_sim_lm %>% dplyr::select(`hh-area-mean-ha`, `LUT-1-specialize`, select.metrics) %>% 
  gather(metric, value, -1, -2)

## Add landuse id:
spec_sim_lm$lu <- ifelse(substr(spec_sim_lm$metric, 4, 4) == 0, "matrix", ifelse(substr(spec_sim_lm$metric, 4, 4) == 1, "oilpalm", "rubber"))
spec_sim_lm$metric <- substr(spec_sim_lm$metric, 6, nchar(spec_sim_lm$metric))

## HH size group:
spec_sim_lm$hh.size <- ifelse(spec_sim_lm$`hh-area-mean-ha` <= 1.65, "small", ifelse(spec_sim_lm$`hh-area-mean-ha` <= 2.32, "medium", "large"))

## PLOT:
library(grid)

#png("plot_output/specialization.png", width=30, height=25, units="cm", res=300)
tiff("plot_output/specialization.tiff", width=19, height=12, units="cm", res=300)
ggplot(spec_sim_lm, aes(x=`LUT-1-specialize`, y=value, color=hh.size)) +
  facet_wrap(~lu+metric, scales="free", ncol=5) +
  geom_point(alpha=0.3, size=0.7) +
  geom_smooth(size=0.7) +
  scale_color_viridis_d() +
  scale_x_continuous(limits=c(0,1), breaks=c(0,0.5,1)) +
  xlab("oilpalm specialization level") +
  ylab("") +
  theme_classic() +
  guides(color="none") +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        plot.margin = unit(c(3,1,1,1), "lines"),
        legend.position = "top",
        axis.text=element_text(size=11),
        axis.title = element_text(size=11),
        panel.spacing=unit(0.5, "lines"))



grid.text("LSI", x = unit(0.14, "npc"), y = unit(0.95, "npc"), gp=gpar(fontsize=11))
grid.text("LPI", x = unit(0.33, "npc"), y = unit(0.95, "npc"), gp=gpar(fontsize=11))
grid.text("mean patch area", x = unit(0.52, "npc"), y = unit(0.95, "npc"), gp=gpar(fontsize=11))
grid.text("n patches", x = unit(0.71, "npc"), y = unit(0.95, "npc"), gp=gpar(fontsize=11))
grid.text("PCI", x = unit(0.9, "npc"), y = unit(0.95, "npc"), gp=gpar(fontsize=11))
grid.text("index value\nmatrix", x = unit(0.025, "npc"), y = unit(0.77, "npc"), rot=90, gp=gpar(fontsize=11))
grid.text("index value\noilpalm", x = unit(0.025, "npc"), y = unit(0.5, "npc"), rot=90, gp=gpar(fontsize=11))
grid.text("index value\nrubber", x = unit(0.025, "npc"), y = unit(0.2, "npc"), rot=90, gp=gpar(fontsize=11))

dev.off()


#### Calculate models from raw data:


# Calculate landscape metrics for all rasters:
select.metrics <- c("cs.0.landscape.shape.index",
                    "cs.0.largest.patch.index",
                    "cs.0.mean.patch.area",
                    "cs.0.n.patches",
                    "cs.0.patch.cohesion.index",
                    "cs.1.landscape.shape.index",
                    "cs.1.largest.patch.index",
                    "cs.1.mean.patch.area",
                    "cs.1.n.patches",
                    "cs.1.patch.cohesion.index",
                    "cs.2.landscape.shape.index",
                    "cs.2.largest.patch.index",
                    "cs.2.mean.patch.area",
                    "cs.2.n.patches",
                    "cs.2.patch.cohesion.index")



spec_sim_sp_lm <- rasterToMetrics(spec_sim_sp[[2]], select.metrics)

## Attach metrics as output to the current simoutput table and remove turtle.metrics and patch.metrics
spec_sim_lm <- spec_sim %>% dplyr::select(-metrics.patches) %>% bind_cols(spec_sim_sp_lm)

## Remove all parameters that we dont need:
spec_sim_lm <- spec_sim_lm %>% dplyr::select(`hh-area-mean-ha`, `LUT-1-specialize`, select.metrics) %>% 
  gather(metric, value, -1, -2)

## Add landuse id:
spec_sim_lm$lu <- ifelse(substr(spec_sim_lm$metric, 4, 4) == 0, "others", ifelse(substr(spec_sim_lm$metric, 4, 4) == 1, "oilpalm", "rubber"))
spec_sim_lm$metric <- substr(spec_sim_lm$metric, 6, nchar(spec_sim_lm$metric))

## Calculate standardized regression coefficients (SRC)
spec_sim_lm_models <- NULL
for (i in unique(spec_sim_lm$metric))
{
  print(i)
  for (j in unique(spec_sim_lm$lu))
  {
    spec_sim_lm.ij <- spec_sim_lm %>% dplyr::filter(metric == i) %>% dplyr::filter(lu == j)
    #plot(value ~ `hh-area-mean-ha` + `LUT-1-specialize`, data=spec_sim_lm.ij)
    spec_sim_lm.ij.fit <- lm(scale(value) ~ scale(`hh-area-mean-ha`) * scale(`LUT-1-specialize`), data=spec_sim_lm.ij)
    #plot(spec_sim_lm.ij.fit)
    test <- summary(spec_sim_lm.ij.fit)
    
    spec_sim_lm_models.ij.hh <- tibble(metric=i,
                                       lu=j,
                                       var="hh.size",
                                       coeff=test$coefficients[2,1],
                                       coeff.scale=test$coefficients[2,1] / test$coefficients[1,1],
                                       p=test$coefficients[2,4],
                                       sig=ifelse(test$coefficients[2,4] < 0.05, "*", ""))
    
    spec_sim_lm_models.ij.lut <- tibble(metric=i,
                                        lu=j,
                                        var="lut.spec",
                                        coeff=test$coefficients[3,1],
                                        coeff.scale=test$coefficients[3,1] / test$coefficients[1,1],
                                        p=test$coefficients[3,4],
                                        sig=ifelse(test$coefficients[3,4] < 0.05, "*", ""))
    
    spec_sim_lm_models.ij.hh.lut <- tibble(metric=i,
                                           lu=j,
                                           var="hh.size*lut.spec",
                                           coeff=test$coefficients[4,1],
                                           coeff.scale=test$coefficients[4,1] / test$coefficients[1,1],
                                           p=test$coefficients[4,4],
                                           sig=ifelse(test$coefficients[4,4] < 0.05, "*", ""))
    
    spec_sim_lm_models <- rbind(spec_sim_lm_models, spec_sim_lm_models.ij.hh, spec_sim_lm_models.ij.lut, spec_sim_lm_models.ij.hh.lut)
    
  }
}

spec_sim_lm_models$var <- factor(spec_sim_lm_models$var, 
                                 levels=c("hh.size", "lut.spec", "hh.size*lut.spec"),
                                 labels=c("size", "specialization", "size*specialization"))

spec_sim_lm_models$lu <- factor(spec_sim_lm_models$lu, levels=c("oilpalm", "rubber", "others"))

## Filte rout unsignificant bars:
spec_sim_lm_models <- spec_sim_lm_models %>% dplyr::filter(sig == "*")

saveRDS(spec_sim_lm_models, "data_output/specialization_models.rds")

library(ggsci)

tiff("plot_output/specialization.tiff", width=16, height=12, units="cm", res=300)
ggplot(spec_sim_lm_models, aes(x=lu, y=coeff, fill=var)) +
  facet_wrap(~metric, ncol=3) +
  geom_bar(stat="identity") +
  guides(alpha="none", fill=guide_legend(title="parameter")) +
  scale_fill_jama() +
  geom_hline(yintercept = 0) +
  guides(fill=guide_legend(title="Parameter")) +
  xlab("Patch type") +
  ylab("Standardized regression coefficient (SRC)") +
  theme_minimal() +
  #theme_ipsum(base_size=11, axis_title_size = 11, axis_title_just = "m") +
  theme(panel.spacing=unit(0.5, "lines"),
        legend.position = c(0.85,0.2),
        axis.text = element_text(size=11, color="black"),
        axis.title = element_text(size=11, color="black"),
        strip.text = element_text(size=11, color="black"),
        legend.text = element_text(size=11, color="black"),
        legend.title = element_text(size=11, color="black"))
dev.off()

