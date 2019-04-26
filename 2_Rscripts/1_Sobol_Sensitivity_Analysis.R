######################################################################################
# EFForTS-LGraf: Sobol sensitivity analysis                                          
######################################################################################
#
# This script was used to execute a sensitivity analysis of EFForTS-LGraf.
# We used the nlrx package to set up simulations and calculate sensitivity indices.
#
######################################################################################

######################################################################################
# Step 1: Globals
######################################################################################

# Load packages:
library(nlrx)
library(tidyverse)
library(raster)
library(SDMTools)

# Set R random seed:
set.seed(6784557)

######################################################################################
# Step 2: Use nlrx to run simulations
######################################################################################

# Define nl object:
nl <- nl(nlversion = "6.0.4",
         nlpath = file.path("1_Model/NetLogo 6.0.4/"),
         modelpath = file.path("1_Model/EFForTS-LGraf/EFForTS-LGraf.nlogo"),
         jvmmem = 2024)

# Define experiment (varying all model parameters):
nl@experiment <- experiment(expname="LGraf",
                            outpath=file.path("3_Data"),
                            repetition=1,
                            tickmetrics="false",
                            idrunnum="foldername",
                            idsetup=c("setup"),
                            idgo=c("establish_fields", "assign-land-uses"),
                            idfinal=NA_character_,
                            metrics.turtles=list("turtles" = c("who", "pxcor", "pycor")),
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
                                             "write.param.file?" = "FALSE", ## useful for debugging
                                             "print-messages?" = "FALSE",
                                             "setup-model" = "\"agricultural-area\"",
                                             "width" = 100,
                                             "height" = 100,
                                             "cell-length-meter" = 50,
                                             "road.algorithm" = "\"artificial.perlin\"",
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
                                             "land-use-types" = "\"landscape-level-fraction\"",
                                             "default.maps" = "\"landuse-type\"",
                                             "write-household-ids" = "\"only-first-households\""))

# Evaluate experiment:
eval_variables_constants(nl)

# Add a soboljansen simdesign:
nl@simdesign <- simdesign_soboljansen(nl=nl,
                                      samples=500,
                                      sobolnboot = 10,
                                      sobolconf=0.95,
                                      nseeds=1,
                                      precision=3)

# Convert some variables of the siminput parameter matrix to integer values:
input.values.integer <- c("perlin-octaves", "cone-angle", "households-per-cell", "change-strategy", "field-strategies-id", "total-road-length")
nl@simdesign@siminput <- nl@simdesign@siminput %>% mutate_at(input.values.integer, list(as.integer))

# Execute simulations in parallel
# Detect number of cores and split the parameter matrix into chunks with run_nl_all()
ncores <- 10
LGraf_sim <- run_nl_all(nl, split=ncores)

# Attach results to nl object
setsim(nl, "simoutput") <- LGraf_sim
# Store nl object as rds file
saveRDS(nl, file.path("3_Data/Sobol_nl.rds"))
# Store raw data as rds file
saveRDS(LGraf_sim, file.path("3_Data/Sobol_raw.rds"))

######################################################################################
# Step 3: Output post processing to calculate sensitivity indices
######################################################################################

# Convert metrics.patches to raster data
LGraf_sim_sp <- nl_to_raster(nl)

# Which landscape metrics do we want to calculate?
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


# Calculate landscape metrics for all rasters:
LGraf_sim_sp_lm <- purrr::map_dfr(LGraf_sim_sp$spatial.raster, function(x) {
  # Calculate patch and class statistics
  map.ps <- PatchStat(x)
  map.cs <- ClassStat(x)
  
  ## We have multiple classes but we need one row of observations for each run -> convert to wide format:
  map.ps.wide <- map.ps %>%
    gather(key, val, 2:12) %>%
    mutate(stat="ps") %>% 
    unite(key2, stat, patchID, key, sep = ".") %>%
    spread(key2, val) 
  map.cs.wide <- map.cs %>%
    gather(key, val, 2:38) %>%
    mutate(stat="cs") %>% 
    unite(key2, stat, class, key, sep = ".") %>%
    spread(key2, val)
  
  map.pscs.wide <- cbind(map.ps.wide, map.cs.wide)
})

# Filter table and keep only selected metrics:
LGraf_sim_sp_lm <- LGraf_sim_sp_lm %>% dplyr::select(select.metrics)

# Attach metrics as output to the current simoutput table and remove turtle.metrics and patch.metrics
LGraf_sim_lm <- LGraf_sim %>% dplyr::select(-metrics.turtles, -metrics.patches) %>% bind_cols(LGraf_sim_sp_lm)

# Attach to nl to calculate sensitivity indices:
saveRDS(LGraf_sim_lm, file.path("3_Data/Sobol_lm.rds"))
setsim(nl, "simoutput") <- LGraf_sim_lm
#sa_index <- sobolIndices(nl, select.metrics)

sa_index <- analyze_nl(nl)

sa_index$value <- sa_index$original - sa_index$bias
saveRDS(sa_index, file.path("3_Data/Sobol_indices.rds"))

# PLOTTING:
# Add class variable:
sa_index$class <- substr(sa_index$metric, 0, 4)
sa_index$class <- ifelse(sa_index$class == "cs.0", "others", "fields")
sa_index$landscapeMetric <- substr(sa_index$metric, 6, nchar(sa_index$metric))
sa_index$landscapeMetricType <- substr(sa_index$landscapeMetric, (nchar(sa_index$landscapeMetric) - 2), nchar(sa_index$landscapeMetric))
sa_index$landscapeMetric <- sub("*_.*", "", sa_index$landscapeMetric)
sa_index$landscapeMetric <- ifelse(sa_index$landscapeMetric == "landscape.shape.index", "LSI",
                                   ifelse(sa_index$landscapeMetric == "largest.patch.index", "LPI",
                                          ifelse(sa_index$landscapeMetric == "mean.patch.area", "mean patch area",
                                                 ifelse(sa_index$landscapeMetric == "n.patches", "n patches", "PCI"))))

# only keep means of indices:
sa_index <- sa_index %>% filter(landscapeMetricType=="ean")
sa_index_total <- sa_index %>% filter(index=="total")
sa_index_first <- sa_index %>% filter(index=="first-order")

fontsize <- 10
ggplot(sa_index_total) +
  facet_grid(.~landscapeMetric, scales="free") +
  geom_tile(data=sa_index_total, aes(x=class, y=parameter, fill=value)) +
  geom_point(data=sa_index_first, aes(x=class, y=parameter, fill=value), size=4, shape=21) +
  scale_fill_gradient(low="white", high="black") +
  guides(fill=FALSE) +
  xlab("land-use class") +
  ylab("parameter") +
  theme_classic() +
  theme(strip.text=element_text(size=fontsize),
        axis.text=element_text(size=fontsize),
        axis.title=element_text(size=fontsize),
        panel.spacing=unit(0, "lines"))

ggsave(file.path("4_Plots/sobol_effects.triff"), width = 7.5, height = 4.5, dpi=300)


######################################################################################
# Step 4: Compare maps with classified land-use map
######################################################################################

# Convert the metrics from LGraf to long format:
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
# Loop over land-use and metrics:
plotmetrics <- as.list(expand.grid(unique(metrics.plot$metric), unique(metrics.plot$lu)))

# Convert cells to cells / 1000:
metrics.plot$forest.area <- metrics.plot$forest.area / 1000
allplots <- purrr::map(seq_along(plotmetrics[[1]]), function(x) {
  ## Subset data:
  plotdata <- metrics.plot %>% filter(metric==plotmetrics[[1]][[x]] & lu==plotmetrics[[2]][[x]])
  ## Plot
  p1 <- plotOneIndex(plotdata)
  return(p1)
})

lay <- rbind(c(1,2,3,4,5),
             c(6,7,8,9,10)) 

pall <- grid.arrange(arrangeGrob(allplots[[1]], top="LSI", left="index value matrix"),
                     arrangeGrob(allplots[[2]], top="LPI"),
                     arrangeGrob(allplots[[3]], top="mean.patch.area"),
                     arrangeGrob(allplots[[4]], top="n.patches"),
                     arrangeGrob(allplots[[5]], top="PCI"),
                     arrangeGrob(allplots[[6]], bottom="", left="index value fields"),
                     arrangeGrob(allplots[[7]], bottom=""),
                     arrangeGrob(allplots[[8]], bottom="matrix area [1000 cells]"),
                     arrangeGrob(allplots[[9]], bottom=""),
                     arrangeGrob(allplots[[10]], bottom=""),
                     layout_matrix=lay)

ggsave(file.path("4_Plots/comparison_model_maps.tiff"), width=7.5, height=3.5, pall, dpi=300)

