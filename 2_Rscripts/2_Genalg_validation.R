######################################################################################
# EFForTS-LGraf: Genetic algorithm validation                                    
######################################################################################
#
# Rebuild sample landscapes from landuse map with genetic algorithm
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
library(future)

# Set R random seed:
set.seed(6784557)

## Postprocessing function for genetic algorithm:
galg_postpro <- function(nl, results, basemap) {
  
  
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
  
  
  ## Calculate metrics for current map:
  ## Postprocessing:
  setsim(nl, "simoutput") <- results
  
  lgraf_metrics <- purrr::map_dfr(nl@simdesign@simoutput$metrics.patches, function(x) {
    # Convert to landuse raster:
    x <- x %>% dplyr::select(pxcor, pycor, `p_landuse-type`)
    x <- raster::rasterFromXYZ(x)
    
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
  
  # Bind to results:
  lgraf_metrics <- results %>% dplyr::select('proportion-agricultural-area') %>% bind_cols(lgraf_metrics)
  lgraf_metrics <- lgraf_metrics %>% dplyr::select(c('proportion-agricultural-area', select.metrics))
  lgraf_metrics$agri.area <- 10000 * lgraf_metrics$`proportion-agricultural-area`
  lgraf_metrics$forest.area <- 10000 - lgraf_metrics$agri.area
  lgraf_metrics <- lgraf_metrics %>% dplyr::select(-'proportion-agricultural-area') %>% 
    dplyr::select(forest.area, agri.area, everything())
  
  
  mapdev <- (abs(basemap - lgraf_metrics)) / basemap
  #mapdev <- mapdev %>% mutate_all(funs(ifelse(. < 5, TRUE, FALSE)))
  #mapmatch <- length(mapdev[mapdev==TRUE]) / length(mapdev)
  mapmatch <- sum(mapdev)
  
  
  return(as.numeric(mapmatch))
}

### Modified run_nl_dyn function to allow postprocessing:
run_nl_dyn_mod <- function(nl,
                           seed,
                           cleanup.csv = TRUE,
                           cleanup.xml = TRUE,
                           cleanup.bat = TRUE,
                           silent = TRUE,
                           basemap=NA) {
  nl_results <- NULL
  
  if (getsim(nl, "simmethod") == "GenAlg") {
    nl_results <- util_run_nl_dyn_GenAlg_mod(
      nl = nl,
      seed = seed,
      cleanup.csv = cleanup.csv,
      cleanup.xml = cleanup.xml,
      cleanup.bat = cleanup.bat,
      silent = silent,
      basemap = basemap
    )
  }
  
  
  return(nl_results)
}

util_run_nl_dyn_GenAlg_mod <- function(nl,
                                       seed,
                                       cleanup.csv,
                                       cleanup.xml,
                                       cleanup.bat,
                                       silent,
                                       basemap) {
  
  # Get GenSA object from simdesign:
  galg <- getsim(nl, "simobject")
  
  # Call the GenSA function from the GenSA package:
  results <- genalg::rbga(
    stringMin = galg$lower,
    stringMax = galg$upper,
    popSize = galg$popSize,
    iters = galg$iters,
    elitism = galg$elitism,
    mutationChance = galg$mutationChance,
    evalFunc = function(par, ...) {
      util_run_nl_dyn_GenAlg_fn_mod(
        param = par,
        nl = nl,
        evalcrit = galg$evalcrit,
        seed = seed,
        cleanup.csv = cleanup.csv,
        cleanup.xml = cleanup.xml,
        cleanup.bat = cleanup.bat,
        silent = silent,
        basemap = basemap,
        ...
      )
    }
  )
  
  return(results)
}

util_run_nl_dyn_GenAlg_fn_mod <- function(param,
                                          nl,
                                          evalcrit,
                                          seed,
                                          cleanup.csv,
                                          cleanup.xml,
                                          cleanup.bat,
                                          silent,
                                          basemap) {
  
  # Generate a parameterset:
  names(param) <- names(getexp(nl, "variables"))
  
  ## Generate parameterset
  gensa_param <- tibble::as.tibble(t(param))
  
  ## Add constants if any:
  if(length(getexp(nl, "constants")) > 0)
  {
    gensa_param <- tibble::as.tibble(cbind(gensa_param,
                                           getexp(nl, "constants"),
                                           stringsAsFactors = FALSE))
    
  }
  
  # Attach current parameterisation to nl object:
  setsim(nl, "siminput") <- gensa_param
  # Call netlogo:
  results <- run_nl_one(
    nl = nl,
    siminputrow = 1,
    seed = seed,
    cleanup.csv = cleanup.csv,
    cleanup.xml = cleanup.xml,
    cleanup.bat = cleanup.bat,
    silent = silent
  )
  
  # Select metric for gensa:
  results <- galg_postpro(nl, results, basemap = basemap)
  # Calc mean and convert to numeric:
  if (length(results) > 1) {
    results <- mean(results)
  }
  results <- as.numeric(results)
  
  return(results)
}



######################################################################################
# Step 2: Use nlrx to run simulations
######################################################################################

## Load harapan sample
lu_harapan_sample <- readRDS("3_Data/lu_harapan_sample_genalg.rds")
nsamples <- max(lu_harapan_sample$id)

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
  
  
  nl <- nl(nlversion = "6.0.4",
           nlpath = file.path("1_Model/NetLogo 6.0.4/"),
           modelpath = file.path("1_Model/EFForTS-LGraf/EFForTS-LGraf.nlogo"),
           jvmmem = 2024)
  
  nl@experiment <- experiment(expname="LGraf",
                              outpath=file.path("3_Data"),
                              repetition=1,
                              tickmetrics="false",
                              idrunnum="foldername",
                              idsetup=c("setup"),
                              idgo=c("establish_fields", "assign-land-uses"),
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
                                               "write.param.file?" = "FALSE", ## useful for debugging
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
  # Genetic algorithm parameters:
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
  
  ggsave(plot=fitness.plot, paste0("4_Plots/mapsample_", h, "_fitness.png"), width = 6.0, height = 6.0, dpi=300)
  
  ## Extract variable values:
  results.summary <- strsplit(summary(results), " ")
  results.summary.varend <- (length(results.summary[[1]]) - 1)
  results.summary.varstart <- (results.summary.varend - length(nl@experiment@variables) + 1)
  
  results.summary <- setNames(as.list(as.numeric(results.summary[[1]][results.summary.varstart:results.summary.varend])), names(nl@experiment@variables))
  
  fitness.best.table <- tableGrob(tibble(var=names(results.summary), value=unlist(results.summary)), rows=rep("",length(results.summary)))
  ggsave(plot=grid.arrange(fitness.best.table), paste0("4_Plots/mapsample_", h, "_bestparam.png"), width = 3.5, height = 5.0, dpi=300)
  
  
  # Store best parameterization together with remaining constants as constants list
  all.constants <- c(nl@experiment@constants, results.summary)
  
  ## Do run with this parameterisation:
  nl <- nl(nlversion = "6.0.4",
           nlpath = file.path("1_Model/NetLogo 6.0.4/"),
           modelpath = file.path("1_Model/EFForTS-LGraf/EFForTS-LGraf.nlogo"),
           jvmmem = 2024)
  
  nl@experiment <- experiment(expname="LGraf",
                              outpath=file.path("3_Data"),
                              repetition=1,
                              tickmetrics="false",
                              idrunnum="foldername",
                              idsetup=c("setup"),
                              idgo=c("establish_fields", "assign-land-uses"),
                              metrics.patches=c("pxcor", "pycor", "p_landuse-type"),
                              constants = all.constants)
  
  nl@simdesign <- simdesign_simple(nl, nseeds=5)
  
  res_final <- run_nl_all(nl)
  setsim(nl, "simoutput") <- res_final
  
  
  return(nl)
})

## Store nlsamples as rds:
saveRDS(nl_samples, file="3_Data/Validation_genAlg_nl.rds")


######################################################################################
# Step 3: Analyze results
######################################################################################

## Restore data from file:
nl_samples <- readRDS("3_Data/Validation_genAlg_nl.rds")


mapdev.all <- NULL
for (i in 1:length(nl_samples))
{
  nl <- nl_samples[[i]]
  
  basemap <- lu_harapan_sample %>% dplyr::filter(metric %in% select.metrics) %>% 
    dplyr::filter(id == i) %>% 
    dplyr::select(-'id') %>% 
    spread(metric, value)
  
 # res_final_sp <- get_nl_spatial(nl, turtles = TRUE, patches = TRUE, format = "spatial")
  res_final_sp <- nl@simdesign@simoutput
  
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
  
  # Calculate landscape metrics for all rasters:
  lgraf_metrics <- purrr::map_dfr(res_final_sp$metrics.patches, function(x) {
    # Convert to landuse raster:
    x <- x %>% dplyr::select(pxcor, pycor, `p_landuse-type`)
    x <- raster::rasterFromXYZ(x)
    
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
    res_final_sp.df <- res_final_sp$metrics.patches[[k]] %>% dplyr::select(pxcor, pycor, `p_landuse-type`)
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
    
    ggsave(paste0("4_Plots/mapsample_", i, "_lgrafsample_", k, ".png"), width = 6.0, height = 6.0, dpi=300)
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

tiff("4_Plots/Genalg_validation_devs.tiff", width=17.5, height=10, units="cm", res=300)
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

sample1 <- grid::rasterGrob(readPNG("4_Plots/mapsample_1.png"))
sample2 <- grid::rasterGrob(readPNG("4_Plots/mapsample_2.png"))
sample3 <- grid::rasterGrob(readPNG("4_Plots/mapsample_3.png"))

lgraf11 <- grid::rasterGrob(readPNG("4_Plots/mapsample_1_lgrafsample_1.png"))
lgraf12 <- grid::rasterGrob(readPNG("4_Plots/mapsample_1_lgrafsample_2.png"))
lgraf13 <- grid::rasterGrob(readPNG("4_Plots/mapsample_1_lgrafsample_3.png"))
lgraf14 <- grid::rasterGrob(readPNG("4_Plots/mapsample_1_lgrafsample_4.png"))
lgraf21 <- grid::rasterGrob(readPNG("4_Plots/mapsample_2_lgrafsample_1.png"))
lgraf22 <- grid::rasterGrob(readPNG("4_Plots/mapsample_2_lgrafsample_2.png"))
lgraf23 <- grid::rasterGrob(readPNG("4_Plots/mapsample_2_lgrafsample_3.png"))
lgraf24 <- grid::rasterGrob(readPNG("4_Plots/mapsample_2_lgrafsample_4.png"))
lgraf31 <- grid::rasterGrob(readPNG("4_Plots/mapsample_3_lgrafsample_1.png"))
lgraf32 <- grid::rasterGrob(readPNG("4_Plots/mapsample_3_lgrafsample_2.png"))
lgraf33 <- grid::rasterGrob(readPNG("4_Plots/mapsample_3_lgrafsample_3.png"))
lgraf34 <- grid::rasterGrob(readPNG("4_Plots/mapsample_3_lgrafsample_4.png"))

# Store sample pngs as plots with border and margin:
mg <- 2
s1 <- ggarrange(sample1, ncol=1, nrow=1)+ theme(plot.margin = margin(rep(mg, 4)), plot.background = element_rect(color = "black"))
s2 <- ggarrange(sample2, ncol=1, nrow=1)+ theme(plot.margin = margin(rep(mg, 4)), plot.background = element_rect(color = "black")) 
s3 <- ggarrange(sample3, ncol=1, nrow=1)+ theme(plot.margin = margin(rep(mg, 4)), plot.background = element_rect(color = "black")) 

# Create subplots from lgraf samples with border without margin:
mg <- 0.1
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
mg <- 2
lg1 <- ggarrange(lg11, lg12, lg13, lg14, ncol=2, nrow=2, labels=c("A.1", "A.2", "A.3", "A.4"), label.x = 0, align = "hv", font.label = list(size=11, face="plain"))+ theme(plot.margin = margin(rep(mg, 4)), plot.background = element_rect(color = "black"))
lg2 <- ggarrange(lg21, lg22, lg23, lg24, ncol=2, nrow=2, labels=c("B.1", "B.2", "B.3", "B.4"), label.x = 0, align = "hv", font.label = list(size=11, face="plain"))+ theme(plot.margin = margin(rep(mg, 4)), plot.background = element_rect(color = "black"))
lg3 <- ggarrange(lg31, lg32, lg33, lg34, ncol=2, nrow=2, labels=c("C.1", "C.2", "C.3", "C.4"), label.x = 0, align = "hv", font.label = list(size=11, face="plain"))+ theme(plot.margin = margin(rep(mg, 4)), plot.background = element_rect(color = "black"))

# Final plot:
tiff("4_Plots/Genalg_validation_maps.tiff", width=18, height=12, units="cm", res=300)
ggarrange(s1, s2, s3, lg1, lg2, lg3,
          ncol=3, nrow=2, 
          labels=c("A", "B", "C", "", "", ""),
          label.x = 0.05,
          align = "hv",
          font.label = list(size=11, face="plain"))


dev.off()

