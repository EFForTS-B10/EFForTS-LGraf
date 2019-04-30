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

######################################################################################
# Step 2: Use nlrx to run simulations
######################################################################################

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
                            idfinal=NA_character_,
                            metrics.patches=c("pxcor", "pycor", "p_landuse-type"),
                            variables = list("hh-area-mean-ha" = list(min=1, max=3, step=0.1, qfun="qunif"),
                                             "LUT-1-specialize" = list(min=0, max=1, step=0.1, qfun="qunif")),
                            constants = list("households-per-cell" = 1,
                                             "setup-model" = "\"agricultural-area\"",
                                             "proportion-agricultural-area" = 0.5,
                                             "reproducable?" = "FALSE",   ## random seed is set via nlrx
                                             "write.param.file?" = "FALSE", ## useful for debugging
                                             "width" = 100,
                                             "height" = 100,
                                             "cell-length-meter" = 50,
                                             "road.algorithm" = "\"real.shapefile\"",
                                             "road-map-id" = "\"jambi3\"",
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
                                             "land-use-types" = "\"household-level-specialization\"",
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
saveRDS(nl, file.path("3_Data/Specialization_nl.rds"))

######################################################################################
# Step 3: Output post processing to calculate sensitivity indices
######################################################################################

## Load output from file:
nl <- readRDS(file.path("3_Data/Specialization_nl.rds"))

## Convert data to spatial data (raster)
#spec_sim_sp <- get_nl_spatial_noTurtles(nl)

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

# Calculate landscape metrics for all rasters:
spec_sim_lm <- purrr::map_dfr(nl@simdesign@simoutput$metrics.patches, function(x) {
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

## Attach metrics as output to the current simoutput table and remove turtle.metrics and patch.metrics
spec_sim_lm <- spec_sim %>% dplyr::select(-metrics.patches) %>% bind_cols(spec_sim_lm)

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
tiff("4_Plots/specialization.tiff", width=19, height=12, units="cm", res=300)
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

