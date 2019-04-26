######################################################################################
# EFForTS-LGraf: Sobol validation                                  
######################################################################################
#
# This script was used to compare Landscape Metrics of all maps that were generated
# with the Sobol Sensitivity Analysis (see 1_Sobol_Sensitivity_Anakysis) with map
# samples from a classified land-use map of the Harapan Region in Jambim Indonesia,
#
######################################################################################

# Load metrics from sobol sensitivity analysis landscapes:
LGraf_sim_lm < readRDS(file.path("3_Data/Sobol_lm.rds"))

# Which metrics?
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

## Loop over land-use and metrics:
plotmetrics <- as.list(expand.grid(unique(metrics.plot$metric), unique(metrics.plot$lu)))

## Convert cells to cells / 1000:
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


