######################################################################################
# EFForTS-LGraf: Use case readme                            
######################################################################################


library(nlrx)
netlogopath <- file.path("1_Model/NetLogo 6.0.4/")
modelpath <- file.path("1_Model/EFForTS-LGraf/EFForTS-LGraf.nlogo")

nl <- nl(nlversion = "6.0.4",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 2024)


nl@experiment <- experiment(expname="LGraf",
                            outpath=file.path("3_Data"),
                            repetition=1,
                            tickmetrics="false",
                            idrunnum="foldername",
                            idsetup=c("setup"),
                            idgo=c("establish_fields", "assign-land-uses"),
                            metrics.patches=c("pxcor", "pycor", "p_landuse-type", "p_road"),
                            variables = list("proportion-agricultural-area" = list(values=c(0.1, 0.4, 0.7)),
                                             "total-road-length" = list(values=c(200, 400, 600)),
                                             "LUT-1-fraction" = list(values=c(0.1, 0.4, 0.7))),
                            constants = list("households-per-cell" = 1,
                                             "setup-model" = "\"agricultural-area\"",
                                             "reproducable?" = "FALSE",   ## random seed is set via nlrx
                                             "write.param.file?" = "FALSE", ## useful for debugging
                                             "width" = 100,
                                             "height" = 100,
                                             "cell-length-meter" = 50,
                                             "road.algorithm" = "\"artificial.perlin\"",
                                             "min-dist-roads" = 5,
                                             "perlin-octaves" = 2,
                                             "perlin-persistence" = 0.1,
                                             "cone-angle" = 90,
                                             "dist-weight" = 0.5,
                                             "occ-probability" = 0,
                                             "hh-area-mean-ha" = 1,
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
                                             "land-use-types" = "\"household-level-specialization\"",
                                             "default.maps" = "\"landuse-type\"",
                                             "write-household-ids" = "\"only-first-households\""))

# Add a distinct simdesign
nl@simdesign <- simdesign_distinct(nl=nl, nseeds=1)

# Run all three simulations
results <- run_nl_all(nl)
                         
# Attach results to nl object:
setsim(nl, "simoutput") <- results

# Convert spatial data to raster stacks
nl_sp <- nl_to_raster(nl)

# Combine roads and landuse raster and plot:
library(raster)
tiff("4_Plots/readme_example_nlrx.tiff", width=20, height=8, units="cm", res=300)
par(mfrow=c(1,3))
nl_sp_plot <- purrr::map(nl_sp$spatial.raster, function(x) {
  t1 <- x[[1]]
  t2 <- x[[2]]
  rst1_mod <- overlay(t1, t2, fun = function(x, y) {
    x[(y[] == 1)] <- NA
    return(x)
  })
  plot(rst1_mod, colNA="black")  
})
dev.off()
