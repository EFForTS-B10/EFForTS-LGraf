#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# EFForTS-LGraf Manuscript Analyses
#
# FUNCTIONS
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Setup future for HPC execution
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setupFuturePlanHPC <- function(template, queue, walltime, coremem, processes)
{
  ## Run in parallel:
  options(future.makeNodePSOCK.rshcmd = c("plink", "-ssh"))
  options(future.makeNodePSOCK.rshopts = c("-i", "D:/owncloud/Coding/ecomodHPC/puttyprivate.ppk"))
  login <- tweak(remote, workers = "gwdu101.gwdg.de", user = 'jsaleck')
  bsub <- tweak(batchtools_lsf, template = 'lsfexclusive.tmpl',
                resources = list(job.name = 'Writing_nodes',
                                 log.file = 'Writing_nodes.log',
                                 queue = 'mpi',
                                 walltime = '24:00',
                                 coremem = 2000,
                                 processes = 16))
  
  ## Specify future topology
  plan(list(
    login,
    bsub,
    multiprocess
  ))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# run_nl_split function:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
run_nl_split <- function(nl, parts, cleanup="all") 
{
  
  ## Split up siminput and store as list:
  siminput <- getsim(nl, "siminput")
  nr <- nrow(siminput)
  n_per_part <- nr / parts
  part_ids <- seq(1:parts)
  jobs <- as.list(expand.grid(getsim(nl, "simseeds"), part_ids))
  
  nl_results <- furrr::future_map_dfr(seq_along(jobs[[1]]), 
                                      function(job) {
                                        
                                        job_seed <- jobs[[1]][[job]]
                                        job_part <- jobs[[2]][[job]]
                                        rowids <- seq(1:n_per_part) + (job_part - 1) * n_per_part
                                        
                                        furrr::future_map_dfr(rowids, 
                                                              function(siminputrow) {
                                                                run_nl_one(nl = nl, seed = job_seed, siminputrow = siminputrow, 
                                                                           cleanup = "all")
                                                              })
                                      })
  
  return(nl_results) 
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load and reclassify raster:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
loadHarapan <- function(rasterlocation, rasterout, reclassmatrix, plotout) 
{
  
  ## Load the shapefile with land-uses
  lu_2013_harapan <- raster(rasterlocation)

  ## Do the reclassification
  lu_2013_harapan50_rc <- reclassify(lu_2013_harapan, reclassmatrix)
  
  ## Store reclassified file:
  writeRaster(lu_2013_harapan50_rc, rasterout, format = "GTiff", overwrite=TRUE)
  
  ## Load raster
  lu_2013_harapan50_rc <- raster(paste0(rasterout, ".tif"))
  
  ## Smooth raster:
  lu_2013_harapan50_rc <- raster::focal(lu_2013_harapan50_rc, w=matrix(1,3,3), fun=modal)
  
  
  ## Convert raster to DF and create nice plot of harapan raster:
  lu_2013_harapan50_rc.df <- data.frame(rasterToPoints(lu_2013_harapan50_rc))
  names(lu_2013_harapan50_rc.df) <- c("x", "y", "z")
  
  cols <- c("0"="#8D8D8D", "1"="#fde725ff")
  fontsize <- 12
  ggplot(lu_2013_harapan50_rc.df, aes(x=x, y=y, fill=factor(z))) +
    geom_tile() +
    coord_equal() +
    xlab("Easting") +
    ylab("Northing") +
    scale_fill_manual(values = cols) +
    guides(fill=FALSE) +
    theme_minimal() +
    theme(axis.text=element_text(size=fontsize),
          axis.title=element_text(size=fontsize),
          legend.position="top")
  
  ggsave(plotout, width = 5, height = 7, dpi=300)
  
  
  return(lu_2013_harapan50_rc)
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create samples from raster:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sampleLandscapes <- function(x, seed, nsamples, cutoff, plotIDs)
{
  
  
  ### SAMPLE NEW -- COLUMN/ROW Based:
  set.seed(seed)
  rndCells <- NULL
  x.sample.pscs <- NULL
  i <- 1
  
  while(i <= nsamples)
  {
    rndCol <- round(runif(1, min=1, max=ncol(x)))
    rndRow <- round(runif(1, min=1, max=nrow(x)))
    rndCell <- data.frame(col=rndCol, row=rndRow)
    rndExtent <- extent(x, (rndCol - cutoff), (rndCol + cutoff), (rndRow - cutoff), (rndRow + cutoff))
    
    
    
    ## Crop from the original raster and report FALSE if extents do not overlap
    rndValidCrop <- tryCatch(!is.null(crop(x, rndExtent)), error=function(e) return(FALSE)) 
    
    ## If we have cropped a valid raster, Check if there are NAs in the raster:
    if(rndValidCrop == TRUE)
    {
      rndCrop <- crop(x, rndExtent)
      #plot(rndCrop)
      # Check if there are NAs
      rndValidCrop <- ifelse(sum(is.na(getValues(rndCrop))) > 0, FALSE, TRUE)
    }
    if(rndValidCrop == TRUE)
    {
      # Check if we have enough agriculture:
      rndValidCrop <- ifelse(length(getValues(rndCrop)[getValues(rndCrop) > 0]) < 500, FALSE, TRUE)
    }
    if(rndValidCrop == TRUE)
    {
      # Check if we have too much agriculture:
      rndValidCrop <- ifelse(length(getValues(rndCrop)[getValues(rndCrop) > 0]) > 7500, FALSE, TRUE)
    }
    
    ## If no NAs were found, keep the extent and increase the loop counter
    if(rndValidCrop == TRUE)
    {
      ## Store selected sample locations
      rndCells <- rbind(rndCells, rndCell)
      ## Set the cropped values in the original ratser to NA to prevent them from be drawing again
      x[rndCrop] <- NA
      
      ## Calculate patch statistics and write output
      rndCrop.ps <- PatchStat(rndCrop)
      rndCrop.cs <- ClassStat(rndCrop)
      
      if(!is.na(plotIDs))
      {
        if (i %in% plotIDs)
        {
          ## Convert raster to DF and create nice plot of harapan raster:
          rndCrop.df <- data.frame(rasterToPoints(rndCrop))
          names(rndCrop.df) <- c("x", "y", "z")
          
          cols <- c("0"="#8D8D8D", "1"="#fde725ff")
          ggplot(rndCrop.df, aes(x=x, y=y, fill=factor(z))) +
            geom_tile() +
            coord_equal() +
            scale_fill_manual(values = cols) +
            guides(fill=FALSE) +
            theme_void() + 
            scale_x_continuous(expand=c(0,0)) +
            scale_y_continuous(expand=c(0,0)) +
            labs(x = NULL, y = NULL)
          
          ggsave(paste0("mapsample_", i, ".png"), width = 6.0, height = 6.0, dpi=300)
          
          
          
        }
      }
      
      
      ## We have multiple classes but we need one row of observations for each run -> convert to wide format:
      rndCrop.ps.long <- rndCrop.ps %>%
        gather(key, val, 2:12) %>%
        mutate(stat="ps") %>% 
        unite(key2, stat, patchID, key, sep = ".") %>%
        spread(key2, val) 
      rndCrop.cs.long <- rndCrop.cs %>%
        gather(key, val, 2:38) %>%
        mutate(stat="cs") %>% 
        unite(key2, stat, class, key, sep = ".") %>%
        spread(key2, val)
      rndCrop.pscs.long <- cbind(rndCrop.ps.long, rndCrop.cs.long)
      
        ## Convert back to long format:
        rndCrop.pscs <- rndCrop.pscs.long %>% gather(metric, value, c(2:11, 13:96))
        names(rndCrop.pscs) <- c("forest.area", "agri.area", "metric", "value")
        rndCrop.pscs$id <- i
        x.sample.pscs <- rbind(x.sample.pscs, rndCrop.pscs)
        
        ## Increase loop counter
        i <- i + 1
        
        ## Print cells left without NA:
        print(paste0("Non NA Cells left: ", length(getValues(x)[!is.na(getValues(x))])))
    }
  }
  return(x.sample.pscs)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create plots for household size, vllg area and field size distributions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
createDistrPlots <- function(hhdatalocation, plotout) 
{
  
  fontsize <- 11
  
  ## Read data:
  hhdata <- read.csv(hhdatalocation, header = TRUE, sep = ";")
  hhdata <- hhdata[-1,]
  
  ## Aggregate household sizes:
  hhs <- aggregate(x=list(hh_area =hhdata$Area..ha.), by=list(hhid=hhdata$Household.ID), FUN="sum")
  ## Extract sizes and drop na:
  hhs <- data.frame(x=na.omit(hhs$hh_area))
  # Fit distribution
  hhsfit <- fitdistr(hhs$x, densfun = "log-normal")
  
  hhplot <- ggplot(hhs) +
    geom_histogram(aes(x, ..density..), bins = 50, fill="white", color="black") +
    stat_function(fun=dlnorm, args=list(hhsfit$estimate[1], hhsfit$estimate[2]), size=1, color="red") +
    xlab("agricultural household area [ha]") +
    ylab("density") +
    theme_minimal() +
    theme(axis.text=element_text(size=fontsize, color="black"),
          axis.title=element_text(size=fontsize, color="black")) 
  
  ## Extract field sizes and drop NAs
  fields <- data.frame(x=na.omit(hhdata$Area..ha.))
  ## Fit distribution to log normal:
  fieldsfit <- fitdistr(fields$x, densfun = "log-normal")
  
  ## Plot histogram and log normal model:
  ffplot <- ggplot(fields) +
    geom_histogram(aes(x, ..density..), bins = 25, fill="white", color="black") +
    stat_function(fun=dlnorm, args=list(fieldsfit$estimate[1], fieldsfit$estimate[2]), size=1, color="red") +
    xlab("agricultural field size [ha]") +
    ylab("density") +
    theme_minimal() +
    theme(axis.text=element_text(size=fontsize, color="black"),
          axis.title=element_text(size=fontsize, color="black")) 
  
  # Village area:
  vlgs <- hhdata %>% group_by(Village.ID) %>% summarise(vlgarea = sum(Area..ha.)) %>% filter(!is.na(vlgarea))
  vlgsfit <- fitdistr(vlgs$vlgarea, densfun = "log-normal")
  
  vlgsplot <- ggplot(vlgs) +
    geom_histogram(aes(vlgarea, ..density..), bins = 15, fill="white", color="black") +
    stat_function(fun=dlnorm, args=list(vlgsfit$estimate[1], vlgsfit$estimate[2]), size=1, color="red") +
    xlab("agricultural household area in village [ha]") +
    ylab("") +
    theme_minimal() +
    theme(axis.text=element_text(size=fontsize, color="black"),
          axis.title=element_text(size=fontsize, color="black")) 
  
  
  hists <- list(hhplot, ffplot, vlgsplot)
  lay <- rbind(c(1,1),c(2,3)) 
  grid.arrange(grobs=hists, layout_matrix=lay)
  ggsave(plotout, width=7, height=5, dpi=300, arrangeGrob(grobs=hists, layout_matrix=lay))
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate landscape metrics for a raster stack
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rasterToMetrics <- function(rasterlist, select.metrics)
{
  
  metrics <- purrr::map_dfr(rasterlist, function(x) {
    
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
  
  ## Filter table and keep only selected metrics:
  metrics <- metrics %>% dplyr::select(select.metrics)
  
  return(metrics)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate eFast indices from simulation data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
eFastIndices <- function(nl, metric.cols)
{
  sensindex <- NULL
  f99 <- getsim(nl, "simobject")[[1]]
    
    # Calculate sensitivity indices separately for each random seed:
    for(i in getsim(nl, "simseeds")) {
      
      # Select seed runs, aggregate across steps and select only output columns:
      simoutput.i <- getsim(nl, "simoutput") %>%
        dplyr::filter(`random-seed` == i) %>%
        dplyr::group_by(siminputrow) %>%
        dplyr::summarise_at(metric.cols, dplyr::funs(min, max, mean, stats::sd)) %>%
        dplyr::select(-siminputrow) %>%
        dplyr::select_if(~!all(is.na(.)))
      
      metrics <- colnames(simoutput.i)
      simoutput.i <- t(as.matrix(simoutput.i))
      
      
      # Loop over metric columns and calculate sensitivity indices:
      for (j in seq_len(nrow(simoutput.i))) {
        sensitivity::tell(f99, simoutput.i[j,])
        
        D1 <- tibble::tibble(value = f99$D1,
                             index="first-order",
                             parameter=names(getexp(nl, "variables")),
                             metric=metrics[j],
                             seed=i)
        Dt <- tibble::tibble(value = f99$Dt,
                             index="total",
                             parameter=names(getexp(nl, "variables")),
                             metric=metrics[j],
                             seed=i)
        
        sensindex <- rbind(sensindex, D1, Dt)
      }
    }
    # Remove rownames
    rownames(sensindex) <- NULL
    sensindex <- tibble::as.tibble(sensindex)
    
    return(sensindex)
    
}
  
sobolIndices <- function(nl, metric.cols) 
{
  
  sensindex <- NULL
  so <- getsim(nl, "simobject")[[1]]
  
  # Calculate sensitivity indices separately for each random seed:
  for(i in getsim(nl, "simseeds")) {
    
    # Select seed runs, aggregate across steps and select only output columns:
    simoutput.i <- getsim(nl, "simoutput") %>%
      dplyr::filter(`random-seed` == i) %>% dplyr::group_by(siminputrow) %>%
      dplyr::summarise_at(metric.cols, dplyr::funs(min, max, mean, stats::sd)) %>%
      dplyr::select(-siminputrow) %>%
      dplyr::select_if(~!all(is.na(.)))
    
    metrics <- colnames(simoutput.i)
    simoutput.i <- t(as.matrix(simoutput.i))
    
    
    # Loop over metric columns and calculate sensitivity indices:
    for (j in seq_len(nrow(simoutput.i))) {
      sensitivity::tell(so, simoutput.i[j,])
      soS <- so$S
      soS[soS < 0] <- 0
      soS[soS > 1] <- 1
      soS$index <- "first-order"
      soS$parameter <- rownames(soS)
      soS$metric <- metrics[j]
      soS$seed <- i
      soT <- so$T
      soT[soT < 0] <- 0
      soT[soT > 1] <- 1
      soT$index <- "total"
      soT$parameter <- rownames(soT)
      soT$metric <- metrics[j]
      soT$seed <- i
      
      sensindex <- rbind(sensindex, soS, soT)
    }
  }
  # Remove rownames
  rownames(sensindex) <- NULL
  sensindex <- tibble::as.tibble(sensindex)
  
  return(sensindex)
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot sobol indices as rasterplot:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
saIndexPlot <- function(sa_index, plotout) 
{
  ## PLOTTING:
  ## Add class variable:
  sa_index$class <- substr(sa_index$metric, 0, 4)
  sa_index$class <- ifelse(sa_index$class == "cs.0", "others", "fields")
  sa_index$landscapeMetric <- substr(sa_index$metric, 6, nchar(sa_index$metric))
  sa_index$landscapeMetricType <- substr(sa_index$landscapeMetric, (nchar(sa_index$landscapeMetric) - 2), nchar(sa_index$landscapeMetric))
  sa_index$landscapeMetric <- sub("*_.*", "", sa_index$landscapeMetric)
  sa_index$landscapeMetric <- ifelse(sa_index$landscapeMetric == "landscape.shape.index", "LSI",
                                     ifelse(sa_index$landscapeMetric == "largest.patch.index", "LPI",
                                            ifelse(sa_index$landscapeMetric == "mean.patch.area", "mean patch area",
                                                   ifelse(sa_index$landscapeMetric == "n.patches", "n patches", "PCI"))))
 
  ## only keep means of indices:
  sa_index <- sa_index %>% filter(landscapeMetricType=="ean")
  sa_index_total <- sa_index %>% filter(index=="total")
  sa_index_first <- sa_index %>% filter(index=="first-order")
  
  
  fontsize <- 10
  
  ggplot(sa_index_total) +
    facet_grid(.~landscapeMetric, scales="free") +
    geom_tile(data=sa_index_total, aes(x=class, y=parameter, fill=value)) +
    geom_point(data=sa_index_first, aes(x=class, y=parameter, fill=value), size=4, shape=21) +
    #scale_fill_viridis_c(option="magma") +
    scale_fill_gradient(low="white", high="black") +
    guides(fill=FALSE) +
    xlab("land-use class") +
    ylab("parameter") +
    theme_classic() +
    theme(strip.text=element_text(size=fontsize),
          axis.text=element_text(size=fontsize),
          axis.title=element_text(size=fontsize),
          panel.spacing=unit(0, "lines"))
  
  ggsave(plotout, width = 7.5, height = 4.5, dpi=300)
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot metrics of generated landscapes and smpled landscapes from the land-use map
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plotOneIndex <- function(plotdat)
{
  p1 <- ggplot(plotdat, aes(x=forest.area, y=value)) +
    #facet_wrap(~lu, scales="free", nrow=2) +
    scale_fill_gradient(high="black", low="white", na.value="white") +
    stat_density_2d(data=plotdat %>% filter(source=="EFForTS-LGraf"), geom = "raster", aes(fill = sqrt(stat(density))), contour = FALSE) +
    #geom_density_2d(data=plotdat %>% filter(source=="Land-use-map"), aes(color=lu), size=0.3) +
    geom_point(data=plotdat %>% filter(source=="Land-use-map"), aes(color=lu), pch=21, fill=NA, size=1, stroke=0.5) +
    scale_color_manual(values=c("fields"="#EBBD28", "matrix"="#41A355")) +
    guides(fill="none", color="none") +
    scale_y_continuous(labels = function(x) format(x, width = 5)) +
    #xlim(2000, 10000) +
    xlab("") +
    ylab("") +
    theme_classic() +
    theme(axis.text=element_text(size=11),
          axis.title=element_text(size=11),
          plot.margin=unit(c(0,0,0,0),"pt"))
  
  return(p1)
}

validationPlot <- function(metrics.plot, plotout) 
{
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
  
  ggsave(plotout, width=7.5, height=3.5, pall, dpi=300)
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# nlrx get spatial data, modified (no Turtles)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_nl_spatial_noTurtles <- function(nl)
{
  x_coord_ind <- grepl(c("pxcor"), names(getsim(nl, "simoutput")$metrics.patches[[1]]))
  x_coord_ind <- which(x_coord_ind == TRUE)
  y_coord_ind <- grepl(c("pycor"), names(getsim(nl, "simoutput")$metrics.patches[[1]]))
  y_coord_ind <- which(y_coord_ind == TRUE)
  
  patches_own <- which(1:ncol(getsim(nl, "simoutput")$metrics.patches[[1]]) != c(x_coord_ind, y_coord_ind))
  
  patches_own_names <- names(getsim(nl, "simoutput")$metrics.patches[[1]])[patches_own]
  
  patches <-  purrr::map(seq_along(getsim(nl, "simoutput")$metrics.patches), function(raster_ind){
    
    patches_raster <- raster::rasterFromXYZ(getsim(nl, "simoutput")$metrics.patches[[raster_ind]][,c(x_coord_ind,
                                                                                                     y_coord_ind,
                                                                                                     patches_own)])
    patches_raster <- raster::flip(patches_raster, 2)
    names(patches_raster) <- purrr::map_chr(patches_own_names, function(name){
      paste("S", getsim(nl, "simoutput")[raster_ind, "random-seed"],"_R",
            getsim(nl, "simoutput")[raster_ind, "siminputrow"],
            "_N", name,
            sep = "")
    })
    
    return(patches_raster)
  })
  
  patches_tib <- tibble::enframe(patches, "id", "patches")
  patches_tib$step <- getsim(nl, "simoutput")$`[step]`
  patches_tib$siminputrow <- getsim(nl, "simoutput")$siminputrow
  patches_tib$`random-seed` <- getsim(nl, "simoutput")$`random-seed`
  
  return(patches_tib)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# New validation2019 - Functions:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
  
  ## Convert data to spatial data (raster)
  lgraf_metrics <- get_nl_spatial(nl, turtles = TRUE, patches=TRUE, format="spatial")
  
  ## Calculate landscape metrics for all rasters:
  lgraf_metrics <- rasterToMetrics(lgraf_metrics$metrics.patches, select.metrics)
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



