######################################################################################
# EFForTS-LGraf: Harapan sample                                      
######################################################################################
#
# This script was used to cerate map samples from a landuse map.
# As this map cannot be provided, the following code section cannot be executed.
# This code example is just explanatory.
#
######################################################################################

library(raster)
library(tidyverse)
library(SDMTools)
library(MASS)
library(gridExtra)

# Function to load land-use map
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

# Function to create samples from raster:
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
          
          ggsave(paste0("4_Plots/mapsample_", i, ".png"), width = 6.0, height = 6.0, dpi=300)
          
          
          
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


## Reclass landuse map to binary map: Matrix/Agriculture
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
                          rasterout="3_Data/_2013_harapan_land_use_r50",
                          reclassmatrix=reclassmatrix,
                          plotout="4_Plots/lu_map_raster.tiff")

## Sample landscapes for sensitivity analysis comparison (5 samples) from the harapan map:
lu_harapan_sample <- sampleLandscapes(lu_harapan, 2358, 5, 50, NA)
saveRDS(lu_harapan_sample, file.path("3_Data/lu_harapan_sample_sobol.rds"))

## Sample landscapes for genetic algorithm validation (3 samples) from the harapan map:
lu_harapan_sample <- sampleLandscapes(lu_harapan, 446, 3, 50, 1:3)
saveRDS(lu_harapan_sample, file.path("3_Data/lu_harapan_sample_genalg.rds"))
