library('rLiDAR')
library('lidR')
library('rgdal')
library('ForestTools')
library('raster')
library('smoothr')
library('rgeos')
library('foreign')
library('ggplot2')

# Define function

ITCD <- function(aoi,
                 true_point,
                 true_poly,
                 treedata, 
                 chm,
                 outputdirectory) {
  
  # Initialize data & directory
  
  point_stat <- data.frame()
  dir.create(file.path(outputdirectory, "Point_Accuracy"), showWarnings = FALSE)
  
  poly_stat_rg <- data.frame()
  dir.create(file.path(outputdirectory, "RG"), showWarnings = FALSE)
  dir.create(file.path(outputdirectory, "RG/Accuracy"), showWarnings = FALSE)

  
  # smooth chm
  
  smoothchm <- CHMsmoothing(chm, filter = "mean", ws = 3)
  
  for (i in c(seq(3,15,2), "vary")) {
    
    # fixed window size
    
    if (!is.na(as.numeric(i)) == TRUE) {
      i <- as.numeric(i)
      j <- i/2
      treetops <- tree_detection(smoothchm, lmf(ws = j, hmin = 4, shape = "circular"))
      
    } else if (i == "vary") {
      
      # varying window size (prediction interval)
      
      linearmodel <- lm(Crown_Size ~ Height, treedata)
      lin <- function(x){predict(linearmodel, 
                                 data.frame(Height=x), 
                                 interval="predict", level=0.95)[,2]}
      lin2 <- function(x){
        y <- lin(x)
        y[y < 1.5] <- 1.5
        return(y)
      }
      treetops <- tree_detection(smoothchm, lmf(ws = lin2, hmin = 4, shape = "circular"))
    }
    
    # projection
    
    treetops$id <- seq_len(nrow(treetops@data))
    proj4string(treetops) <- CRS(proj4string(aoi))
    
    # region growing
    
    treetops_rg <- extract(chm, treetops, sp=TRUE)
    rg_raster <- dalponte2016(chm = smoothchm, treetops_rg, th_tree = 4, th_seed = 0.8,
                              th_cr = 0.85, max_cr = 30, ID = "id") ()
    treetops_rg <- treetops_rg[aoi, ]
    rg_raster[!(rg_raster %in% unique(treetops_rg$id))] <- NA
    treepolygon_rg <- rasterToPolygons(rg_raster, dissolve = TRUE)
    proj4string(treepolygon_rg) <- CRS(proj4string(aoi))
    treepolygon_rg <- smoothr::smooth(treepolygon_rg, method = "ksmooth", smoothness = 2)
      
    writeOGR(treepolygon_rg, 
              outputdirectory, paste0("RG/polygon_rg_",i), 
              driver="ESRI Shapefile")
      
    # Accuracy assessment
      
    poly_stat_rg_i <- PolygonAccuracy(treepolygon_rg, true_poly, 
                                      paste0(outputdirectory, "/RG/Accuracy/DetResult", i, ".csv"),
                                      paste0(outputdirectory, "/RG/Accuracy/RefResult", i, ".csv"),
                                      ReturnSummaryStat = TRUE)
    poly_stat_rg_i$ws <- i
    poly_stat_rg <- rbind(poly_stat_rg, poly_stat_rg_i)
      
    
    # Accuracy assessment
    
    treetops <- treetops[aoi, ]
    
    writeOGR(treetops, 
             outputdirectory, paste0("treetop",i), 
             driver="ESRI Shapefile")
    
    point_stat_i <- PointAccuracy(treetops, true_point,
                                  search_distance = 2,
                                  paste0(outputdirectory, "/Point_Accuracy/Point_accu_", i, ".csv"),
                                  paste0(outputdirectory, "/Point_Accuracy"), 
                                  paste0("Point_result_", i),
                                  ReturnSummaryStat = TRUE)
    point_stat_i$ws <- i
    point_stat <- rbind(point_stat, point_stat_i)
    
  }
  
  # Write output
  
  write.csv(point_stat, paste0(outputdirectory, "/Point_Accuracy/Summary_Point_accuracy.csv"))
  write.csv(poly_stat_rg, paste0(outputdirectory, "/RG/Accuracy/Summary_Poly_accuracy_rg.csv"))

}


# Execute function

# four study areas
for (h in c("natural", "park", "slope", "roadside"))  {
  
  setwd(paste0("D:/TPK/", h))
  dir.create("ITCD", showWarnings = FALSE)
  
  # Read field data
  
  aoi <- readOGR("aoi.shp")
  true_point <- readOGR("Centroid.shp")
  true_poly <- readOGR("Polygon.shp")
  treedata <- read.dbf("Centroid.dbf")
  
  # three datasets
  for (k in c("lidar", "uav", "dap")) {
    
    # Read las & chm
    
    chm <- raster(paste0("raster/", k, "_chm.tif"))
    outputdirectory <- paste0("D:/TPK/", h, "/ITCD/", k)
    dir.create(outputdirectory, showWarnings = FALSE)
    
    # Execute
    
    ITCD(aoi, true_point, true_poly, treedata, chm, outputdirectory)
    
  }
  
}

