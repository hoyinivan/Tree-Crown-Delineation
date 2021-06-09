# Treetop Point Accuracy Assessment
# Define function
# Input as two point shapefiles
# Input search distance
# Output as one csv & one point shapefile
# Return a row of summary statistics if true

PointAccuracy <- function(det_input, ref_input, search_distance = 2,
                          csv_output, shp_output_folder, shp_output_name,
                          ReturnSummaryStat = TRUE) {
  
  # library
  library(sp)
  library(raster)
  library(rgdal)
  library(rgeos)
  
  # input
  if (class(det_input) == "character") {
    det <- readOGR(det_input)
  } else if (class(det_input) == "SpatialPointsDataFrame") {
    det <- det_input
  }
  if (class(ref_input) == "character") {
    ref <- readOGR(ref_input)
  } else if (class(ref_input) == "SpatialPointsDataFrame") {
    ref <- ref_input
  }
  
  det@data[1:ncol(det@data)] <- list(NULL)
  det$id_det <- seq_len(nrow(det@data))
  det$X_det <- det@coords[,1]
  det$Y_det <- det@coords[,2]
  ref@data[1:ncol(ref@data)] <- list(NULL)
  ref$id_ref <- seq_len(nrow(ref@data))
  ref$X_ref <- ref@coords[,1]
  ref$Y_ref <- ref@coords[,2]
  n_det <- nrow(det@data)
  n_ref <- nrow(ref@data)
  
  result <- data.frame(id_det=double(), id_ref=double(),
                       X_det=double(), Y_det=double(), 
                       X_ref=double(), Y_ref=double(), 
                       d=double(), match=character(),
                       X_plot=double(), Y_plot=double())
  
  # row as ref, col as det
  d <- gDistance(det, ref, byid=TRUE)
  
  while (min(d) <= search_distance) {
    inds <- arrayInd(which.min(d), dim(d))
    row <- inds[1]
    col <- inds[2]
    match <- data.frame(det$id_det[col], ref$id_ref[row], 
                        det$X_det[col], det$Y_det[col],
                        ref$X_ref[row], ref$Y_ref[row], 
                        d[row, col], 'correct', 
                        ref$X_ref[row], ref$Y_ref[row])
    names(match) <- c('id_det', 'id_ref', 'X_det', 'Y_det', 
                      'X_ref', 'Y_ref', 'd', 'match', 'X_plot', 'Y_plot')
    result <- rbind(result, match)
    
    d <- d[-row, , drop=FALSE]
    d <- d[, -col, drop=FALSE]
    det@data <- det@data[-col,]
    ref@data <- ref@data[-row,]
  }
  
  # remaining ref in row as omission
  ref_remain <- as.data.frame(ref@data)
  if (nrow(ref_remain) > 0) {
    ref_remain$id_det <- NA
    ref_remain$X_det <- NA
    ref_remain$Y_det <- NA
    ref_remain$d <- NA
    ref_remain$match <-'omission'
    ref_remain$X_plot <- ref_remain$X_ref
    ref_remain$Y_plot <-  ref_remain$Y_ref
    
    ref_remain <- ref_remain[c('id_det', 'id_ref', 'X_det', 'Y_det', 
                               'X_ref', 'Y_ref', 'd', 'match', 'X_plot', 'Y_plot')]
    result <- rbind(result, ref_remain)
  }
  
  # remaining det in col as commission
  det_remain <- as.data.frame(det@data)
  if (nrow(det_remain) > 0) {
    det_remain$id_ref <- NA
    det_remain$X_ref <- NA
    det_remain$Y_ref <- NA
    det_remain$d <- NA
    det_remain$match <-'commission'
    det_remain$X_plot <- det_remain$X_det
    det_remain$Y_plot <-  det_remain$Y_det
    
    det_remain <- det_remain[c('id_det', 'id_ref', 'X_det', 'Y_det', 
                               'X_ref', 'Y_ref', 'd', 'match', 'X_plot', 'Y_plot')]
    result <- rbind(result, det_remain)
  }

  # csv & shapefile
  write.csv(result, csv_output)
  xy <- result[,c('X_plot', 'Y_plot')]
  shp <- SpatialPointsDataFrame(coords = xy, data = result,
                                proj4string = CRS(proj4string(ref)))
  writeOGR(shp, shp_output_folder, shp_output_name, driver="ESRI Shapefile")
  
  # count
  if (ReturnSummaryStat == TRUE) {
    t <- as.data.frame(table(result$match))
    names(t) <- c('match', 'Freq')
    n_correct <- subset(t, match == 'correct')[1,2]
    n_omission <- subset(t, match == 'omission')[1,2]
    n_commission <- subset(t, match == 'commission')[1,2]
    if (is.na(n_correct)) {n_correct <- 0}
    if (is.na(n_omission)) {n_omission <- 0}
    if (is.na(n_commission)) {n_commission <- 0}
    detect_rate <- n_det / n_ref
    precision <- n_correct / (n_correct + n_commission)
    recall <- n_correct / (n_correct + n_omission)
    Fscore <- 2*precision*recall / (precision+recall)
    
    count <- data.frame(n_det, n_ref, 
                        n_correct, n_omission, n_commission,
                        detect_rate, precision, recall, Fscore)
    names(count) <- c('n_det', 'n_ref', 
                      'n_correct', 'n_omission', 'n_commission',
                      'detect_rate', 'precision', 'recall', 'Fscore')
    
    return(count)
  }
}

# Sample Run

# stat <- PointAccuracy('D:/AccuracyAssessment/TestR/DetPoint.shp',
#                       'D:/AccuracyAssessment/TestR/RefPoint.shp',
#                       search_distance = 2,
#                       'D:/AccuracyAssessment/TestR/Point_csv.csv',
#                       'D:/AccuracyAssessment/TestR',
#                       'Point_result', 
#                       ReturnSummaryStat = TRUE)
