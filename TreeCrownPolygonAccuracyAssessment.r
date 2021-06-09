# Tree Crown Polygon Accuracy Assessment
# Define function
# Input as two shapefiles
# Output as two csv
# Return a row of summary statistics if true

PolygonAccuracy <- function(det_input, ref_input,
                            det_output, ref_output, 
                            ReturnSummaryStat = TRUE) {
  
  # library
  library(sp)
  library(raster)
  library(rgdal)
  library(rgeos)
  library(maptools)
  
  # Input
  if (class(det_input) == "character") {
    det <- readOGR(det_input)
  } else if (class(det_input) == "SpatialPolygonsDataFrame") {
    det <- det_input
  }
  if (class(ref_input) == "character") {
    ref <- readOGR(ref_input)
  } else if (class(ref_input) == "SpatialPolygonsDataFrame") {
    ref <- ref_input
  }

  det$id_det <- seq_len(nrow(det@data))
  det$Area_det <- area(det)
  ref$id_ref <- seq_len(nrow(ref@data))
  ref$Area_ref <- area(ref)
  
  # intersect from raster package
  int <- raster::intersect(det, ref)
  int$Area_int <- area(int)
  int$Area_det_per <- int$Area_int/int$Area_det
  int$Area_ref_per <- int$Area_int/int$Area_ref
  
  # delineated crown side
  det_result <- data.frame("id_det"= det$id_det)
  int_df <- as.data.frame(int)
  for (i in seq_len(nrow(det_result))) {
    int_df_1 <- subset(int_df, id_det == i)
    int_df_1 <- int_df_1[order(-int_df_1$Area_det_per),] 
    if (is.na(int_df_1$Area_det_per[1])) {
      det_result[i,2] <- "Commission"
      det_result[i,3] <- NA
    } else if (int_df_1$Area_det_per[1] >= 0.5) {
      if (int_df_1$Area_ref_per[1] >= 0.5) {
        det_result[i,2] <- "Matched"
        det_result[i,3] <- int_df_1$id_ref[1]
      } else {
        det_result[i,2] <- "Under-segmented"
        det_result[i,3] <- int_df_1$id_ref[1]
      }
    } else {
      if (is.na(int_df_1$Area_det_per[2])) {
        det_result[i,2] <- "Over-segmented"
        det_result[i,3] <- int_df_1$id_ref[1]
      } else {
        det_result[i,2] <- "Merged"
        det_result[i,3] <- int_df_1$id_ref[1]
      }
    }
  }
  names(det_result) <- c("id_det", "match_det", "match_id_ref")
  
  # reference crown side
  ref_result <- data.frame("id_ref"= ref$id_ref)
  int_df <- as.data.frame(int)
  for (i in seq_len(nrow(ref_result))) {
    int_df_1 <- subset(int_df, id_ref == i)
    int_df_1 <- int_df_1[order(-int_df_1$Area_ref_per),] 
    if (is.na(int_df_1$Area_ref_per[1])) {
      ref_result[i,2] <- "Omission"
      ref_result[i,3] <- NA
    } else if (int_df_1$Area_ref_per[1] >= 0.5) {
      if (int_df_1$Area_det_per[1] >= 0.5) {
        ref_result[i,2] <- "Matched"
        ref_result[i,3] <- int_df_1$id_det[1]
      } else {
        ref_result[i,2] <- "Over-represented"
        ref_result[i,3] <- int_df_1$id_det[1]
      }
    } else {
      if (is.na(int_df_1$Area_ref_per[2])) {
        ref_result[i,2] <- "Under-represented"
        ref_result[i,3] <- int_df_1$id_det[1]
      } else {
        ref_result[i,2] <- "Split"
        ref_result[i,3] <- int_df_1$id_det[1]
      }
    }
  }
  names(ref_result) <- c("id_ref", "match_ref", "match_id_det")
  
  # Output csv
  write.csv(det_result, det_output)
  write.csv(ref_result, ref_output)
  
  # count
  if (ReturnSummaryStat == TRUE) {
    t_det <- as.data.frame(table(det_result$match_det))
    t_ref <- as.data.frame(table(ref_result$match_ref))
    names(t_det) <- c("match", "Freq")
    names(t_ref) <- c("match", "Freq")
    n_matched_det <- subset(t_det, match == "Matched")[1,2]
    n_underseg <- subset(t_det, match == "Under-segmented")[1,2]
    n_overseg <- subset(t_det, match == "Over-segmented")[1,2]
    n_merged <- subset(t_det, match == "Merged")[1,2]
    n_commission <- subset(t_det, match == "Commission")[1,2]
    n_matched_ref <- subset(t_ref, match == "Matched")[1,2]
    n_overrep <- subset(t_ref, match == "Over-represented")[1,2]
    n_underrep <- subset(t_ref, match == "Under-represented")[1,2]
    n_split <- subset(t_ref, match == "Split")[1,2]
    n_omission <- subset(t_ref, match == "Omission")[1,2]
    
    count <- data.frame(n_matched_det, n_underseg, n_overseg, n_merged, n_commission,
                        n_matched_ref, n_overrep, n_underrep, n_split, n_omission)
    names(count) <- c("Matched_det", "Under-segmented", "Over-segmented", "Merged", "Commission",
                      "Matched_ref", "Over-represented", "Under-represented","Split", "Omission")
    
    return(count)
  }
}

# Sample Run
# stat <- PolygonAccuracy("D:/AccuracyAssessment/TestR/DetCrowns.shp",
#                         "D:/AccuracyAssessment/TestR/RefCrowns.shp",
#                         "D:/AccuracyAssessment/TestR/DetResult.csv",
#                         "D:/AccuracyAssessment/TestR/RefResult.csv",
#                         ReturnSummaryStat = TRUE)
