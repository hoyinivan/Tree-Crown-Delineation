### R code used in the following publication

**Kwong, I. H. Y., & Fung, T. (2020). Tree height mapping and crown delineation using LiDAR, large format aerial photographs, and unmanned aerial vehicle photogrammetry in subtropical urban forest. *International Journal of Remote Sensing, 41*(14), 5228–5256. doi: 10.1080/01431161.2020.1731002**<br>(https://www.tandfonline.com/doi/full/10.1080/01431161.2020.1731002)

If the code helps you, please consider citing the above article.

## 1. Treetop detection and tree crown delineation

- Apply 3 × 3 mean filter to canopy height model (CHM)
- Identify treetop points using local maxima method
- 7 fixed window sizes from 3 × 3 stepping up to 15 × 15
- Variable search window using linear relationship between field-measured crown size and tree height
- Delineate tree boundaries using region growing method
- Adopt implementation in lidR package (https://github.com/Jean-Romain/lidR)

![Fig3_22_Window size_2](https://user-images.githubusercontent.com/68047356/121375325-3b679c00-c973-11eb-9c1b-ee229dd8fb64.jpg)

## 2. Accuracy assessment of treetop points

- Individual tree-level accuracy
- Match detected treetops and field-measured trees within a search distance of 2 m
- Compute distance matrix between all detected and reference points
- Iteratively find pairs of points with the shortest distance
- Label remaining points with a distance larger than 2 m as commission and omission
- Evaluate further in terms of precision, recall, and F-score

![PointAccuracy_graph](https://user-images.githubusercontent.com/68047356/121371876-73211480-c970-11eb-884a-506d677e629a.png)
![Fig3_25_Pointaccuracy demo_2](https://user-images.githubusercontent.com/68047356/121375465-5803d400-c973-11eb-84a3-857b97456291.jpg)

## 3. Accuracy assessment of tree crown polygons

- Analyze overlapping area between delineated and reference polygons into 
- Group each polygon into one of the nine defined cases of assessment categories
- E.g. considered as 'matched' if they have more than 50% area overlap
- Other cases included under-segmented, over-segmented, merged, commission, over-represented, under-represented, split and omission

![Figure_4_PolygonAccuracy](https://user-images.githubusercontent.com/68047356/121375756-8c779000-c973-11eb-909c-3c879f649bcb.jpg)

