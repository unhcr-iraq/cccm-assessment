

### Script for Regionalisation 
## http://www.rise-group.org/risem/clusterpy/index.html


import clusterpy
clustersite = clusterpy.importArcData("/home/rstudio/unhcr_r_project/cccm-assessment/out/voronoi3")

#clustersite = clusterpy.importDBF("/home/edouard/iraq/rapidneedsassessment/baseline/clip/voronoi2.dbf")
#clustersite.Wrook
#clustersite.Wqueen

#instancesite.results[0].exportArcData("/home/edouard/iraq/rapidneedsassessment/baseline/clip/voronoi_solution")

clustersite.dataOperation("individuals = float(individual)")

#clustersite.dataOperation("individual")
#clustersite.exportArcData("/home/edouard/iraq/rapidneedsassessment/baseline/clip/voronoi_input")
#clustersite.cluster('azpRTabu', ['individuals'], 1000, dissolve=1)
clustersite.cluster('maxpTabu', ['individuals'], threshold=5000, dissolve=1)
clustersite.results[0].exportArcData("/home/edouard/iraq/rapidneedsassessment/baseline/clip/voronoi_solution1")
