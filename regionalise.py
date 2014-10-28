

### Script for Regionalisation 
## http://www.rise-group.org/risem/clusterpy/index.html
## Using Max p
### http://www.rise-group.org/risem/clusterpy/clusterpy0_9_9/endogenous.html#maxp-description

import clusterpy

clustersite = clusterpy.importArcData("/home/rstudio/unhcr_r_project/cccm-assessment/out/voronoi")

#clustersite = clusterpy.importDBF("/home/rstudio/unhcr_r_project/cccm-assessment/out/voronoi.dbf")

#clustersite.Wrook

#clustersite.Wqueen

#instancesite.results[0].exportArcData("/home/rstudio/unhcr_r_project/cccm-assessment/out/voronoi_solution")

clustersite.dataOperation("individuals = float(individual)")

#clustersite.dataOperation("individual")

#clustersite.exportArcData("/home/rstudio/unhcr_r_project/cccm-assessment/out/voronoi_input")

#clustersite.cluster('azpRTabu', ['individuals'], 1000, dissolve=1)

#clustersite.cluster('maxpTabu', ['individuals'], threshold=5000, dissolve=1)

clustersite.results[0].exportArcData("/home/rstudio/unhcr_r_project/cccm-assessment/out/voronoi_solution1")
