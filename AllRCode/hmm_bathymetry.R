# DST bathymethry exploration
# By Pieterjan Verhelst


library(HMMoce)
library(raster)

# Check vignette!
# https://cran.r-project.org/web/packages/HMMoce/vignettes/Using_HMMoce.html


# Select individual
ptt <- R1642

# TAG/POPUP DATES AND LOCATIONS (dd, mm, YYYY, lat, lon)
iniloc <- data.frame(matrix(c(12, 05, 1999, 53.61666667, 2.18333333, 
                              08, 10, 1999, 53.61763039, 1.94133175), nrow = 2, ncol = 5, byrow = T))
colnames(iniloc) = list('day','month','year','lat','lon')
tag <- as.POSIXct(paste(iniloc[1,1], '/', iniloc[1,2], '/', iniloc[1,3], sep=''), format = '%d/%m/%Y')
pop <- as.POSIXct(paste(iniloc[2,1], '/', iniloc[2,2], '/', iniloc[2,3], sep=''), format = '%d/%m/%Y')

# VECTOR OF DATES FROM DATA. THIS WILL BE THE TIME STEPS, T, IN THE LIKELIHOODS
dateVec <- as.Date(seq(tag, pop, by = 'day')) 






# Download bathymetry data

sp.lim <- list(lonmin = 0.5, lonmax = 2, latmin = 50, latmax = 51)
sp.lim <- list(lonmin = 1, lonmax = 5, latmin = 51, latmax = 54)


sp.lim <- list(lonmin = -1, lonmax = 8, latmin = 49, latmax = 59)
bathy <- get.bath.data(sp.lim$lonmin, sp.lim$lonmax, sp.lim$latmin, 
                       sp.lim$latmax, folder = tempdir())


plot(bathy)




# Setup grid
grid <- setup.grid.raster(bathy)


hmm.filter(grid, L, K1, K2, P, maskL = T, bound.thr = 0.1, minBounds = 10)




# Plot results
## plotHMM()

# needs output from hmm.smoother()
# needs output from calc.track()
