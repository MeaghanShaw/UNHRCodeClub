#R Code Club 8/12/19 - presentation of my work 

#Spatial Analysis and Diversity calculations

#working directory - where all files being used are stored and where you want to save objects to
setwd("C:/Users/David Moore/Documents/R Coding Club/Kaitlyn Baillargeon - Spatial Data Analysis - 8-12-2019/RCodeClubPresentationFiles_8-12-19_fileOutputs")
setwd("C:/Users/David Moore/Documents/R Coding Club/Kaitlyn Baillargeon - Spatial Data Analysis - 8-12-2019/RCodeClubPresentationFiles_8-12-19_requiredDataFiles")

#topics:
  #1) Calculating Diversity 
  #2) Creating plots 
  #3) Calculating vegetation indicies 
  #4) Extracting data by plot 

#packages used
install.packages("reshape"); library(reshape)
install.packages("vegan"); library(vegan)
install.packages("rgdal"); library(rgdal)
install.packages("raster"); library(raster)
install.packages("sf"); library(sf)
install.packages("sp"); library(sp)
install.packages("dplyr"); library(dplyr)

#many different ways biodiversity can be calculated 
  #species, function, phylogenetic, structure, taxonomic, geodiversity, etc. 
#packages used for most calculations -  
  #ape, BiodiversityR, FD, phylocomr, phylobase, PhyloMeasures, picante, vegan, phytools 

#NEON (National Ecological Obervatory Network) comes with multiple tutorials for spatial analyses to work with their data
#can also find other sources online to help 
#common packages - 
  #RStoolbox, raster, rgdal, rLiDAR, sp, lidR
#https://www.neonscience.org/resources/series/introduction-working-raster-data-r

##example data used in this presentation include: 
  #Harvard Forest (HF) 2014 Inventory data 
    #Orwig D, Foster D, Ellison A. 2015. Harvard Forest CTFS-ForestGEO Mapped Forest Plot since 2014. 
    #Harvard Forest Data Archive: HF253.http://harvardforest.fas.harvard.edu:8080/exist/apps/datasets/showData.html?id=hf253
  #NEON sample imagery of HF and San Joaquin Experimental Range (SJER) obtained through raster tutorials

###############################################################################################################################

##calculate diversity measures from species abundances 

#read in HF inventory data
trees <- read.csv("HF_trees_2014.csv")
head(trees)

#turn inventory into an abundance matrix
library(reshape) #used to transform dataframe

#melt data - identifies variables that are to be used as "id" and values to be "melted"
trees_melt <- melt(trees, id = (c("tree.id", "stem.id", "sp", "species", "quadrat", "gx", 
                                "gy", "dbh")), measure.vars = c("count"))
head(trees_melt)

#cast dataframe - recast dataframe so column names = quadrat and row names = species, values = count
trees_cast <- cast(trees_melt, quadrat~species, sum)
trees_cast <- as.data.frame(trees_cast)
View(trees_cast)
#now you have an abundance matrix!

#one thing that wasn't done: filtering out dead trees from alive ones
#use subset function and it will get rid of objects based on the selected critera 
#trees <- subset(trees, TREE_STATUS1 == "alive")

#calculate diversity 
library(vegan)

#shannon's diversity 
shannon <- diversity(trees_cast[-1], index = "shannon") 
#simpson's diversity 
simpson <- diversity(trees_cast[-1], index = "simpson")
#inverse simpson diversity 
inversesimp <- diversity(trees_cast[-1], index = "invsimpson")
#species richness
richness <- specnumber(trees_cast[-1])
#evenness
evenness <- shannon/log(richness) #Peilou J 1966
#need [-1] because you dont want to count the first column in the diversity calculations

#combine into a data frame 
diversitycalcs <- cbind(shannon, simpson, inversesimp, richness, evenness)

#add quadrat names 
diversitycalcs <- cbind(diversitycalcs, trees_cast[1])
head(diversitycalcs)

#optional - write data into csv
write.csv(diversitycalcs, "HF_diversitycalcs_2014Inventory.csv")

###############################################################################################################################

##create own plots

#shapefile of quadrats based on ones listed in inventory data used above
library(rgdal) #used to read in vector files and work with projections

quadrats <- readOGR("HF_quadrats_20m.shp")
plot(quadrats)
#dont want to use this many plots and dont want continous coverage, need to create own plots 

#read in image being used as referance to see where plots will be placed 
library(raster) #used to read in and worth with raster files 

#Canopy height model for Harvard Forest 
rasterimage <- raster("NEON-DS-Airborne-Remote-Sensing/HARV/CHM/HARV_chmCrop.tif")
plot(rasterimage)
View(rasterimage) #gives information about the raster files
plot(quadrats, add = T) #add orginal shapefile on top of chm

#crop to area of shapefile to obtain required area
newrasterimage <- crop(rasterimage, quadrats)
plot(newrasterimage)
plot(quadrats, add = T)
#now everything lines up 

#setting random plots came from code found from online search (stackoverflow?) 
library(sf) #merge geometric features together

lin <- rasterToContour(newrasterimage) #creating boundry around image
pol <- as(st_union(st_polygonize(st_as_sf(lin))), 'Spatial') #dissolve geometries 
  #creates boundry area to place points, turn image into one big polygon

library(sp) #for working with spatial data

#seed generator to make sure plots are plotted the same with each run, can be any number
set.seed(98) 

pts <- spsample(pol[1,], 15, type = 'regular') #creating subsamples of points 
  #geometry points fall into, number of points, type of distribution 
    #types of distrubution: 
      #random, regular, stratified, nonalighned, hexagonal, clustered, Fibonacci

plot(newrasterimage)
plot(pts, add = T, col = 'black') #plots created points over raster image

#obtain the coordinates for each of the points
pointCoord <- coordinates(pts) 
View(pointCoord)

#want to make square plots around random points created
  #going to use point coordinates as centroids 
  #https://www.neonscience.org/field-data-polygons-centroids - tutorial used 

centroids <- as.data.frame(coordinates(pts)) #turn point coordinates into a dataframe
tail(centroids) #look at how many points there are
plots <- c(1:17) #add plot numbers, needs to match up with number of random plots created
  #can use character string to name plots as well 
new <- cbind(centroids, plots) #add plot numbers to dataframe 
head(new)
colnames(new) <- c("easting", "northing", "plots")  #rename columns 
head(new) 

#going to make 30m by 30m plots, radius = 15
radius <- 15 #radius of plots found in BEF, in meters

#designating the edges from plot center
yPlus <- new$northing+radius
xPlus <- new$easting+radius
yMinus <- new$northing-radius
xMinus <- new$easting-radius

#creating the square buffer around plot center 
square=cbind(xMinus,yPlus, #NWcorner
             xPlus,yPlus, #NEcorner
             xPlus,yMinus, #SEcorner
             xMinus,yMinus, #SWcorner
             xMinus,yPlus) #NW again

ID=new$plots #creating id variable

#creating polygons 
polys <- SpatialPolygons(mapply(function(poly, id){
  xy <- matrix(poly, ncol = 2, byrow = TRUE)
  Polygons(list(Polygon(xy)), ID=id)
},
split(square, row(square)), ID),
proj4string=CRS(as.character("+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))

#to find projection of image plots are being referenced to 
View(newrasterimage) #look under crs -> projargs

plot(newrasterimage)
plot(polys, add = T)
plot(pts, add = T)
#can see new plots placed on image where points were located before 

#create spatialPolygonDataFrame -- step is required to output multiple polygons
polys.df <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names = ID))

#Write shape file if wanted
writeOGR(polys.df,'.', 'HF_randomplots','ESRI Shapefile')

###############################################################################################################################

##matching inventory data with new polygons 

#issue - if you want diversity for each plot, need to associate trees with new plots
#inventory trees come with corrdinates, not all inventories allow for this 
head(trees)

#need to convert tree coordinates from unique coordinates into common UTM values 
library(dplyr) #used for mutate function, can also use package plyr

vars.add <- mutate(trees, Ny = gy + 4713221.9, Ex = gx + 731592.9)
head(vars.add) #can see new columns added to dataframe 
#have the option to filter for trees alive or dead, did not do it here

write.csv(vars.add, "HF_trees_2014_new.csv")

#need to use trees coordinates and create a shapefile of their locations 
#used QGIS for this, but can also do it in R with the following NEON tutorial 
  #https://www.neonscience.org/dc-csv-to-shapefile-r
#QGIS steps used: 
  #Create Layer from delinated text file 
  #put in correct x and y values under geometry definition
  #choose UTM
  #layers -> save as -> name layer and choose location to save to
  
#read in new shapefile
trees2 <- readOGR("HF_trees_locations.shp")
  #uses points to represent the location of each tree
plot(trees2)

#loop calling polygons created above
#associates tree coordinates that fall within the coordinates of the polygons
for (i in 1:length(polys@polygons)){
  print (i)
  polyi <- polys[i,]
  treein <- over(polyi, trees2, returnList = T) #returnList needs to be true to output all of y that falls in x
  plot(polyi)
  print(treein)
  mapply(write.csv, treein, file=paste0(names(treein), '.csv')) 
  #writes each plot into indivdual .csv's, 
  #not sure how to combine into one file where trees are listed with the plot name they fall into
}

#could have written into list then turned list into dataframe 

#new file created by hand merging all the individual files together
#can use new file in same way oringal file was used before for the diversity calcualtions
trees3 <- read.csv("HF_treesbynewplots.csv")

###############################################################################################################################

##calculaiton of vegetation indicies 

#have individual images from four bands of a hyperspectral image 
band19 <- raster("NEON-DS-Airborne-Remote-Sensing/SJER/RGB/band19.tif") #blue band
band34 <- raster("NEON-DS-Airborne-Remote-Sensing/SJER/RGB/band34.tif") #green band
band58 <- raster("NEON-DS-Airborne-Remote-Sensing/SJER/RGB/band58.tif") #red band
band90 <- raster("NEON-DS-Airborne-Remote-Sensing/SJER/RGB/band90.tif") #Near Infared band (NIR)
#extent of band 90 is not the same as the others, need to fit extend of band to match the others
band90 <- setExtent(band90, band58, snap=T) 

#combine images into a raster stack 
hype <- raster::brick(band19, band34, band58, band90)
  #this is the form of most hyperspectral images 

plot(hype) #will put the band indivdually 

#plot one image in true color 
plotRGB(hype, r = 3, g = 2, b = 1, stretch = "lin")
plotRGB(hype, r = 3, g = 2, b = 1, stretch = "hist")
plotRGB(hype, r = 3, g = 2, b = 1, stretch = "NULL")
#r = red = band 3; g = green = band 2; b = blue = band 1
  #note: band numbers used will depend on image being used
#strech = visual preference 

#also a way to plot true color images using ggplot - save for another day


#band math - use the different bands to create vegetation indicies 

#Normalized Difference Vegetation Index
#NDVI = (NIR - Red) / (NIR + Red) 
  #most commonly used index, used for veg health, drought impacts, nutrient concentrations, etc. 

ndvi <- (hype$band90-hype$band58)/(hype$band90+hype$band58)
plot(ndvi)
#can now see contrast between vegetation, bareground, and pavement 


#Ratio Vegetation Index 
#RVI = R/NIR; one of the first indices made

rvi = hype$band58/hype$band90
plot(rvi)
#looks almost like the inverse of NDVI

#Vegetation Index Number 
#VIN = NIR/R; inverse of RVI, functionally the same as NDVI

vin = hype$band90/hype$band58
plot(vin)

#many more indices to be calculated 
  #e.g. Atmospherically Resistant Vegetation Index (ARVI), Soil-Adjusted Vegetation Index (SAVI),
    #Enhanced Vegetation Index (EVI), etc. 

###############################################################################################################################

##extract imagery information by plot

#say you want the ndvi values for set of random plots found over the area
#can extract values using extract function 

#making random plots for the image being used 
#same code as used above, just repalced newrasterlayer with ndvi, pt type, number of points, 
  #seed, UTM, and shapefile name
plot(ndvi)
lin <- rasterToContour(ndvi) 
pol <- as(st_union(st_polygonize(st_as_sf(lin))), 'Spatial')
set.seed(5)
pts <- spsample(pol[1,], 10, type = 'random')  
plot(ndvi)
plot(pts, add = T, col = 'black') 
pointCoord <- coordinates(pts) 
View(pointCoord)
centroids <- as.data.frame(coordinates(pts))
tail(centroids)
plots <- c(1:10) 
new <- cbind(centroids, plots) 
colnames(new) <- c("easting", "northing", "plot") 
str(new) 
radius <- 15
yPlus <- new$northing+radius
xPlus <- new$easting+radius
yMinus <- new$northing-radius
xMinus <- new$easting-radius
square=cbind(xMinus,yPlus, #NWcorner
             xPlus,yPlus, #NEcorner
             xPlus,yMinus, #SEcorner
             xMinus,yMinus, #SWcorner
             xMinus,yPlus) #NW again
ID=new$plot
polys <- SpatialPolygons(mapply(function(poly, id){
  xy <- matrix(poly, ncol = 2, byrow = TRUE)
  Polygons(list(Polygon(xy)), ID=id)
},
split(square, row(square)), ID),
proj4string=CRS(as.character("+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")))
plot(ndvi)
plot(polys, add = T)
plot(pts, add = T)
polys.df <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names = ID))
writeOGR(polys.df,'.', 'SJER_randomplots','ESRI Shapefile')

#extracting ndvi file per plot 
#shapefile = object polys or read in shapefile with readOGR
ndviValues <- raster::extract(ndvi, polys, fun = mean, df = T)
  #uses raster file ndvi, extracts values per plot, outputs mean value of pixels within plot, 
  #returns as dataframe
View(ndviValues)

#if no function is provided, outputs ndvi value for each pixel found in each plot 
ndviValues2 <- raster::extract(ndvi, polys, df = T)
View(ndviValues2)

#function options = mean, sd, cv, var, min, max, sum, etc... 

#output more than one stat for the image (only works with one layer images)
stats <- function(x, na.rm) c(mean = mean(x, na.rm = na.rm), sd = sd(x, na.rm = na.rm), 
                              min = min(x, na.rm = na.rm), max = max(x, na.rm = na.rm), 
                              var = var(x, na.rm = na.rm), cv = cv(x, na.rm = na.rm),
                              sum = sum(x, na.rm = na.rm))
  #this skips over missing values in the data

#extract all stats from image
ndviValues3 <- raster::extract(ndvi, polys, fun = stats, na.rm = T)
View(ndviValues3)
#missing plot names with values - can extract those from shape file and bind with data


#want to output ndvi values in csv file 
write.csv(ndviValues, "SJER_NDVIvalues.csv")
