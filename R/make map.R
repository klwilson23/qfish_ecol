# See: https://github.com/bcgov/bcmaps
# see: https://cran.r-project.org/web/packages/bcmaps/bcmaps.pdf
# availble layers in bcmaps:
# https://gist.github.com/ateucher/86674e12ffba66ecce87cceb7bffbf41
# https://github.com/poissonconsulting/fwabc#<https://github.com/poissonconsulting/fwabc>
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html

buffer <- 25000

library(dbplyr)
library(bcdata)
library(dplyr)
library(bcmaps)
library(sf)
library(sp)
library(ggplot2)
library(ggspatial) # this is for adding the arrows
library(rgdal) #use this for data conversion
library(ggrepel) # to offset labels using geom_sf_label_repel  --> Not done here
library(riverdist) # to snap points to River --> Not done here
library(bcmapsdata)
library(viridis)
library(ggnewscale)
library(tidyr)
library(cowplot)

co_pops<-read.table("Data/coho_groups.txt",header=TRUE)
### lumping Rivers Smith Inlet with Area 7-8
co_pops[which(co_pops$group==7),4]<-6
group_names <- c("Central Coast (South)","Hecate Lowlands","Inner Waters","Haida Gwaii","Skeena","Nass")
group_names <- group_names[c(4,6,5,2,3,1)]
group_names <- factor(group_names,levels=c("Haida Gwaii","Nass","Skeena","Hecate Lowlands","Inner Waters","Central Coast (South)"))
co_pops$region <- group_names[co_pops$group]

NCC_coho <- read.csv("Data/NCC coho.csv",header=TRUE)
NCC_coho$region <- co_pops$region[match(NCC_coho$population,co_pops$population)]
NCC_coho$lon <- NCC_coho$Longitude
NCC_coho$lat <- NCC_coho$Latitude

data2 <- NCC_coho[,c("lon","lat")] #%>% mutate(UTM_W=as.numeric(UTM_W),UTM_N=as.numeric(UTM_N)) #
sputm <- SpatialPoints(data2, proj4string=CRS("+proj=longlat"))
spgeo <- spTransform(sputm, CRS("+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
data_points <- as_tibble(NCC_coho) %>% mutate(lat=spgeo@coords[,2],lon=spgeo@coords[,1]) %>%
  st_as_sf(coords=c("lon","lat"),crs=3005)

## Subset the points data to be a single point for each location (for labelling Locations)
data_point_labels <- data_points %>% group_by(population) %>% slice(1)

ncc_extent <- raster::extent(data_points)
ncc_extent@xmin <- ncc_extent@xmin - 3*buffer
ncc_extent@ymin <- ncc_extent@ymin - buffer 
ncc_extent@xmax <- ncc_extent@xmax + buffer 
ncc_extent@ymax <- ncc_extent@ymax + buffer 
plot_area_ncc <- ncc_extent %>%
  st_bbox() %>%                 # Turn into a square
  st_as_sfc(crs=st_crs(data_points))
st_crs(plot_area_ncc) <- st_crs(data_points)
#bcdc_get_record("https://catalogue.data.gov.bc.ca/dataset/freshwater-atlas-rivers")
rivers_in_plot_area <- bcdc_query_geodata('f7dac054-efbf-402f-ab62-6fc4b32a619e') %>%
  #filter(STREAM_ORDER %in% c(3,4,5)) %>%  #Defines as only streams order 3,4,5 (too many including 1 &2)
  filter(INTERSECTS(plot_area_ncc)) %>%      # not sure about this line
  collect() %>%                           #Extracts the data
  st_intersection(plot_area_ncc)             #Where it intersects with plot line

#bcdc_get_record("https://catalogue.data.gov.bc.ca/dataset/freshwater-atlas-watersheds-groups")
watersheds <- bcdc_query_geodata('51f20b1a-ab75-42de-809d-bf415a0f9c62') %>%
  filter(INTERSECTS(plot_area_ncc)) %>%      # not sure about this line
  collect() %>%                           #Extracts the data
  st_intersection(plot_area_ncc)             #Where it intersects with plot line

bcdc_get_record("https://catalogue.data.gov.bc.ca/dataset/freshwater-atlas-stream-network")
all_streams <- bcdc_query_geodata("92344413-8035-4c08-b996-65a9b3f62fca") %>%
  filter(INTERSECTS(plot_area_ncc)) %>%
  filter(STREAM_ORDER%in% c(4,5,6)) %>%
  collect() %>%                           #Extracts the data
  st_intersection(plot_area_ncc)             #Where it intersects with plot line

rivers_index <- data_point_labels %>%
  st_nearest_feature(rivers_in_plot_area,data_point_labels)
rivers_ncc <- rivers_in_plot_area[rivers_index,]

rivernames <- rivers_ncc$GNIS_NAME_1
rivernames2 <- data_point_labels$population

major_riverGroup <- substr(rivers_ncc$FWA_WATERSHED_CODE,1,3)
major_riverID <- paste(major_riverGroup,paste(rep("000000",20),collapse="-"),sep="-")
major_groups <- substr(rivers_in_plot_area$FWA_WATERSHED_CODE,1,3)
major_riverID <- ifelse(as.numeric(major_riverGroup)>=900,rivers_ncc$FWA_WATERSHED_CODE,"x")

nass <- rivers_in_plot_area[rivers_in_plot_area$GNIS_NAME_1%in%"Nass River" & rivers_in_plot_area$WATERSHED_GROUP_CODE=="LNAR",]
skeena <- rivers_in_plot_area[rivers_in_plot_area$GNIS_NAME_1%in%"Skeena River" & rivers_in_plot_area$WATERSHED_GROUP_CODE=="LSKE",]

major_rivers <- rivers_in_plot_area[match(major_riverID,rivers_in_plot_area$FWA_WATERSHED_CODE,nomatch = 0),]
major_rivers <- dplyr::bind_rows(major_rivers,nass,skeena)

data_point_labels$river_name <- rivernames
major_riverGroup[which(data_point_labels$population=="lachmach")] <- "500"
data_point_labels$waterbodyid <- ifelse(as.numeric(major_riverGroup)>=900,rivers_ncc$FWA_WATERSHED_CODE,ifelse(as.numeric(major_riverGroup)==500,nass$FWA_WATERSHED_CODE,skeena$FWA_WATERSHED_CODE))
data_point_labels$drainage <- ifelse(as.numeric(major_riverGroup)>=900,rivers_ncc$FWA_WATERSHED_CODE,ifelse(as.numeric(major_riverGroup)==500,"Nass","Skeena"))

## --- Collect Mapping layers
# below grabs the coastline data within plot box
bchres <- bc_bound_hres()
coast_line <- bchres %>% st_intersection(plot_area_ncc)

#Below is to get a dataset of lakes in plot box
lakes_in_plot_area <- bcdc_query_geodata("freshwater-atlas-lakes") %>%
  filter(INTERSECTS(plot_area_ncc)) %>%
  collect() %>%
  st_intersection(plot_area_ncc)

# Ocean
ocean <- bc_neighbours(ask=FALSE) %>% 
  filter(type=="Ocean")

river_mouth <- major_rivers %>%
  group_by(BLUE_LINE_KEY) %>%
  st_nearest_points(ocean)
pts <- st_cast(river_mouth, "POINT")[seq(2, 2*length(river_mouth), 2)]

major_rivers$ocean_entry <- pts
data_point_labels$ocean_entry <- major_rivers$ocean_entry[match(data_point_labels$waterbodyid,major_rivers$FWA_WATERSHED_CODE,nomatch = NA)]
data_point_labels$ocean_entry[is.na(data_point_labels$ocean_entry)] <- ifelse(data_point_labels$drainage=='Skeena' & is.na(data_point_labels$ocean_entry),major_rivers$ocean_entry[major_rivers$GNIS_NAME_1%in%"Skeena River"],major_rivers$ocean_entry[major_rivers$GNIS_NAME_1%in%"Nass River"])


ggplot(data=watersheds) +
  geom_sf(data = watersheds, fill = "grey90") +
  #geom_sf(data=rivers_ncc,lwd=1.5,colour = "black") +
  geom_sf(data=major_rivers,lwd=1,colour = "black") +
  geom_sf(data=pts,pch=22,cex=2,bg='seagreen') +
  geom_sf(data=data_points,pch=21,cex=1.5,bg='orange')+
  scale_shape_manual(values=c("Rivers"=21,"Ocean entry"=22),breaks=c("Rivers","Ocean entry"))

ggplot(data=watersheds) +
  geom_sf(data = watersheds, fill = "grey90") +
  #geom_sf(data=rivers_ncc,lwd=1.5,colour = "black") +
  geom_sf(data=major_rivers,lwd=1,colour = "black") +
  geom_sf(data=data_point_labels$ocean_entry[data_point_labels$population=="bella_coola"],pch=22,cex=2,bg='seagreen')

#alaska <- bcmaps::bc_neighbours() %>% st_intersection(plot_area_ncc)
#alaska_full <- bcmaps::bc_neighbours()
pfma <- st_read("Data/PFMA/DFO_PFMA_SUBAREAS_SP/DFO_SBAREA_polygon.shp")
pfma <- pfma[pfma$MGMT_AREA>=2 & pfma$MGMT_AREA<=10,]
catch_area <- pfma %>% st_intersection(plot_area_ncc) %>% st_difference(bchres)
bc_sub <- bchres %>% st_intersection(plot_area_ncc)
bc_fish <- bchres %>% st_intersection(catch_area)

alaska <- st_transform(USAboundaries::us_states(states = "Alaska", resolution = "high"),st_crs(plot_area_ncc))
alaska_full <- alaska %>% st_intersection(bchres)
alaska <- alaska  %>% st_intersection(plot_area_ncc)
###########

## The fun part! we get to make a map
## Plot map -- Order of plotting MATTERS
catch_area_union <- catch_area %>% group_by(MGMT_AREA) %>% summarize(geometry = st_union(geometry)) %>% st_sf() # collapse cc_sub to one polygon

ncc_map <- ggplot() +
  geom_sf(data = catch_area_union, aes(fill=as.factor(MGMT_AREA))) +
  scale_fill_manual(name="Management areas",values=c("#7570b3","#d95f02","#e7298a","#a6761d","#6a3d9a","#fdbf6f","#66c2a4","#e6ab02","#cab2d6")) +
  new_scale("fill") +
  geom_sf(data=alaska, fill='grey85') +
  geom_sf(data = coast_line, fill = "grey90") +                 #Plot coastline
  geom_sf(data = rivers_in_plot_area, colour = "#1f78b4") +  #Plot Rivers
  geom_sf(data = all_streams, colour = "#1f78b4",lwd=0.15) +  #Plot Rivers
  geom_sf(data = rivers_ncc, colour = "#1f78b4") +
  geom_sf(data = lakes_in_plot_area, fill = "#1f78b4",colour=NA) +     #Plot Lakes
  ggsflabel::geom_sf_label_repel(data=data_point_labels,aes(label=population,fill=region),size=1.5, force = 1, nudge_x = -2, seed = 10,max.overlaps=20)+ #Add labels
  geom_sf(data=data_point_labels,pch=21,aes(fill=region)) +
  guides(fill = guide_legend(override.aes = aes(label = ""))) +
  scale_fill_brewer(name="Region",palette = "Paired",direction=1) +
  #geom_sf(data=pts,pch=22,fill="seagreen") +
  geom_sf(data = plot_area_ncc, alpha = 0,colour='black') +        #Plot area box
  coord_sf(expand = FALSE) +                                    #Expands box to axes
  xlab('Longitude') + ylab('Latitude') +                        #Axis titles
  annotation_scale(location = "bl", width_hint = 0.5) +         #Rose Compass
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.5, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering,
                         height = unit(1,"cm"), width = unit(1, "cm"))+
  theme(panel.background = element_rect('lightblue1'), panel.grid.major = element_line('lightblue1'),legend.position="top",legend.box.just="center",legend.box="horizontal",legend.justification = "center",legend.key.size=unit(1, "lines"),legend.margin = margin(c(0,0,0,-1),unit="lines"),legend.title=element_text(size=6),legend.text = element_text(size=5))

bc_neigh <- bc_neighbours(ask=FALSE)
bc_neigh <- bc_neigh[bc_neigh$name%in%c("Alaska"),]
bc_map <- ggplot() +
  geom_sf(data = bc_bound(ask=FALSE), fill = "grey10",colour=NA) +
  geom_sf(data=bc_neigh, fill='grey50',colour=NA) +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(panel.background = element_rect(fill=adjustcolor("white",0.90), colour="black")) +
  theme(legend.justification = c(0, 1),legend.position = c(0, .95)) +
  theme(text = element_text(family = "Futura-Medium"),legend.title = element_text(family = "Futura-Bold", size = 10),legend.text = element_text(family = "Futura-Medium", size = 10))
bc <- bc_map + geom_rect(aes(xmin=ncc_extent@xmin,ymin=ncc_extent@ymin,xmax=ncc_extent@xmax,ymax=ncc_extent@ymax),fill = "tomato", colour = "grey70",alpha=0.5)

ncc_inset <- ggdraw(ncc_map) +
  draw_plot({
    bc},
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = 0.20, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.71,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.2, 
    height = 0.2)
ggsave('Figures/ncc_coho.jpeg',plot=ncc_inset,width = 6, height = 7,units='in',dpi=800)
