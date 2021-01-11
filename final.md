
library(sf)
library(tidyverse)
library(spatstat)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(geojson)
library(geojsonio)
library(tmaptools)
library(stringr)

### Visualize the changes of London Airbnb listings count between 2019 and 2020

Londonboroughs <- st_read("data/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")
mycsv <-  read_csv("data/change.csv")  
Merge <- Londonboroughs%>%
  merge(.,
        mycsv,
        by.x="GSS_CODE", 
        by.y="GSS_CODE")

tmap_mode("plot")
qtm()

Merge %>%
  qtm(.,fill = "Change of Airbnb listings count")

###Kernel Density Estimation 
### we use Kernel Density Estimation to exam the distribution of two main types of Airbnb listings at  Borough level. The sigma value sets the diameter of the Kernel and we set the sigma as 500m.


BoroughMap <- Londonboroughs %>%
  dplyr::filter(str_detect(GSS_CODE, "^E09"))%>%
  st_transform(., 27700) 

e2020<- read_csv("data/2020-10listings.csv") %>%
  st_as_sf(., coords = c("longitude", "latitude"), 
           crs = 4326) %>%
  st_transform(., 27700)%>%
  filter(room_type == 'Entire home/apt')

e2020 <- distinct(e2020)
window <- as.owin(BoroughMap)
e2020<- e2020 %>%
  as(., 'Spatial')
e2020.ppp <- ppp(x=e2020@coords[,1],
                 y=e2020@coords[,2],
                 window=window)

e2020.ppp %>%
  density(., sigma=500) %>%
  plot(main="2020entire home/apt  KDE")

### 2019 entire home/apt KDE
e2019<- read_csv("data/2019-12-09listings.csv") %>%
  st_as_sf(., coords = c("longitude", "latitude"), 
           crs = 4326) %>%
  st_transform(., 27700)%>%
  filter(room_type == 'Entire home/apt')

e2019 <- distinct(e2019)
window <- as.owin(BoroughMap)
e2019<- e2019 %>%
  as(., 'Spatial')
e2019.ppp <- ppp(x=e2019@coords[,1],
                 y=e2019@coords[,2],
                 window=window)

e2019.ppp %>%
  density(., sigma=500) %>%
  plot(main="2019entire home/apt  KDE")

###2019private room  KDE

p2019<- read.csv("data/2019-12-09listings.csv") %>%
  st_as_sf(., coords = c("longitude", "latitude"), 
           crs = 4326) %>%
  st_transform(., 27700)%>%
  filter(room_type == 'Private room')

tmap_mode("view")
tm_shape(BoroughMap) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(p2019) +
  tm_dots(col = "blue")

p2019 <- distinct(p2019)
tmap_mode("view")
tm_shape(BoroughMap) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(p2019) +
  tm_dots(col = "blue")
window <- as.owin(BoroughMap)
plot(window)
p2019<- p2019 %>%
  as(., 'Spatial')

p2019.ppp <- ppp(x=p2019@coords[,1],
                 y=p2019@coords[,2],
                 window=window)
p2019.ppp %>%
  plot(.,pch=16,cex=0.5, main="2019 private room count")

p2019.ppp %>%
  density(., sigma=500) %>%
  plot(main="2019private room  KDE")

###2020private room  KDE

p2020<- read.csv("data/2020-10listings.csv") %>%
  st_as_sf(., coords = c("longitude", "latitude"), 
           crs = 4326) %>%
  st_transform(., 27700)%>%
  filter(room_type == 'Private room')

tmap_mode("view")
tm_shape(BoroughMap) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(p2020) +
  tm_dots(col = "blue")

p2020 <- distinct(p2020)
tmap_mode("view")
tm_shape(BoroughMap) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(p2020) +
  tm_dots(col = "blue")
window <- as.owin(BoroughMap)
plot(window)
p2020<- p2020 %>%
  as(., 'Spatial')

p2020.ppp <- ppp(x=p2020@coords[,1],
                 y=p2020@coords[,2],
                 window=window)
p2020.ppp %>%
  plot(.,pch=16,cex=0.5, main="2020 private room count")

p2020.ppp %>%
  density(., sigma=500) %>%
  plot(main="2020private room  KDE")


###spatial autocorrelation

LondonWards <- st_read('data/statistical-gis-boundaries-london/ESRI/London_Ward.shp')
WardData <- read.csv("data/ward-profiles-excel-version.csv")
LondonWardsMerged <- st_read('data/statistical-gis-boundaries-london/ESRI/London_Ward_CityMerged.shp')%>%
  st_transform(.,27700)
LondonWardsMerged <- LondonWardsMerged %>% 
  left_join(WardData, 
            by = c("GSS_CODE" = "New.code"))%>%
  distinct(GSS_CODE, NAME, Ward.name,LB_GSS_CD,BOROUGH)


e2019<- read.csv("data/2019-12-09listings.csv") %>%
  st_as_sf(., coords = c("longitude", "latitude"), 
           crs = 4326) %>%
  st_transform(., 27700)%>%
  filter(room_type == 'Entire home/apt')

e2019 <- e2019[LondonWardsMerged,]

tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(e2019) +
  tm_dots(col = "blue")

points_sf_joined <- LondonWardsMerged%>%
  st_join(e2019)%>%add_count(GSS_CODE)%>%
  janitor::clean_names()%>%
  mutate(area=st_area(.))%>%
  mutate(density=n/area)%>%
  dplyr::select(density, ward_name, gss_code,n)


points_sf_joined<- points_sf_joined %>%                    
  group_by(gss_code) %>%         
  summarise(density = first(density),
            wardname= first(ward_name),
            plaquecount= first(n))

ed2019<-tm_shape(points_sf_joined) +
  tm_polygons("density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("wardname", "density"),
              title="2019 entire home/apt Density")

tmap_save(ed2019,'2019 entire home Density.png')

library(spdep)
coordsW <- points_sf_joined%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW,axes=TRUE)

LWard_nb <- points_sf_joined %>%
  poly2nb(., queen=T)

#plot them
plot(LWard_nb, st_geometry(coordsW), col="red")
#add a map underneath
plot(points_sf_joined$geometry, add=T)
Lward.lw <- LWard_nb %>%
  nb2listw(., style="C")

head(Lward.lw$neighbours)

I_LWard_Global_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  moran.test(., Lward.lw)

I_LWard_Global_Density

C_LWard_Global_Density <- 
  points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  geary.test(., Lward.lw)

C_LWard_Global_Density

G_LWard_Global_Density <- 
  points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  globalG.test(., Lward.lw)

G_LWard_Global_Density

I_LWard_Local_count  <- points_sf_joined %>%
  pull(plaquecount) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

I_LWard_Local_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()


points_sf_joined <- points_sf_joined %>%
  mutate(density_I =as.numeric(I_LWard_Local_Density$Ii))%>%
  mutate(density_Iz =as.numeric(I_LWard_Local_Density$Z.Ii))

breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
MoranColours<- rev(brewer.pal(8, "RdGy"))


tm_shape(points_sf_joined) +
  tm_polygons("density_Iz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, 2019 Airbnb entire home/apt in London")
Gi_LWard_Local_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  localG(., Lward.lw)

head(Gi_LWard_Local_Density)


points_sf_joined <- points_sf_joined %>%
  mutate(density_G = as.numeric(Gi_LWard_Local_Density))

GIColours<- rev(brewer.pal(8, "RdBu"))


tm_shape(points_sf_joined) +
  tm_polygons("density_G",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, 2019 Airbnb entire home/apt in London")



###ed2020
e2020<- read.csv("data/2020-10listings.csv") %>%
  st_as_sf(., coords = c("longitude", "latitude"), 
           crs = 4326) %>%
  st_transform(., 27700)%>%
  filter(room_type == 'Entire home/apt')

e2020 <- e2020[LondonWardsMerged,]

tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(e2020) +
  tm_dots(col = "blue")

points_sf_joined <- LondonWardsMerged%>%
  st_join(e2020)%>%add_count(GSS_CODE)%>%
  janitor::clean_names()%>%
  mutate(area=st_area(.))%>%
  mutate(density=n/area)%>%
  dplyr::select(density, ward_name, gss_code,n)


points_sf_joined<- points_sf_joined %>%                    
  group_by(gss_code) %>%         
  summarise(density = first(density),
            wardname= first(ward_name),
            plaquecount= first(n))

ed2020<-tm_shape(points_sf_joined) +
  tm_polygons("density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("wardname", "density"),
              title="2020 entire home/apt Density")

tmap_save(ed2020,'2020 entire home Density.png')


coordsW <- points_sf_joined%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW,axes=TRUE)

LWard_nb <- points_sf_joined %>%
  poly2nb(., queen=T)

#plot them
plot(LWard_nb, st_geometry(coordsW), col="red")
#add a map underneath
plot(points_sf_joined$geometry, add=T)
Lward.lw <- LWard_nb %>%
  nb2listw(., style="C")

head(Lward.lw$neighbours)

I_LWard_Global_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  moran.test(., Lward.lw)

I_LWard_Global_Density

C_LWard_Global_Density <- 
  points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  geary.test(., Lward.lw)

C_LWard_Global_Density

G_LWard_Global_Density <- 
  points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  globalG.test(., Lward.lw)

G_LWard_Global_Density

I_LWard_Local_count  <- points_sf_joined %>%
  pull(plaquecount) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

I_LWard_Local_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()


points_sf_joined <- points_sf_joined %>%
  mutate(density_I =as.numeric(I_LWard_Local_Density$Ii))%>%
  mutate(density_Iz =as.numeric(I_LWard_Local_Density$Z.Ii))

breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
MoranColours<- rev(brewer.pal(8, "RdGy"))


tm_shape(points_sf_joined) +
  tm_polygons("density_Iz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, 2020 Airbnb entire home/apt in London")
Gi_LWard_Local_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  localG(., Lward.lw)

head(Gi_LWard_Local_Density)


points_sf_joined <- points_sf_joined %>%
  mutate(density_G = as.numeric(Gi_LWard_Local_Density))

GIColours<- rev(brewer.pal(8, "RdBu"))


tm_shape(points_sf_joined) +
  tm_polygons("density_G",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, 2020 Airbnb entire home/apt in London")

###p2019

p2019<- read.csv("data/2019-12-09listings.csv") %>%
  st_as_sf(., coords = c("longitude", "latitude"), 
           crs = 4326) %>%
  st_transform(., 27700)%>%
  filter(room_type == 'Private room')
p2019 <- p2019[LondonWardsMerged,]

tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(p2019) +
  tm_dots(col = "blue")

points_sf_joined <- LondonWardsMerged%>%
  st_join(p2019)%>%add_count(GSS_CODE)%>%
  janitor::clean_names()%>%
  mutate(area=st_area(.))%>%
  mutate(density=n/area)%>%
  dplyr::select(density, ward_name, gss_code,n)


points_sf_joined<- points_sf_joined %>%                    
  group_by(gss_code) %>%         
  summarise(density = first(density),
            wardname= first(ward_name),
            plaquecount= first(n))

pd2019<-tm_shape(points_sf_joined) +
  tm_polygons("density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("wardname", "density"),
              title="2019 private room Density")

tmap_save(pd2019,'2019 private room Density.png')


coordsW <- points_sf_joined%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW,axes=TRUE)

LWard_nb <- points_sf_joined %>%
  poly2nb(., queen=T)

#plot them
plot(LWard_nb, st_geometry(coordsW), col="red")
#add a map underneath
plot(points_sf_joined$geometry, add=T)
Lward.lw <- LWard_nb %>%
  nb2listw(., style="C")

head(Lward.lw$neighbours)

I_LWard_Global_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  moran.test(., Lward.lw)

I_LWard_Global_Density

C_LWard_Global_Density <- 
  points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  geary.test(., Lward.lw)

C_LWard_Global_Density

G_LWard_Global_Density <- 
  points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  globalG.test(., Lward.lw)

G_LWard_Global_Density

I_LWard_Local_count  <- points_sf_joined %>%
  pull(plaquecount) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

I_LWard_Local_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()


points_sf_joined <- points_sf_joined %>%
  mutate(density_I =as.numeric(I_LWard_Local_Density$Ii))%>%
  mutate(density_Iz =as.numeric(I_LWard_Local_Density$Z.Ii))

breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
MoranColours<- rev(brewer.pal(8, "RdGy"))


tm_shape(points_sf_joined) +
  tm_polygons("density_Iz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, 2019 Airbnb private room in London")
Gi_LWard_Local_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  localG(., Lward.lw)

head(Gi_LWard_Local_Density)


points_sf_joined <- points_sf_joined %>%
  mutate(density_G = as.numeric(Gi_LWard_Local_Density))

GIColours<- rev(brewer.pal(8, "RdBu"))


tm_shape(points_sf_joined) +
  tm_polygons("density_G",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, 2019 Airbnb private room in London")



###p2020
p2020<- read.csv("data/2020-10listings.csv") %>%
  st_as_sf(., coords = c("longitude", "latitude"), 
           crs = 4326) %>%
  st_transform(., 27700)%>%
  filter(room_type == 'Private room')
p2020 <- p2020[LondonWardsMerged,]

tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(p2020) +
  tm_dots(col = "blue")

points_sf_joined <- LondonWardsMerged%>%
  st_join(p2020)%>%add_count(GSS_CODE)%>%
  janitor::clean_names()%>%
  mutate(area=st_area(.))%>%
  mutate(density=n/area)%>%
  dplyr::select(density, ward_name, gss_code,n)


points_sf_joined<- points_sf_joined %>%                    
  group_by(gss_code) %>%         
  summarise(density = first(density),
            wardname= first(ward_name),
            plaquecount= first(n))

pd2020<-tm_shape(points_sf_joined) +
  tm_polygons("density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("wardname", "density"),
              title="2020 private room Density")

tmap_save(pd2020,'2020 private room Density.png')


coordsW <- points_sf_joined%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW,axes=TRUE)

LWard_nb <- points_sf_joined %>%
  poly2nb(., queen=T)

#plot them
plot(LWard_nb, st_geometry(coordsW), col="red")
#add a map underneath
plot(points_sf_joined$geometry, add=T)
Lward.lw <- LWard_nb %>%
  nb2listw(., style="C")

head(Lward.lw$neighbours)

I_LWard_Global_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  moran.test(., Lward.lw)

I_LWard_Global_Density

C_LWard_Global_Density <- 
  points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  geary.test(., Lward.lw)

C_LWard_Global_Density

G_LWard_Global_Density <- 
  points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  globalG.test(., Lward.lw)

G_LWard_Global_Density

I_LWard_Local_count  <- points_sf_joined %>%
  pull(plaquecount) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

I_LWard_Local_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()


points_sf_joined <- points_sf_joined %>%
  mutate(density_I =as.numeric(I_LWard_Local_Density$Ii))%>%
  mutate(density_Iz =as.numeric(I_LWard_Local_Density$Z.Ii))

breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
MoranColours<- rev(brewer.pal(8, "RdGy"))


tm_shape(points_sf_joined) +
  tm_polygons("density_Iz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, 2020 Airbnb entire home/apt in London")
Gi_LWard_Local_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  localG(., Lward.lw)

head(Gi_LWard_Local_Density)


points_sf_joined <- points_sf_joined %>%
  mutate(density_G = as.numeric(Gi_LWard_Local_Density))

GIColours<- rev(brewer.pal(8, "RdBu"))


tm_shape(points_sf_joined) +
  tm_polygons("density_G",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, 2020 Airbnb private room in London")

###

