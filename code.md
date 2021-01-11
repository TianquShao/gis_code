---
title: "final"
author: "Tianqu Shao"
date: "11/01/2021"
output: html_document
---

## Part I--Spatial distribution variation of London Airbnb listing
1.Distribution of London Airbnb listing count at borough level
  We used Airbnb data in December 2019 and Airbnb data in October 2020 to make spatial distribution maps at Borough Level. The darker the color, the larger the interval and the greater the quantity. 

```{r warning=FALSE, include=FALSE}

library(sf)
library(tmap)
library(tmaptools)
library(tidyverse)

Londonborough <- st_read("data/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")%>%
  st_transform(., 27700)

l2019<- read.csv("data/2019-12-09listings.csv") %>%
  st_as_sf(., coords = c("longitude", "latitude"), 
           crs = 4326) %>%
  st_transform(., 27700)

l2020<- read.csv("data/2020-10listings.csv") %>%
  st_as_sf(., coords = c("longitude", "latitude"), 
           crs = 4326) %>%
  st_transform(., 27700)


Joinfun <- function(data1, data2){
  
  output<- data1%>%
    st_join(data2,.)%>%
    add_count(GSS_CODE, name="numbers_in_borough") 
  
  return(output)
}


l2020<- Joinfun(l2020, Londonborough)
l2020<- l2020%>%
  group_by(., GSS_CODE, NAME)%>%
  summarise(`Accomodation count` = unique(numbers_in_borough))
l2019<- Joinfun(l2019, Londonborough)
l2019<- l2019%>%
  group_by(., GSS_CODE, NAME)%>%
  summarise(`Accomodation count` = unique(numbers_in_borough))

tmap_mode("plot")

breaks1 = c(1,500, 1000, 4000,7000,8000) 
tm <- tm_shape(l2020) + 
  tm_polygons("Accomodation count",
              breaks=breaks1, 
              palette="PuBu") + 
  tm_legend(show=FALSE)+
  tm_layout(frame=FALSE)+
  tm_credits("(b)2020", position=c(0,0.85), size=1.5)

tm1 <- tm_shape(l2019) + 
  tm_polygons("Accomodation count",
              breaks=breaks1, 
              palette="PuBu") + 
  tm_legend(show=FALSE)+
  tm_layout(frame=FALSE)+
  tm_credits("(a)2019", position=c(0,0.85), size=1.5)


legend <- tm_shape(l2019) +
  tm_polygons("Accomodation count",
              palette="PuBu") +
  tm_scale_bar(position=c(0.2,0.1), text.size=0.6)+
  tm_compass(north=0, position=c(0.65,0.6))+
  tm_layout(legend.only = TRUE,
            legend.position=c(0.2,0.25),asp=0.1)+
  tm_credits("(c) Airbnb contrbutors ", position=c(0.0,0.0))
```
```{r}
t=tmap_arrange(tm1,tm,legend,ncol=3)
t
```

### analysis:
From the figures, we can see some similarities and differences
1. Similarities: agglomeration in the middle - diffusion around.In general, Airbnb listings are strongest in city centres and spread out.In addition, the spread west of the city center is greater
2. Differences: The color of three Boroughs in 2020 is lighter than previously, namely Tow Hamlets, Hammersmith and Fulham, and Wandsworth. However, the drawback of this graph is that only the interval can be seen, not the travel value. Therefore, we can visualize the spatial distribution by calculating the interpolation of the two data set.

### Change of London Airbnb listing count at borough level


```{r include=FALSE}
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
```


```{r include=FALSE}
Londonboroughs <- st_read("data/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")
mycsv <-  read_csv("data/change.csv")  
Merge <- Londonboroughs%>%
  merge(.,
        mycsv,
        by.x="GSS_CODE", 
        by.y="GSS_CODE")

tmap_mode("plot")
```
```{r message=FALSE, warning=TRUE}

Merge %>%
  qtm(.,fill = "Change of Airbnb listings count")
```

### analysis
  In order to further explore the spatial variation of the number distribution after the epidemic, the Airbnb listings count for December 2019 and October 2020 were calculated according to Boroughs, and the difference of changes was obtained. As the figure shows, the darker the color, the greater the difference. In general, except for the three areas on the edge of London which showed positive changes, all the other areas showed negative changes, that is, the total number of boroughs decreased.To be specific, we can see that Tower Hamlets is the only one with the deepest color, and the number of houses decreased the most, with a decrease of 1591.The City of Westminster is next, with 1,378 listings down.

## Part II——Spatial Cluster Analysis of Different Room Types

1.KDE Kernel Density Estimation: Determine the spatial clusters of different house types
we use Kernel Density Estimation to exam the distribution of two main types of Airbnb listings at ward level. The sigma value sets the diameter of the Kernel and we set the sigma as 500m.

```{r include=FALSE}
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
```
### 2019 entire home/apt  KDE
```{r}
e2019.ppp %>%
  density(., sigma=500) %>%
  plot(main="2019entire home/apt  KDE")
```
### 2020entire home/apt  KDE
```{r}
e2020.ppp %>%
  density(., sigma=500) %>%
  plot(main="2020entire home/apt  KDE")
```

### 2019private room  KDE
```{r include=FALSE}
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
```

```{r}
p2019.ppp %>%
  density(., sigma=500) %>%
  plot(main="2019private room  KDE")
```


```{r include=FALSE}
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
```
### 2020private room  KDE
```{r}
p2020.ppp %>%
  density(., sigma=500) %>%
  plot(main="2020private room  KDE")
```

### analysis
  It can be seen from Figures that the two types of house types have a high density in the city center and the north bank of the Thames River, but the geographical location is different.For Private Room, the highest kernel density is concentrated in the area east of central London, which is about the area of Tower Hamlets. However, entire home/ apt has the highest nuclear density in areas north and west of central London.
  Since the Kernel Density Map (KDM) could not clearly see the change difference of spatial aggregation between 2019 and 2020, we used spatial autocorrelation for further analysis.

### 2.2 Spatial autocorrelation: analysis of spatial agglomeration changespatial autocorrelation

```{r include=FALSE}
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

```

```{r include=FALSE}
library(spdep)
coordsW <- points_sf_joined%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW,axes=TRUE)

LWard_nb <- points_sf_joined %>%
  poly2nb(., queen=T)


plot(LWard_nb, st_geometry(coordsW), col="red")

plot(points_sf_joined$geometry, add=T)
Lward.lw <- LWard_nb %>%
  nb2listw(., style="C")

head(Lward.lw$neighbours)
```
### 2019 entire room/apt  Spatial autocorrelation statistics

```{r}
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

```

```{r include=FALSE}
points_sf_joined <- points_sf_joined %>%
  mutate(density_I =as.numeric(I_LWard_Local_Density$Ii))%>%
  mutate(density_Iz =as.numeric(I_LWard_Local_Density$Z.Ii))

breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
MoranColours<- rev(brewer.pal(8, "RdGy"))

```
```{r include=FALSE}
tm_shape(points_sf_joined) +
  tm_polygons("density_Iz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, 2019 Airbnb entire home/apt in London")
```
```{r include=FALSE}
Gi_LWard_Local_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  localG(., Lward.lw)

head(Gi_LWard_Local_Density)


points_sf_joined <- points_sf_joined %>%
  mutate(density_G = as.numeric(Gi_LWard_Local_Density))

GIColours<- rev(brewer.pal(8, "RdBu"))

```

### Gi*, 2019 Airbnb entire home/apt in London

```{r}
tm_shape(points_sf_joined) +
  tm_polygons("density_G",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, 2019 Airbnb entire home/apt in London")

```

### 2020 entire home/apt spatial autocorrelation tests
```{r include=FALSE}
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
ed2020

coordsW <- points_sf_joined%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW,axes=TRUE)

LWard_nb <- points_sf_joined %>%
  poly2nb(., queen=T)


plot(LWard_nb, st_geometry(coordsW), col="red")

plot(points_sf_joined$geometry, add=T)
Lward.lw <- LWard_nb %>%
  nb2listw(., style="C")

head(Lward.lw$neighbours)
```

### 2020 entire room/apt  Spatial autocorrelation statistics

```{r}

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
```
```{r include=FALSE}
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

```
## Gi*, 2020 Airbnb entire home/apt in London

```{r}
tm_shape(points_sf_joined) +
  tm_polygons("density_G",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, 2020 Airbnb entire home/apt in London")

```
```{r include=FALSE}

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



coordsW <- points_sf_joined%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW,axes=TRUE)

LWard_nb <- points_sf_joined %>%
  poly2nb(., queen=T)

plot(LWard_nb, st_geometry(coordsW), col="red")

plot(points_sf_joined$geometry, add=T)
Lward.lw <- LWard_nb %>%
  nb2listw(., style="C")

head(Lward.lw$neighbours)
```

### 2019 private room  Spatial autocorrelation analysis

```{r}
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
```

```{r include=FALSE}
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

```

### Gi*, 2019 Airbnb private room in London

```{r}
tm_shape(points_sf_joined) +
  tm_polygons("density_G",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, 2019 Airbnb private room in London")


```



```{r icnclude=FALSE}

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



coordsW <- points_sf_joined%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW,axes=TRUE)

LWard_nb <- points_sf_joined %>%
  poly2nb(., queen=T)

plot(LWard_nb, st_geometry(coordsW), col="red")

plot(points_sf_joined$geometry, add=T)
Lward.lw <- LWard_nb %>%
  nb2listw(., style="C")

head(Lward.lw$neighbours)
```

### 2020 private room  Spatial autocorrelation analysis
```{r}
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
```
```{r include=FALSE}
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
              title="Local Moran's I, 2020 Airbnb private room in London")
Gi_LWard_Local_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  localG(., Lward.lw)

head(Gi_LWard_Local_Density)


points_sf_joined <- points_sf_joined %>%
  mutate(density_G = as.numeric(Gi_LWard_Local_Density))

GIColours<- rev(brewer.pal(8, "RdBu"))

```

### Gi*, 2020 Airbnb private room in London

```{r}
tm_shape(points_sf_joined) +
  tm_polygons("density_G",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, 2020 Airbnb private room in London")
```

## Spatial autocorrelation test results analysis
  All data passed the significance level test. This indicates that the distribution of London Airbnb listings has a significant spatial positive correlation characteristic, that is, it shows a spatial agglomeration distribution.
  In addition, Global Moran's index increases year by year, indicating that the spatial agglomeration of Airbnb distribution is further enhanced, and the change of Geary's also confirms this.

## Getis Ord Gi* analysis

  By Getis Ord Gi* We can see the high-value aggregation and low-value aggregation in different periods and compare them. Red represents high-value clusters and blue represents low-value clusters.
  First of all, we can clearly see that there are obvious differences between different types interms of the location of high-value areas and low-value areas. In general, the high value area of Private Room is larger and spreads to some areas south of the Thames River.
  As for Entire home/ apt home type, high value areas are concentrated in Westminster, Camden, Islington, and Hackney. What’ more, the change in the high concentration is very slight, with the range of high values spreading a little bit north along the 2019 boundary.
  As for private rooms mainly concentrated in the north bank of the Thames River, and Tower Hamlets, Hackney, Islington and Camden has a strong spatial correlation, followed by Kensington and Chelsea to the southeast of Hammersmith and Fuiham, and Wandsworth Lambeth Southwark to the north of the south bank. 
  In addition, private rooms mainly concentrated in the north bank of the Thames River, and Tower Hamlets, Hackney, Islington and Camden has a strong spatial correlation, followed by Kensington and Chelsea to the southeast of Hammersmith and Fuiham, and Wandsworth Lambeth Southwark to the north of the south bank. In addition, the range of the low values is going to be larger when we look at the eastern edge, the western edge, and the southeast edge of Greater London.

