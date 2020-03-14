pacman::p_load(datasets,tidyverse,ggdendro, pander, cluster)
dat = state.x77

distance <- dist(as.matrix(dat))
hc <- hclust(distance)
plot(hc)
ggdendro::ggdendrogram(hc, rotate = 1)
#Scaling eliminated the effects or population 
#and area that overpowered the other variables. 
#this allowed the population to have asimilar  
#effect to area. i.e cali and texas being the 
#closest to eachother.

datsc <- as.data.frame(scale(dat))
scdist <- dist(as.matrix(datsc))
hcsc <- hclust(scdist)
ggdendro::ggdendrogram(hcsc, rotate = 1)

#Taking out area really starts to show the 
#important stuff like HS grad and literacy, 
#these
#things were not nearly as important when area
#was involved

namos <- dat[,0]
datAsc <- datsc %>% 
  select(-Area) 
Ascdist <- dist(as.matrix(datAsc))
Ahc <- hclust(Ascdist)
ggdendro::ggdendrogram(Ahc, rotate = 1)

#Frost only is super cool, and since I lived in 
#Texas and Washington can confirm that Frost
#-wise they are extremely similar.

Fdat <- datsc %>% 
  select(Frost)
Fdist <- dist(as.matrix(Fdat))
Fhc <- hclust(Fdist)
ggdendrogram(Fhc, rotate = 1)

#K-means

clus1 <- kmeans(datsc, 3)
clusplot(datsc, clus1$cluster, color = TRUE, shade = TRUE, labels = 2, lines =0)
#large population/large area is the dominating
#factor for the clusters

for (i in 1:25) {
thisclus <- kmeans(datsc, i)
clusplot(datsc, thisclus$cluster, color = TRUE, shade = TRUE, labels = 2, lines =0)
}
#I like 6

clus6 <- kmeans(datsc, 6)
clusplot(datsc, clus6$cluster, color = TRUE, shade = TRUE, labels = 2, lines =0)
pander(clus6$cluster)

#New york texas and cali are clearly grouped 
#due to poulation. Alaska is alone due to area, 
#A lot of the middle sized states are grouoed 
#together in the bottom right, and these ultr 
#small states are all in the bottom left, 