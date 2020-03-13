library(datasets)
dat = state.x77

distance <- dist(as.matrix(dat))
hc <- hclust(distance)
plot(hc)
ggdendro::ggdendrogram(hc, rotate = 1)
#Scaling eliminated the effects or population and area that overpowered the other variables. this allowed the population to have asimilar  effect to area. i.e cali and texas being the closest to eachother.

datsc <- scale(dat)
scdist <- dist(as.matrix(datsc))
hcsc <- hclust(scdist)
ggdendro::ggdendrogram(hcsc, rotate = 1)


namos <- dat[,0]
datAsc <- as_tibble(datsc) %>% 
  select(-Area) 
Ascdist <- dist(as.matrix(datAsc))
datAsc <- ggdendro::as.dendro(Ascdist, namos, class = "hclust")
ggdendro::ggdendrogram(datAsc)

