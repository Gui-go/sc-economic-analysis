library(rpart)
cart <- rpart(sum_hex_vl_fob_cat2~., data=as_Spatial(sul_hex))
printcp(cart)
plotcp(cart)
plot(cart, uniform=TRUE, main="Regression Tree")
# text(cart, use.n=TRUE, all=TRUE, cex=.8)
text(cart, cex=.8, digits=1)

rp <- predict(as.numeric(sul_hex$sum_hex_vl_fob_cat2), as.numeric(sul_hex$meanSum_hex_vl_fob), ext=sul_hex)
plot(rp)

library(latticeExtra)
install.packages("latticeExtra")
## Loading required package: lattice
## Loading required package: RColorBrewer
grps <- 10
brks <- quantile(h$houseValue, 0:(grps-1)/(grps-1), na.rm=TRUE)
p <- spplot(h, "houseValue", at=brks, col.regions=rev(brewer.pal(grps, "RdBu")), col="transparent" )
p + layer(sp.polygons(hh))

sul_hexs <- as_Spatial(sul_hex)
summary(lm(median_hex_vl_fob ~ meanSum_hex_vl_fob, data = sul_hexs))


ds <- density(sul_hexs$sum_hex_vl_fob)
class(ds)
## [1] "im"
plot(ds, main='crime density')
nrow(pts)
r <- raster(ds)
s <- sum(values(r), na.rm=TRUE)
s * prod(res(r))
## [1] 2640.556
s <- Smooth.ppp(pp)
## Warning: Cross-validation criterion was minimised at right-hand end of
## interval [89.7, 3350]; use arguments hmin, hmax to specify a wider interval
plot(s)
plot(city, add=TRUE)