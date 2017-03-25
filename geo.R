dato$street_number=0
dato$route=0
dato$neighborhood <- 0
dato$political=0
for (i in 1:length(batch1000$bathrooms)){
  res <- revgeocode(c(batch1000$longitude[i], batch1000$latitude[i]), output="more")
  dato$street_number[i]=levels(res$street_number)
  dato$route[i]=levels(res$route)
  dato$neighborhood[i]=levels(res$neighborhood)
  dato$political[i]=levels(res$political)
}
q=1000
q2=5841
trailbatch <-  dato[q:q2,]
library("ggmap")
batch3 <- dato[]
dato[1001,]
batch4 <- dato[1000:5841,]
#1781
for (i in q:q2){
  res <- revgeocode(c(batch4$longitude[i], batch4$latitude[i]), output="more")
  if(is.null(levels(res$street_number))==TRUE){dato$street_number[i]=NA}
  else {dato$street_number[i]=levels(res$street_number)}
  if(is.null(levels(res$route))==TRUE){dato$route[i]=NA}
  else {dato$route[i]=levels(res$route)}
  if(is.null(levels(res$political))==TRUE){dato$political[i]=NA}
  else {dato$political[i]=levels(res$political)}
  if(is.null(levels(res$neighborhood))==TRUE){dato$neighborhood[i]=NA}
  else {dato$neighborhood[i]=levels(res$neighborhood)}
}