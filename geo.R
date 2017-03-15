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
q=161
q2=1000
trailbatch <-  dato[q:q2,]
library("ggmap")
batch3 <- dato[841:5841,]
for (i in q:q2){
  res <- revgeocode(c(batch3$longitude[i], batch3$latitude[i]), output="more")
  if(is.null(levels(res$street_number))==TRUE){dato$street_number[i]=NA}
  else {dato$street_number[i]=levels(res$street_number)}
  if(is.null(levels(res$route))==TRUE){dato$route[i]=NA}
  else {dato$route[i]=levels(res$route)}
  if(is.null(levels(res$political))==TRUE){dato$political[i]=NA}
  else {dato$political[i]=levels(res$political)}
  if(is.null(levels(res$neighborhood))==TRUE){dato$neighborhood[i]=NA}
  else {dato$neighborhood[i]=levels(res$neighborhood)}
}