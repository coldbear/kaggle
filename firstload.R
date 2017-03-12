packages <- c("jsonlite", "dplyr", "purrr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

dato <- fromJSON("/Users/coldbear/Desktop/sigma_rent_kaggle/train.json")
  # unlist every variable except `photos` and `features` and convert to tibble
vars <- setdiff(names(dato), c("photos", "features"))
dato <- map_at(dato, vars, unlist) %>% tibble::as_tibble(.)

#Log1-AK-data cleaning
dato$bathrooms <- as.numeric((dato$bathrooms))
dato$bedrooms <- as.numeric((dato$bedrooms))
#"building_id"
dato$building_id[dato$building_id == "0"] <- "absent"
#creating an additional variable 'build_freq' reflecting the frequency of the listing of a certain building.
library(dplyr)
factor(dato$building_id)
#y=count(dato, 'building_id')
dato$build_freq <- NULL
dato$build_freq <- ave(as.numeric(dato$building_id), dato$building_id, FUN=length) 
#to check
dato[dato$building_id=="0021440c04241281a436ec21accc40b1",]
#correct, though I guess we want to replace the frequency of the absent id with NA
#"created"
dato$created <- as.POSIXct(dato$created)
#now lets pull the time, month and day of the week
dato$posthours <- NULL
dato$posthours <- strftime(dato$created, format="%H:%M:%S")
#shall we slpit it into 4-6 timeslots in the day?
library(lubridate)
dato$postweek <- NULL
dato$postweek <- week(as.Date(dato$created))
dato$postweek <- as.factor(dato$postweek)
dato$postday <- NULL  
dato$postday <- weekdays(as.Date(dato$created))
dato$postday <- as.factor(dato$postday)
dato$interest_level <- as.factor(dato$interest_level)
dato$postmonth <- NULL
dato$postmonth <- months(dato$created)
newtime <- dato$posthours 
newtime=as.POSIXct()
#"display address" - actually I think the key here is how much of the address is given, shall we count the words? or the difference between the displayed one and actual one?
#'neighbourhoods' - reverse geocoding
library("ggmap")
dato$latitude <- as.numeric(dato$latitude)
dato$longitude <- as.numeric(dato$longitude)
res <- mapply(FUN = function (longitude, latitude) { 
revgeocode(c(firstbatch$longitude, firstbatch$latitude), output = "more") return(res$neighborhood)
})
batch1000 <- dato[1:1000,]
res <- revgeocode(c(firstbatch$longitude[3], firstbatch$latitude[3]), output="more")
res$neighborhood
#williamsburg, midtown - fuanction needed
#other vales
#dato$description[dato$description == "  "] <-"NA"
#ILYAS - Log 1
#Process features
dato$features=unname(dato$features)#get rid of lists
i=0
x=0
for (i in 1:length(dato$features)){
  x=c(x,unlist(c(dato$features[i],"next")))
}
j=1
feat=matrix(data=0,nrow=49352,ncol=1556)
featname=vector(mode="character",length=1556)
k=0
maxk=0
for (i in 1:length(x)){
  if (x[i]=="next"){j=j+1}
  else {
    flag=FALSE
    if (featname[1]!=""){
      for (k in 1:maxk){if (featname[k]==x[i]){
        feat[j,k]=1
        flag=TRUE
      }}}
    if (flag==FALSE){
      maxk=maxk+1
      featname[maxk]=x[i]
      feat[j,maxk]=1
    }
  }
}
x=NULL
for (i in 1:300){
 print(featname[i])
print(sum(feat[,i]))}

#Manager ID
dato$manager_id=factor(dato$manager_id)
dato$manager_grade=0
dato$manager_rank="E"
for (i in 1:length(dato$manager_id)){
  dato$manager_grade[i]=sum(dato$manager_id==dato$manager_id[i])
  if (dato$manager_grade[i]>1000){dato$manager_rank[i]="A"}
  else if (dato$manager_grade[i]>120){dato$manager_rank[i]="B"}
  else if (dato$manager_grade[i]>46){dato$manager_rank[i]="C"}
  else if (dato$manager_grade[i]>17){dato$manager_rank[i]="D"}
}

#Image recognition
source("http://bioconductor.org/biocLite.R")
biocLite()
biocLite("EBImage")
library("EBImage")
Image <- readImage('C:\\Users\\ilias\\Desktop\\kaggle\\images_sample\\6811984\\6811984_07aab16f822c3c6e5cdc07801388b101.jpg')
coor=seq(30,70,1)
ch=sum((Image[coor,,1]>0.96) & (Image[coor,,2]>0.96) & (Image[coor,,3]>0.96))

# Is there an image?
dato$image_missing=FALSE
for (i in 1:length(dato$bathrooms)){if (length(unlist(dato$photos[i]))==0){dato$image_missing[i]=TRUE}}

#Trying first functions
dato1 <- dato[1:10000,]
dato1$description <- NULL
dato1$features <- NULL
dato1$photos <- NULL
regr <- glm(interest_level~., dato1, family = quasibinomial(link="logit"))

#Sentiment anaylsis
library(syuzhet)
library(DT)
sentiment <- get_nrc_sentiment(dato$description)

datatable(head(sentiment))
dato<-merge(dato,sentiment, by.x="ID", by.y="ID", all.x=T, all.y=T)
dato1 <- dato[1:90,]
dato1$description <- NULL
dato1$features <- NULL
dato1$photos <- NULL
dato1$manager_id <- NULL
dato1$latitude <- NULL
dato1$longitude <- NULL
dato1$building_id <- NULL
dato1$created <- NULL
dato1$manager_rank <-as.factor(dato1$manager_rank)
dato1$neighborhood <-as.factor(dato1$neighborhood)

regr1 <- glm(interest_level~bathrooms+bedrooms+price+build_freq+postweek+postday+manager_grade+manager_rank+neighborhood, dato1, family = quasibinomial(link="logit"))
summary(regr1)
regr2 <- glm(interest_level~bathrooms+bedrooms+price+build_freq+postweek+postday+manager_grade+manager_rank+neighborhood+anger+fear+joy+anticipation+disgust+sadness+surprise+trust+negative+positive, dato1, family = quasibinomial(link="logit"))
summary(regr2)
sentiment$ID=0
dato$ID=0
for (i in 1:length(dato$bathrooms)){
  sentiment$ID[i]=i
  dato$ID[i]=i
}