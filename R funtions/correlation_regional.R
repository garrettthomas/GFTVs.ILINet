regionalwho <- read.csv("~/Downloads/FluViewPhase2Data Regional/WHO_NREVSS_REGIONAL.csv", header = TRUE)[,3:6]
region1who <- regionalwho[seq(1,8751,by=10),]
region2who <- regionalwho[seq(2,8752,by=10),]
region3who <- regionalwho[seq(3,8753,by=10),]
region4who <- regionalwho[seq(4,8754,by=10),]
region5who <- regionalwho[seq(5,8755,by=10),]
region6who <- regionalwho[seq(6,8756,by=10),]
region7who <- regionalwho[seq(7,8757,by=10),]
region8who <- regionalwho[seq(8,8758,by=10),]
region9who <- regionalwho[seq(9,8759,by=10),]
region10who <- regionalwho[seq(10,8760,by=10),]



region1who <- region1who[,3]*region1who[,4]*0.01
region2who <- region2who[,3]*region2who[,4]*0.01
region3who <- region3who[,3]*region3who[,4]*0.01
region4who <- region4who[,3]*region4who[,4]*0.01
region5who <- region5who[,3]*region5who[,4]*0.01
region6who <- region6who[,3]*region6who[,4]*0.01
region7who <- region7who[,3]*region7who[,4]*0.01
region8who <- region8who[,3]*region8who[,4]*0.01
region9who <- region9who[,3]*region9who[,4]*0.01
region10who <- region10who[,3]*region10who[,4]*0.01

reginalgoogle <- read.csv(file = "~/P-MEDDS/febris/source/data.csv",header=TRUE)[,c(1,54:63)]
region1goog <- reginalgoogle[,2]
region2goog <- reginalgoogle[,3]
region3goog <- reginalgoogle[,4]
region4goog <- reginalgoogle[,5]
region5goog <- reginalgoogle[,6]
region6goog <- reginalgoogle[,7]
region7goog <- reginalgoogle[,8]
region8goog <- reginalgoogle[,9]
region9goog <- reginalgoogle[,10]
region10goog <- reginalgoogle[,11]

regionalILI <- read.csv("~/R-Library/DATA/FluViewPhase2Data Regional/ILINetRegional.csv")[,3:5]
region1ILI <- regionalILI[seq(1,8751,by=10),c(1:3)]
region2ILI <- regionalILI[seq(2,8752,by=10),c(1:3)]
region3ILI <- regionalILI[seq(3,8753,by=10),c(1:3)]
region4ILI <- regionalILI[seq(4,8754,by=10),c(1:3)]
region5ILI <- regionalILI[seq(5,8755,by=10),c(1:3)]
region6ILI <- regionalILI[seq(6,8756,by=10),c(1:3)]
region7ILI <- regionalILI[seq(7,8757,by=10),c(1:3)]
region8ILI <- regionalILI[seq(8,8758,by=10),c(1:3)]
region9ILI <- regionalILI[seq(9,8759,by=10),c(1:3)]
region10ILI <- regionalILI[seq(10,8760,by=10),c(1:3)]
ccc1 <- list("04-05" = NA,"05-06"=NA, "06-07"=NA, "07-08"=NA, "08-09"=NA, "09-10"=NA, "10-11"=NA, "11-12"=NA, "12-13"=NA, "13-14"=NA)
ccd1 <- list("04-05" = NA,"05-06"=NA, "06-07"=NA, "07-08"=NA, "08-09"=NA, "09-10"=NA, "10-11"=NA, "11-12"=NA, "12-13"=NA, "13-14"=NA)
cce1 <- list("04-05" = NA,"05-06"=NA, "06-07"=NA, "07-08"=NA, "08-09"=NA, "09-10"=NA, "10-11"=NA, "11-12"=NA, "12-13"=NA, "13-14"=NA)
sink("regionalCorrelation.txt")
print("REGION 1\n")
for(i in 0:9){
  c <- 355 +52*i
  d <- 407 +52*i
  a = 42+52*i 
  b = 94+52*i  
  ccc1[[i+1]] <- ccf(region1ILI[c:d], region1goog[a:b], plot = FALSE)[0]
  ccd1[[i+1]] <- ccf(region1ILI[c:d], region1who[c:d], plot = FALSE)[0]
  cce1[[i+1]] <- ccf(region1goog[a:b], region1who[c:d], plot = FALSE)[0]
}
print("Region 1 Google and ILINet data correlation")
print(ccc1)
print("Region 1 WHO and ILINet data correlation")
print(ccd1)
print("Region 1 Google and WHO data correlation")
print(cce1)
ccc2 <- list("04-05" = NA,"05-06"=NA, "06-07"=NA, "07-08"=NA, "08-09"=NA, "09-10"=NA, "10-11"=NA, "11-12"=NA, "12-13"=NA, "13-14"=NA)
ccd2 <- list("04-05" = NA,"05-06"=NA, "06-07"=NA, "07-08"=NA, "08-09"=NA, "09-10"=NA, "10-11"=NA, "11-12"=NA, "12-13"=NA, "13-14"=NA)
cce2 <- list("04-05" = NA,"05-06"=NA, "06-07"=NA, "07-08"=NA, "08-09"=NA, "09-10"=NA, "10-11"=NA, "11-12"=NA, "12-13"=NA, "13-14"=NA)
print("REGION 2\n")
for(i in 0:9){
  c <- 355 +52*i
  d <- 407 +52*i
  a = 42+52*i 
  b = 94+52*i  
  ccc2[[i+1]] <- ccf(region2ILI[c:d], region2goog[a:b], plot = FALSE)[0]
  ccd2[[i+1]] <- ccf(region2ILI[c:d], region2who[c:d], plot = FALSE)[0]
  cce2[[i+1]] <- ccf(region2goog[a:b], region2who[c:d], plot = FALSE)[0]
}
print("Region 2 Google and ILINet data correlation")
print(ccc2)
print("Region 2 WHO and ILINet data correlation")
print(ccd2)
print("Region 2 Google and WHO data correlation")
print(cce2)
ccc3 <- list("04-05" = NA,"05-06"=NA, "06-07"=NA, "07-08"=NA, "08-09"=NA, "09-10"=NA, "10-11"=NA, "11-12"=NA, "12-13"=NA, "13-14"=NA)
ccd3 <- list("04-05" = NA,"05-06"=NA, "06-07"=NA, "07-08"=NA, "08-09"=NA, "09-10"=NA, "10-11"=NA, "11-12"=NA, "12-13"=NA, "13-14"=NA)
cce3 <- list("04-05" = NA,"05-06"=NA, "06-07"=NA, "07-08"=NA, "08-09"=NA, "09-10"=NA, "10-11"=NA, "11-12"=NA, "12-13"=NA, "13-14"=NA)
print("REGION 3\n")
for(i in 0:9){
  c <- 355 +52*i
  d <- 407 +52*i
  a = 42+52*i 
  b = 94+52*i  
  ccc3[[i+1]] <- ccf(region3ILI[c:d], region3goog[a:b], plot = FALSE)[0]
  ccd3[[i+1]] <- ccf(region3ILI[c:d], region3who[c:d], plot = FALSE)[0]
  cce3[[i+1]] <- ccf(region3goog[a:b], region3who[c:d], plot = FALSE)[0]
}
print("Region 3 Google and ILINet data correlation")
print(ccc3)
print("Region 3 WHO and ILINet data correlation")
print(ccd3)
print("Region 3 Google and WHO data correlation")
print(cce3)
ccc4 <- list("04-05" = NA,"05-06"=NA, "06-07"=NA, "07-08"=NA, "08-09"=NA, "09-10"=NA, "10-11"=NA, "11-12"=NA, "12-13"=NA, "13-14"=NA)
ccd4 <- list("04-05" = NA,"05-06"=NA, "06-07"=NA, "07-08"=NA, "08-09"=NA, "09-10"=NA, "10-11"=NA, "11-12"=NA, "12-13"=NA, "13-14"=NA)
cce4 <- list("04-05" = NA,"05-06"=NA, "06-07"=NA, "07-08"=NA, "08-09"=NA, "09-10"=NA, "10-11"=NA, "11-12"=NA, "12-13"=NA, "13-14"=NA)
print("REGION 4\n")
for(i in 0:9){
  c <- 355 +52*i
  d <- 407 +52*i
  a = 42+52*i 
  b = 94+52*i  
  ccc4[[i+1]] <- ccf(region4ILI[c:d], region4goog[a:b], plot = FALSE)[0]
  ccd4[[i+1]] <- ccf(region4ILI[c:d], region4who[c:d], plot = FALSE)[0]
  cce4[[i+1]] <- ccf(region4goog[a:b], region4who[c:d], plot = FALSE)[0]
}
print("Region 4 Google and ILINet data correlation")
print(ccc4)
print("Region 4 WHO and ILINet data correlation")
print(ccd4)
print("Region 4 Google and WHO data correlation")
print(cce4)
ccc5 <- list("04-05" = NA,"05-06"=NA, "06-07"=NA, "07-08"=NA, "08-09"=NA, "09-10"=NA, "10-11"=NA, "11-12"=NA, "12-13"=NA, "13-14"=NA)
ccd5 <- list("04-05" = NA,"05-06"=NA, "06-07"=NA, "07-08"=NA, "08-09"=NA, "09-10"=NA, "10-11"=NA, "11-12"=NA, "12-13"=NA, "13-14"=NA)
cce5 <- list("04-05" = NA,"05-06"=NA, "06-07"=NA, "07-08"=NA, "08-09"=NA, "09-10"=NA, "10-11"=NA, "11-12"=NA, "12-13"=NA, "13-14"=NA)
print("REGION 5\n")
for(i in 0:9){
  c <- 355 +52*i
  d <- 407 +52*i
  a = 42+52*i 
  b = 94+52*i  
  ccc5[[i+1]] <- ccf(region5ILI[c:d], region5goog[a:b], plot = FALSE)[0]
  ccd5[[i+1]] <- ccf(region5ILI[c:d], region5who[c:d], plot = FALSE)[0]
  cce5[[i+1]] <- ccf(region5goog[a:b], region5who[c:d], plot = FALSE)[0]
}
print("Region 5 Google and ILINet data correlation")
print(ccc5)
print("Region 5 WHO and ILINet data correlation")
print(ccd5)
print("Region 5 Google and WHO data correlation")
print(cce5)
print("REGION 6\n")
ccc6 <- list("04-05" = NA,"05-06"=NA, "06-07"=NA, "07-08"=NA, "08-09"=NA, "09-10"=NA, "10-11"=NA, "11-12"=NA, "12-13"=NA, "13-14"=NA)
ccd6 <- list("04-05" = NA,"05-06"=NA, "06-07"=NA, "07-08"=NA, "08-09"=NA, "09-10"=NA, "10-11"=NA, "11-12"=NA, "12-13"=NA, "13-14"=NA)
cce6 <- list("04-05" = NA,"05-06"=NA, "06-07"=NA, "07-08"=NA, "08-09"=NA, "09-10"=NA, "10-11"=NA, "11-12"=NA, "12-13"=NA, "13-14"=NA)
for(i in 0:9){
  c <- 355 +52*i
  d <- 407 +52*i
  a = 42+52*i 
  b = 94+52*i  
  ccc6[[i+1]] <- ccf(region6ILI[c:d], region6goog[a:b], plot = FALSE)[0]
  ccd6[[i+1]] <- ccf(region6ILI[c:d], region6who[c:d], plot = FALSE)[0]
  cce6[[i+1]] <- ccf(region6goog[a:b], region6who[c:d], plot = FALSE)[0]
}
print("Region 6 Google and ILINet data correlation")
print(ccc6)
print("Region 6 WHO and ILINet data correlation")
print(ccd6)
print("Region 6 Google and WHO data correlation")
print(cce6)
ccc7 <- list("04-05" = NA,"05-06"=NA, "06-07"=NA, "07-08"=NA, "08-09"=NA, "09-10"=NA, "10-11"=NA, "11-12"=NA, "12-13"=NA, "13-14"=NA)
ccd7 <- list("04-05" = NA,"05-06"=NA, "06-07"=NA, "07-08"=NA, "08-09"=NA, "09-10"=NA, "10-11"=NA, "11-12"=NA, "12-13"=NA, "13-14"=NA)
cce7 <- list("04-05" = NA,"05-06"=NA, "06-07"=NA, "07-08"=NA, "08-09"=NA, "09-10"=NA, "10-11"=NA, "11-12"=NA, "12-13"=NA, "13-14"=NA)
print("REGION 7\n")
for(i in 0:9){
  c <- 355 +52*i
  d <- 407 +52*i
  a = 42+52*i 
  b = 94+52*i  
  ccc7[[i+1]] <- ccf(region7ILI[c:d], region7goog[a:b], plot = FALSE)[0]
  ccd7[[i+1]] <- ccf(region7ILI[c:d], region7who[c:d], plot = FALSE)[0]
  cce7[[i+1]] <- ccf(region7goog[a:b], region7who[c:d], plot = FALSE)[0]
}
print("Region 7 Google and ILINet data correlation")
print(ccc7)
print("Region 7 WHO and ILINet data correlation")
print(ccd7)
print("Region 7 Google and WHO data correlation")
print(cce7)
ccc8 <- list("04-05" = NA,"05-06"=NA, "06-07"=NA, "07-08"=NA, "08-09"=NA, "09-10"=NA, "10-11"=NA, "11-12"=NA, "12-13"=NA, "13-14"=NA)
ccd8 <- list("04-05" = NA,"05-06"=NA, "06-07"=NA, "07-08"=NA, "08-09"=NA, "09-10"=NA, "10-11"=NA, "11-12"=NA, "12-13"=NA, "13-14"=NA)
cce8 <- list("04-05" = NA,"05-06"=NA, "06-07"=NA, "07-08"=NA, "08-09"=NA, "09-10"=NA, "10-11"=NA, "11-12"=NA, "12-13"=NA, "13-14"=NA)
print("REGION 8\n")
for(i in 0:9){
  c <- 355 +52*i
  d <- 407 +52*i
  a = 42+52*i 
  b = 94+52*i  
  ccc8[[i+1]] <- ccf(region8ILI[c:d], region8goog[a:b], plot = FALSE)[0]
  ccd8[[i+1]] <- ccf(region8ILI[c:d], region8who[c:d], plot = FALSE)[0]
  cce8[[i+1]] <- ccf(region8goog[a:b], region8who[c:d], plot = FALSE)[0]
}
print("Region 8 Google and ILINet data correlation")
print(ccc8)
print("Region 8 WHO and ILINet data correlation")
print(ccd8)
print("Region 8 Google and WHO data correlation")
print(cce8)
print("REGION 9\n")
ccc9 <- list("04-05" = NA,"05-06"=NA, "06-07"=NA, "07-08"=NA, "08-09"=NA, "09-10"=NA, "10-11"=NA, "11-12"=NA, "12-13"=NA, "13-14"=NA)
ccd9 <- list("04-05" = NA,"05-06"=NA, "06-07"=NA, "07-08"=NA, "08-09"=NA, "09-10"=NA, "10-11"=NA, "11-12"=NA, "12-13"=NA, "13-14"=NA)
cce9 <- list("04-05" = NA,"05-06"=NA, "06-07"=NA, "07-08"=NA, "08-09"=NA, "09-10"=NA, "10-11"=NA, "11-12"=NA, "12-13"=NA, "13-14"=NA)
for(i in 0:9){
  c <- 355 +52*i
  d <- 407 +52*i
  a = 42+52*i 
  b = 94+52*i  
  ccc9[[i+1]] <- ccf(region9ILI[c:d], region9goog[a:b], plot = FALSE)[0]
  ccd9[[i+1]] <- ccf(region9ILI[c:d], region9who[c:d], plot = FALSE)[0]
  cce9[[i+1]] <- ccf(region9goog[a:b], region9who[c:d], plot = FALSE)[0]
}
print("Region 9 Google and ILINet data correlation")
print(ccc9)
print("Region 9 WHO and ILINet data correlation")
print(ccd9)
print("Region 9 Google and WHO data correlation")
print(cce9)
ccc10 <- list("04-05" = NA,"05-06"=NA, "06-07"=NA, "07-08"=NA, "08-09"=NA, "09-10"=NA, "10-11"=NA, "11-12"=NA, "12-13"=NA, "13-14"=NA)
ccd10 <- list("04-05" = NA,"05-06"=NA, "06-07"=NA, "07-08"=NA, "08-09"=NA, "09-10"=NA, "10-11"=NA, "11-12"=NA, "12-13"=NA, "13-14"=NA)
cce10 <- list("04-05" = NA,"05-06"=NA, "06-07"=NA, "07-08"=NA, "08-09"=NA, "09-10"=NA, "10-11"=NA, "11-12"=NA, "12-13"=NA, "13-14"=NA)
print("REGION 10\n")
for(i in 0:9){
  c <- 355 +52*i
  d <- 407 +52*i
  a = 42+52*i 
  b = 94+52*i  
  ccc10[[i+1]] <- ccf(region10ILI[c:d], region10goog[a:b], plot = FALSE)[0]
  ccd10[[i+1]] <- ccf(region10ILI[c:d], region10who[c:d], plot = FALSE)[0]
  cce10[[i+1]] <- ccf(region10goog[a:b], region10who[c:d], plot = FALSE)[0]
}
print("Region 10 Google and ILINet data correlation")
print(ccc10)
print("Region 10 WHO and ILINet data correlation")
print(ccd10)
print("Region 10 Google and WHO data correlation")
print(cce10)
sink()