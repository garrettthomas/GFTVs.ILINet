national = read.csv(file = "~/P-MEDDS/febris/source/data.csv", header = TRUE)[, 1:53]
sum = rowSums(national[2:563, 2:53], na.rm = TRUE)
nationalwho = read.csv("~/Downloads/FluViewPhase2DataNational/WHO_NREVSS_national.csv")[, 3:6]
nationalcdc <- read.csv("~/Downloads/FluViewPhase2DataNational/ILINet_national.csv")[, 3:5]
ccc <- list(`04-05` = NA, `05-06` = NA, `06-07` = NA, `07-08` = NA, `08-09` = NA, `09-10` = NA, `10-11` = NA, `11-12` = NA, `12-13` = NA, `13-14` = NA)
ccd <- list(`04-05` = NA, `05-06` = NA, `06-07` = NA, `07-08` = NA, `08-09` = NA, `09-10` = NA, `10-11` = NA, `11-12` = NA, `12-13` = NA, `13-14` = NA)
cce <- list(`04-05` = NA, `05-06` = NA, `06-07` = NA, `07-08` = NA, `08-09` = NA, `09-10` = NA, `10-11` = NA, `11-12` = NA, `12-13` = NA, `13-14` = NA)
nationalWHO <- nationalwho[, 3] * nationalwho[, 4] * 0.01
for (i in 0:9) {
    c <- 355 + 52 * i
    d <- 407 + 52 * i
    a = 42 + 52 * i
    b = 94 + 52 * i
    ccc[[i + 1]] <- ccf(nationalcdc[c:d, 3], nationalWHO[c:d], plot = FALSE)[0]
    ccd[[i + 1]] <- ccf(nationalcdc[c:d, 3], sum[a:b], plot = FALSE)[0]
    cce[[i + 1]] <- ccf(nationalWHO[c:d], sum[a:b], plot = FALSE)[0]
}
sink("corr.txt")
print("ILINet and WHO/NREVSS national data correlation by year")
print(ccc)
print("ILINet and Google national data correlation by year")
print(ccd)
print("WHO/NREVSS and Google national data correlation by year")
print(cce)
sink() 
