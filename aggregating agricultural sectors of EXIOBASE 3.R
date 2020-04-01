#aggregation of agricultural sectors in the matrices of EXIOBASE 3
for(year in 1995:2015) {
  print(year)
  Y <- read.delim(paste0(rawpath, year, "/Y.txt"),header = FALSE,skip = 3,stringsAsFactors = FALSE)
  Y <- Y[,c(-1,-2)]
  Y <- as.matrix(Y)
  save(Y,file = paste0(rawpath, year, "/", year, "_Y.RData"))
  
  A <- read.delim(paste0(rawpath, year, "/A.txt"),header = FALSE,skip = 3,stringsAsFactors = FALSE)
  A <- A[,c(-1,-2)]
  A <- as.matrix(A)
  save(A,file = paste0(rawpath, year, "/", year, "_A.RData"))
  
  L <- solve(diag(nrow(A))-A)
  save(L,file = paste0(rawpath, year, "/", year,"_L.RData"))
  
  x <- rowSums(L%*%Y)
  save(x,file = paste0(rawpath, year, "/", year,"_x.RData"))
  
  Z <- t(t(A)*x)
  save(Z,file = paste0(rawpath, year, "/", year,"_Z.RData"))
  
  #aggregate Z
  Zagg <- Z
  colnames(Zagg) <- paste0(rep(1:49, each=163), "_", rep(c(rep(1, 17), 2:147), 49))#sectors get IDs: 1-17=1 for agriculture, the rest is 2-147, the remaining sectors
  Zagg <- t(agg(Zagg))
  colnames(Zagg) <- paste0(rep(1:49, each=163), "_", rep(c(rep(1, 17), 2:147), 49))
  Zagg <- t(agg(Zagg))
  save(Zagg,file = paste0(rawpath, year, "/", year,"_Zagg.RData"))
  
  #aggregate Y
  Yagg <- t(Y)#transpose Y-matrix to aggregate sectors
  colnames(Yagg) <- paste0(rep(1:49, each=163), "_", rep(c(rep(1, 17), 2:147), 49))
  Yagg <- t(agg(Yagg))#re-transpose and sectors 1 to 17 (ID=1) are aggregated
  save(Yagg,file = paste0(rawpath, year, "/", year, "_Yagg.RData"))
  
  xagg <- rowSums(Zagg) + rowSums(Yagg)
  save(xagg,file = paste0(rawpath, year, "/", year,"_xagg.RData"))
  
  Aagg <- t(t(Zagg)/xagg)
  Aagg[!is.finite(Aagg)] <- 0
  save(Aagg,file = paste0(rawpath, year, "/", year,"_Aagg.RData"))
  Lagg <- solve(diag(nrow(Aagg))-Aagg)
  save(Lagg,file = paste0(rawpath, year, "/", year,"_Lagg.RData"))
  
  #dividing L by its own main diagonal to get output multiplier (total flow concept)
  Lagg_star <- t(t(Lagg) / diag(Lagg))
  save(Lagg_star,file = paste0(rawpath, year, "/", year,"_Lagg_star.RData"))
  
  E <- read.delim(paste0(rawpath, year, "/F.txt"),header = FALSE,skip = 2,stringsAsFactors = FALSE)
  E <- E[,c(-1)]
  E <- as.matrix(E)
  save(E,file = paste0(rawpath, year, "/", year,"_E.RData"))
  Eagg <- E
  colnames(Eagg) <- paste0(rep(1:49, each=163), "_", rep(c(rep(1, 17), 2:147), 49))
  Eagg <- agg(Eagg)
  save(Eagg,file = paste0(rawpath, year, "/", year,"_Eagg.RData"))
  
  E_hh <- read.delim(paste0(rawpath, year, "/F_hh.txt"),header = FALSE,skip = 2,stringsAsFactors = FALSE)
  E_hh <- E_hh[,c(-1)]  
  E_hh <- as.matrix(E_hh)
  save(E_hh,file = paste0(rawpath, year, "/", year,"_E_hh.RData"))
}