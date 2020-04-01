###########################################
# allocating HANPP data
###########################################

#IO parser
for(year in 1995:2015) {
  Y <- read.delim(paste0(rawpath, year, "/Y.txt"),header = TRUE, stringsAsFactors = FALSE)
  Y <- Y[c(-1,-2),c(-1,-2)]
  Y <- sapply(Y, as.numeric)
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
  
  F_mat <- read.delim(paste0(rawpath, year, "/F.txt"),header = FALSE,skip = 2,stringsAsFactors = FALSE)
  F_mat <- F_mat[447:464,]
  
  F_hh <- read.delim(paste0(rawpath, year, "/F_hh.txt"),header = FALSE,skip = 2,stringsAsFactors = FALSE)
  F_hh <- F_hh[c(447:464,466),]
  
  F_mat <- t(F_mat[,-1])
  F_hh <- t(F_hh[,-1])
  
  #creating an aggregation function
  agg <- function(x){
    x <- as.matrix(x) %*% sapply(unique(colnames(x)), "==", colnames(x))
    return(x)}
  
  colnames(F_mat) <- c(rep("cropland",13),"forest","other",rep("pasture",3))
  colnames(F_hh) <- c(rep("cropland",13),"forest","other",rep("pasture",3),"forest")
  
  F_mat <- as.data.frame(agg(F_mat))
  F_hh <- as.data.frame(agg(F_hh))
  
  # reallocate other land in F_mat to forest and grassland
  temp1 <- F_mat$other / (F_mat$forest + F_mat$pasture) * F_mat$forest
  temp1[!is.finite(temp1)] <- 0
  temp2 <- F_mat$other / (F_mat$forest + F_mat$pasture) * F_mat$pasture
  temp2[!is.finite(temp2)] <- 0
  
  F_mat$forest <- F_mat$forest + temp1
  F_mat$pasture <- F_mat$pasture + temp2
  F_mat <- F_mat[,c(1,4,2)]
  
  # derive country totals
  load("IO.codes.RData")
  conc <- read.csv("regions.csv")
  F_mat$ISO <- conc$ISO3[match(IO.codes$Country.Code,conc$ISO2)]
  
  F_hh <- t(F_hh)
  colnames(F_hh) <- rep(unique(F_mat$ISO), each=7)
  F_hh <- as.data.frame(t(agg(F_hh)))
  
  F_mat_total <- t(F_mat[,-4]) 
  colnames(F_mat_total) <- F_mat$ISO
  F_mat_total <- as.data.frame(t(agg(F_mat_total)))
  
  F_total <- F_mat_total
  F_total$cropland <- F_total$cropland + F_hh$cropland[match(rownames(F_total),rownames(F_hh))]
  F_total$pasture <- F_total$pasture + F_hh$pasture[match(rownames(F_total),rownames(F_hh))]
  F_total$forest <- F_total$forest + F_hh$forest[match(rownames(F_total),rownames(F_hh))]
  
  # reallocate other land in F_hh to forest and grassland (weighted with coefficients from file)
  coefficients <- read.csv("hanpp_ffh_coefficient.csv")
  coefficients <- coefficients[coefficients$year==year,] %>% 
    gather(ISO, value, -year, -land) %>% 
    select(-year) %>% 
    spread(land, value)
  
  coefficients <- coefficients[match(rownames(F_hh),coefficients$ISO),]
  
  temp1 <- F_hh$other / (F_total$forest + F_total$pasture) * F_total$forest * coefficients$woodland
  temp1[!is.finite(temp1)] <- 0
  temp2 <- F_hh$other / (F_total$forest + F_total$pasture) * F_total$pasture * coefficients$grassland
  temp2[!is.finite(temp2)] <- 0
  
  F_hh$forest <- F_hh$forest + temp1
  F_hh$pasture <- F_hh$pasture + temp2
  F_hh <- F_hh[,c(1,4,2)]
  
  F_total <- F_mat_total + F_hh
  
  #load HANPP data  
  HANPP <- read.csv("hanpp.csv", header = TRUE, sep = ";")
  h <- HANPP[HANPP$year==year,] %>% 
    gather(ISO, value, -year, -HANPP) %>% 
    select(-year) %>% 
    spread(HANPP, value) %>% 
    select(-`built-up`)
  
  h <- h[match(rownames(F_hh),h$ISO),]
  
  shares_mat <- F_mat_total / F_total
  shares_mat[is.na(shares_mat)] <- 0
  h_mat_total <- h
  h_mat_total[,-1] <- h[,-1] * shares_mat
  
  shares_hh <- F_hh / F_total
  shares_hh[is.na(shares_hh)] <- 0
  h_hh <- h[,-1] * shares_hh
  
  h_mat <- F_mat
  reg="AUT"
  for(reg in unique(h_mat$ISO)){
    h_mat$cropland[h_mat$ISO==reg] <- h_mat$cropland[h_mat$ISO==reg] / sum(h_mat$cropland[h_mat$ISO==reg]) * h_mat_total$cropland[h_mat_total$ISO==reg]
    h_mat$pasture[h_mat$ISO==reg] <- h_mat$pasture[h_mat$ISO==reg] / sum(h_mat$pasture[h_mat$ISO==reg]) * h_mat_total$grassland[h_mat_total$ISO==reg]
    h_mat$forest[h_mat$ISO==reg] <- h_mat$forest[h_mat$ISO==reg] / sum(h_mat$forest[h_mat$ISO==reg]) * h_mat_total$woodland[h_mat_total$ISO==reg]
  }
  
  h_mat[is.na(h_mat)] <- 0
  
  #transpose and sum up HANPP values of each sector
  total_HANPP <- h_mat$cropland + h_mat$forest + h_mat$pasture#sums HANPP columns (cropland, forestland, grassland) to one total HANPP value
  total_HANPP <- t(total_HANPP)
  total_HANPP <- as.numeric(total_HANPP)
  
  #HANPP-coefficient
  HANPP_coef <- (total_HANPP/x)
  HANPP_coef[is.na(HANPP_coef)] <-0
  
  #diagonalize and save
  HANPP_flows <- HANPP_coef * L
  HANPP_footprint <- HANPP_flows %*% Y
  write.csv(HANPP_footprint, file = paste0(rawpath, year, "/", year,"_HANPP_footprint_resub2.csv"))
  
  results <- data.frame(country = substr(colnames(Y),1,2), value = colSums(HANPP_footprint)) %>% 
    group_by(country) %>% 
    summarise(value = sum(value))
}