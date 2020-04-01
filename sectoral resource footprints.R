##########################################################################
# Calculate Sectoral Footprints
##########################################################################

FP_year <- list()
year = 2015

pathIO <- paste0("./",year,"/")
resultspath <- "./"

load(file=paste0(pathIO,year,"_Lagg_star.RData"))
load(file=paste0(pathIO,year,"_Zagg.RData"))
load(file=paste0(pathIO,year,"_Yagg.RData"))
load(file=paste0(pathIO,year,"_Eagg.RData"))
load(file=paste0(pathIO,year,"_xagg.RData"))

nrreg = 49    # number of regions
nrsec = nrow(Zagg)/nrreg   # number of IO sectors (industries)

##########################################################################
# Calculate Multipliers (total flow concept)
##########################################################################
# prepare extension
Eagg[1,] <- Eagg[470,]  # Energy Carrier Use: Total
Eagg[2,] <- colSums(Eagg[16:21,])  # Employment Hours: Total
Eagg[3:5,] <- Eagg[24:26,]  # GHG Emissions Combustion
Eagg[6:8,] <- Eagg[c(427,428,430),]  # GHG Emissions Agriculture
Eagg[9,] <- Eagg[429,]  # N emissions to water
Eagg[10,] <- colSums(Eagg[c(911:948),])  # Green water consumption
#add materials (except biomass - that is captured by HANPP):
Eagg[11,] <- Eagg[500,] # DE Used - Fossil Fuels
Eagg[12,] <- colSums(Eagg[c(502:513),]) # DE Used - Metal Ores
Eagg[13,] <- colSums(Eagg[c(514:521),]) # DE Used - Non-Metallic Minerals
Eagg <- Eagg[1:13,]#only use these reduced number of env. ext. any more

rownames(Eagg) <- c("Energy Carrier Use: Total", "Employment Hours: Total", "CO2 combustion", "CH4 combustion", "N2O combustion", 
                    "CH4 livestock", "CO2 peat decay", "N2O agriculture", "N emissions to water","Green water consumption",
                    "DE used - Fossil Fuels", "DE used - metal ores", "DE used - non-metallic minerals")

Eagg <- t(t(Eagg) / xagg)
Eagg[!is.finite(Eagg)] <- 0

nree = nrow(Eagg)  # number of extensions

FP_ext <- list()

e="Energy Carrier Use: Total"#change here the env. ext. for the loop
for(e in rownames(Eagg)){
  print(e)
  # Calculate Multiplier Matrix (Ext * LINV)
  MP_star <- (as.vector(Eagg[e,]) * as.matrix(Lagg_star))#to get the direct and indirect flows
  
  # Calculate X-Footprint
  FP <- as.data.frame(t(t(MP_star) * xagg))
  rownames(FP) <- paste0(rep(sprintf("%02d", 1:nrreg), each=nrsec), "_", rep(sprintf("%03d", 1:nrsec), nrreg))
  colnames(FP) <- paste0(rep(sprintf("%02d", 1:nrreg), each=nrsec), "_", rep(sprintf("%03d", 1:nrsec), nrreg))
  FP_ext[[e]] <- as.matrix(FP[])
}
FP_year[[as.character(year)]] <- FP_ext
rm(FP,Lagg_star,MP_star,Yagg,Zagg)
gc()

##########################################################################
# Rearrange and write results
##########################################################################

#chose one extension:
extensions <- c("Energy Carrier Use: Total")#change this for the loop
#"DE used - metal ores", "DE used - non-metallic minerals", "Employment Hours: Total"
#"CO2 combustion", "CH4 combustion", "N2O combustion", "CH4 livestock","CO2 peat decay", "N2O agriculture",
#"N emissions to water", "Green water consumption", "DE used - Fossil Fuels"

# write all results into list and save
for(year in names(FP_year)){
  print(year)
  fname <- paste0("results_",year,".csv")
  try(data.table::fwrite(list("country","sector","from_country","from_sector","year","extension","value"), file = fname, row.names = FALSE, col.names = TRUE, append = FALSE))
  
  for(e in extensions){
    FP_agri <- FP_year[[year]][[e]][,(0:(nrreg-1))*(nrsec)+1]#1st sector=agriculture
    FP_fore <- FP_year[[year]][[e]][,(0:(nrreg-1))*(nrsec)+2]#2nd sector=forestry
    
    f <- melt(FP_agri)
    f$year <- year
    f$extension <- e
    f$sector <- "agriculture"
    f$country <- as.integer(substr(f$Var2, 1, 2))
    f$Var2 <- NULL
    f$from_country <- as.integer(substr(f$Var1, 1, 2))
    f$from_sector <- as.integer(substr(f$Var1, 4, 6))
    f$Var1 <- NULL
    f <- f[,c(5,4,6,7,2,3,1)]
    
    try(data.table::fwrite(f[abs(f$value) > 0, ], file = fname, row.names = FALSE, col.names = FALSE, append = TRUE))
    
    f <- melt(FP_fore)
    f$year <- year
    f$extension <- e
    f$sector <- "forestry"
    f$country <- as.integer(substr(f$Var2, 1, 2))
    f$Var2 <- NULL
    f$from_country <- as.integer(substr(f$Var1, 1, 2))
    f$from_sector <- as.integer(substr(f$Var1, 4, 6))
    f$Var1 <- NULL
    f <- f[,c(5,4,6,7,2,3,1)]
    
    try(data.table::fwrite(f[abs(f$value) > 0, ], file = fname, row.names = FALSE, col.names = FALSE, append = TRUE))
  }
}