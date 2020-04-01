##########################################################################
# Use Footprints of agriculture and forestry to calculate upstream flows
##########################################################################

for(year in c(1995:2015)){

pathIO <- paste0("./",year,"/")
resultspath <- "./"

load(file=paste0(pathIO,year,"_Yagg.RData"))
load(file=paste0(pathIO,year,"_Lagg.RData"))
load(file=paste0(pathIO,year,"_xagg.RData"))


nrreg = 49    # number of regions
nrsec = nrow(Lagg)/nrreg   # number of IO sectors (industries)

###############
# load environmental extensions for agriculture/forestry sectors for 1995-2015
###############
env.ext._footprint = read.csv("year_env.ext._footprint.csv", sep=";", header = FALSE)
env.ext._footprint <- as.numeric(env.ext._footprint$V1)

#env.ext._coefficient
env.ext._coefficient <- (env.ext._footprint/xagg)
env.ext._coefficient[is.na(env.ext._coefficient)] <- 0

env.ext._flows <- diag(env.ext._coefficient) %*% Lagg
env.ext._flows <- env.ext._flows%*%Yagg
write.csv(env.ext._flows, "env.ext._flows.csv")
}