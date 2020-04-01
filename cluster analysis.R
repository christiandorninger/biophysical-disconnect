#################################################
#Ward's hierarchical cluster using agnes and indval function
#################################################

data <- scale(data)

#ward cluster wigh agnes function
agnes(data, metric = "euclidean", stand = FALSE, method = "ward")
res.agnes <- agnes(data, method = "ward")

#using indval function to determine cluster coefficients
nk<-5
cutmodel<-cutree(as.hclust(res.agnes),k=nk)
table(cutmodel)
rect.hclust(as.hclust(res.agnes), k = nk, border = 2:5)
temp5<-indval(data,cutmodel,numitr = 1000)
summary(temp5)