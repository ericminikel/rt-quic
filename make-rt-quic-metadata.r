metadata_dir = "~/d/j/cureffilab/data/rtq/"

# use letters directly as indices into plate rows
A=1; B=2; C=3; D=4; E=5; F=6; G=7; H=8

# name the wells of the plate
rowname = matrix(rep(c("A","B","C","D","E","F","G","H"),12),nrow=8)
colname = matrix(rep(1:12,each=8),nrow=8)
wellname = matrix(paste(rowname,colname,sep=""),nrow=8)

# create variables
seed       = matrix(nrow=8,ncol=12)
dilution   = matrix(nrow=8,ncol=12)
technician = matrix(nrow=8,ncol=12)
substrate  = matrix(nrow=8,ncol=12)
comments   = matrix(nrow=8,ncol=12)
used       = matrix(nrow=8,ncol=12)

### BEGIN variable region where enter in info about each plate
used[A:D,] = TRUE
used[E:H,1:6] = TRUE
used[E:H,7:12] = FALSE

technician[A:D,1:6] = "sonia"
technician[E:H,1:6] = "eric"
technician[A:D,7:12] = "matteo"

substrate[A:D,] = "SHaPrP90-231"
substrate[E:H,1:6] = "SHaPrP90-231"

seed[,c(1,7,9,11)] = "NBH"
seed[,c(2:6,8,10,12)] = "263K"

comments[A:D,7:8] = "matteo dilutions?"
comments[A:D,9:10] = "sonia dilutions?"
comments[A:D,11:12] = "eric dilutions?"

dilution[,1] = 5e-5
dilution[,2] = 5e-5
dilution[,3] = 5e-6
dilution[,4] = 5e-7
dilution[,5] = 5e-8
dilution[,6] = 5e-9
### END variable region

# transpose the matrices (since platereaders output data sorted by row, then column)
# and then make them into a dataframe
metadata = data.frame(wellname=as.vector(t(wellname)))
metadata$technician = as.vector(t(technician))
metadata$substrate = as.vector(t(substrate))
metadata$seed = as.vector(t(seed))
metadata$dilution = as.vector(t(dilution))
metadata$used = as.vector(t(used))
metadata$comments = as.vector(t(comments))

write.table(metadata[,c("wellname","used","technician","substrate","seed","dilution","comments")],
    "~/d/j/cureffilab/data/rtq/rtq00001.metadata2.txt",
    row.names=FALSE,col.names=TRUE,quote=FALSE,sep="\t")
