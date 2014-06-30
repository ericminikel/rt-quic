options(stringsAsFactors=FALSE)
require(stringr)

data = read.table("~/d/j/cureffilab/data/rtq/rtq00001.csv",skip=5,header=TRUE,sep=',')
metadata=read.table("~/d/j/cureffilab/data/rtq/rtq00001.metadata.txt",sep="\t",header=TRUE)

dynrange = c(0,260000)

# extract timepoints in minutes from column names
extract_timepoints = function(column_names) {
  # first, take the part after the triple period
  time_string = gsub(".*\\.\\.\\.","",column_names)
  # if perl regex lookaheads were easier to work with in R, this would be correct:
  # h_string = str_extract(time_string,"[0-9]+(?=\\.h)")
  # instead, have to do it in two steps - an extraction and a blank replacement
  h_string = str_extract(time_string,"[0-9]+\\.h")
  h = as.integer(gsub("\\.h","",h_string))
  # do the same for minutes
  m_string = str_extract(time_string,"[0-9]+\\.min")
  m = as.integer(gsub("\\.min","",m_string))
  # hours times sixty
  timepts = h*60
  # plus minutes, only where minutes is non-NA
  timepts[!is.na(m)] = timepts[!is.na(m)] + m[!is.na(m)]
  return (timepts)
}

get_dilution_color = function(dilution) {
  log_dilution = -log10(as.numeric(dilution))
  color_add_value = round(log_dilution*10)
  hexcolor = paste("#",paste(rep(as.character(as.hexmode(color_add_value)),3),collapse=""),sep="")
  return (hexcolor)
}

mat = as.matrix(data[4:(dim(data)[2]-1)])
timepts = extract_timepoints(colnames(mat))


technician = "eric"

toplot = metadata$technician==technician & metadata$used
sum(toplot)
metadata$curve = paste(metadata$seed[toplot],metadata$dilution[toplot],sep=" ")
curves = unique(metadata$curve)
plot(NA,NA,xlim=range(timepts)*1.2,ylim=dynrange,
     xlab='Timepoint', ylab='ThT fluorescence units',
     main=paste('rtq00001',technician,sep="-"))
for (curve in curves) {
  userows = toplot & metadata$curve==curve
  seed = unique(metadata$seed[userows]) # there should be only one
  if (seed=="NBH") {
    color = 'green'
  } else {
    color = get_dilution_color(unique(metadata$dilution[userows]))
  }
  curvedata = colMeans(mat[toplot & metadata$curve==curve,])
  points(timepts,curvedata,type='l',lwd=3,col=color)
  text(timepts[length(timepts)],curvedata[length(curvedata)],label=curve,col=color,pos=4,cex=.8)
}


