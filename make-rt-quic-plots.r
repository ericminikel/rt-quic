
options(stringsAsFactors=FALSE)


siteroot = "~/d/j/cureffilab"

suppressPackageStartupMessages(require(optparse)) # http://cran.r-project.org/web/packages/optparse/optparse.pdf
suppressPackageStartupMessages(require(stringr))
options(stringsAsFactors=FALSE) 

option_list = list(
  make_option(c("-d", "--datafile"), action="store", default='', 
              type='character', help="Path to CSV of platereader data"),
  make_option(c("-m", "--metafile"), action="store", default='',
              type='character', help="Path to metadata file"),
  make_option(c("-q", "--rtquicno"), action="store", default='',
              type='character', help="RT-QuIC plate number"),
  make_option(c("-o", "--outdir"), action="store", default='',
              type='character', help="Path to save images"),
  make_option(c("-p", "--plotby"), action="store", nargs='?', default='',
              type='character', help="Variables by which to separate plots")
)
opt = parse_args(OptionParser(option_list=option_list))

# uncomment this line for debugging in interactive mode:
opt = data.frame(rtquicno="rtq00001",outdir="~/d/sci/src/rt-quic/",plotby="technician")

if (opt$rtquicno=="") {
  metafile = opt$metafile
  pngbase = opt$metafile
  datafile = opt$datafile
  outdir = opt$outdir
} else {
  pngbase = opt$rtquicno
  metafile = paste(siteroot,"/data/rtq/",opt$rtquicno,".metadata.txt",sep="")
  datafile = paste(siteroot,"/data/rtq/",opt$rtquicno,".csv",sep="")
  if (is.na(opt$outdir)) {
    curr_year  = format(Sys.Date(),"%Y")
    curr_month = format(Sys.Date(),"%m")
    outdir = paste(siteroot,"/media/",curr_year,"/",curr_month,"/",sep="")
  }
}
data = read.table(datafile,skip=5,header=TRUE,sep=',')
metadata=read.table(metafile,sep="\t",header=TRUE)

# data = read.table("~/d/j/cureffilab/data/rtq/rtq00001.csv",skip=5,header=TRUE,sep=',')
# metadata=read.table("~/d/j/cureffilab/data/rtq/rtq00001.metadata.txt",sep="\t",header=TRUE)


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

# convert numerical portion of the data to a matrix
mat = as.matrix(data[4:(dim(data)[2]-1)])
# normalize the matrix
mat = (mat - min(mat)) / (max(mat) - min(mat))

# get the timepoints for the x axis
timepts = extract_timepoints(colnames(mat))
timepts_h = timepts/60

# figure out grayscale levels for serial dilutions
metadata$dil_log10 = -log10(metadata$dilution)
dil_range = range(metadata$dil_log10,na.rm=TRUE)
desired_range = c(0,255*.8) # dilution colors will range from #000 to #CCC
metadata$graylevel = round((metadata$dil_log10 - min(dil_range))/max(dil_range) * (max(desired_range) - min(desired_range)))

plotby = "technician"
curveby = c("seed","dilution","comments")

plotbyval="eric"
metadata$curve = do.call(paste,metadata[,curveby]) 
for (plotbyval in unique(metadata[metadata$used,plotby])) {
  legend = data.frame(name=character(),color=character())
  pngname = paste(pngbase,"-",plotbyval,".png",sep="")
  png(paste(outdir,pngname,sep=""),width=600,height=450)
  plot(NA,NA,xlim=range(timepts_h),ylim=c(0,1),
       xlab='Timepoint', ylab='Relative ThT fluorescence units',
       main=paste('rtq00001',plotbyval,sep="-"))
  curvenames = unique(metadata$curve[metadata$used & metadata[,plotby]==plotbyval])
  for (curvename in curvenames) {
    # figure out which rows to average for this curve
    userows = metadata$used & metadata[,plotby]==plotbyval & metadata$curve==curvename
    # calculate the y values by averaging those rows
    yvals = colMeans(mat[userows,])
    # figure out what color to plot
    current_seed = unique(metadata$seed[userows])
    # figure out what dilution this is
    dilution = unique(metadata$dilution[userows])
    seed = unique(metadata$seed[userows])
    graylevel = unique(metadata$graylevel[userows])
    if (seed=="NBH") {
      curve_hexcolor = "#00CC00" # green
#      rgb_multiplier = c(0,1,0) # green scale
    } else {
      rgb_multiplier = c(1,1,1) # gray scale
      curve_rgb = round(rgb_multiplier * graylevel)
      curve_rgb[is.na(curve_rgb)] = 0 # fix in case there are NA
      curve_hexcolor = paste("#",paste(sprintf("%02x",curve_rgb),collapse=""),sep="")
    }
    points(timepts_h,yvals,type='l',lwd=3,col=curve_hexcolor)
#    text(timepts[length(timepts)],curvedata[length(curvedata)],label=curve,col=color,pos=4,cex=.8)
    legend = rbind(legend,cbind(curvename,curve_hexcolor))
  }
  legend('bottomright',legend[,1],col=legend[,2],lwd=2)
  dev.off()
}



