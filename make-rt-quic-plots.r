
options(stringsAsFactors=FALSE)


siteroot = "~/d/j/cureffilab"

suppressPackageStartupMessages(require(optparse)) # http://cran.r-project.org/web/packages/optparse/optparse.pdf
suppressPackageStartupMessages(require(stringr))

option_list = list(
  make_option(c("-d", "--datafile"), action="store", default='', 
              type='character', help="Path to CSV of platereader data"),
  make_option(c("-m", "--metafile"), action="store", default='',
              type='character', help="Path to metadata file"),
  make_option(c("-q", "--rtquicno"), action="store", default='',
              type='character', help="RT-QuIC plate number"),
  make_option(c("-o", "--outdir"), action="store", default=NULL,
              type='character', help="Path to save images"),
  make_option(c("-p", "--plotby"), action="store", default='',
              type='character', help="Variables by which to separate plots"),
  make_option(c("-c", "--curveby"), action="store", default=NULL,
              type='character', help="Variables by which to separate curves"),
  make_option(c("-k", "--colorby"), action="store", default='',
              type='character', help="Variable by which to vary darkness of color"),
  make_option(c("-n", "--normalize"), action="store_true", default=FALSE,
              help="Normalize fluorescence data on a 0 to 1 scale [default %default]"),
  make_option(c("-l", "--location"), action="store", default='topleft',
              type='character', help="Location of legend on plot")
)
opt = parse_args(OptionParser(option_list=option_list))

# uncomment this for debugging in interactive mode:
# opt = data.frame(rtquicno="rtq00001",
#                  outdir="~/d/sci/src/rt-quic/",
#                  plotby="compound",
#                  colorby="dilution",
#                  normalize=TRUE,
#                  location="topleft")


if (opt$rtquicno=="") {
  metafile = opt$metafile
  pngbase = opt$metafile
  datafile = opt$datafile
} else {
  pngbase = opt$rtquicno
  metafile = paste(siteroot,"/data/rtq/",opt$rtquicno,".metadata.txt",sep="")
  datafile = paste(siteroot,"/data/rtq/",opt$rtquicno,".csv",sep="")
}
# use the current month's media directory if not otherwise specified
if (is.null(opt$outdir)) {
  curr_year  = format(Sys.Date(),"%Y")
  curr_month = format(Sys.Date(),"%m")
  outdir = paste(siteroot,"/media/",curr_year,"/",curr_month,"/",sep="")
} else {
  outdir = opt$outdir
}
colorby = opt$colorby
plotby = split(opt$plotby,",")[[1]]
if (is.null(opt$curveby)) {
  input_curveby = NULL
} else {
  input_curveby = strsplit(opt$curveby,",")[[1]]
}
if (opt$normalize) {
  normalize = TRUE
} else {
  normalize = FALSE
}

# unfortunately the input data have a variable number of header lines
# so first, figure out how many lines to skip in the header of the CSV
lines_to_skip = grep("Well Row,Well Col",readLines(datafile))-1
# then read in the data and metadata
data = read.table(datafile,skip=lines_to_skip,header=TRUE,sep=',')
metadata=read.table(metafile,sep="\t",header=TRUE)

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
if (normalize) {
  mat = (mat - min(mat)) / (max(mat) - min(mat)) 
  ylims = c(0,1)
} else {
  ylims = range(mat)
}

# get the timepoints for the x axis
timepts = extract_timepoints(colnames(mat))
timepts_h = timepts/60

# figure out grayscale levels for serial dilutions
dil_log10 = -log10(metadata[,colorby])
dil_range = range(dil_log10,na.rm=TRUE)
desired_range_gray = c(0,255*.8) # dilution colors will range from #000 to #CCC
desired_range_cex = c(.3,1)
graylevel = round((dil_log10 - min(dil_range))/(max(dil_range) - min(dil_range)) * (max(desired_range_gray) - min(desired_range_gray)))
cexlevel = (dil_log10 - min(dil_range))/(max(dil_range) - min(dil_range))* (max(desired_range_cex) - min(desired_range_cex)) + min(desired_range_cex)

# columns for which separate curves should never be plotted
nevercurveby = c("wellname","used")

if (length(plotby) > 1) {
  plotbyval = do.call(paste,metadata[,plotby])
} else {
  plotbyval = metadata[,plotby]
}
for (current_plotbyval in unique(plotbyval[metadata$used])) {
  legend = data.frame(name=character(),color=character())
  pngname = gsub("[/ ]","-",paste(pngbase,"-",current_plotbyval,".png",sep=""))
  list_of_attributes = paste(plotby,": ",unique(metadata[metadata$used & plotbyval==current_plotbyval,plotby]),collapse="\n",sep="")
  main = paste(pngbase,"\n",list_of_attributes,sep="")
  png(paste(outdir,pngname,sep=""),width=600,height=450)
  plot(NA,NA,xlim=range(timepts_h),ylim=ylims,
       xlab='Timepoint', ylab='Relative ThT fluorescence units',
       main=main)
  # within the data to be plotted, any metadata cols which are polymorphic must be 
  # separate curves, unless otherwise specified by user
  if (is.null(input_curveby)) {
    curveby = c() 
    for (colno in 1:dim(metadata)[2]) {
      n_unique_vals = length(unique(metadata[metadata$used & plotbyval == current_plotbyval,colno]))
      if (n_unique_vals > 1 & !(colnames(metadata)[colno] %in% nevercurveby)) {
        curveby = c(curveby, colnames(metadata)[colno])
      }
    }
  } else {
    curveby = input_curveby
  }
  # just create a "curvename" vector for all rows of the metadata table, even though only some being plotted
  if (length(curveby) > 1) {
    curvename = do.call(paste,metadata[,curveby])
  } else {
    curvename = metadata[,curveby]
  }
  #curvenames = unique(do.call(paste,metadata[metadata$used & metadata[,plotby] == plotbyval,curveby]))#[metadata$used & metadata[,plotby]==plotbyval])
  for (current_curvename in unique(curvename[metadata$used & plotbyval == current_plotbyval])) {
    # figure out which rows to average for this curve
    userows = metadata$used & plotbyval==current_plotbyval & curvename==current_curvename
    # calculate the y values by averaging those rows
    yvals = colMeans(mat[userows,])
    # figure out what color to plot
    current_seed = unique(metadata$seed[userows])
    # figure out what dilution this is
    current_graylevel = unique(graylevel[userows])
    if (current_seed=="NBH") {
      curve_hexcolor = "#00CC00" # green
#      rgb_multiplier = c(0,1,0) # green scale
    } else {
      rgb_multiplier = c(1,1,1) # gray scale
      curve_rgb = round(rgb_multiplier * current_graylevel)
      curve_rgb[is.na(curve_rgb)] = 0 # fix in case there are NA
      curve_hexcolor = paste("#",paste(sprintf("%02x",curve_rgb),collapse=""),sep="")
    }
    points(timepts_h,yvals,type='l',lwd=3,col=curve_hexcolor)
#    text(timepts[length(timepts)],curvedata[length(curvedata)],label=curve,col=color,pos=4,cex=.8)
    legend = rbind(legend,cbind(current_curvename,curve_hexcolor))
  }
  legend(opt$location,legend[,1],col=legend[,2],lwd=2)
  dev.off()
}



