
options(stringsAsFactors=FALSE)


siteroot = "~/d/j/cureffilab"

suppressPackageStartupMessages(require(optparse)) # http://cran.r-project.org/web/packages/optparse/optparse.pdf
suppressPackageStartupMessages(require(stringr))

option_list = list(
  make_option(c("-d", "--datafile"), action="store", default='', 
              type='character', help="Path to CSV of platereader data"),
  make_option(c("-m", "--metafile"), action="store", default='',
              type='character', help="Path to metadata file"),
  make_option(c("-t", "--maintitle"), action="store", default='',
              type='character', help="Main title of all plots [defaults generated automatically]"),
  make_option(c("-q", "--rtquicno"), action="store", default='',
              type='character', help="RT-QuIC plate number"),
  make_option(c("-o", "--outdir"), action="store", default=NULL,
              type='character', help="Path to save images"),
  make_option(c("-p", "--plotby"), action="store", default='',
              type='character', help="Variables by which to separate plots"),
  make_option(c("-c", "--curveby"), action="store", default=NULL,
              type='character', help="Variables by which to separate curves"),
  make_option(c("-f", "--fadeby"), action="store", default='',
              type='character', help="Variable by which to vary darkness of color"),
  make_option(c("-n", "--normalize"), action="store_true", default=FALSE,
              help="Normalize fluorescence data on a 0 to 1 scale [default %default]"),
  make_option(c("-l", "--location"), action="store", default='topleft',
              type='character', help="Location of legend on plot"),
  make_option(c("-k", "--colorby"), action="store", default="",
              type='character', help="Variable by which to vary base color"),
  make_option(c("-j", "--colormap"), action="store", default="#000000",
              type='character', help="Mapping scheme for colors")
)
opt = parse_args(OptionParser(option_list=option_list))
# 
# # uncomment this for debugging in interactive mode:
# opt = data.frame(rtquicno="rtq00019",
#                  outdir="~/d/sci/src/rt-quic/",
#                  plotby="seed,compound,dilution,solvent,solvent_conc",
#                  fadeby="compound_molarity",
#                  colorby="compound",
#                  colormap="none:#000000,IND24:#FF1100,cpd-b:#AA7700,anle138b:#55CC00",
#                  maintitle="",
#                  normalize=TRUE,
#                  location="bottomright")


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
fadeby = opt$fadeby
plotby = strsplit(opt$plotby,",")[[1]]
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

# figure out color scheme
# this is implemented by creating a function that accepts a set of rows and returns a color for them
# if no colorby variable, then all one color.
if (colorby == "") {
  getcolor = function(userows) {
    return (opt$colormap)
  }
} else { # if a colorby variable, then parse the color mapping
  # get set of unique values of the colorby variable. ideally these should all have a mapping.
  # later can add checks to make sure this is the case.
  colorbyvals = unique(metadata[metadata$used,colorby])
  # original string should say for instance "NBH:#00CC00,RML:#000000"
  colortuples = strsplit(opt$colormap,',')[[1]] # split on ,
  colormap = list() # a dictionary to hold mappings
  for (colortuple in colortuples) {
    # tuples now say for instance "NBH:#00CC00"
    mapping = strsplit(colortuple,":")[[1]] # split on :
    colorbyval = mapping[1] # e.g. "NBH"
    color = mapping[2] # e.g. "#00CC00"
    colormap[[colorbyval]] = color
  }
  getcolor = function(userows) {
    colorbyval = unique(metadata[userows,colorby])
    current_color = colormap[[colorbyval]]
    if (is.null(current_color)) {
      warning(paste("Using default color of #000000 for",colorbyval))
      current_color = '#000000'
    }
    return (current_color)
  }
}


# convert back and forth between rgb integers and hex color strings
hex_to_rgb = function(hexcolor) {
  red = strtoi(substr(hexcolor,2,3),base=16)
  green = strtoi(substr(hexcolor,4,5),base=16)
  blue = strtoi(substr(hexcolor,6,7),base=16)
  return (c(red,green,blue))  
}
rgb_to_hex = function(rgbvector) {
  hexcolor = toupper(paste("#",paste(sprintf("%02x",rgbvector),collapse=""),sep=""))
  return (hexcolor)
}

# fade a color by a proportion from 0 to 1. 0 is no change, 1 results in white.
fadecolor = function(hexcolor, fadeamount) {
  white = c(255,255,255)
  rgbvector = hex_to_rgb(hexcolor)
  newrgb = floor(rgbvector + fadeamount*(white - rgbvector))
  newhex = rgb_to_hex(newrgb)
  return (newhex)
}

# figure out grayscale levels for serial dilutions
fadebyvals = metadata[,fadeby]
# if the data contain 0 or negatives, assign these a -log10 value 20% higher than the highest finite value.
well_behaved_range = range(fadebyvals[fadebyvals > 0])
well_behaved_logrange = -log10(well_behaved_range)
dil_log10 = -log10(fadebyvals)
dil_log10[fadebyvals <= 0] = max(well_behaved_logrange) + .2*(max(well_behaved_logrange) - min(well_behaved_logrange))
dil_range = range(dil_log10,na.rm=TRUE)
desired_range_gray = c(0,.8) # never fade more than 80%, for legibility
graylevel = (dil_log10 - min(dil_range))/(max(dil_range) - min(dil_range)) * (max(desired_range_gray) - min(desired_range_gray))
# fill NA with zero - for instance, an unseeded well might have dilution = NA, just plot this with no fade
graylevel[is.na(graylevel)] = 0


desired_range_cex = c(.3,1)
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
  if (opt$maintitle=="") {
        main = paste(pngbase,"\n",list_of_attributes,sep="") 
  } else {
        main = gsub("\\n","\n",opt$maintitle,fixed=TRUE) # for some reason newlines come through still escaped. fix that here.
  }

  png(paste(outdir,pngname,sep=""),width=600,height=450)
  plot(NA,NA,xlim=range(timepts_h),ylim=ylims,
       xlab='Hours', ylab='Relative ThT fluorescence units',
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
    yvals = colMeans(matrix(mat[userows,],nrow=sum(userows),ncol=ncol(mat)))
    # figure out what base color this curve ought to be
    curve_basecolor = getcolor(userows)
    # figure out what fade level (usually dilution) this is
    current_graylevel = unique(graylevel[userows])
    # calculate the color for this curve
    curve_hexcolor = fadecolor(curve_basecolor,current_graylevel)
    points(timepts_h,yvals,type='l',lwd=3,col=curve_hexcolor)
#    text(timepts[length(timepts)],curvedata[length(curvedata)],label=curve,col=color,pos=4,cex=.8)
    legend = rbind(legend,cbind(current_curvename,curve_hexcolor))
  }
  colorvec = sapply(colormap,"[[",1)
  legend(opt$location,names(colorvec),col=colorvec,lwd=2)#,title=as.character(curveby))
  dev.off()
}

