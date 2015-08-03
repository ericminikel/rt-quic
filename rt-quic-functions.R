options(stringsAsFactors=FALSE)
require(stringr) # to do: refactor to remove this dependency
require(RColorBrewer)

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

kinetic_matrix = function(raw_dataframe, normalize=TRUE) {
  # convert numerical portion of the data to a matrix
  mat = as.matrix(raw_dataframe[4:(dim(raw_dataframe)[2]-1)])
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
  colnames(mat) = timepts_h
  return (mat)
}

alpha = function(rgb_hexcolor, proportion) {
  hex_proportion = sprintf("%02x",round(proportion*255))
  rgba = paste(rgb_hexcolor,toupper(hex_proportion),sep='')
  return (rgba)
}

percent = function(proportion,digits=2) {
  return ( gsub(' ','',paste(formatC(proportion*100, digits=digits, format='fg'),"%",sep="") ) )
}

find_t_up = function(mat, threshold=.5, max_out = TRUE, ignore=0) {
  timepts = as.numeric(colnames(mat))
  min_time_index = min(which(timepts > ignore)) # e.g. if ignore = 2, take index of 1st timepoint after 2h
  max_time = max(timepts)
  t_up = numeric(dim(mat)[1])
  for (i in 1:dim(mat)[1]) {
    if (max(mat[i,min_time_index:dim(mat)[2]]) <= threshold) {
      if (max_out) {
        t_up[i] = max_time
      } else {
        t_up[i] = Inf
      }
    } else {
      t_up[i] = timepts[min(which(mat[i,min_time_index:dim(mat)[2]] >= threshold)) + min_time_index - 1]
    }
  }
  return (t_up)
}

# converts numeric units (e.g. 5e-6 M) to human-readable molarities (e.g. 5 &mu;M)
human_readable_unit = function(x, u, digits=1, encoding='ascii', sep='') {
  if (encoding == 'ascii') {
    prefixes = c('p','n','u','m','','k','M','G','T','P','E')
  } else if (encoding == 'utf8') {
    prefixes = c('p','n','\u2265','m','','k','M','G','T','P','E')
  } else if (encoding == 'html') {
    prefixes = c('p','n','&mu;','m','','k','M','G','T','P','E')
  }
  echelons = c(-12,-9,-6,-3,0,3,6,9,12,15,18) / 3
  echelon = floor(log10(x) / 3)
  prefix = prefixes[echelons==echelon]
  output = paste(formatC(x / (10^(3*echelon)), format='fg', digits=digits), sep, prefix, u, sep='')
  return (output)
}

# function to execute a command stored as text
exec = function(command, envir) {
  return (eval(parse(text=command), envir=envir))
}

# load metadata from an R script
read_metadata = function(entryfile, size=96) {
  nrows = sqrt(size / 1.5)
  ncols = nrows * 1.5
  
  # name the wells of the plate
  A=1; B=2; C=3; D=4; E=5; F=6; G=7; H=8; I=9; J=10; K=11; L=12; M=13; N=14; O=15; P=16;
  alphabet = c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P")
  rowname = matrix(rep(alphabet[1:nrows],ncols),nrow=nrows)
  colname = matrix(rep(1:ncols,each=nrows),nrow=nrows)
  wellname = matrix(paste(rowname,colname,sep=""),nrow=nrows)
  
  # read commands from a file to create metadata
  commands = read.table(entryfile,comment.char="#",flush=TRUE)
  colnames(commands) = c("var","equals","value")
  # get a list of all metadata variables the user wants
  metadata_vars = unique(gsub("\\[","",str_extract(commands$var,".*\\[")))
  # create a matrix for each metadata variable
  for (metadata_var in metadata_vars) {
    assign(metadata_var, matrix(nrow=nrows,ncol=ncols), envir = environment())
  }
  # now execute the slice assignments in the script
  source(entryfile, local=environment())
  
  # transpose the matrices (since platereaders output data sorted by row, then column)
  # and make them into a dataframe
  metadata = data.frame(wellname = as.vector(t(wellname)))
  for (metadata_var in metadata_vars) {
    metadata[,metadata_var] = as.vector(t(exec(metadata_var, envir=environment())))
  }
  
  return (metadata)
}

read_rtquic_matrix = function(path, normalize=TRUE) {
  lines_to_skip = grep("Well Row,Well Col",readLines(path))-1 # figure out where the data actually starts
  data = read.table(path,skip=lines_to_skip,header=TRUE,sep=',') # read it in
  mat = kinetic_matrix(data, normalize)
  return (mat)
}

# to_plot: a logical vector corresponding to the rows of metadata and mat
kinetic_base_plot = function(xlim=c(0,24), ylim=c(0,1), axes=TRUE, labels=TRUE) {
  x_interval = (max(xlim) - min(xlim)) / 4
  y_interval = (max(ylim) - min(ylim)) / 4
  par(mar=c(4,4,4,4))
  plot(NA, NA, xlim=xlim, ylim=ylim, axes=FALSE, xlab='', ylab='', xaxs='i', yaxs='i')
  abline(h=0,lwd=2)
  if (axes) {
    axis(side=1, at=(0:4)*x_interval, lwd=0, lwd.ticks=1, cex=.8)
    axis(side=2, at=(0:4)*y_interval, labels=percent((0:4)/4), lwd=0, lwd.ticks=1, las=2)
  }
  if (labels) {
    mtext(side=1, line=3, text='Time (h)', font=2, cex=.8)
    mtext(side=2, line=3, text='Relative ThT fluorescence', font=2, cex=.8)
  }
}

rtquic_legend = function(metadata, mat, to_plot, color_by) {
  legend = data.frame(unique(metadata[to_plot,color_by]))
  colnames(legend)[1:length(color_by)] = color_by
  legend$color = rainbow(n=dim(legend)[1])
  return (legend)
}

# useful for stepping through functions while testing
wait_for_user = function() {
  cat ("Press enter to continue\n")
  line = readline()
}

rtquic_kinetic_curves = function(metadata, mat, to_plot, curve_by, legend_df, type='l', lwd=5, ...) {
  if (length(curve_by)==1) { # if just one column, use that
    curve_ids = as.character(metadata[,curve_by])
  } else { # otherwise paste them together
    curve_ids = apply(metadata[,curve_by], 1, FUN="paste", collapse='_')
  }
  for (uid in unique(curve_ids[to_plot])) {
    to_plot_this_curve = to_plot & curve_ids == uid
    if (sum(to_plot_this_curve) > 1) { # if >1 replicate take average of replicates
      ydata = colMeans(mat[to_plot_this_curve,]) 
    } else { # if just 1 replicate, show that
      ydata = as.numeric(mat[to_plot_this_curve,]) 
    }
    xdata = as.numeric(colnames(mat)) # timepoints
    curve_color = unique(merge(metadata[to_plot_this_curve,], legend_df)$color)[1]
    points(x=xdata, y=ydata, col=curve_color, type=type, lwd=5, ...)
    # points(x=xdata, y=ydata, col=curve_color, type=type, lwd=5 )
  } 
}
