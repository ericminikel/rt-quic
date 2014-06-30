# plot plate layouts

siteroot = "~/d/j/cureffilab"

suppressPackageStartupMessages(require(optparse)) # http://cran.r-project.org/web/packages/optparse/optparse.pdf
suppressPackageStartupMessages(require(stringr))
options(stringsAsFactors=FALSE) 

option_list = list(
  make_option(c("-m", "--metafile"), action="store", default='', 
              type='character', help="Path to metadata file"),
  make_option(c("-p", "--pngfile"), action="store", default='',
              type='character', help="Path to PNG file"),
  make_option(c("-q", "--rtquicno"), action="store", default='',
              type='character', help="RT-QuIC plate number")
)
opt = parse_args(OptionParser(option_list=option_list))

if (opt$rtquicno=="") {
  metafile  = opt$metafile
  pngfile   = opt$pngfile
  plottitle = basename(opt$metafile)
} else {
  metafile  = paste(siteroot,"/data/rtq/",opt$rtquicno,".metadata.txt",sep="")
  pngfile   = paste(siteroot,"/data/rtq/",opt$rtquicno,".layout.png",sep="")
  plottitle = opt$rtquicno
}

metadata = read.table(metafile,sep='\t',header=TRUE)

# colors
softline = "#DDDDDD"

# function to add text to a well
textwell = function (wellname, text, ...) {
  row = substr(wellname,1,1)
  col = as.integer(substr(wellname,2,nchar(wellname)))
  xval = col - .5
  yval = 8 - .5 - (as.integer(charToRaw(row))-as.integer(charToRaw("A")))
  text(xval,yval,labels=text,...)
}

# write to a file
png(pngfile,width=600,height=600)

# plot the basic plate layout
par(mar=c(10,2,3,1))
plot(NA,NA,xlim=c(0,12),ylim=c(0,8),xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',
     main=plottitle)
abline(h=1:8,col=softline)
abline(v=1:12,col=softline)
axis(side=1,at=1:12-.5,labels=1:12,lwd.ticks=0,lwd=0,cex.axis=.8)
axis(side=2,at=8:1-.5,labels=c("A","B","C","D","E","F","G","H"),lwd.ticks=0,cex.axis=.8,las=2)

# X out the empty wells
for (tablerow in 1:dim(metadata)[1]) {
  if (!metadata$used[tablerow]) {
    textwell(metadata$wellname[tablerow],"X",col=softline)
  }
}

# write the particulars
bottom_offset = 4
within_offset = -.5
variable_cols = c()
for (metaval in colnames(metadata)[c(-1,-2)]) {
  nvals = length(unique(metadata[metadata$used,metaval]))
  # for things that are invariant, print at the bottom and move on
  if (nvals == 1) {
    string_to_print = paste(metaval,": ",unique(metadata[metadata$used,metaval]),sep="")
    mtext(side=1,adj=0,padj=bottom_offset,text=string_to_print)
    bottom_offset = bottom_offset + 1.5
  }
  if (nvals > 1) {
    for (tablerow in which(metadata$used)) {
      textwell(metadata$wellname[tablerow],metadata[tablerow,metaval],cex=.5,pos=1,offset=within_offset)
    }
    within_offset = within_offset + .5
    variable_cols = c(variable_cols,metaval) # later i want to handle these more intelligently
  }
}

dev.off()
