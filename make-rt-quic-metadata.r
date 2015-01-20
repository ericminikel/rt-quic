
# two ways to use this script
# 1. specify an input metadata-entry file and an output CSV file
#    Rscript make-rt-quic-metadata.r -e example.metadata-entry.txt -o example.metadata.csv
# 2. specify an RT-QuiC plate number, and it will find the entry and output paths automatically
#    Rscript make-rt-quic-metadata.r -q rtq00001

siteroot = "~/d/j/cureffilab"

suppressPackageStartupMessages(require(optparse)) # http://cran.r-project.org/web/packages/optparse/optparse.pdf
suppressPackageStartupMessages(require(stringr))
options(stringsAsFactors=FALSE) 

option_list = list(
  make_option(c("-e", "--entry"), action="store", default='', 
              type='character', help="Path to metadata entry file"),
  make_option(c("-o", "--output"), action="store", default='',
              type='character', help="Path to create metadata CSV file"),
  make_option(c("-q", "--rtquicno"), action="store", default='',
              type='character', help="RT-QuIC plate number"),
  make_option(c("-s", "--size"), action="store", default=96,
              type="integer", help="Plate size")
)
opt = parse_args(OptionParser(option_list=option_list))

if (opt$rtquicno=="") {
    entryfile = opt$entry
    outfile   = opt$output
} else {
    entryfile = paste(siteroot,"/data/rtq/",opt$rtquicno,".metadata-entry.txt",sep="")
    outfile   = paste(siteroot,"/data/rtq/",opt$rtquicno,".metadata.txt",sep="")
}

# function to execute a command stored as text
exec = function(command) {
    return (eval(parse(text=command)))
}

metadata_dir = "~/d/j/cureffilab/data/rtq/"

if (opt$size == 96) {
  nrows = 8
  ncols = 12
} else if (opt$size == 384) {
  nrows = 16
  ncols = 24
}

# use letters directly as indices into plate rows
A=1; B=2; C=3; D=4; E=5; F=6; G=7; H=8; I=9; J=10; K=11; L=12; M=13; N=14; O=15; P=16;

alphabet = c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P")


# name the wells of the plate
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
    assign(metadata_var,matrix(nrow=nrows,ncol=ncols))
}
# now execute the slice assignments in the script
source(entryfile)


# transpose the matrices (since platereaders output data sorted by row, then column)
# and make them into a dataframe
metadata = data.frame(wellname = as.vector(t(wellname)))
for (metadata_var in metadata_vars) {
    metadata[,metadata_var] = as.vector(t(exec(metadata_var)))
}

write.table(metadata,outfile,row.names=FALSE,col.names=TRUE,quote=FALSE,sep="\t")
