# use the make-rt-quic-metadata.r script to "compile" a metadata entry file into a metadata file
Rscript make-rt-quic-metadata.r -e example.metadata-entry.txt -o example.metadata.txt
# check that it is correct
diff example.metadata.txt example.metadata.correct.txt
