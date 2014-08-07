# use the make-rt-quic-metadata.r script to "compile" a metadata entry file into a metadata file
Rscript make-rt-quic-metadata.r -e example.metadata-entry.txt -o example.metadata.txt
# check that it is correct
diff example.metadata.txt example.metadata.correct.txt
# make a graphic of the platefile
Rscript make-rt-quic-plate-layout.r -m example.metadata.txt -p example.png
# make plots
Rscript make-rt-quic-plots.r -p technician -f dilution -q rtq00001