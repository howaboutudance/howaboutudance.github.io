
#! /bin/bash

for arg; do
    Rscript -e 'library(rmarkdown); rmarkdown::render("~/develop/pubstatmeth/assignments/'$arg'", "html_document")'
done
