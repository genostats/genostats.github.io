#!/bin/bash

R -e 'rmarkdown::render_site()'

# enlever quelques <p> ... </p> nuisibles insérés par pandoc
sed -i 's/^<p><input\(.*\)<\/p>$/<input\1/' *.html 

