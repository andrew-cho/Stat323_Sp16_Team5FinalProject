all: election_vis.html

election_vis.html: election_vis.Rmd df.Rdata
  Rscript -e "library(rmarkdown);render('election_vis.Rmd')"

df.Rdata: scrape.R
  Rscript scrape.R

clean:
  rm -f election_vis.html

.PHONY: clean
