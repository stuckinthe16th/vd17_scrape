

cat("\nUploading results to Wordpress...")

##Install RWordpress
if (!require('RWordPress')) {
     devtools::install_github(c("duncantl/XMLRPC", "duncantl/RWordPress"))
}
library(RWordPress)
if (!require('knitr')) {
     devtools::install_github('yihui/knitr', build_vignettes = TRUE)
}
library(knitr)


#Set login parameters
options(WordpressLogin = c(russg = 'skoal90'), WordpressURL = 'http://earlymodernprinting.com/xmlrpc.php')

#Post new entry to the wordpress blog
library(knitr)
knit2wp('scrape_test_results.Rmd', title = 'Most Recent Test Scrape of the VD17', action="editPost", postid=357, publish=T)

