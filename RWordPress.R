# if (!require('RWordPress')) {
#   devtools::install_github(c("duncantl/XMLRPC", "duncantl/RWordPress"))
# }
library(RWordPress)
options(WordpressLogin = c(jefferyip = 'u1(TEQ9mM^I!&P!v2$1GRfNf'),
        WordpressURL = 'http://dataiching.com/xmlrpc.php')

Sys.setlocale(category = "LC_ALL", locale = "cht")

require(knitr) # required for knitting from rmd to md
require(markdown) # required for md to html

knit('./Markdown/HK Population COG.Rmd', './Markdown/HK Population COG.md') # creates md file
knit2wp('./Markdown//HK Population COG.md', encoding = "UTF-8", 
        title = 'Hong Kong Population Center of Gravity (COG)')


# require(knitr) # required for knitting from rmd to md
# require(markdown) # required for md to html
# knit('./Markdown/Web Scraping_ARIMA Modelling.Rmd', 'Web Scraping_ARIMA Modelling.md') # creates md file
# markdownToHTML('Web Scraping_ARIMA Modelling.md', encoding = "UTF-8", 'Web Scraping_ARIMA Modelling.html') # creates html file
# browseURL(paste('file://', file.path(getwd(),'Web Scraping_ARIMA Modelling.html'), sep='')) # open file in browser