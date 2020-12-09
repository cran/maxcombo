#devtools::document(roclets = c('rd', 'collate', 'vignette','namespace') ) #R command to update the NAMESPACE file in case there is no NAMESPACE file yet
#see https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html about why there is a line saying "Roxygen: list(markdown = TRUE)" in the DESCRIPTION file.  It is totally optional, but using it can allow for nicer looking documentation of the functions in the package.
#' @importFrom magrittr %>% %<>% %$% %T>%
#' @import rlang
# Please note that all of the packages mentioned in the above @import and @importFrom statements need to be manually added to this package's DESCRIPTION file
NULL
