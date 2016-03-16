
require(tiff)

my.path<- "K:/error luc 2014-06-05/wells verwijderd vanwege error/H10_1"
all.image<- dir(my.path)
my.list =list()
all.image[25]
for(i in seq_along(all.image)) 
{
  my.list[[i]] <- 
    readTIFF( paste(my.path, all.image[i], sep="/"), native = FALSE, all = FALSE, convert = FALSE,
              info = FALSE, indexed = FALSE, as.is = TRUE)
}


lapply(my.list, function (x) {any(is.null(x))})
2
warnings()
2^16
65325
?readTIFF 
image(my.list[[1]])
