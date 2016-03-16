# gather genes from significant pathways:

ox.stress: "NRF2_mediated_Oxidative_Stress_Response"
inflammatory : "Toll_like_Receptor_Signaling" | 
                             "Death_Receptor_Signaling"  
                             "TNFR2_Signaling" 
                             "NF_kB_Signaling" 
                             "TNFR1_Signaling"

rm(list=ls())
options(stringsAsFactors = FALSE)
options(encoding = "UTF-8")

library(xlsx)
library(stringr)

rootPath <- "H:\\R_HOME\\R_WORK\\mArray BAC reporter studie\\results\\TING pathway analysis\\DEG voor TING\\DEG high 8h BA\\IPA"
myFolders <- dir(rootPath)
myPaths <- paste(rootPath, myFolders, sep = "\\")
myFile <- dir(myPaths)
pathToFiles <- paste(myPaths, myFile, sep = "\\")

allCompounds = character()
rawList = list( )
myList = list( )
myListInflam = list()
nameList = list()
for ( i in 1 : length( myFolders ) )
  { 
   rawList[[ i ]]<- read.xlsx(  pathToFiles[ i ], 1 , startRow = 2, encoding="UTF-8")  
      if(nrow((rawList[[ i ]])) != 0 ){
          myList[[ i ]] <- rawList[[ i ]][ , c( "Ingenuity.Canonical.Pathways" , "Molecules") ]
          myList[[ i ]] <- myList[[ i ]][ myList[[ i ]]$Ingenuity.Canonical.Pathways == "NRF2-mediated Oxidative Stress Response",  ]
          genes.tmp.o <- unlist(strsplit(myList[[ i ]]$Molecules, split = ","))
          if( is.null(genes.tmp.o) ){
            genes.tmp.o <- NA
          }
          myList[[i]] <- genes.tmp.o
          names(myList)[i] <- myFolders[i]
          
          myListInflam[[ i ]] <- rawList[[ i ]][ , c( "Ingenuity.Canonical.Pathways" , "Molecules") ]
          myListInflam[[ i ]] <- myListInflam[[ i ]][ myListInflam[[ i ]]$Ingenuity.Canonical.Pathways %in% 
                                                        c("Toll-like Receptor Signaling",
                                                          "Death Receptor Signaling",
                                                          "TNFR2 Signaling",
                                                          "TNFR1 Signaling",
                                                          "NF-κB Signaling"),  ]
          genes.tmp<- unlist(strsplit(myListInflam[[ i ]]$Molecules, split = ","))
            if(is.null(genes.tmp)) {
              genes.tmp <- NA
              }
          myListInflam[[ i ]] <- genes.tmp
          names(myListInflam)[i] <- myFolders[i]
          
          
          #(myList[[ i ]][[1]][2]) zo
          } else  {
              rawList[[ i ]] <- list(Ingenuity.Canonical.Pathways = NA, Molecules =NA)
              myList[[ i ]] <- rawList[[ i ]]
              names(myList)[i] <- myFolders[i]
              myListInflam[[ i ]] <-  rawList[[ i ]]
              names(myListInflam)[i] <- myFolders[i]
          }
}



# average fold change of these genes and number of genes per compound

# first make unique

myListInflam<- lapply(myListInflam, function(x)  unique(x)  )

numDF.inflam <- data.frame(compound = names(myListInflam))
numDF.inflam$geneNumber <- unlist(lapply(myListInflam, length))

numDF.oxi <- data.frame(compound = names(myList))
numDF.oxi$geneNumber <- unlist(lapply(myList, length))

setwd("C:/Users/winks/Documents/own papers/NFKB Nrf2 Bram/data")
write.table(numDF.inflam, file = "number genes in pathway inflammatory8.txt", sep = "\t", row.names=F)
write.table(numDF.oxi, file = "number genes in pathway oxidative stress8.txt", sep = "\t", row.names=F)

#function that can write list including names to file


#all entries as char vectors
myList<-lapply(myList, function(x) x <- as.character(x)  )
myListInflam<-lapply(myListInflam, function(x) x <- as.character(x)  )

fnlist <- function(x, fil){ z <- deparse(substitute(x))
                            cat(z, "\n", file=fil)
                            nams=names(x) 
                            for (i in seq_along(x) ){ cat(nams[i], "\t",  x[[i]], "\n", 
                                                          file=fil, append=TRUE) }
}

fnlist(myListInflam, "infl.stress genes 8h high.txt")
tmp<- myList
save( tmp, file = "ox.stress.list8.Rdata")
rm(tmp)
load("ox.stress.list8.Rdata")

tmp
tmp<- myListInflam
save( tmp, file = "inflamm.list8.Rdata")
load("inflamm.list8.Rdata")

# now make a file with all the genes, and look up FC values with "extract_gene_set_exprs_sept 2013.Rmd"

oxi.g <-unlist(myList)
names(oxi.g )<-NULL
oxi.g  <- unique(oxi.g )
inflami.g <- unlist(myListInflam)
names(inflami.g) <- NULL
inflami.g <- unique(inflami.g)
all.g<- unique(c(oxi.g, inflami.g))

# map to ENTREZ id's

#source("http://bioconductor.org/biocLite.R")
#biocLite("org.Hs.eg.db")

require(org.Hs.eg.db)

x <- org.Hs.egSYMBOL2EG
# Get the entrez gene identifiers that are mapped to a gene symbol
mapped_genes <- mappedkeys(x)
# Convert to a list
head(mapped_genes)
xx <- as.list(x[mapped_genes])
head(all.g)
xx[all.g]
output <- unlist(xx[all.g])
write.table(output, file = "symbols and entrez genes from inflam and oxi from IPA pathways8.txt"   ,sep = "\t", col.names = NA)
getwd()












