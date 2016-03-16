# dose response script for DILI data


library(data.table)
library(ggplot2)


source("D:/R_HOME/R_WORK/R scipts/theme_sharp.R")
rm(list=ls())
setwd("D:/DILI screen/meta analyse DILI screen/Summary data files")
dir()


raw.data =list()
dir.files <- dir()
for (i in seq_along(dir.files)){
  raw.data[[i]] <- read.table(file = dir.files[i], header = TRUE, sep ="\t")
}
head(raw.data[[1]])
# first 3 no timeAfterExposure
# 2013-07-10 1.2  0.8
# 2013-07-24 1.076 tr  40min del
# 2013-07-29  59 min  20 min

timeBetweenFrames <- 1.2
exposureDelay <- 0.8
raw.data[[1]]$timeAfterExposure<- as.integer(raw.data[[1]]$timeID) * timeBetweenFrames + exposureDelay - timeBetweenFrames
head(raw.data[[1]])

timeBetweenFrames <- 1.076
exposureDelay <- 0.667
raw.data[[2]]$timeAfterExposure<- as.integer(raw.data[[2]]$timeID) * timeBetweenFrames + exposureDelay - timeBetweenFrames
head(raw.data[[2]])

timeBetweenFrames <- 0.983
exposureDelay <- 0.333
raw.data[[3]]$timeAfterExposure<- as.integer(raw.data[[3]]$timeID) * timeBetweenFrames + exposureDelay - timeBetweenFrames
head(raw.data[[3]])
raw.data[[26]]$doseLevel <- NULL
lapply(raw.data, ncol)
raw.data <- do.call("rbind" ,raw.data)
head(raw.data)
raw.data$variable <- gsub("_>_", "_larger_", raw.data$variable)
raw.data$variable <- gsub("_<_", "_smaller_", raw.data$variable)

# fix cell line names
unique(raw.data$cell_line)
raw.data$cell_line <- gsub("Chop", "DDIT3", raw.data$cell_line)

unique(raw.data$variable)

# add replicate ID
unique(raw.data$plateID)
raw.data$replID <-NA

raw.data[ raw.data$plateID == "2013_07_10_Srxn1_z1", "replID"] <-   "replicate_1"  #Srxn1 100cmax
raw.data[ raw.data$plateID == "2013_07_24_Srxn1", "replID"] <-      "replicate_2"  #Srxn1 100cmax
raw.data[ raw.data$plateID == "2013_07_29_Srxn1", "replID"] <-      "replicate_3"  #Srxn1 100cmax
raw.data[ raw.data$plateID == "2013_08_01_Srxn1", "replID"] <-      "replicate_4"  #Srxn1 100cmax
raw.data[ raw.data$plateID == "2013_08_14_Srxn1", "replID"] <-      "replicate_5"  #Srxn1 100cmax
raw.data[ raw.data$plateID == "2013_08_15_Srxn1", "replID"] <-      "replicate_1"  #Srxn1 50cmax
raw.data[ raw.data$plateID == "2013_08_21_Srxn1", "replID"] <-      "replicate_2"  #Srxn1 50cmax
raw.data[ raw.data$plateID == "2013_08_22_Srxn1", "replID"] <-      "replicate_3"  #Srxn1 50cmax     
raw.data[ raw.data$plateID == "2013_08_28_nii_Srxn1", "replID"] <-  "replicate_1"  #Srxn1 10cmax
raw.data[ raw.data$plateID == "2013_08_28_niii_Srxn1", "replID"] <- "replicate_2"  #Srxn1 10cmax  
raw.data[ raw.data$plateID == "2013_09_04_Srxn1", "replID"] <-      "replicate_3"  #Srxn1 10cmax
raw.data[ raw.data$plateID == "2013_09_05_Srxn1", "replID"] <-      "replicate_1"  #Srxn1 5cmax
raw.data[ raw.data$plateID == "2013_09_09_Srxn1", "replID"] <-      "replicate_2"  #Srxn1 5cmax
raw.data[ raw.data$plateID == "25-9-2013_Srxn1", "replID"] <-       "replicate_1"  #Srxn1 1cmax

raw.data[ raw.data$plateID == "2013_09_26_DDIT3", "replID"] <-      "replicate_1"  #DDIT3 100cmax
raw.data[ raw.data$plateID == "2013_10_03_DDIT3", "replID"] <-      "replicate_2"  #DDIT3 100cmax
raw.data[ raw.data$plateID == "2013_10_04_DDIT3", "replID"] <-      "replicate_1"  #DDIT3 50cmax
raw.data[ raw.data$plateID == "2013_10_17_nii_DDIT3", "replID"] <-  "replicate_2"  #DDIT3 50cmax
raw.data[ raw.data$plateID == "2013_10_17_niii", "replID"] <-       "replicate_3"  #DDIT3 50 cmax


raw.data[ raw.data$plateID == "2013_10_09_niii_p21", "replID"] <-   "replicate_1"  #p21 100cmax
raw.data[ raw.data$plateID == "2013_10_09_nii_p21", "replID"] <-    "replicate_2"  #p21 100cmax
raw.data[ raw.data$plateID == "2014_02_05_nii_p21", "replID"] <-    "replicate_1" #p21 10cmax
raw.data[ raw.data$plateID == "2014_02_05_p21_niii", "replID"] <-   "replicate_2" #p21 10cmax
raw.data[ raw.data$plateID == "2014_02_11_p21", "replID"] <-        "replicate_1" #p21 50cmax
raw.data[ raw.data$plateID == "2014_02_18_p21", "replID"] <-        "replicate_1" #p21 5cmax 
raw.data[ raw.data$plateID == "2014_02_19_p21", "replID"] <-        "replicate_2" #p21 5cmax
raw.data[ raw.data$plateID == "2014_02_25_p21", "replID"] <-        "replicate_1" #p21 1cmax
raw.data[ raw.data$plateID == "2014_03_05_p21", "replID"] <-        "replicate_2" #p21 1cmax

sum(is.na(raw.data$replID)) #0

#fix dosages: should be exactly the same digits if same concentration


raw.data$comp_dose <- paste(raw.data$treatment, raw.data$dose_uM, sep="_")
sort(unique(raw.data[, "comp_dose"]))

raw.data$dose_uM <- round(raw.data$dose_uM, digits = 4)

ind.more10 <- raw.data$dose_uM >= 10
raw.data$dose_uM[ind.more10] <- round(raw.data$dose_uM[ind.more10], digits = 1)
ind.more1 <- raw.data$dose_uM >= 1
raw.data$dose_uM[ind.more1] <- round(raw.data$dose_uM[ind.more1], digits = 2)

sort(unique(raw.data[, "comp_dose"]))

# count how many doses each compound has and fix if incorrect
comps <- unique(raw.data[ , c("treatment", "dose_uM")])[ order(unique(raw.data$comp_dose)),  ]
comps <- comps[ order(comps$treatment, comps$dose_uM),]
tt <- ddply(comps, .(treatment), summarize, length(treatment))
tt[ tt[, "..1"] >5,]

incDose <- comps[ comps$treatment %in%  c("digoxin","epinephrine", "haloperidol", "moxisylyte",
                               "paroxetine","phenacetin", "propranolol","ribavirin",
                               "simvastatin","sulindac", "tacrine", "ticlopidine"), ]
incDose
raw.data[ raw.data$treatment == "digoxin" & raw.data$dose_uM == 0.2830, "dose_uM" ] <-0.2800
raw.data[ raw.data$treatment == "epinephrine" & raw.data$dose_uM == 0.1763, "dose_uM" ] <-0.1800
raw.data[ raw.data$treatment == "haloperidol" & raw.data$dose_uM == 0.5321, "dose_uM" ] <-0.53
raw.data[ raw.data$treatment == "moxisylyte" & raw.data$dose_uM == 15.7000, "dose_uM" ] <-15.8
raw.data[ raw.data$treatment == "paroxetine" & raw.data$dose_uM == 3.0400, "dose_uM" ] <-3.05
raw.data[ raw.data$treatment == "phenacetin" & raw.data$dose_uM == 30.8000, "dose_uM" ] <-31.0
raw.data[ raw.data$treatment == "propranolol" & raw.data$dose_uM == 10.1000, "dose_uM" ] <-10.0
raw.data[ raw.data$treatment == "ribavirin" & raw.data$dose_uM == 130.6000, "dose_uM" ] <-130.7
raw.data[ raw.data$treatment == "simvastatin" & raw.data$dose_uM == 4.1200, "dose_uM" ] <-4.1
raw.data[ raw.data$treatment == "sulindac" & raw.data$dose_uM == 1599.2000, "dose_uM" ] <-1599.3
raw.data[ raw.data$treatment == "tacrine" & raw.data$dose_uM == 3.9800, "dose_uM" ] <-4
raw.data[ raw.data$treatment == "ticlopidine" & raw.data$dose_uM ==  403.7000, "dose_uM" ] <- 403.8
# wat niet klopt: 
#digoxin 
#epinephrine
#haloperidol
#moxisylyte
#paroxetine
#phenacetin
#propranolol
#ribavirin
#simvastatin
#sulindac
#tacrine
#ticlopidine

raw.data$comp_dose <- NULL
# normalization considerations
  # normalize over entire set per cell line?
  # or use the above DMSO background?
  # definitely don't use the per-plate min max normalized...
  # min max normalization over multiple plates will not work.. need per plate normalization

# --> first try the above DMSO background counts

# collapse time to make dose response curves
# 1) max based
# 2) final time point based
peak.data <- x<- x[, lapply(.SD,  function(x) { mean(as.numeric(x), na.rm = TRUE)}
)
)), 
by =  eval(input$by.what),
.SDcols = input$idsChoice]


p<- ggplot(data=raw.data, aes(x = ))


# plotjes alles voor Srxn1 
# plotjes voor MIP-DILI


