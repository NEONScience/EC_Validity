options(stringsAsFactors=F)
library(neonUtilities)

# This script can be used to download data from the portal using neonUtilities 
# and stack the data for processing. Once stacked, the output is saved as an
# rds file in a new directory off the working directory, named for the domain ID
# (ie wd/D09/fluxD09.rds). The fluxPlots script can then be used to generate 
# plots from the stacked data.

# Note for an 8GB RAM setup, 3 towers is generally the maximum that can stack at
# one time.

# Input the domain ID for directory and file saving consistency
domainID = "D01"

# Download EC bundled data product
zipsByProduct(dpID="DP4.00200.001",
              package = "expanded",
              site = c("HARV","BART"),
              startdate = "2018-01",
              enddate="2018-12",
              savepath = getwd(), # creates filesToStack00200/ directory here
              check.size=F)

# Stack and save by domain after download
flux <- stackEddy(paste0(getwd(), "/filesToStack00200"), level = "dp04")

# Makes directory to save stacked .rds file used for plot generation.
if (dir.exists(paste0(getwd(),"/", domainID)) == FALSE){
    dir.create(paste0(getwd(),"/", domainID))
}

saveRDS(flux, paste0(getwd(), "/",domainID,"/flux", domainID, ".rds"))





