##install required packages
#install.packages("neonUtilities")
#install.packages("tidyverse")
#install.packages("BiocManager")
#BiocManager::install('rhdf5')

##Download eddy co bundled data package
##zipsByProduct(dpID="DP4.00200.001", package="expanded",
                # site=c("TREE"),
                # startdate="2019-01", enddate="2019-12",
                # savepath="C:/Users/cslemmons/Documents/ExploreNEON",
                # check.size=F)

##Stack and save to list of data frames
##flux <- stackEddy("C:/Users/cslemmons/Documents/ExploreNEON/filesToStack00200",
                  # level = "dp04")

##save to data frame
##TREEflux <- flux$TREE

##Save flux data frame locally
##saveRDS(TREEflux, "C:/Users/cslemmons/Documents/ExploreNEON/filesToStack00200/TREEflux.rds")

##strings not automatically assigned to factor and load packages
options(stringsAsFactors=F)
library(neonUtilities)
library(ggplot2)
library(tidyverse)
library(lubridate)

##Load flux data
flux <- readRDS(file="C:/Users/cslemmons/Documents/ExploreNEON/filesToStack00200/TREEflux.rds")

##Convert time stamp to R date - time format, add to data frame
flux$TimeB <- as.POSIXct(flux$timeBgn,
                      format="%Y-%m-%dT%H:%M:%S",
                      tz="GMT")

##Add year and month column for summary
flux$YearMonth <- substr(flux$TimeB,0,7)

##Add year, month, day column for summary
flux$YearMonthDay = substr(flux$TimeB,0,10)

##Evaluate fluxCor and fluxRaw column for NAs, calculate % NAs by month
NAsPerMonth <- flux %>%
  mutate(FluxCorNA=is.na(data.fluxCo2.turb.fluxCor)) %>%
  mutate(FluxRawNA=is.na(data.fluxCo2.turb.fluxRaw)) %>%
  group_by(YearMonth) %>%
  summarise(FluxCorNAPercent = mean(FluxCorNA*100), FluxRawNAPercent = mean(FluxRawNA*100))

##calculate difference between raw and corrected flux, save to data frame. Calculate mean difference by month
flux$DiffCor <- flux$data.fluxCo2.turb.fluxRaw - flux$data.fluxCo2.turb.fluxCor

flux %>%
  mutate(DiffCor=data.fluxCo2.turb.fluxRaw-data.fluxCo2.turb.fluxCor) %>%
  group_by(YearMonth) %>%
  summarise(DiffCor = mean(DiffCor, na.rm=TRUE))

flux$DiffCor %>%
  group_by(YearMonth) %>%
  summarise(DiffCor = mean(DiffCor, na.rm=TRUE))

##calculate difference between raw and corrected flux and calculate mean difference grouped by day, add column for difference to data frame
fluxDiff <- flux %>%
  mutate(DiffCor=data.fluxCo2.turb.fluxRaw-data.fluxCo2.turb.fluxCor) %>%
  group_by(YearMonthDay) %>%
  summarise(DiffCor = mean(DiffCor, na.rm=FALSE))

##Plot raw and corrected turbulent flux data for whole dataset
ggplot(flux) +geom_point(aes(x=TimeB, y=data.fluxCo2.turb.fluxRaw, color="blue"))+
  geom_point(aes(x=TimeB, y=data.fluxCo2.turb.fluxCor, color="green"))+
  labs(title="2019 TREE Turbulent Flux", x="Date", y="CO2 umol m2/s-1")+
  scale_color_identity(name=" ",
                       breaks = c("blue", "green"),
                       labels = c("Raw Flux","Corrected Flux"),
                       guide = "legend")+ theme(plot.title = element_text(hjust=0.5),legend.position = c(.9,.1), legend.background = element_rect(color=NULL, fill="transparent"))

##Plot mean difference between raw and corrected flux per day
ggplot(fluxDiff) +geom_point(aes(x=YearMonthDay, y=DiffCor))+
  theme(axis.text.x=element_text(size=3, angle=90))
  labs(title="2019 TREE Turbulent Flux", x="Date", y="CO2 umol m2/s-1")
  
##Plot Percent NA by Month
ggplot(NAsPerMonth) + aes(x=YearMonth, y=FluxCorNAPercent)+
  geom_col()+
    labs(x = "Date", y = "Percent", title = "Mean Percent of Missing Corrected Flux")

##save to CSV
#savefilename <- "C:/Users/cslemmons/Documents/ExploreNEON/filesToStack00200/TREEFlux.csv"
#write.table(flux, "C:/Users/cslemmons/Documents/ExploreNEON/filesToStack00200/TREEFlux.csv", sep=",")

