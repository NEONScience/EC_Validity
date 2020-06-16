options(stringsAsFactors=F)
library(tidyverse)
library(lubridate)

# Input the filepath including file name and number of towers and the function
# will generate plots for all supplied flux sites. Assumes RDS formatted data.

fluxPlots <- function(filepath, numTowers){
  
# Plot variables
  plotDir <- paste0(getwd(), "/plots")
  plotW   <- 11
  plotH   <- 8.5 # Landscape
  units   <- "in"
  
  if (dir.exists(plotDir) == FALSE){
      dir.create(plotDir)
  }
  
  flux <- readRDS(filepath)

  
  for (i in seq(1:numTowers)){
    
    # Filter out old versions of data that don't contain Raw and Cor columns
    if (any(grepl("data.fluxCo2.turb.fluxCor", names(flux[[i]]))) == TRUE){
    
    flux[[i]]$TimeB <- as.POSIXct(flux[[i]]$timeBgn,
                                     format="%Y-%m-%dT%H:%M:%S",
                                     tz="GMT")
    
    ##Add year and month column for summary
    flux[[i]]$YearMonth <- substr(flux[[i]]$TimeB,0,7)
    
    year <- substr(flux[[i]]$TimeB[1],0,7) # First occurence of year in ts
    
    ##Add year, month, day column for summary
    flux[[i]]$YearMonthDay = substr(flux[[i]]$TimeB,0,10)
    
    ##Evaluate fluxCor and fluxRaw column for NAs, calculate % NAs by month
    NAsPerMonth <- flux[[i]] %>%
      mutate(FluxCorNA=is.na(data.fluxCo2.turb.fluxCor)) %>%
      mutate(FluxRawNA=is.na(data.fluxCo2.turb.fluxRaw)) %>%
      group_by(YearMonth) %>%
      summarise(FluxCorNAPercent = mean(FluxCorNA*100),
                FluxRawNAPercent = mean(FluxRawNA*100))
    
        ##Evaluate fluxCor and fluxRaw column for NAs, calculate % NAs by day
      NAsPerDay <- flux[[i]] %>%
        mutate(FluxCorNA=is.na(data.fluxCo2.turb.fluxCor)) %>%
        mutate(FluxRawNA=is.na(data.fluxCo2.turb.fluxRaw)) %>%
        group_by(YearMonthDay) %>%
        summarise(FluxCorNAPercent = mean(FluxCorNA*100),
                  FluxRawNAPercent = mean(FluxRawNA*100))
      
      ##calculate difference between raw and corrected flux, save to data frame. 
      ##Calculate mean difference by month
      flux[[i]]$DiffCor <- flux[[i]]$data.fluxCo2.turb.fluxRaw -
                              flux[[i]]$data.fluxCo2.turb.fluxCor
  
      ## it breaks below here
  
      flux[[i]] %>%
        group_by(YearMonth) %>%
        summarise(DiffCor = mean(DiffCor, na.rm=TRUE))
  
      fluxDiffbyMonth <- flux[[i]] %>%
        mutate(DiffCor=data.fluxCo2.turb.fluxRaw-data.fluxCo2.turb.fluxCor) %>%
        group_by(YearMonth) %>%
        summarise(DiffCor = mean(DiffCor, na.rm=TRUE))
  
      ##calculate difference between raw and corrected flux and calculate mean 
      ##difference grouped by day
      fluxDiffbyDay <- flux[[i]] %>%
        mutate(DiffCor=data.fluxCo2.turb.fluxRaw-data.fluxCo2.turb.fluxCor) %>%
        group_by(YearMonthDay) %>%
        summarise(DiffCor = mean(DiffCor, na.rm=TRUE))
  
      ##calculate number of final Turb QF by Month and Day
      finQFbyMonth <- flux[[i]] %>% 
        group_by(YearMonth) %>%
        summarise(QFPercent=mean(qfqm.fluxCo2.turb.qfFinl*100))
  
      finQFbyDay <- flux[[i]] %>% 
        group_by(YearMonthDay) %>%
        summarise(QFPercent=mean(qfqm.fluxCo2.turb.qfFinl*100))
  
      ##Plot raw and corrected turbulent flux data for whole dataset
      # which(flux[[i]]$data.fluxCo2.turb.fluxRaw > 4000) # returns index of outliers
      # flux[[i]]$data.fluxCo2.turb.fluxRaw[12924] = NA
      # flux[[i]]$data.fluxCo2.turb.fluxCor[12924] = NA# Single outlier removal
      ggplot(flux[[i]]) +
        geom_point(aes(x=TimeB, y=data.fluxCo2.turb.fluxRaw, color="blue"))+
        geom_point(aes(x=TimeB, y=data.fluxCo2.turb.fluxCor, color="green"))+
        labs(title= paste0(year," ", names(flux)[i], " Turbulent Flux"),
             x="Date", y="CO2 umol m2/s-1")+
        scale_color_identity(name=" ",
                             breaks = c("blue", "green"),
                             labels = c("Raw Flux","Corrected Flux"),
                             guide = "legend")+
        theme(plot.title = element_text(hjust=0.5), legend.position = c(.9,.1),
              legend.background = element_rect(color=NULL, fill="transparent"))
        ggsave(filename = paste0(names(flux)[i], "_ECTERawCorrScatterPlot.pdf"),
               width = plotW, height = plotH, units = units, path = plotDir)
  
      ##Plot Percent NA and Percent QF by Month
      ggplot(NAsPerMonth) + aes(x=YearMonth, y=FluxCorNAPercent) +
        geom_col()+
        ylim(0,100)+
        labs(x = "Date", y = "Percent",
             title = paste0(names(flux)[i], year,
                            " - Mean Percent of Missing Corrected Turbulent Flux"))
      ggsave(filename= paste0(names(flux)[i], "_ECTEPercentMissingCorrFlux.pdf"),
         width = plotW, height = plotH, units = units, path = plotDir)
      
      ggplot(finQFbyMonth) + aes(x=YearMonth, y=QFPercent)+
        geom_col()+
        ylim(0,100)+
        labs(x = "Date", y = "Percent",
             title = paste0(names(flux)[i], year,
                            " - Mean Percent of Quality Flags Turbulent Flux"))
      ggsave(filename= paste0(names(flux)[i], "_ECTEPercentQF.pdf"),
           width = plotW, height = plotH, units = units, path = plotDir)
    } else{
        print(paste0(names(flux)[i], " data is missing Raw and Cor flux columns.",
                     " No plots were generated."))
    }
  }
}

