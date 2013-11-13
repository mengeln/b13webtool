# This script is for analyzing bight13 intercal day 3 data (Enterococcus only, with CE)

library(xtable)
library(knitr)
library(plyr)
library(lubridate)
library(reshape2)

interCalDay3 <- function (file, org) {
  options(stringsAsFactors=FALSE)
  
  eff.max <- 2.1
  eff.min <- 1.87
  r2.min <- 0.98
  m <- 45  # Ct to assign to unamplified wells
  thres <- 1.7 # inhibition threshold
  
  cfxtest <- read.csv(file,
                      skip=19,
                      stringsAsFactors=FALSE)
  
  ### Pull metadata
  
  meta <- read.csv(file, nrows = 13, header = FALSE)[, 1:2]
  metadata <- meta[, 2]
  names(metadata) <- meta[, 1]
  metadata <- gsub("_", " ", metadata)
  
  # data Clean Up 
  
  names(cfxtest)[names(cfxtest) == "Starting.Quantity..SQ."] <- "CopyPeruL"  #For bite13, it is CopyPerRxn
  cfxtest$CopyPeruL <- as.numeric(cfxtest$CopyPeru)
  cfxtest$Cq[cfxtest$Cq == "N/A"] <- m
  cfxtest$Cq <- as.numeric(cfxtest$Cq)
  cfxtest$Target <- tolower(cfxtest$Target)
  cfxtest$Content[grepl("Std", cfxtest$Content)] <- "Std"
  cfxtest$Content[grepl("Unkn", cfxtest$Content)] <- "Unkn"
  
  # Subset by target
  
  entData <- cfxtest[cfxtest$Target == "ent", ]
  sketaData <- cfxtest[cfxtest$Target == "sketa22" | cfxtest$Target == "sketa", ]  
  
  # Standard Curve
  
  standardQC <- function(eff, r2, ef.max=eff.max, ef.min=eff.min, r2.m=r2.min) {
    eff <- ifelse((eff > ef.min & eff < ef.max), "PASS", "FAIL")
    r2 <- ifelse(r2 > r2.m, "PASS", "FAIL")
    c(eff = eff, r2 = r2)
  }
  
  entStandard <- entData[grepl("Std", entData$Content), ]
  entStandard$Log10CopyPeruL <- log10(as.numeric(entStandard$CopyPeruL)) 
  
  ent.model <- lm(data = entStandard,  Cq ~ Log10CopyPeruL)
  
  ent.yint <- coef(ent.model)[[1]]
  ent.Slope <- coef(ent.model)[[2]]
  ent.r2 <- summary(ent.model)$r.squared
  ent.Efficiency <- 10^(-1/coef(ent.model)[[2]])
  
  ent.StdQC <- standardQC(ent.Efficiency, ent.r2)
  
  # Negative Control QC
  
  controlFrame <- function (data, assay) {
    data$Sample <- toupper(data$Sample)
    cData <- data[grepl("NTC|NEC", data$Sample),]
    cData <- ddply(cData, .(Sample), function(df){
      df$Replicate <- paste0("Ct$_{Rep", 1:nrow(df), "}$")
      df
    })
    controlDF <- dcast(cData, Sample ~ Replicate, value.var="Cq")
    controlDF$"PASS?" <- apply(controlDF[, -1], 1, function(x)ifelse(all(x == m), "PASS", "FAIL"))
    controlDF[controlDF ==m] <- "N/A"
    controlDF$Assay <- assay
    controlDF
  }
  controlDF <- controlFrame(entData, "Entero1A")
  controlSk <- controlFrame(sketaData, "Sketa22")
  
  controlsDF <- rbind(controlDF, controlSk[controlSk$Sample == "NTC",])
  controlsDF <- controlsDF[, c(ncol(controlsDF), 1:(ncol(controlsDF) -1))]
 
  # Inhibition QC
  NECmean <- mean(sketaData$Cq[grepl("NEC", sketaData$Sample)], na.rm=TRUE)
  
  calibratorQC <- data.frame(CalibratorCt = sketaData$Cq[grepl("calibrator", sketaData$Sample)])
  calibratorQC$delta <- calibratorQC$CalibratorCt - NECmean
  calibratorQC$PASS <- ifelse(calibratorQC$delta > thres | calibratorQC$delta < (-thres), "FAIL", "PASS")
  names(calibratorQC) <- c("Calibrator Ct", "$\\Delta$ Ct", "PASS?")
  
  sketaQC.cal <- function(data=sketaData, threshold=thres){
    sk.unkn <- data[grepl("Unkn", data$Content), ]
    Ct.sk.calibrator <- mean(data$Cq[grepl("calibrator", data$Sample)]) 
    
    sk.calibrator <<- Ct.sk.calibrator
    sk.unkn$sk.dct <- sk.unkn$Cq - Ct.sk.calibrator
    sk.unkn$Inhibition <- ifelse(sk.unkn$sk.dct > threshold | sk.unkn$sk.dct < (-threshold),  #yc: need to update variable name
                                 "FAIL", "PASS")
    names(sk.unkn)[names(sk.unkn)=="Cq"] <- "sk.Ct"
    sk.unkn
  }
  
  sketaQC.nec <- function(data=sketaData, threshold=thres){
    sk.unkn.nec <- data[grepl("Unkn", data$Content), ]
    Ct.sk.calibrator <- mean(data$Cq[grepl("NEC", data$Sample)]) 
    
    sk.calibrator <<- Ct.sk.calibrator
    sk.unkn.nec$sk.dct <- sk.unkn.nec$Cq - Ct.sk.calibrator
    sk.unkn.nec$Inhibition <- ifelse(sk.unkn.nec$sk.dct > threshold | sk.unkn.nec$sk.dct < (-threshold),  #yc: need to update variable name
                                 "FAIL", "PASS")
    names(sk.unkn.nec)[names(sk.unkn.nec)=="Cq"] <- "sk.Ct"
    sk.unkn.nec
  }
  
  sketaData <- sketaQC.cal(sketaData)
  # need to generate sketaData based on sketaQC.nec function (as a separate table in the report)
  
  sketaDataTrim <- sketaData[, c("Sample", "sk.Ct", "sk.dct", "Inhibition")]
  
  sketaDataTrim <- ddply(sketaDataTrim, .(Sample), summarize, sk.Ct = mean(sk.Ct, na.rm=TRUE),
                         sk.dct = mean(sk.dct, na.rm=TRUE), Inhibtion = ifelse(all(Inhibition == "PASS"), "PASS", "FAIL"))
  names(sketaDataTrim) <- c("Sample", "sketaCt$_{mean}$", "$\\Delta$Ct$_{mean}$", "Pass?")
  
  # Ct to CE/filter number (dct quantification model)
  
  dct <- function(data, mlFiltered=100, cal=1e5){
    Ct.ent.calibrator <<- mean(data$Cq[data$Sample == "calibrator"])
    
    data$ent.dct <- data$Cq - Ct.ent.calibrator
    data$log10cellPerFilter <- data$ent.dct/ent.Slope  + log10(cal)
    data$log10cellPer100ml <- round((data$log10cellPerFilter + log10(100/mlFiltered)), digits=3)
        
    data[data$Cq == m, c("log10cellPer100ml")] <- "ND"
    
    data[!grepl("Std", data$Content), ]
    
  }
  
  entData <- dct(entData)
  entData <- ddply(entData, .(Sample), function(df){
    df$Mean <- round(log10(mean(df$cellPer100ml)), digits=3)
    df
  })
  
  sketaDataMean <- ddply(sketaData, .(Sample), summarize, sk.Ct = mean(sk.Ct, na.rm=TRUE),
                         sk.dct = mean(sk.dct, na.rm=TRUE),
                         Inhibition = ifelse(all(Inhibition == "PASS"), "PASS", "FAIL"))
  result <- merge(entData, sketaDataMean)
  # result <- merge(entData, sketaData[, c("Sample", "sk.Ct", "sk.dct", "Inhibition")])
  
  
  # Inhibited flag
  result$log10cellPer100ml[result$Inhibition == "FAIL"] <- "inhibited"
  
  resultsTrim <- subset(result, select = c(Sample, Target, Cq, log10cellPer100ml, Mean))
  names(resultsTrim)[3:4] <- c("Ct", "log10")
  resultsTrim$Ct[resultsTrim$Ct == m] <- "N/A"
  
  resultsTrim <- ddply(resultsTrim, .(Sample), function(df){
    df$Replicate <- 1:nrow(df)
    if(any(df$log10 == "inhibited"))
      df$Mean[!is.na(df$Mean)] <- "inhibited" 
    df
  })
  
  #resultsTrim <- arrange(resultsTrim, Sample, Mean)
  rmelt <- melt(resultsTrim, id.vars=c("Sample", "Target", "Replicate"))
  resultsTrim2 <- dcast(rmelt, Sample + Target  ~ Replicate + variable, value.var="value")
  
  resultsTrim2 <- resultsTrim2[, c("Sample", "Target", "1_Ct", "2_Ct", "1_log10", "2_log10",
                                   "1_Mean")]
  names(resultsTrim2) <- c("Sample", "Target", "Ct$_{Rep 1}$", "Ct$_{Rep 2}$", "$\\log_{10}$ cells/100 \\si{\\milli\\litre}$_{Rep1}$",
                           "$\\log_{10}$ cells/100 \\si{\\milli\\litre}$_{Rep2}$", "Mean $\\log_{10}$ cells/100 \\si{\\milli\\litre}")
  
  # Generate report
  oname <- tail(strsplit(file, "/")[[1]], 1)
  outputName <- substr(oname, 1, nchar(oname)-4)
  
  # direct output to a file
  if(.Platform$OS == "unix")
    knit("/var/scripts/qpcr/qpcr/day3report.Rtex", paste0("/var/www/qpcr/files/", outputName, ".tex"))
  else
    knit("day3report.Rtex", "../tests/report.tex")
}
