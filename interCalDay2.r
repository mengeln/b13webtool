library(xtable)
library(knitr)
library(plyr)
library(lubridate)
library(reshape2)

interCalDay2 <- function (file, org) {
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
  
  names(cfxtest)[names(cfxtest) == "Starting.Quantity..SQ."] <- "CopyPeruL"
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
  
  ### Genomic ent standard curve
  
  entStandard <- entData[grepl("Std", entData$Sample), ]
  entStandard$Log10CopyPeruL <- log10(as.numeric(entStandard$CopyPeruL)) 
  
  entGen.model <- lm(data = entStandard,  Cq ~ Log10CopyPeruL)
  
  entGen.yint <- coef(entGen.model)[[1]]
  entGen.Slope <- coef(entGen.model)[[2]]
  entGen.r2 <- summary(entGen.model)$r.squared
  entGen.Efficiency <- 10^(-1/coef(entGen.model)[[2]])
  
  entGen.StdQC <- standardQC(entGen.Efficiency, entGen.r2)
  
  ### Calibrator standard curve
  entCalStandard <- entData[grepl("Cal", entData$Sample), ]
  entCalStandard$Log10CopyPeruL <- log10(as.numeric(entCalStandard$CopyPeruL)) 
  
  entCal.model <- lm(data = entCalStandard,  Cq ~ Log10CopyPeruL)
  
  entCal.yint <- coef(entCal.model)[[1]]
  entCal.Slope <- coef(entCal.model)[[2]]
  entCal.r2 <- summary(entCal.model)$r.squared
  entCal.Efficiency <- 10^(-1/coef(entCal.model)[[2]])
  
  entCal.StdQC <- standardQC(entCal.Efficiency, entCal.r2)
  
  ### Sketa Standard Curve
  
  sketaStandard <- sketaData[grepl("Std", sketaData$Content), ]
  sketaStandard$Log10CopyPeruL <- log10(as.numeric(sketaStandard$CopyPeruL)) 
  
  sketa.model <- lm(data = sketaStandard,  Cq ~ Log10CopyPeruL)
  sketa.yint <- coef(sketa.model)[[1]]
  sketa.Slope <- coef(sketa.model)[[2]]
  sketa.r2 <- summary(sketa.model)$r.squared
  sketa.Efficiency <- 10^(-1/coef(sketa.model)[[2]])
  
  sketa.StdQC <- standardQC(sketa.Efficiency, sketa.r2)
  
  # Negative Control QC
  NECmean <- mean(sketaData$Cq[grepl("NEC", sketaData$Sample)], na.rm=TRUE)
  
  calibratorQC <- data.frame(CalibratorCt = sketaData$Cq[grepl("calibrator", sketaData$Sample)])
  calibratorQC$delta <- calibratorQC$CalibratorCt - NECmean
  calibratorQC$PASS <- ifelse(calibratorQC$delta > thres, "FAIL", "PASS")
  names(calibratorQC) <- c("Calibrator Ct", "$\\Delta$ Ct", "PASS?")
  
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
  
  # Generate report
  oname <- tail(strsplit(file, "/")[[1]], 1)
  outputName <- substr(oname, 1, nchar(oname)-4)
  
  # direct output to a file
  if(.Platform$OS == "unix")
    knit("/var/scripts/qpcr/qpcr/day2report.Rtex", paste0("/var/www/qpcr/files/", outputName, ".tex"))
  else
    knit("day2report.Rtex", "../tests/report.tex")
}