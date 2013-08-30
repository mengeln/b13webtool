library(xtable)
library(knitr)
library(plyr)
library(lubridate)
library(reshape2)

process_enteroTaq <- function (file, org) {
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
  
  sketaStandard <- sketaData[grepl("Std", sketaData$Content), ]
  sketaStandard$Log10CopyPeruL <- log10(as.numeric(sketaStandard$CopyPeruL)) 
  
  sketa.model <- lm(data = sketaStandard,  Cq ~ Log10CopyPeruL)
  # sketa.yint <- coef(sketa.model)[[1]]
  sketa.Slope <- coef(sketa.model)[[2]]
  sketa.r2 <- summary(sketa.model)$r.squared
  sketa.Efficiency <- 10^(-1/coef(sketa.model)[[2]])
  
  sketa.StdQC <- standardQC(sketa.Efficiency, sketa.r2)
  
  # Negative Control QC
  
  controlFrame <- function (data, assay) {
    data$Sample <- toupper(data$Sample)
    cData <- data[data$Sample %in% c("NTC", "NEC"),]
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
  controlsDF <- controlsDF[, c(5, 1, 2, 3, 4)]
  
  # Inhibition QC
  
  sketaQC <- function(data=sketaData, threshold=thres){
    sk.unkn <- data[grepl("Unkn", data$Content), ]
    Ct.sk.calibrator <- mean(data$Cq[data$Sample == "calibrator"])
    
    sk.calibrator <<- Ct.sk.calibrator
    sk.unkn$sk.dct <- sk.unkn$Cq - Ct.sk.calibrator
    sk.unkn$Inhibition <- ifelse(sk.unkn$sk.dct > threshold,
                                 "FAIL", "PASS")
    names(sk.unkn)[6] <- "sk.Ct"
    sk.unkn
  }
  
  sketaData <- sketaQC(sketaData)
  
  sketaDataTrim <- sketaData[, c("Sample", "sk.Ct", "sk.dct", "Inhibition")]
  
  sketaDataTrim <- ddply(sketaDataTrim, .(Sample), summarize, sk.Ct = mean(sk.Ct, na.rm=TRUE),
                         sk.dct = mean(sk.dct, na.rm=TRUE), Inhibtion = ifelse(all(Inhibition == "PASS"), "PASS", "FAIL"))
  names(sketaDataTrim) <- c("Sample", "sketaCt$_{mean}$", "$\\Delta$Ct$_{mean}$", "Pass?")
  
  # Ct to copy number (dct quantification model)
  
  dct <- function(data, ulPerRxn=5, mlFiltered=100, ulCE=500, ulCErecovered=300, ulPE=100, cal=1e5/500){
    Ct.ent.calibrator <- mean(data$Cq[data$Sample == "calibrator"])
    
    data$ent.dct <- data$Cq - Ct.ent.calibrator
    data$cellPerRxn <- 10^(data$ent.dct/ent.Slope  + log10(cal))
    data$cellPer100ml <- data$cellPerRxn/ulPerRxn * ulPE * (ulCE/ulCErecovered) * 100/mlFiltered
    data$log10cellPer100ml <- round(log10(data$cellPer100ml), digits=3)
    
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
  names(resultsTrim2) <- c("Sample", "Target", "Ct$_{Rep 1}$", "Ct$_{Rep 2}$", "$\\log_{10}$ copies/100 \\si{\\milli\\litre}$_{Rep1}$",
                           "$\\log_{10}$ copies/100 \\si{\\milli\\litre}$_{Rep2}$", "Mean $\\log_{10}$ copies/100 \\si{\\milli\\litre}")
  
  # Generate report
  oname <- tail(strsplit(file, "/")[[1]], 1)
  outputName <- substr(oname, 1, nchar(oname)-4)
  
  # direct output to a file
  if(.Platform$OS == "unix")
    knit("/var/scripts/qpcr/qpcr/report.Rtex", paste0("/var/www/qpcr/files/", outputName, ".tex"))
  else
    knit("report.Rtex", "../tests/report.tex")
 
  
  #Return results to be sent to database
  result$Project <- "Bight13"
  result$Date <- metadata["Run Started"]
  names(result)[names(result) %in% c("cellPerRxn", "cellPer100ml", "log10cellPer100ml", "sk.Ct", "sk.dct")] <- 
    c("QuantPerReaction", "QuantPerFilter", "log10QuantPerFilter", "sk_Ct", "sk_dct")
  result$IAC_Ct <- NA
  result$Quantifier <- "cell_equivalents"
  result$yint <- ent.yint 
  result$Slope <- ent.Slope
  result$r2 <- ent.r2 
  result$Efficiency <- ent.Efficiency
  result
}
