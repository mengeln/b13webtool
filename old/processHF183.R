
library(reshape2)
library(xtable)
library(knitr)
library(plyr)
# file <- "C:/Users/yipingc/Desktop/b13webtool/HF183_multiplex_test_data.csv"
process_HF183 <- function (file, org) {
options(stringsAsFactors=FALSE)

  
  eff.max <- 2.1
  eff.min <- 1.87
  r2.min <- 0.98
  m <- 45  # Ct to assign to unamplified wells
  thres <- 3.0 # sketa sample processing control failure threshold

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
  HFData <- cfxtest[cfxtest$Target == "hf183", ]
  sketaData <- cfxtest[cfxtest$Target == "sketa22" | cfxtest$Target == "sketa", ]
  IACdata <- cfxtest[cfxtest$Target == "iac", ]
  
  
  # Standard Curve
  
  standardQC <- function(eff, r2, ef.max=eff.max, ef.min=eff.min, r2.m=r2.min) {
    eff <- ifelse((eff > ef.min & eff < ef.max), "PASS", "FAIL")
    r2 <- ifelse(r2 > r2.m, "PASS", "FAIL")
    c(eff = eff, r2 = r2)
  }
  
  HFStandard <- HFData[grepl("Std", HFData$Content), ]
  HFStandard$Log10CopyPeruL <- log10(as.numeric(HFStandard$CopyPeruL)) 
  
  HF.model <- lm(data = HFStandard,  Cq ~ Log10CopyPeruL) # create standard curve model
  HF.yint <- coef(HF.model)[[1]]
  HF.Slope <- coef(HF.model)[[2]]
  HF.r2 <- summary(HF.model)$r.squared
  HF.Efficiency <- 10^(-1/HF.Slope)
  
  HF.StdQC <- standardQC(HF.Efficiency, HF.r2)
  
  sketaStandard <- sketaData[grepl("Std", sketaData$Content), ]
  sketaStandard$Log10CopyPeruL <- log10(as.numeric(sketaStandard$CopyPeruL)) 
  
  
  sketa.model <- lm(data = sketaStandard,  Cq ~ Log10CopyPeruL)
  sketa.Slope <- coef(sketa.model)[[2]]
  sketa.r2 <- summary(sketa.model)$r.squared
  sketa.Efficiency <- 10^(-1/coef(sketa.model)[[2]])
  
  sketa.StdQC <- standardQC(sketa.Efficiency, sketa.r2)
  
  # Controls
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
controlDF <- controlFrame(HFData, "Entero1A")
controlSk <- controlFrame(sketaData, "Sketa22")

controlsDF <- rbind(controlDF, controlSk[controlSk$Sample == "NTC",])
numCols <- ncol(controlsDF)
controlsDF <- controlsDF[, c(numCols, 1:(numCols - 1))]

  # Sketa Inhibition QC
  
  sketaQC <- function(data=sketaData, threshold=thres){
    sk.unkn <- data[grepl("Unkn", data$Content), ]
    calibrators <- sketaData$Cq[toupper(sketaData$Sample) == "NEC"]
    Ct.sk.calibrator <- mean(calibrators)
    
    sk.calibrator <<- Ct.sk.calibrator
    sk.unkn$sk.dct <- sk.unkn$Cq - Ct.sk.calibrator
    sk.unkn$Inhibition <- ifelse(sk.unkn$sk.dct > threshold,
                                 "FAIL", "PASS")
    names(sk.unkn)[6] <- "sk.Ct"
    sk.unkn
  }
  
  sketaData <- sketaQC(sketaData)
  sketaDataTrim <- sketaData[, c("Sample", "sk.Ct", "sk.dct", "Inhibition")]
  
  sketaDataTrim <- ddply(sketaDataTrim, .(Sample), function(df){
    data.frame(Sample = unique(df$Sample),
               sk.Ct = mean(df$sk.Ct, na.rm=TRUE),
               sk.dct = mean(df$sk.dct, na.rm=TRUE),
               Inhibtion = ifelse(all(df$Inhibition == "PASS"), "PASS", "FAIL")
    )
  })
  names(sketaDataTrim) <- c("Sample", "sketa Ct$_{mean}$", "$\\Delta$Ct$_{mean}$", "Pass?")
  
  # IAC Inhibition QC
  
  IACstd <- IACdata[IACdata$Content == "Std", ]
  IACstd <- IACstd[order(IACstd$CopyPeruL, decreasing =FALSE), ]
  Istd <- ddply(IACstd, .(CopyPeruL), summarize, Cq = mean(Cq, na.rm=TRUE))
  Istd$compare <- Istd$Cq - Istd$Cq[Istd$CopyPeruL == min(Istd$CopyPeruL)]
  ROQ <- Istd$Cq[Istd$compare < 0.75 & Istd$compare > - 0.75]
  Max.comp <- max(Istd$CopyPeruL[Istd$compare < 0.75 & Istd$compare > - 0.75])
  
  IACcompetition <- predict(HF.model, data.frame(Log10CopyPeruL = log10(Max.comp)))
  
  IACinterference <- mean(ROQ) + 4*sd(ROQ, na.rm=TRUE)
  
  IACinhib <- ddply(IACdata[IACdata$Content == "Unkn", ], .(Sample), function(df){ 
    mean <- mean(df$Cq, na.rm=TRUE)
    inhibited <- mean > IACinterference 
    data.frame(Sample = unique(df$Sample),
               Ctmean = mean,
               "PASS?" = ifelse(!inhibited, "PASS", "FAIL")
               )
  })
  IACinhib$Ctmean[IACinhib$Ctmean == m] <- "ND"
  names(IACinhib) <- c("Sample", "IAC Ct$_{mean}$", "Pass?")
  
  # CCE interpolation
  directCT <- function(data, ulPerRxn=2, mlFiltered=100, ulCE=500, ulCErecovered=300, ulPE=100){
    data$r2 <- HF.r2
    data$Eff <- HF.Efficiency
    data$Slope <- HF.Slope
    data$yint <- HF.yint
    data$HF.predict <- (data$Cq - HF.yint) / HF.Slope  # direct interpolation from standard curve
    data$copiesPerRxn <- 10^(data$HF.predict)
    data$copiesPer100ml <- data$copiesPerRxn/ulPerRxn * ulPE * (ulCE/ulCErecovered) * 100/mlFiltered
    data$log10copiesPer100ml <- round(log10(data$copiesPer100ml), digits=3)
    
    data[data$Cq == m, c("log10copiesPer100ml")] <- "ND"
    
    data[!grepl("Std", data$Content), ]
    
  }
  
  HFData2 <- directCT(HFData)
  if(nrow(HFData2) > 0) {
  HFData2$Date <- metadata["Run Started"]
  }
  
  
  
  ### Integrate results ###
  result <- Reduce(function(x,y)merge(x,y, by="Sample"), list(HFData2, sketaDataTrim, IACinhib))

  result$Competition <- result$"Pass?.y" == "FAIL" & result$Cq < IACcompetition  # need to modify in the future for accidental overdose of iac
  IACinhib$Competition <- ifelse(result$Competition[match(IACinhib$Sample, result$Sample)], "Yes", NA)

  resultsTrim <- rbind.fill(lapply(split(result, result$Sample), function(df){
    inhibition <- !all(rbind(df$"Pass?.x" == "PASS", df$"Pass?.y" == "PASS"))
  
    res <- df[, c("Sample", "Target", "Cq", "log10copiesPer100ml", "copiesPer100ml")]
    res$Mean <- NA
    res$Mean[1] <- ifelse(any(inhibition), "inhibited", round(log10(mean(res$copiesPer100ml)), digits=2))
    res$Cq[res$Cq == m] <- "N/A"
    res$Replicate <- 1:nrow(res)
    if(any(inhibition))res$log10copiesPer100ml <- "inhibited"
    subset(res, select=-c(copiesPer100ml))
  }))
  resultsTrim <- arrange(resultsTrim, Sample, Mean)

  names(resultsTrim)[3] <- c("Ct")

  rmelt <- melt(resultsTrim, id.vars=c("Sample", "Target", "Replicate"))
  resultsTrim2 <- dcast(rmelt, Sample + Target  ~ Replicate + variable, value.var="value")

  resultsTrim2 <- resultsTrim2[, c("Sample", "Target", "1_Ct", "2_Ct", "1_log10copiesPer100ml", "2_log10copiesPer100ml",
                                   "1_Mean")]
  resultsTrim2[is.na(resultsTrim2$"1_Ct") & is.na(resultsTrim2$"2_Ct"), c("1_Mean")] <- "ND"
  names(resultsTrim2) <- c("Sample", "Target", "Ct$_{Rep 1}$", "Ct$_{Rep 2}$", "$\\log_{10}$ copies/100 \\si{\\milli\\litre}$_{Rep1}$",
                           "$\\log_{10}$ copies/100 \\si{\\milli\\litre}$_{Rep2}$", "Mean $\\log_{10}$ copies/100 \\si{\\milli\\litre}")

  # Generate report
  oname <- tail(strsplit(file, "/")[[1]], 1)
  outputName <- substr(oname, 1, nchar(oname)-4)

  
  # Knit PDF#
  if(.Platform$OS == "unix")
    knit("/var/scripts/qpcr/qpcr/HFreport.Rtex", paste0("/var/www/qpcr/files/", outputName, ".tex"))
  else
    knit("HFreport.Rtex", "../tests/report.tex")
  
  ## Return result ##
  result$Project <- "Bight13" 
  result$Inhibition <- ifelse(result$"Pass?.x" == "FAIL", "FAIL", ifelse(
    result[, "Pass?.y"] == "FAIL" & !result$Competition, "FAIL", ifelse(
      result$"Pass?.y" == "FAIL" & result$Competition, "Competition", "PASS")))
  names(result)[names(result) %in% c("Eff", "copiesPerRxn", "copiesPer100ml", "log10copiesPer100ml",
                "sketa Ct$_{mean}$", "$\\Delta$Ct$_{mean}$", "IAC Ct$_{mean}$")] <-
    c("Efficiency", "QuantPerReaction", "QuantPerFilter", "log10QuantPerFilter", "sk_Ct", "sk_dct", "IAC_Ct")
  result$Quantifier <- "copies"
  result
}
