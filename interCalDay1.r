library(xtable)
library(knitr)
library(plyr)
library(lubridate)
library(reshape2)

interCalDay1 <- function (file, org) {
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
  HF183Data <- cfxtest[cfxtest$Target == "hf183", ]  

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
  
  HF183Standard <- HF183Data[grepl("Std", HF183Data$Content), ]
  HF183Standard$Log10CopyPeruL <- log10(as.numeric(HF183Standard$CopyPeruL)) 
  
  HF183.model <- lm(data = HF183Standard,  Cq ~ Log10CopyPeruL)
  HF183.yint <- coef(HF183.model)[[1]]
  HF183.Slope <- coef(HF183.model)[[2]]
  HF183.r2 <- summary(HF183.model)$r.squared
  HF183.Efficiency <- 10^(-1/coef(HF183.model)[[2]])
  
  HF183.StdQC <- standardQC(HF183.Efficiency, HF183.r2)
  
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
  controlSk <- controlFrame(HF183Data, "HF183")
  
  controlsDF <- rbind(controlDF, controlSk[controlSk$Sample == "NTC",])
  controlsDF <- controlsDF[, c(5, 1, 2, 3, 4)]
 
  # direct output to a file
  if(.Platform$OS == "unix")
    knit("/var/scripts/qpcr/qpcr/day1report.Rtex", paste0("/var/www/qpcr/files/", outputName, ".tex"))
  else
    knit("day1report.Rtex", "../tests/report.tex")
}
  