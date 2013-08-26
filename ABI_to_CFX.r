

abiToCfx <- function (abiFile) {
  options(stringsAsFactors=FALSE)
  
  data <- read.csv(abiFile , skip =8)
  cols <- c("Well", "Sample.Name", "Target.Name", "Task", "Reporter", "Quantity")
  data_subset <- data[, cols]
  names(data_subset) <- c("Well", "Sample", "Target", "Content", "Fluor", "Starting Quantity (SQ)")
  data_subset$Cq <- data[, 7]
  data_subset$Cq[data_subset$Cq == "Undetermined"] <- "N/A"
  data_subset$Content[data_subset$Content == "UNKNOWN"] <- "Unkn"
  data_subset$Content[data_subset$Content == "STANDARD"] <- "Std"
  
  
  metadata <- read.csv(abiFile, nrow=5)[, 1]
  
  file <- tail(strsplit(metadata[2], "\\\\")[[1]], 1)
  
  endtime <- strsplit(metadata[3], "=")[[1]][2]
  
  metaNames <- c("File Name", "Created By User", "Notes", "ID", "Run Started", "Run Ended", "Sample Vol",
                 "Lid Temp", "Protocol File Name", "Plate Setup File Name", "Base Serial Number",
                 "Optical Head Serial Number", "CFX Manager Version", "", "Well group", "Amplification step",
                 "Melt step", "", "")
  metaValues <-c(file, NA, NA, NA, endtime, endtime, 25, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
  meta <- cbind(metaNames, metaValues)
  colnames(meta) <- NULL
  
  write.table(meta, abiFile, row.names=FALSE, col.names=FALSE, sep=",")
  write.table(data_subset, abiFile, row.names=FALSE, append=TRUE, sep=",")
}


