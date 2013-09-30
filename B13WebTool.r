suppressPackageStartupMessages(library("optparse"))

options(stringsAsFactor = FALSE)

`%nin%` <- function (x, table) {
  match(x, table, nomatch = 0) == 0
}

if(.Platform$OS == "unix"){
  source("/var/scripts/qpcr/qpcr/ABI_to_CFX.r")
  source("/var/scripts/qpcr/qpcr/enteroTaq.r")
  source("/var/scripts/qpcr/qpcr/processHF183.R")
  source("/var/scripts/qpcr/qpcr/qpcrDB.r")
  source("/var/scripts/qpcr/qpcr/interCalDay1.r")
  source("/var/scripts/qpcr/qpcr/interCalDay2.r")
  source("/var/scripts/qpcr/qpcr/interCalDay3.r")
} else {
  source("ABI_to_CFX.r")
  source("enteroTaq.r")
  source("processHF183.r")
  source("qpcrDB.r")
  source("interCalDay1.r")
  source("interCalDay2.r")
  source("interCalDay3.r")
}


option_list <- list(
  make_option(c("-f", "--file"), type="character", 
              help="Path to the input file"),
  make_option(c("-p", "--platform"),type="character", default="CFX",
              help="The type of platform. Valid options are CFX or ABI"),
  make_option(c("-a", "--assay"),type="character", default="ent",
              help="The assay type. Valid options are ent or hf183"),
  make_option(c("-o", "--organization"),type="character", 
              help="Who is submitting the data")
)

opt <- parse_args(OptionParser(option_list=option_list))

if(opt$platform == "ABI")
  abiToCfx(opt$file)

if(opt$assay == "ent") {
  result <- process_enteroTaq(opt$file, opt$organization)
  p <- "enteroTaqEnviron"
} else if(opt$assay == "HF183"){
  result <- process_HF183(opt$file, opt$organization)
  p <- "HF183_duplex"
} else if(opt$assay == "day1"){
  result <- interCalDay1(opt$file, opt$organization)
} else if(opt$assay == "day2"){
  result <- interCalDay2(opt$file, opt$organization)
} else if(opt$assay == "day3"){
  result <- interCalDay3(opt$file, opt$organization)
}



# submitData(result, opt$organization, p, opt$platform)

cat("Test Return")
