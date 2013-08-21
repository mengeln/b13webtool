suppressPackageStartupMessages(library("optparse"))

source("ABI_to_CFX.r")
source("enteroTaq.r")
source("processHF183.r")
source("qpcrDB.r")

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
  result <- process_enteroTaq(opt$file)
  p <- "enteroTaqEnviron"
}
else {
  result <- process_HF183(opt$file)
  p <- "HF183_duplex"
}


submitData(result, opt$organization, p)

cat("Test Return")