#library(R.oo)

iniParse <- function(fname.control)
{
  ini.file <- file(fname.control)
  Lines  <- readLines(ini.file)
  close(ini.file)
  
  Lines <- chartr("[]", "==", Lines)  # change section headers
  
  ini.file <- textConnection(Lines)
  d <- read.table(ini.file, as.is = TRUE, sep = "=", fill = TRUE)
  close(ini.file)
  
  L <- d$V1 == ""                    # location of section breaks
  d <- subset(transform(d, V3 = V2[which(L)[cumsum(L)]])[1:3],
              V1 != "")
  
  to.parse  <- trim(paste("ini.list$", trim(d$V3), "$",  trim(d$V1), " <- '",
                          trim(d$V2), "'", sep=""))
  
  ini.list <- list()
  eval(parse(text=to.parse))
  
  return(ini.list)
}
