Sample.s1 <- function(i, n){
  samp <- read.table(paste0("S:/COS/PyroGeog/amartinez/Persistance/Intermediates/VegSampling/samp_", n, ".txt"), fill = T)
  samp <- samp[-c(1,2,3,4,5,6),]
  samp[,1] <- as.numeric(as.character(samp[,1]))
  samp <- c(samp[,1], samp[,2])
  no.na <- which(samp != -9999)
  samp <- samp[no.na]
  p <- read.table(paste0(substring("rastertoascii",1,10-nchar(i)), "_", tolower(i), "_", n, "1.txt"), fill = T)
  p <- p[-c(1,2,3,4,5,6),]
  p[,1] <- as.numeric(as.character(p[,1]))
  p <- c(p[,1], p[,2])
  p <- p[no.na]
  df <- data.frame(UI = samp)
  df[,i] <- p
  return(df)
}

Sample <- function(i){
  setwd(paste0("S:/COS/PyroGeog/amartinez/Persistance/Intermediates/Variables/Iterators/", i))
  p0 <- Sample.s1(i, 0)
  p1 <- Sample.s1(i, 1)
  p2 <- Sample.s1(i, 2)
  p3 <- Sample.s1(i, 3)
  sample.df <- rbind(p0,p1,p2,p3)
  write.csv(sample.df, paste0(i, "_Sample.csv"), row.names = F)
}

Sample("TRI")
Sample("TPI")

