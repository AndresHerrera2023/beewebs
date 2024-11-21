#####FINAL PROJECT PLANT BEE INTERACTION ####


#Required libraries:
library(bipartite)
library(dplyr)

#Set your working directoru
setwd("Z:/Andres/Plantbee_web/")
getwd()

####Tribe level####

#Reading file with network matrix

D <- list.files(path = "C:/Users/a124h117/Desktop/networks/input/", pattern = ".csv$", 
                full.names = T)
nam <- list.files(path = "C:/Users/a124h117/Desktop/networks/input/", pattern = ".csv$", 
                  full.names = F)
Finalnam <- paste0("C:/Users/a124h117/Desktop/networks/output/metrics/metrics_", nam) #Create output filder
for (i in 1:length(D)){
  matriz <- read.csv(D[i])
  matriz <- as.data.frame(matriz)
  matriz <- matriz %>%
    group_by(Tribe) %>%
    summarise(across(everything(), sum, na.rm=TRUE))
  matriz_values <- matriz[,2:length(matriz)]
  final_values = as.matrix(matriz_values)
  rownames(final_values) = matriz$Tribe
  net <-networklevel(final_values)
  write.csv(net, Finalnam[i], row.names = TRUE)
}


#plotweb from all your matrices
D <- list.files(path = "C:/Users/a124h117/Desktop/networks/input/", pattern = ".csv$", 
                full.names = T)
output_plotweb <- ("C:/Users/a124h117/Desktop/networks/output/plots/plotweb/")
for (i in 1:length(D)){
  matriz <- read.csv(D[i])
  matriz <- as.data.frame(matriz)
  matriz <- matriz %>%
    group_by(Tribe) %>%
    summarise(across(everything(), sum, na.rm=TRUE))
  matriz_names <- matriz[1]
  matriz_names
  matriz_values <- matriz[,2:length(matriz)]
  final_values <- as.matrix(matriz_values)
  rownames(final_values) = matriz$Tribe
  tiff_filename <- paste(output_plotweb,"plot", i, ".tiff", sep = "")
  tiff(filename = tiff_filename, width = 13, height = 9.5, units = "in", res = 600)
  plotweb(sortweb(final_values, sort.order="dec"),
          method="normal", 
          col.high = "darkgreen", 
          bor.col.high = "darkgreen",
          col.low = "darkorange", 
          bor.col.low = "darkorange",
          bor.col.interaction = "gray70",
          text.rot = 90,
          labsize =1.5)
  dev.off()
}

#visweb all your matrices
D <- list.files(path = "C:/Users/a124h117/Desktop/networks/input/", pattern = ".csv$", 
                full.names = T)
output_visweb <- ("C:/Users/a124h117/Desktop/networks/output/plots/visweb/")
for (i in 1:length(D)){
  matriz <- read.csv(D[i])
  matriz <- as.data.frame(matriz)
  matriz <- matriz %>%
    group_by(Tribe) %>%
    summarise(across(everything(), sum, na.rm=TRUE))
  matriz_names <- matriz[1]
  matriz_names
  matriz_values <- matriz[,2:length(matriz)]
  final_values <- as.matrix(matriz_values)
  rownames(final_values) = matriz$Tribe
  tiff_filename <- paste(output_visweb,"plot", i, ".tiff", sep = "")
  tiff(filename = tiff_filename, width = 6, height = 6, units = "in", res = 600)
  ncolo <- length(table(final_values))
  col <- hcl.colors(ncolo-1, palette = "YlOrRd", rev = TRUE)
  rgb_cols <- c("#FFFFFFFF",col)
  visweb(final_values, 
         square="defined", 
         def.col=rgb_cols, 
         labsize = 1.5,
         box.border="grey")
  dev.off()
}
