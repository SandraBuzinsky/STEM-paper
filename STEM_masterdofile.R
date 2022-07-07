# WOMEN IN STEM MASTER SCRIPT --------------------------


# directory --------------------------------------------------------------------

if(Sys.info()[["user"]] == "sandr") project_file_path <- "C:/Users/sandr/Dropbox/STEM paper/Data"



# Libraries --------------------------------------------------------------------
listOfPackages <- c("sp","raster","readr","dplyr","parallel","pbmcapply",
                    "rgdal","rgeos","geosphere","sf","velox","broom","ggplot2","gdistance",
                    "data.table","ggpubr","reshape","doBy","readstata13","haven","ggmap",
                    "gtools","readxl","ggrepel","readr","plm","stargazer","xml2","mapsapi",
                    "leaflet","XML","tmap","lubridate","hrbrthemes","tidyr","stringr","lfe",
                    "RColorBrewer","osmdata","usethis","devtools","pander","ggsn",
                    "sjPlot","sandwich","miceadds","panelr","foreign","purrr", "ymd", "stringr",
                    "UsingR", "rdrobust")

invisible( {
  lapply(setdiff(listOfPackages, .packages(all = TRUE)), install.packages)
  lapply(intersect(listOfPackages, .packages(all = TRUE)), 
         function(z) library(z, character.only = T))
} )
