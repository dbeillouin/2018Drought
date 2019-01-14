x<-c("data.table","tidyverse","magrittr", 'rlist')
lapply(x, require, character.only = TRUE)
setwd("~/Documents/2018Drought/Raw_Climate")

LIST<-list.files(pattern = "NUTS3", recursive = TRUE)


Climate   = lapply(LIST, function(x) fread(x,header=TRUE , sep=";", dec=".", na.strings="NA"))%>%
            bind_rows()
