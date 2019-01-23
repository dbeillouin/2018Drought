x<-c("data.table","tidyverse","magrittr")
lapply(x, require, character.only = TRUE)

setwd("~/Documents/2018Drought/Raw_data_yield")

############## NUTS1 Scale  ###############

LIST<-list.files(path="~/Documents/2018Drought/Raw_data_yield",pattern = "apro", recursive = TRUE)

Europe_1975           <- fread(LIST[1], header=TRUE)
Europe_2018           <- fread(LIST[3], header=TRUE)
names(Europe_2018)[1]<- names(Europe_1975)[1]
Europe_NUTS1 <- merge(Europe_1975,Europe_2018, by=c("Icrops,strucpro,geo\\time"), all=TRUE)


Europe_NUTS1  %<>%      separate("Icrops,strucpro,geo\\time", c("Code_crops","Code_strucpro","Code_geo_time"), ",")
Europe_NUTS1  %<>%     gather(year, value, -Code_crops,-Code_strucpro,-Code_geo_time)

DD <- Europe_NUTS1 %>%  filter(value==":")
# on charge les fichiers de correspondances entre les codes. 
#https://ec.europa.eu/eurostat/cache/metadata/Annexes/apro_cp_esms_an1.pdf
Code_Crops    <-  read.delim("~/Documents/2018Drought/Raw_data_yield/Code_Crops.csv",sep=";", header=TRUE)
Europe_NUTS1  %<>%  left_join(., Code_Crops, by=c("Code_crops"))


Code_strucpro  <-  read.delim("~/Documents/2018Drought/Raw_data_yield/Code_strucpro.csv", sep=";", header=TRUE)
Code_strucpro %<>%  select(-"variable_origin")
Europe_NUTS1  %<>%  left_join(., Code_strucpro, by=c("Code_strucpro"))


Code_geo_time  <-  read.delim("~/Documents/2018Drought/Raw_data_yield/Code_NUTS2.csv")
Europe_NUTS1  %<>%  left_join(., Code_geo_time, by=c("Code_geo_time"))

DD<- Europe_NUTS1 %>%  filter(NUTS2=="Romania")
DD<- DD %>%  filter(year==2000)
DD<- DD %>%   filter(grepl("Tomatoes",Crop))
DD

Europe_NUTS1  %<>% select(-Code_crops, -Code_strucpro, - Code_geo_time)
Europe_NUTS1  %<>%  filter(!Crop=="???")
Europe_NUTS1  %<>%  filter(!value==":")
Europe_NUTS1  %<>%  filter(!value==": z")
Europe_NUTS1  %<>%  filter(!value=="0.00 n")
Europe_NUTS1  %<>%  filter(!value=="0.00")
Europe_NUTS1  <- distinct(Europe_NUTS1)



Europe_NUTS1[389036,]
Europe_NUTS1[389070,]


Europe_NUTS1  %<>% spread( variable,value)
names(Europe_NUTS1)[3]<- "NUTS1"

# un graphique pour vérifier
FR<- Europe_NUTS1 %>%  filter(NUTS1=="Italy")
FR<- FR %>%   filter(grepl("Durum wheat",Crop))
plot(FR$year, FR$Yield)

# Faisons le point sur les données blé.
DD<-  Europe_NUTS1 %>%   filter(grepl("wheat",Crop))
DD<-  Europe_NUTS1 %>%   filter(grepl("Common winter wheat and spelt",Crop))
reshape::cast(DD, year~NUTS1, length)

setwd("~/Documents/2018Drought")
fwrite(Europe_NUTS1, "Yield_Europe_NUTS1.csv")

