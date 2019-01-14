x<-c("data.table","tidyverse","magrittr")
lapply(x, require, character.only = TRUE)

setwd("~/Documents/2018Drought/data_yield")
# join yield data
LIST<-list.files(path="~/Documents/2018Drought/Raw_data_yield",pattern = "", recursive = TRUE)
fread(LIST[1])

# NUTS`
NUTS<-fread(LIST[4], header=TRUE)
setnames(NUTS, old= c("NUTS 3 ID (2010)","NUTS 3 2010 code and name"),
               new= c("NUTS3_CODE","NUTS3"))

######## Netherlands     @#####
NL           <- fread(LIST[2])
NL$Periods   <- sub("JJ00", "", NL$Periods)
setnames(NL, old = c("ArableCrops","Regions"   ,'Periods','AreaUnderCultivation_1','HarvestedArea_2',"GrossYieldPerHa_3","GrossYieldTotal_4"), 
             new = c("Crop",'NUTS3','Year'   ,'Area_S'                ,'Area'           ,"Yield"            ,"Production"))
NL          %<>%  select(-ID)
NL          %<>%  mutate_at(vars(Year,Area_S,Area,Yield,Production), as.numeric)
NL          %<>%  mutate(Country= "NL")
NL$Crop      <-   as.factor(NL$Crop) 
#description
# 28 crops
# 17 regions
# 25 ans : 1994-2018
# il me manque le code des région et des crops!
#NL         %<>% left_join(., NUTS, by='NUTS3_CODE')

head(NL)

# Netherlands : national mean
NL2<-fread(LIST[3])

# Poland
PO   <-fread(LIST[6])
setnames(PO, old = c("Name"   ,'Types of crop','Data coverage','Forms of ownership'), 
         new = c('Region',    'Crop'          ,'type'        ,'type2'              ))
PO   %<>% select(-"Measure Unit",-"Attribute", -"V10", -"Code", -"type2")
PO   %<>% spread(type, Value)
setnames(PO, old = c("Region", "crops from 1 ha", "area","harvest"), 
         new = c('NUTS3','Yield',"Area","Production"))
PO   %<>%  mutate(Country= "PO")
head(PO)

# Poland
PO2<-fread(LIST[7])
PO2 %<>% select(-"Code")
PO2<- melt(PO2)

PO2 %<>%  separate(variable, c("Crop","variable","type","year", "hh"), ";")
PO2 %<>%  select(-"hh")
PO2 %<>%  filter(!is.na(type))
PO2 %<>%  spread( variable, value)

head(PO2)
# Same data as PO!!!

# Italy
IT    <-fread(LIST[8])
setnames(IT, old = c("year", "yield","area", "production","crop"), 
         new = c('Year',"Yield","Area","Production",'Crop'))
IT   %<>%  mutate(Country= "IT")
head(IT)

# Sweden
SW    <-fread(LIST[9])
setnames(SW, old = c("year", "yield","crop"), 
         new = c('Year',"Yield",'Crop'))
setnames(SW, old = c("region"), 
         new = c('NUTS3'))
SW    %<>%  mutate(Country= "SW")


head(SW)

# France 

setwd("~/Documents/CLAND/files_txt")
temp = list.files(pattern="*16.txt")
FR   = lapply(temp, function(x) read.table(x,header=TRUE , sep=";", dec=".", na.strings="NA") %>%
                 mutate(sp=rep(sub('_data_1900-2016.txt', '', x))))%>% 
  bind_rows()%>%
  mutate(department  = tolower(department)) %>% 
  filter(complete.cases(yield))     

setnames(FR, old = c("department","year", "yield","sp", "area","production"), 
         new = c("NUTS3",'Year',"Yield",'Crop', 'Area',"Producion"))
FR   %<>%  mutate(Country= "FR")
head(FR)

## Germany
GE    <-fread(LIST[1])
setnames(GE, old = c("year", "yield","crop", "area","production"), 
         new = c('Year',"Yield",'Crop', 'Area',"Producion"))
GE    %<>%  mutate(Country= "GE")

# Merge all files
ALL<-bind_rows(list(FR,GE, NL, IT,PO,SW))
setwd("~/Documents/2018Drought")
fwrite(ALL, "Yield_Europe.csv")

